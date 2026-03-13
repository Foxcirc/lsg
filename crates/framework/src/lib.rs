
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

#[cfg(test)]
mod test;

use common::{LogicalSize, SmartMutex};
use desktop::{MouseButton, ScrollAxis, Key};
use std::{collections::{HashMap, VecDeque}, convert::{Infallible, identity}, pin::Pin, sync::{Arc, Weak}, task};
use futures_lite::{FutureExt, future::block_on};

pub struct Config {
    pub appid: String,
}

pub struct App {
    executor: async_executor::LocalExecutor<'static>,
    eventloop: Arc<desktop::EventLoop>,
    handlers: AppEventHandlers,
    windows: SmartMutex<HashMap<desktop::WindowId, Weak<Window>>>,
    renderstate: SmartMutex<AppRenderState>,
}

impl App {

    pub fn run<R, H>(main: H, config: Config) -> Result<R, desktop::EvlError>
        where H: AsyncFnOnce(Arc<Self>) -> R + 'static,
              R: 'static {

            let config2 = desktop::EventLoopConfig {
                appid: config.appid.clone(),
            };

            desktop::EventLoop::run(config2, |eventloop| {

                let renderstate = AppRenderState {
                    shaper: render::GeometryShaper::new(),
                    renderer: render::GlRenderer::new(&*eventloop)
                        .map_err(desktop::EvlError::anyerror)?,
                };

                let this = Arc::new(Self {
                    executor: async_executor::LocalExecutor::new(),
                    windows: SmartMutex::new(HashMap::new()),
                    renderstate: SmartMutex::new(renderstate),
                    handlers: AppEventHandlers::default(),
                    eventloop,
                });

                let this2 = Arc::clone(&this);
                let this3 = Arc::clone(&this);

                // This task will pump the event-loop. Any errors are
                // treated as fatal and forewarded immediatly.
                let eventloop = this.executor.spawn(async move {
                    let result = Self::eventloop(this2).await;
                    Err(result.unwrap_err())
                });

                // This is the task running the user code.
                let main = this.executor.spawn(async move {
                    let result = main(this3).await;
                    Ok(result)
                });

                block_on(this.executor.run(
                    eventloop.or(main)
                ))

            }).and_then(identity)

    }

    async fn eventloop(this: Arc<Self>) -> Result<Infallible, desktop::EvlError> {

        loop {

            let event = this.eventloop.next().await?;

            match event {

                desktop::Event::Quit { reason } => {
                    this.handlers.quit.send(reason)
                },

                desktop::Event::Window { id, event } => {
                    // Foreward the event to the window, if it wasn't dropped yet.
                    this.windows.lock().get(&id)
                        .and_then(Weak::upgrade)
                        .expect("window removed prematurely")
                        .handle(event)?
                },

                _ => (),

            }

        }

    }

    pub fn spawn<F>(&self, fut: F)
        where F: Future<Output = ()> + 'static {

            self.executor.spawn(fut).detach();

    }

    pub fn connect<T, E, L, F>(self: &Arc<Self>, data: &Arc<T>, listener: L, handler: F)
        where F: AsyncFn((&Arc<App>, &Arc<T>, E)) + 'static,
              L: Fn(&T) -> BroadcastFuture<E> + 'static,
              E: Clone,
              T: 'static
        {

            let data2 = Arc::clone(data);
            let app2 = Arc::clone(&self);

            self.spawn(async move {
                let mut source = listener(&data2);
                loop { handler(
                    (&app2, &data2, source.next().await)
                ).await }
            });

    }

    pub fn quit(&self) {
        self.handlers.quit.send(desktop::QuitReason::Program)
    }

    pub fn quitted<'s>(&'s self) -> BroadcastFuture<'s, desktop::QuitReason> {
        self.handlers.quit.listen()
    }

}

#[repr(transparent)]
struct DynamicWidget {
    pub inner: Arc<dyn Widget>,
}

impl DynamicWidget {
    fn new<W: Widget + 'static>(inner: Arc<W>) -> Self {
        Self { inner }
    }
}

impl Default for DynamicWidget {
    fn default() -> Self {
        Self { inner: Arc::new(()) }
    }
}

impl Widget for DynamicWidget {
    fn action(&self, action: Action) {
        self.inner.action(action);
    }
}

#[derive(Default)]
struct AppEventHandlers {
    quit: EventBroadcaster<desktop::QuitReason>,
}

pub struct Window {
    app: Arc<App>,
    inner: desktop::Window,
    handlers: WindowEventHandlers,
    content: SmartMutex<DynamicWidget>,
    renderstate: SmartMutex<WindowRenderState>,
}

impl Drop for Window {
    fn drop(&mut self) {
        // Remove ourselves from the window list.
        self.app.windows.lock().remove(
            &self.inner.id
        );
    }
}

impl Window {

    pub fn new(app: &Arc<App>) -> Result<Arc<Self>, desktop::EvlError> {

        let inner = desktop::Window::new(&app.eventloop);

        let renderer = &app.renderstate.lock().renderer;

        let renderstate = SmartMutex::new(WindowRenderState {
            geometries: Default::default(),
            vertices: Default::default(),
            curves: Default::default(),
            surface: render::GlSurface::new(renderer, &inner)
                .map_err(desktop::EvlError::anyerror)?,
        });

        let this = Arc::new(Self {
            app: Arc::clone(&app),
            handlers: Default::default(),
            content: Default::default(),
            renderstate,
            inner,
        });

        // Insert ourselves into the window list.
        app.windows.lock().insert(
            this.inner.id,
            Arc::downgrade(&this)
        );

        Ok(this)

    }

    fn handle(&self, event: desktop::WindowEvent) -> Result<(), desktop::EvlError> {

        use desktop::WindowEvent;

        let mut windowstate = self.renderstate.lock();
        let mut appstate = self.app.renderstate.lock();

        match event {

            // Event handlers.

            WindowEvent::ShouldClose => self.handlers.closed.fire(),

            // Special events.

            WindowEvent::Resize { size, .. } => {
                windowstate.surface.resize(&appstate.renderer, size)
                    .map_err(desktop::EvlError::anyerror)?
            },

            WindowEvent::Redraw => {

                // Rendering a widget looks as follors.
                // 1. Clear old data and create a blank `Space`.
                // 2. Let the widget tree render into the `Space`.
                // 3. Read the data and render it onto the window.

                windowstate.clear();

                let space = Space {
                    state: &mut *windowstate,
                    size: Size::abs(5000, 5000),
                    offset: Position::abs(0, 0)
                };

                // This will render the whole tree.

                self.content.lock()
                    .action(Action::Render { space });

                // Now we can read back and render the data.

                let AppRenderState { shaper, renderer } = &mut *appstate;

                let curves = &windowstate.curves.data;
                let vertices = shaper.process(curves);

                let drawable = render::DrawableGeometry {
                    source: &[vertices],
                    instances: &windowstate.curves.instances,
                };

                renderer.draw(&drawable, &windowstate.surface)
                    .map_err(desktop::EvlError::anyerror)?;

            },

            _ => (),

        }

        Ok(())

        // let action = Action::Event { event };

        // Propagate the action through the widget tree.
        // self.content.lock().action(action);

    }

    // pub fn show(&self, size: LogicalSize) {
    //     // self.inner.
    // }

    pub fn content<W: Widget + 'static>(&self, widget: Arc<W>) {
        self.content.set(DynamicWidget::new(widget));
    }

    pub fn closed<'s>(&'s self) -> BroadcastFuture<'s, ()> {
        self.handlers.closed.listen()
    }

}

#[derive(Default)]
struct WindowEventHandlers {
    closed: EventBroadcaster<()>,
}

pub trait Widget {
    fn action(&self, action: Action);
}

impl Widget for () {
    fn action(&self, _: Action) {}
}

/*

Some cool ideas:

let rect = ForceResize::build().size((5000, 5000))
    .inner(Rect::build().color(Color::Red));

let rect = ForceResize::new((5000, 5000), "rect-red-round25")

let rect = ForceResize((5000, 5000), Rect().red().round25());

let rect = widget("ForceResize(5000x5000, Rect(red, round25))");

 */

pub enum Action<'a> {

    Render { space: Space<'a> },

    MouseMotion { x: u16, y: u16 },
    MouseDown { x: u16, y: u16, button: MouseButton },
    MouseUp { x: u16, y: u16, button: MouseButton },
    MouseScroll { axis: ScrollAxis, value: i16 },

    Unhover,
    Unfocus,

    KeyDown { key: Key, repeat: bool },
    KeyUp { key: Key },

    TextInput { chr: char },
    TextCompose { chr: char },
    TextComposeCancel,

}

struct AppRenderState {
    shaper: render::GeometryShaper,
    renderer: render::GlRenderer,
}

struct WindowRenderState {
    // Buffers to store render data:
    geometries: RenderStateGeometries,
    vertices: RenderStateVertices,
    curves: RenderStateCurves,
    // Surface:
    surface: render::GlSurface,
}

impl WindowRenderState {
    pub fn clear(&mut self) {
        self.geometries.data.clear();
        self.geometries.instances.clear();
        self.vertices.data.clear();
        self.vertices.instances.clear();
        self.curves.data.clear();
        self.curves.instances.clear();
    }
}

#[derive(Default)]
struct RenderStateGeometries {
    /// Widget-added geometries.
    data: Vec<Arc<render::VertexGeometry>>,
    /// Indexes into `data`.
    instances: Vec<render::Instance>,
}

#[derive(Default)]
struct RenderStateVertices {
    /// Widget-added vertices.
    data: render::VertexGeometry,
    /// Indexes into `data`.
    instances: Vec<render::Instance>,
}

#[derive(Default)]
struct RenderStateCurves {
    // Widget-added curves.
    // Will be triangulated later on.
    data: render::CurveGeometry,
    /// Indexes into `data`.
    instances: Vec<render::Instance>,

}

pub struct Space<'a> {
    state: &'a mut WindowRenderState,
    offset: Position,
    size: Size,
}

impl<'a> Space<'a> {

    pub fn curves(&mut self, data: &[common::CurvePoint]) -> SpaceKey {

        let geometry = &mut self.state.curves.data;

        // Insert the curve data.
        let start = geometry.points.len() as u16;
        geometry.points.extend_from_slice(data);
        let end = geometry.points.len() as u16;

        // Create the shape.
        geometry.shapes.push(common::Shape::new(start..end));

        SpaceKey {
            kind: SpaceKeyKind::Curves,
            index: geometry.shapes.len() as u16 - 1,
        }

    }

    pub fn vertices(&mut self, data: &[render::PartialVertex]) -> SpaceKey {

        // let geometry = Arc::get_mut(&mut self.state.vertices.data)
        //     .expect("exclusive ownership of vertex geometry");

        let geometry = &mut self.state.vertices.data;

        // Insert the curve data.
        let start = geometry.vertices.len() as u16;
        geometry.vertices.extend_from_slice(data);
        let end = geometry.vertices.len() as u16;

        // Create the shape.
        geometry.shapes.push(common::Shape::new(start..end));

        SpaceKey {
            kind: SpaceKeyKind::Vertices,
            index: geometry.shapes.len() as u16 - 1,
        }

    }

    pub fn geometry(&mut self, geometry: Arc<render::VertexGeometry>) -> GeometryKey {
        let items = &mut self.state.geometries.data;
        items.push(geometry);
        GeometryKey {
            index: items.len() as u16 - 1
        }
    }

    pub fn instance(&mut self, key: SpaceKey, instance: Instance) {

        let pos  = self.apply_transform_pos(instance.pos);
        let size = self.apply_transform_size(instance.size);

        let inner = render::Instance {
            target: render::GeometryTarget { geometry: 0, shape: key.index },
            pos: pos.into(),
            size: size.into(),
        };

        match key.kind {
            SpaceKeyKind::Curves => self.state.curves.instances.push(inner),
            SpaceKeyKind::Vertices => self.state.vertices.instances.push(inner),
        }
    }

    pub fn targeted(&mut self, key: GeometryKey, shape: u16, instance: Instance) {

        let pos  = self.apply_transform_pos(instance.pos);
        let size = self.apply_transform_size(instance.size);

        let inner = render::Instance {
            target: render::GeometryTarget { geometry: key.index, shape },
            pos: pos.into(),
            size: size.into(),
        };

        self.state.geometries.instances.push(inner);

    }

    pub fn child<'s>(&'s mut self, offset: Position, size: Size) -> Space<'s> {

        let offset = self.apply_transform_pos(offset);
        let size = self.apply_transform_size(size);

        Space {
            state: self.state,
            offset,
            size
        }

    }

    fn apply_transform_pos(&self, pos: Position) -> Position {
        match pos.measure {
            Measure::Absolute => Position::abs(
                pos.x + self.offset.x,
                pos.y + self.offset.y
            ),
            Measure::Relative => Position::abs(
                Self::rescale_value(pos.x, self.size.w as i16) + self.offset.x,
                Self::rescale_value(pos.y, self.size.h as i16) + self.offset.y,
            ),
        }
    }

    fn apply_transform_size(&self, size: Size) -> Size {
        match size.measure {
            Measure::Absolute => size,
            Measure::Relative => Size::abs(
                Self::rescale_value(size.w as i16, self.size.w as i16) as u16,
                Self::rescale_value(size.h as i16, self.size.h as i16) as u16,
            ),
        }
    }

    /// Computes value% * scale%, but using units per 5000.
    ///
    /// So if value = 1,250 and scale = 2,500 this returns 625, equivalent to
    ///       value = 25%       sccale = 50%       returns 12.5%
    fn rescale_value(value: i16, scale: i16) -> i16 {
        ((value as isize * scale as isize) / 5000isize) as i16
    }

}

// pub const fn size(input: &str) -> u16 {

//     let mut count = 0;

//     for chr in input.chars() {
//         if chr.is_ascii_digit() { count += 1 }
//     }

//     let unit = input.get(count..count + 1)
//         .unwrap_or_default();

//     let percent = unit == "%";

//     let mut number = u16::from_str_radix(&input[..count], 10)
//         .expect("not a valid number");

//     if percent {
//         number *= 100;
//     }

//     number

// }

// const FULL: u16 = size("100%");

pub struct SpaceKey {
    kind: SpaceKeyKind,
    index: u16,
}

enum SpaceKeyKind {
    Curves,
    Vertices,
}

pub struct GeometryKey {
    index: u16,
}

#[derive(Clone, Copy)] // TODO: derive all necessary traits on all types (also impl a good Debug)
pub enum Measure {
    Absolute,
    Relative,
}

#[derive(Clone, Copy)]
pub struct Position {
    x: i16,
    y: i16,
    measure: Measure,
}

impl Position {
    pub fn abs(x: i16, y: i16) -> Self { Self { x, y, measure: Measure::Absolute } }
    pub fn rel(x: i16, y: i16) -> Self { Self { x, y, measure: Measure::Relative } }
}

impl From<Position> for Size {
    #[track_caller]
    fn from(it: Position) -> Self {
        debug_assert!(it.x > 0 && it.y > 0);
        Self {
            w: it.x as u16,
            h: it.y as u16,
            measure: it.measure,
        }
    }
}

impl From<Position> for common::LogicalPoint {
    fn from(it: Position) -> Self {
        Self::new(it.x, it.y)
    }
}

#[derive(Clone, Copy)]
pub struct Size {
    w: u16,
    h: u16,
    measure: Measure,
}

impl Size {
    pub fn abs(w: u16, h: u16) -> Self { Self { w, h, measure: Measure::Absolute } }
    pub fn rel(w: u16, h: u16) -> Self { Self { w, h, measure: Measure::Relative } }
}

impl From<Size> for Position {
    #[track_caller]
    fn from(it: Size) -> Self {
        Self {
            x: it.w as i16,
            y: it.h as i16,
            measure: it.measure,
        }
    }
}

impl From<Size> for common::LogicalSize {
    fn from(it: Size) -> Self {
        Self::new(it.w, it.h)
    }
}

pub struct Instance {
    /// offsetX, offsetY
    pub pos: Position,
    /// Size of the shape in logical pixels.
    pub size: Size,
}

pub struct EventBroadcaster<T: Clone> {
    inner: SmartMutex<EventBroadcasterInner<T>>,
}

impl<T: Clone> Default for EventBroadcaster<T> {
    fn default() -> Self {
        Self::new()
    }
}

struct EventBroadcasterInner<T: Clone> {
    events: VecDeque<Event<T>>,
    wakers: Vec<Option<task::Waker>>,
    listeners: u16, // currently active listeners
    tick: u16, // incremental counter, used to avoid double-reading an event
}

#[derive(Clone)]
struct Event<T: Clone> {
    value: T,
    pending: u16, // listeners that have yet to respond
    tick: u16, // which tick this event belongs to
}

impl<T: Clone> EventBroadcaster<T> {

    pub const fn new() -> Self {
        Self {
            inner: SmartMutex::new(EventBroadcasterInner {
                events: VecDeque::new(),
                wakers: Vec::new(),
                listeners: 0,
                tick: 0,
            }),
        }
    }

    /// Returns `true` if this channel has any listeners.
    pub fn active(&self) -> bool {
        self.inner.with(|it| it.listeners > 0)
    }

    pub fn len(&self) -> usize {
        self.inner.with(|it| it.events.len())
    }

    pub fn send(&self, event: T) {

        // Don't send anything if there are no listeners, as
        // this would create events which can never be consumed.
        if !self.active() {
            return
        }

        let mut inner = self.inner.lock();

        inner.tick = inner.tick.wrapping_add(1);

        let pending = inner.listeners;
        let tick = inner.tick;
        inner.events.push_back(Event {
            value: event,
            pending,
            tick,
        });

        let alive = inner.wakers.iter()
            .filter_map(Option::as_ref);

        for waker in alive {
            waker.wake_by_ref()
        }

    }

    pub fn listen<'s>(&'s self) -> BroadcastFuture<'s, T> {

        let mut inner = self.inner.lock();

        inner.listeners += 1;

        let slot = inner.wakers.iter().enumerate()
            .find(|(.., it)| it.is_none())
            .map(|(idx, ..)| idx)
            .unwrap_or_else(|| {
                inner.wakers.push(None);
                inner.wakers.len() - 1
            });

        BroadcastFuture {
            channel: self,
            slot: slot as u16,
            tick: inner.tick,
        }

    }

}

impl EventBroadcaster<()> {
    /// Convenience method for events with no data.
    pub fn fire(&self) {
        self.send(());
    }
}

pub struct BroadcastFuture<'a, T: Clone> {
    channel: &'a EventBroadcaster<T>,
    slot: u16,
    tick: u16, // only acknowledge events newer then this tick
}

impl<'a, T: Clone> Drop for BroadcastFuture<'a, T> {
    fn drop(&mut self) {
        let mut inner = self.channel.inner.lock();
        inner.wakers[self.slot as usize] = None;
        inner.listeners -= 1;
    }
}

impl<'a, T: Clone> BroadcastFuture<'a, T> {
    pub async fn next(&mut self) -> T {
        self.await
    }
}

impl<'a, T: Clone> Future for &mut BroadcastFuture<'a, T> {

    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<T> {

        let this = self.get_mut();
        let mut inner = this.channel.inner.lock();

        let maybe = inner.events.iter_mut()
            .find(|it| it.tick > this.tick);

        // if we found an event, read it now

        if let Some(it) = maybe {

            // this event was now read by us
            this.tick = it.tick;
            it.pending -= 1;

            let expired = it.pending == 0;

            let result = it.value.clone();

            if expired {
                // remove the event if it was consumed by all listeners
                drop(inner.events.pop_front());
            }

            task::Poll::Ready(result)

        } else {

            let old = &mut inner.wakers[this.slot as usize];
            let new = cx.waker();

            let same = old.as_mut()
                .map(|it| it.will_wake(new))
                .unwrap_or_default();

            if !same {
                *old = Some(new.clone());
            }

            task::Poll::Pending

        }

    }

}

#[test]
fn channels() {

    use futures_lite::future::block_on;

    // let single = EventChannel::new();

    // single.send(1);
    // single.send(2);
    // single.send(3);

    // block_on(async move {
    //     assert_eq!((&single).await, 1);
    //     assert_eq!((&single).await, 2);
    //     assert_eq!((&single).await, 3);
    // });

    let multi = EventBroadcaster::new();

    multi.send(0);

    let mut listener1 = multi.listen();

    multi.send(1);

    let mut listener2 = multi.listen();

    multi.send(2);
    multi.send(3);

    block_on(async move {

        // Both should receive only events that
        // happen after their creation.

        // listener1:
        assert_eq!((&mut listener1).await, 1);
        assert_eq!((&mut listener1).await, 2);
        assert_eq!((&mut listener1).await, 3);

        // listener2:
        assert_eq!((&mut listener2).await, 2);
        assert_eq!((&mut listener2).await, 3);

    });

    // the listeners were moved and dropped
    assert_eq!(multi.active(), false);
    assert_eq!(multi.len(), 0);


}

/*

    Needed features:
    - Add shape as list of points
    - Add shape as vertices
    - Create (many) instances of one shape

    fn action(&self, action: Action) {

        if let Action::Draw(ctx) = action {

            let cached = ...;

            ctx.geometry.add(cached or shape data, instace);

            ctx.child(|inner| {
                child.handle(Action::Draw { ctx: inner });
            })

            let sub = ctx.sub(position, size);
            child.handle(Action::Draw { ctx: sub });

            // impl DrawContext:
            fn sub(&self, size, position) -> &mut Self {
                &mut Self {
                    vertex geometry: &mut self.VertexGeometry,
                    curve geom: ...,
                    self offset + position offset,
                    self factor + size factor,
                }
            }

        }

    }

    fn main() {
        lsg::run(app)
    }

    #[test]
    fn test() {
        lsg::simulate(app, async |sim| {

            let window = sim.window("window-zero");
            window.click("button-counter").await;

            // let textbox = window.content.inner.items[1];
            let textbox = window.widget("textbox-counter");

            let text = textbox.inner.get();

            assert!(text == "1");

        });
    }

    async fn app(app: lsg::App) {

        let window = lsg::Window::new(&ev);
        window.id("window-zero");

        window.content.set(Widget);

        app.connect(&button, Button::leftclicked, async || {
            counter.update(|it| *it += 1);
            text.inner.set(format!("{counter}"));
            app.redraw(&text);
        });

        app.spawn(async {
            loop {
                button.leftclicked().await;
            }
        });

        button.leftclicked(&ev, async || {
            counter.update(|it| *it += 1);
            text.inner.set(format!("{counter}"));
            ev.redraw(&text);
        });

        window.closed().await;

    }));

 */
