
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

#[cfg(test)]
mod test;

use common::{IsSurface, SmartMutex};
use desktop::{MouseButton, Key};
use std::{collections::{HashMap, VecDeque}, convert::{Infallible, identity}, future::{pending, poll_fn}, pin::Pin, sync::{Arc, Weak}, task};
use futures_lite::{FutureExt, future::block_on};

pub struct Config {
    //// This name will be registered in various places around the system.
    ///
    /// # Platform-Specific
    /// - Linux:
    ///     - Main thread name.
    ///     - DBus client name.
    pub appid: String,
    /// If `true` relevant signals will be intercepted and turned into
    /// `Quit` events. Otherwise signals will never be intercepted.
    ///
    /// Recommendation: Enable it only on release builds. When debugging it is
    /// annoying, since it prevents terminating a program which is stuck and
    /// cannot poll the event loop anymore, e.g. when in an infinite loop.
    pub intercept: bool,
}

pub struct App {
    executor: async_executor::LocalExecutor<'static>,
    eventloop: Arc<desktop::EventLoop>,
    handlers: AppEventHandlers,
    windows: SmartMutex<HashMap<desktop::Id, Weak<Window>>>,
    renderstate: SmartMutex<AppRenderState>,
}

impl App {

    pub fn run<R, H>(main: H, config: Config) -> Result<R, desktop::EvlError>
        where H: AsyncFnOnce(Arc<Self>) -> R + 'static,
              R: 'static {

            let config2 = desktop::EvlConfig {
                appid: config.appid.clone(),
                intercept: config.intercept,
            };

            desktop::EventLoop::run(config2, |eventloop| {

                let renderer = render::Renderer::new(&*eventloop)
                    .map_err(desktop::EvlError::anyerror)?;

                let renderstate = AppRenderState {
                    shaper: render::GeometryShaper::new(),
                    atlas: render::TextureAtlas::new(&renderer),
                    renderer,
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
                // treated as fatal and make `App::run` return.
                let eventloop = this.executor.spawn(async move {
                    let result = Self::eventloop(this2).await;
                    Err(result.unwrap_err())
                });

                // This is the task running the user code.
                let main = this.executor.spawn(async move {
                    let result = main(this3).await;
                    Ok(result)
                });

                let futures = this.executor.run(eventloop.or(main));

                block_on(futures)

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
                        .expect("new event after drop")
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
            &self.inner.id()
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
            texture: graphics::Texture::new(&renderer.gp, inner.size(), None),
            surface: graphics::Surface::new(&renderer.gp, &inner),
            instances: Default::default(),
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
            this.inner.id(),
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
                windowstate.surface.resize(size);
                windowstate.texture.resize(size, None);
            },

            WindowEvent::Redraw => {

                // Rendering a widget looks as follows.
                // 1. Clear old data and create a blank `Space`.
                // 2. Let the widget tree render into the `Space`.
                // 3. Read the data and render it onto the window.

                windowstate.clear();

                let size = self.inner.size();

                let space = Space {
                    state: &mut *windowstate,
                    size: Vec2::new(abs(size.w as i16), abs(size.h as i16)),
                    offset: Vec2::ZERO,
                };

                // This will render the whole tree.

                self.content.lock()
                    .action(Action::Render { space });

                // Now we can read back and render the data.

                let AppRenderState { shaper, renderer, atlas } = &mut *appstate;
                let WindowRenderState {
                    ref mut surface,
                    ref mut texture,
                    ref curves,
                    ref instances,
                    ..
                } = *windowstate;

                let curves = &curves.data;
                let vertices = shaper.process(curves);

                let drawable = render::DrawableGeometry {
                    source: &[vertices],
                    instances,
                };

                renderer.draw(&drawable, &atlas, texture);

                surface.blit(texture);
                surface.swap();


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
    MouseScroll { dx: i16, dy: i16 },

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
    renderer: render::Renderer,
    atlas: render::TextureAtlas,
}

struct WindowRenderState {
    // Geometry Buffers:
    geometries: RenderStateGeometries,
    vertices: RenderStateVertices,
    curves: RenderStateCurves,
    instances: Vec<render::Instance>,
    // Intermediate Texture and Surface:
    texture: graphics::Texture,
    surface: graphics::Surface,
}

impl WindowRenderState {
    pub fn clear(&mut self) {
        self.geometries.data.clear();
        self.vertices.data.clear();
        self.curves.data.clear();
        self.instances.clear();
    }
}

#[derive(Default)]
struct RenderStateGeometries {
    /// Widget-added geometries.
    data: Vec<Arc<render::VertexGeometry>>,
}

#[derive(Default)]
struct RenderStateVertices {
    /// Widget-added vertices.
    data: render::VertexGeometry,
}

#[derive(Default)]
struct RenderStateCurves {
    // Widget-added curves.
    // Will be triangulated later on.
    data: render::CurveGeometry,
}

pub struct Space<'a> {
    state: &'a mut WindowRenderState,
    offset: Vec2,
    size: Size,
}

impl<'a> Space<'a> {

    pub fn data(&mut self, data: Data) -> SpaceKey {

        match data {
            Data::Curves(it) => {

                let target = &mut self.state.curves.data;

                let start = target.points.len() as u16;
                target.points.extend_from_slice(it);
                let end = target.points.len() as u16;

                target.shapes.push(common::Shape::new(start..end));
                let idx = (target.shapes.len() - 1) as u16;

                // SpaceKey { index, kind: SpaceKeyKind::Curves }
                SpaceKey::Curves { shape: idx }

            },
            Data::Vertices(it) => {

                let target = &mut self.state.vertices.data;

                let start = target.vertices.len() as u16;
                target.vertices.extend_from_slice(it);
                let end = target.vertices.len() as u16;

                target.shapes.push(common::Shape::new(start..end));
                let idx = (target.shapes.len() - 1) as u16;

                SpaceKey::Vertices { shape: idx }

            },
            Data::Geometry(it) => {

                let items = &mut self.state.geometries.data;
                items.push(it);
                let geometry = (items.len() + 1) as u16;
                //                        ^^^^
                // we need to adjust because when creating the `DrawableGeometry`
                // the first two geometries will be for our own curves and vertices.

                SpaceKey::Geometry { geometry }

            },
        }

    }

    #[track_caller]
    pub fn instance(&mut self, key: SpaceKey, i: Instance) {

        let new = Self::transform(Rect { point: i.pos, size: i.size }, self.offset, self.size);

        let inner = render::Instance {
            target: key.target(),
            pos: new.point.into(),
            size: new.size.into(),
            texture: i.texture,
        };

        self.state.instances.push(inner);

    }

    pub fn child<'s>(&'s mut self, offset: Vec2, size: Size) -> Space<'s> {

        let new = Self::transform(Rect { point: offset, size }, self.offset, self.size);

        Space {
            state: self.state,
            offset: new.point,
            size: new.size
        }

    }

    fn transform(input: Rect, toffset: Vec2, tscale: Vec2) -> Rect {
        let point = Vec2 {
            x: toffset.x + match input.point.mx {
                Measure::Absolute => input.point.x,
                Measure::Relative => Self::rescale(input.point.x, tscale.x)
            },
            y: toffset.x + match input.point.my {
                Measure::Absolute => input.point.y,
                Measure::Relative => Self::rescale(input.point.y, tscale.y)
            },
            mx: Measure::Absolute,
            my: Measure::Absolute
        };
        let size = Size {
            x: match input.size.mx {
                Measure::Absolute => input.size.x,
                Measure::Relative => Self::rescale(input.size.x, tscale.x)
            },
            y: match input.size.my {
                Measure::Absolute => input.size.y,
                Measure::Relative => Self::rescale(input.size.y, tscale.y)
            },
            mx: Measure::Absolute,
            my: Measure::Absolute
        };
        Rect { point, size }
    }

    /// Computes value% * scale%, but using units per 5000.
    ///
    /// So if value = 1,250 and scale = 2,500 this returns 625, equivalent to
    ///       value = 25%       sccale = 50%       returns 12.5%
    fn rescale(value: i16, scale: i16) -> i16 {
        ((value as isize * scale as isize) / 5000isize) as i16
    }

}
// const FULL: u16 = size("100%");


pub enum Data<'a> {
    Curves(&'a [common::CurvePoint]),
    Vertices(&'a [render::PartialVertex]),
    Geometry(Arc<render::VertexGeometry>)
}

#[derive(Clone, Copy)]
pub enum SpaceKey {
    Curves       { shape: u16 },
    Vertices     { shape: u16 },
    Geometry     { geometry: u16 },
    GeometryFull { geometry: u16, shape: u16 }
}

impl SpaceKey {
    #[track_caller]
    pub fn shape(self, shape: u16) -> Self {
        if let Self::Geometry { geometry } = self {
            Self::GeometryFull { geometry, shape }
        } else {
            panic!("Only used for geometry `SpaceKey`.")
        }
    }
    #[track_caller]
    pub fn target(self) -> render::GeometryTarget {
        use render::GeometryTarget;
        match self {
            Self::Curves       { shape }           => GeometryTarget { geometry: 0, shape },
            Self::Vertices     { shape }           => GeometryTarget { geometry: 1, shape },
            Self::GeometryFull { geometry, shape } => GeometryTarget { geometry, shape },
            Self::Geometry     { .. }              => panic!("Incomplete `SpaceKey`."),
        }
    }
}

#[derive(Clone, Copy)] // TODO: derive all necessary traits on all types (also impl a good Debug)
pub enum Measure {
    Absolute,
    Relative,
}

#[derive(Clone, Copy)]
pub struct Rect {
    point: Point,
    size: Size
}

#[derive(Clone, Copy)]
pub struct Vec2 {
    x: i16,
    y: i16,
    mx: Measure,
    my: Measure
}

impl Vec2 {
    const ZERO: Self = Self::new(abs(0), abs(0));
    pub const fn new((x, mx): (i16, Measure), (y, my): (i16, Measure)) -> Self {
        Self { x, y, mx, my }
    }
}

impl From<Vec2> for common::LogicalPoint {
    fn from(it: Vec2) -> Self {
        Self::new(it.x, it.y)
    }
}

impl From<Vec2> for common::LogicalSize {
    fn from(it: Vec2) -> Self {
        Self::new(it.x as u16, it.y as u16)
    }
}

pub type Point = Vec2;
pub type Size  = Vec2;

pub const fn abs(val: i16) -> (i16, Measure) {
    (val, Measure::Absolute)
}

pub const fn rel(val: i16) -> (i16, Measure) {
    (val, Measure::Relative)
}

pub struct Instance {
    /// offsetX, offsetY
    pub pos: Vec2,
    /// Size of the shape in logical pixels.
    pub size: Size,
    // Texture information.
    pub texture: render::TextureKind,
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
    event: Option<T>,
    wakers: Vec<Option<task::Waker>>,
    tick: u16,
}

impl<T: Clone> EventBroadcaster<T> {

    pub const fn new() -> Self {
        Self {
            inner: SmartMutex::new(EventBroadcasterInner {
                event: None,
                wakers: Vec::new(),
                tick: 1,
            }),
        }
    }

    pub fn send(&self, event: T) {

        let mut inner = self.inner.lock();

        inner.event = Some(event);

        inner.wakers.iter()
            .flat_map(identity)
            .for_each(task::Waker::wake_by_ref);

    }

    pub fn listen<'s>(&'s self) -> BroadcastFuture<'s, T> {

        let mut inner = self.inner.lock();

        let slot = inner.wakers.iter().position(Option::is_none).unwrap_or_else(|| {
            inner.wakers.push(None);
            inner.wakers.len()
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
    tick: u16,
}

impl<'a, T: Clone> Drop for BroadcastFuture<'a, T> {
    fn drop(&mut self) {
        // Make our slot available again.
        let mut inner = self.channel.inner.lock();
        inner.wakers[self.slot as usize] = None;
    }
}

impl<'a, T: Clone> BroadcastFuture<'a, T> {

    pub async fn next(&mut self) -> T {
        poll_fn(|cx| self.poll(cx)).await
    }

    pub fn poll(&mut self, cx: &mut task::Context) -> task::Poll<T> {

        let mut inner = self.channel.inner.lock();

        // Read the event if it is new.
        if let Some(ref event) = inner.event && inner.tick > self.tick {
            // This event is now no longer new.
            self.tick = inner.tick;
            task::Poll::Ready(event.clone())
        } else {
            inner.wakers[self.slot as usize]
                .get_or_insert_with(|| cx.waker().clone())
                .clone_from(cx.waker());
            task::Poll::Pending
        }

    }


}

#[test]
fn event_broadcaster() {

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

    let evb = EventBroadcaster::new();

    let mut listener1 = evb.listen();
    evb.send(0);
    let mut listener2 = evb.listen();
    evb.send(1);
    let mut listener3 = evb.listen();

    block_on(async move {

        // Both should receive only events that
        // happen after their creation.

        let mut dummy = task::Context::from_waker(task::Waker::noop());

        assert_eq!((&mut listener1).next().await, 1);
        assert_eq!((&mut listener2).next().await, 1);
        assert_eq!((&mut listener3).poll(&mut dummy), task::Poll::Pending);

    });


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
