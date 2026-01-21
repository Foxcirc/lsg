
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

use common::SmartMutex;

use std::{collections::HashMap, convert::{Infallible, identity}, sync::{Arc, Weak}};
use futures_lite::{FutureExt, future::block_on};

#[cfg(test)]
mod test;

pub use desktop::{
    MouseButton,
    ScrollAxis,
    Key,
};

pub struct Config {
    pub appid: String,
}

pub struct App {
    executor: async_executor::LocalExecutor<'static>,
    eventloop: Arc<desktop::EventLoop>,
    windows: SmartMutex<HashMap<desktop::WindowId, Weak<Window>>>,
    handlers: AppEventHandlers,
}

impl App {

    pub fn run<R, H>(main: H, config: Config) -> Result<R, desktop::EvlError>
        where H: AsyncFnOnce(Arc<Self>) -> R + 'static,
              R: 'static {

            let config2 = desktop::EventLoopConfig {
                appid: config.appid.clone(),
            };

            desktop::EventLoop::run(config2, |eventloop| {

                let this = Arc::new(Self {
                    executor: async_executor::LocalExecutor::new(),
                    windows: SmartMutex::new(HashMap::new()),
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
                        .inspect(|it| it.handle(event));
                },

                _ => (),

            }

        }

    }

    pub fn spawn<F>(&self, fut: F)
        where F: Future<Output = ()> + 'static {

            self.executor.spawn(fut).detach();

    }

    // pub fn connect<T, E, L, F>(&self, data: &Arc<T>, listener: L, handler: F)
    //     where L: AsyncFn(&T) -> E + 'static,
    //           F: AsyncFn(E) + 'static,
    //           T: 'static
    //     {

    //         let cloned = Arc::clone(data);

    //         self.spawn(async move {
    //             loop { handler(listener(&*cloned).await).await }
    //         });

    // }

    pub fn quit(&self) {
        self.handlers.quit.send(desktop::QuitReason::Program)
    }

    pub async fn quitted(&self) -> desktop::QuitReason {
        self.handlers.quit.listen().await
    }

}

#[derive(Default)]
struct AppEventHandlers {
    quit: common::EventChannel<desktop::QuitReason>,
}

pub struct Window {
    app: Arc<App>,
    inner: desktop::Window,
    handlers: WindowEventHandlers,
    pub content: SmartMutex<Arc<dyn Widget>>,
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

    pub fn new(app: &Arc<App>) -> Arc<Self> {

        let this = Arc::new(Self {
            app: Arc::clone(&app),
            inner: desktop::Window::new(&app.eventloop),
            handlers: WindowEventHandlers::default(),
            content: SmartMutex::new(Arc::new(()))
        });

        // Insert ourselves into the window list.
        app.windows.lock().insert(
            this.inner.id,
            Arc::downgrade(&this)
        );

        this

    }

    fn handle(&self, event: desktop::WindowEvent) {

        if let desktop::WindowEvent::ShouldClose = &event {
            self.handlers.closed.send(());
        }

        // let action = Action::Event { event };

        // Propagate the action through the widget tree.
        // self.content.lock().action(action);

    }

    pub async fn closed(&self) {
        self.handlers.closed.listen().await
    }

}

#[derive(Default)]
struct WindowEventHandlers {
    closed: common::EventChannel<()>,
}

pub trait Widget {
    fn action(&self, action: Action);
}

impl Widget for () {
    fn action(&self, _: Action) {}
}

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

struct RenderState {
    geometries: RenderStateGeometries,
    vertices: RenderStateVertices,
    curves: RenderStateCurves,
}

struct RenderStateGeometries {
    /// Widget-added geometries.
    inner: Vec<Arc<render::VertexGeometry>>,
    /// Indexes into `geometries`.
    instances: Vec<common::Instance>,
}

struct RenderStateVertices {
    /// Widget-added vertices.
    data: Arc<render::VertexGeometry>,
    /// Indexes into `geometry`.
    instances: Vec<common::Instance>,
}

struct RenderStateCurves {
    // Widget-added curves.
    // Will be triangulated later on.
    data: render::CurveGeometry,
    /// Indexes into `geometry`.
    instances: Vec<common::Instance>,

}

pub struct Space<'a> {
    state: &'a mut RenderState,
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

        let geometry = Arc::get_mut(&mut self.state.vertices.data)
            .expect("exclusive ownership of vertex geometry");

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
        let items = &mut self.state.geometries.inner;
        items.push(geometry);
        GeometryKey {
            index: items.len() as u16 - 1
        }
    }

    pub fn instance(&mut self, key: SpaceKey, instance: Instance) {

        let pos  = self.apply_transform_pos(instance.pos);
        let size = self.apply_transform_size(instance.size);

        let inner = common::Instance {
            target: [0, key.index as usize],
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

        let inner = common::Instance {
            target: [key.index as usize, shape as usize],
            pos: pos.into(),
            size: size.into(),
        };

        self.state.geometries.instances.push(inner);

    }

    pub fn subdivide<'s>(&'s mut self, offset: Position, size: Size) -> Space<'s> {

        let offset = self.apply_transform_pos(offset);
        let size = self.apply_transform_size(size);

        Space {
            state: self.state,
            offset,
            size
        }

    }

    // /// Computes value% * scale%, but using units per 5000.
    // ///
    // /// So if value = 1,250 and scale = 2,500 this returns 625, equivalent to
    // ///       value = 25%       sccale = 50%       returns 12.5%
    // fn applyscale(value: i16, scale: i16) -> i16 {
    //     ((value as isize * scale as isize) / 5000isize) as i16
    // }

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
