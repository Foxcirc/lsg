
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

#[cfg(test)]
mod test;

pub use common;
pub use widget;

use common::{IsSurface, SmartMutex};
use desktop::{MouseButton, Key};
use widget::Widget;
use std::{collections::{HashMap, VecDeque}, convert::{Infallible, identity}, future::{pending, poll_fn}, pin::Pin, sync::{Arc, Weak}, task};
use futures_lite::{FutureExt, future::block_on};

pub struct Config {
    /// This name will be registered in various places around the system.
    ///
    /// # Platform-Specific
    /// - Linux:
    ///     - Main thread name.
    ///     - DBus client name.
    pub appid: String,
    /// If `true` relevant signals will be intercepted and turned into
    /// `Quit` events. Otherwise signals will never be intercepted.
    ///
    /// Default: Enabled only on release builds. When debugging it is
    /// annoying, since it prevents terminating a program which is stuck and
    /// cannot poll the event loop anymore, e.g. when in an infinite loop.
    pub intercept: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            appid: format!("unknown-lsg-app"),
            #[cfg(debug_assertions)]
            intercept: false,
            #[cfg(not(debug_assertions))]
            intercept: true,
        }
    }
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
                    // shaper: render::GeometryShaper::new(),
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

#[derive(Default)]
struct AppEventHandlers {
    quit: EventBroadcaster<desktop::QuitReason>,
}

pub struct Window {
    app: Arc<App>,
    inner: desktop::Window,
    handlers: WindowEventHandlers,
    content: SmartMutex<widget::basic::DynWidget>,
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

    pub fn new(app: &Arc<App>) -> Arc<Self> {

        let inner = desktop::Window::new(&app.eventloop);

        let renderer = &app.renderstate.lock().renderer;

        let renderstate = SmartMutex::new(WindowRenderState {
            rendered: widget::RenderOutput::default(),
            texture: graphics::Texture::new(&renderer.gp, inner.size(), None),
            surface: graphics::Surface::new(&renderer.gp, &inner),
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

        this

    }

    fn handle(&self, event: desktop::WindowEvent) -> Result<(), desktop::EvlError> {

        use desktop::WindowEvent;

        let mut windowstate = self.renderstate.lock();
        let mut appstate = self.app.renderstate.lock();

        let layout = widget::Layout::new(common::PhysicalRect {
            point: common::PhysicalPoint::ZERO,
            size: self.inner.size()
        });

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

                // This will render the whole tree.
                let action = widget::Action::Render { out: &mut windowstate.rendered };
                self.content.lock().inner.action(layout, action);

                // Now we can read back and render the data.

                let AppRenderState { renderer, atlas } = &mut *appstate;
                let WindowRenderState { rendered, texture, surface } = &mut *windowstate;

                let drawable = render::DrawableGeometry {
                    source: &[&rendered.geometry],
                    instances: &rendered.instances,
                };

                self.inner.present();
                texture.clear([0., 0., 0., 1.]);
                renderer.draw(&drawable, &atlas, texture);
                self.inner.redraw();

                surface.blit(texture);
                surface.swap();


            },

            WindowEvent::MouseScroll { dx, dy } => {
                let action = widget::Action::MouseScroll {
                    point: common::PhysicalPoint::new(100, 100),
                    delta: common::PhysicalPoint::new(dx, dy)
                };
                self.content.lock().inner.action(layout, action);
            }

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

    pub fn content<W: widget::Widget + 'static>(&self, widget: Arc<W>) {
        self.content.set(widget::basic::DynWidget::new(widget));
    }

    pub fn closed<'s>(&'s self) -> BroadcastFuture<'s, ()> {
        self.handlers.closed.listen()
    }

}

#[derive(Default)]
struct WindowEventHandlers {
    closed: EventBroadcaster<()>,
}

struct AppRenderState {
    // shaper: render::GeometryShaper,
    renderer: render::Renderer,
    atlas: render::TextureAtlas,
}

struct WindowRenderState {
    // Geometry Buffers:
    rendered: widget::RenderOutput,
    // Intermediate Texture and Surface:
    texture: graphics::Texture,
    surface: graphics::Surface,
}

impl WindowRenderState {
    pub fn clear(&mut self) {
        self.rendered.clear();
    }
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
    wakers: Vec<Option<task::Waker>>, // TODO: fix bug (slot reserved but still None = treated as empty slot)
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
        inner.tick += 1;

        inner.wakers.iter()
            .flat_map(identity)
            .for_each(task::Waker::wake_by_ref);

    }

    pub fn listen<'s>(&'s self) -> BroadcastFuture<'s, T> {

        let mut inner = self.inner.lock();

        let slot = inner.wakers.iter().position(Option::is_none).unwrap_or_else(|| {
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
