
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

use common::SmartMutex;

use std::{collections::HashMap, convert::{Infallible, identity}, sync::{Arc, Weak}};
use futures_lite::{FutureExt, future::block_on};

#[cfg(test)]
mod test;

pub struct Config {
    pub appid: String,
}

pub struct App {
    executor: async_executor::LocalExecutor<'static>,
    eventloop: desktop::EventLoop,
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
                let dispatcher = this.executor.spawn(async move {
                    let result = Self::dispatcher(this2).await;
                    Err(result.unwrap_err())
                });

                // This is the task running the user code.
                let main = this.executor.spawn(async move {
                    let result = main(this3).await;
                    Ok(result)
                });

                block_on(this.executor.run(
                    dispatcher.or(main)
                ))

            }).and_then(identity)

    }

    async fn dispatcher(this: Arc<Self>) -> Result<Infallible, desktop::EvlError> {

        loop {

            let event = this.eventloop.next().await?;

            match event {

                desktop::Event::Quit { reason } => {
                    this.handlers.quit.send(reason)
                },

                desktop::Event::Window { id, event } => {

                    // Try to get the window, if it wan't dropped already.
                    let handler = this.windows.lock().get(&id)
                        .and_then(Weak::upgrade);

                    // Foreward the event to the window.
                    if let Some(it) = handler {
                        it.handlers.handle(event);
                    }

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
    handlers: WindowEventHandlers,
    inner: desktop::Window
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

        let this = Arc::new(Self {
            app: Arc::clone(&app),
            handlers: WindowEventHandlers::default(),
            inner: desktop::Window::new(&app.eventloop),
        });

        // Insert ourselves into the window list.
        app.windows.lock().insert(
            this.inner.id(),
            Arc::downgrade(&this)
        );

        this

    }

    pub async fn closed(&self) {
        self.handlers.closed.listen().await
    }

}

#[derive(Default)]
struct WindowEventHandlers {
    closed: common::EventChannel<()>,
}

impl WindowEventHandlers {

    fn handle(&self, event: desktop::WindowEvent) {

        match event {

            desktop::WindowEvent::ShouldClose => self.closed.send(()),

            _ => (),

        }

    }

}

/*

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
