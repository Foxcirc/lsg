
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

use common::SmartMutex;

use std::{future, sync::Arc, task};
use futures_lite::future::block_on;

#[cfg(test)]
mod test;

pub struct App {
    executor: Arc<async_executor::LocalExecutor<'static>>,
    evl: SmartMutex<desktop::EventLoop>,
    // windows:
}

impl App {

    pub fn run<R, H>(handler: H) -> R
        where H: AsyncFnOnce(Self) -> R {

            let config = desktop::EventLoopConfig {
                appid: "unknown".into() // TODO
            };

            desktop::EventLoop::run(config, |evl| {

                let this = Self {
                    executor: Arc::new(async_executor::LocalExecutor::new()),
                    evl: SmartMutex::new(evl),
                };

                let executor2 = Arc::clone(&this.executor);

                // This task pumps events and calls the event handlers.
                executor2.spawn(async move {

                    // loop {
                        // evl.next()
                    // }

                }).detach();

                // This task is the user-side which can wait for events.
                block_on(executor2.run(async move {
                    handler(this).await
                }))

            }).unwrap()


    }

}

pub struct Window {
    handlers: Arc<WindowEventHandlers>,
    inner: desktop::Window
}

impl Window {

    pub fn new(app: &App) -> Self {

        let mut evl = app.evl.lock();

        let window = desktop::Window::new(&mut evl);

        let handlers = Arc::new(Default::default());

        Self {
            handlers,
            inner: window,
        }

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

            desktop::WindowEvent::Close => self.closed.send(()),

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
