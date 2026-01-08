
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

use std::{rc::Rc, sync::Arc};

use futures_lite::future::block_on;

#[cfg(test)]
mod test;

pub struct App {
    executor: Rc<async_executor::LocalExecutor<'static>>,
}

impl App {

    pub fn run<R, H>(handler: H) -> R
        where H: AsyncFnOnce(Self) -> R {

            let config = desktop::EventLoopConfig {
                appid: "unknown".into() // TODO
            };

            desktop::EventLoop::<()>::run(config, |el| {

                let this = Self {
                    executor: Rc::new(async_executor::LocalExecutor::new()),
                };

                let executor2 = Rc::clone(&this.executor);

                // This task pumps events and calls the event handlers.
                executor2.spawn(async move {

                }).detach();

                // This task is the user-side which can wait for events.
                block_on(executor2.run(async move {
                    handler(this).await
                }))

            }).unwrap()


    }

}

pub struct Window {
}

impl Window {

    pub fn new(app: &App) -> Self {

        Self {}

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
