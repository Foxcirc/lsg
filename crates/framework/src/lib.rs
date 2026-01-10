
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

use std::{future, mem, sync::{Arc, Mutex}, task};

use futures_lite::future::block_on;

#[cfg(test)]
mod test;

pub struct App {
    executor: Arc<async_executor::LocalExecutor<'static>>,
}

impl App {

    pub fn run<R, H>(handler: H) -> R
        where H: AsyncFnOnce(Self) -> R {

            let config = desktop::EventLoopConfig {
                appid: "unknown".into() // TODO
            };

            desktop::EventLoop::<()>::run(config, |el| {

                let this = Self {
                    executor: Arc::new(async_executor::LocalExecutor::new()),
                };

                let executor2 = Arc::clone(&this.executor);

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
    dispatcher: EventDispatcher<WindowEventHandler>,
}

impl Window {

    pub fn new(app: &App) -> Self {

        Self {
            dispatcher: EventDispatcher::new(),
        }

    }

    pub async fn closed(&self) {

        let key = self.dispatcher.register(WindowEventHandler::Closed(None));

        self.dispatcher.listen(
            key, |h| if let WindowEventHandler::Closed(it) = h { it.take() } else { unreachable!() }
        ).await

    }

    fn handle(&self, event: desktop::WindowEvent) {

        self.dispatcher.notify(|handler| match (handler, &event) {
            (WindowEventHandler::Closed(it), desktop::WindowEvent::Close) => { *it = Some(()); true }
            (..) => false
        });

    }

}

enum WindowEventHandler {
    Closed(Option<()>),
}

struct EventDispatcher<H> {
    handlers: Mutex<slab::Slab<EventHandler<H>>>,
}

impl<H> EventDispatcher<H> {

    pub fn new() -> Self {
        Self {
            handlers: Mutex::new(slab::Slab::new())
        }
    }

    /// Register a new event handler.
    pub fn register(&self, inner: H) -> usize {
        let mut guard = self.handlers.lock().unwrap();
        guard.insert(EventHandler { waker: None, inner })
    }

    pub async fn listen<F, T>(&self, key: usize, mut access: F) -> T
        where F: FnMut(&mut H) -> Option<T> {

            future::poll_fn(move |cx| {

                let mut guard = self.handlers.lock().unwrap();
                let handler = &mut guard[key];

                let event = access(&mut handler.inner);

                if let Some(it) = event {

                    task::Poll::Ready(it)

                } else {

                    match &mut handler.waker {
                        Some(it) => it.clone_from(cx.waker()),
                        None => handler.waker = Some(cx.waker().clone())
                    }

                    task::Poll::Pending

                }

            }).await

    }

    pub fn notify<F>(&self, mut assess: F)
        where F: FnMut(&mut H) -> bool {

        let mut guard = self.handlers.lock().unwrap();

        for (.., handler) in &mut *guard {

            let modified = assess(&mut handler.inner);

            if let Some(it) = &handler.waker && modified {
                it.wake_by_ref();
            }

        }

    }

}

struct EventHandler<H> {
    waker: Option<task::Waker>,
    inner: H,
}

// enum EventFilter<T> {
//     Any,
//     One(mem::Discriminant<T>),
//     Multiple(Vec<mem::Discriminant<T>>),
// }

// impl<T> EventFilter<T> {
//     pub fn matches(&self, value: &T) -> bool {
//         let discr = mem::discriminant(value);
//         match self {
//             Self::Any => true,
//             Self::One(it) => v
//         }
//     }
// }

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
