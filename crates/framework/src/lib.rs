
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

use std::{future, mem, sync::{Arc, Mutex, MutexGuard}, task};

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

                }).detach();

                // This task is the user-side which can wait for events.
                block_on(executor2.run(async move {
                    handler(this).await
                }))

            }).unwrap()


    }

}

#[derive(Default)]
struct WindowEventHandlers {
    closed: EventChannel<()>,
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

    fn handle(&self, event: desktop::WindowEvent) {

        match event {

            desktop::WindowEvent::Close => self.handlers.closed.broadcast(()),

            _ => (),

        }

    }

}

struct EventChannel<T> {
    inner: SmartMutex<EventBroadcasterInner<T>>,
}

struct EventBroadcasterInner<T> {
    waker: Option<task::Waker>,
    event: Option<T>,
}

impl<T> Default for EventChannel<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> EventChannel<T> {

    pub fn new() -> Self {
        Self {
            inner: SmartMutex::new(EventBroadcasterInner {
                waker: None,
                event: None
            })
        }
    }

    pub fn broadcast(&self, event: T) {

        let mut inner = self.inner.lock();

        inner.event = Some(event);

        if let Some(waker) = &inner.waker {
            waker.wake_by_ref();
        }

    }

    pub async fn listen(&self) -> T {

        future::poll_fn(|cx| {

            let mut inner = self.inner.lock();

            if let Some(it) = inner.event.take() {

                task::Poll::Ready(it)

            } else {

                match &mut inner.waker {
                    Some(it) => it.clone_from(cx.waker()),
                    None => inner.waker = Some(cx.waker().clone())
                }

                task::Poll::Pending

            }

        }).await

    }

}

pub struct SmartMutex<T> {
    inner: Mutex<T>,
}

impl<T> SmartMutex<T> {

    pub fn new(inner: T) -> Self {
        Self { inner: Mutex::new(inner) }
    }

    pub fn with<F, R>(&self, f: F) -> R
        where F: FnOnce(&mut T) -> R {

        f(&mut *self.lock())

    }

    pub fn lock<'s>(&'s self) -> MutexGuard<'s, T> {
        self.inner.lock().expect("mutex was poisoned")
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
