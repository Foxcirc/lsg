
// windowing, ...
pub mod wayland;
pub use wayland::*;

// notifs, status icons, ...
pub mod dbus;
pub use dbus::*;

use wayland_client::Proxy;

use crate::shared::*;

use std::{ffi::c_void as void, future};
use futures_lite::FutureExt;

// TODO: add tracing

// TODO: add better and more unit-tests

// TODO: implement cleanup for the event loop, eg. the dbus connection should be flushed
pub struct EventLoop<T: 'static + Send = ()> {
    events: AwaitableVec<Event<T>>,
    proxy: proxy::EventProxyData<T>,
    wayland: wayland::Connection<T>,
    signals: signals::SignalListener,
    app_name: String,
    // dbus: dbus::Connection,
}

impl<T: 'static + Send> EventLoop<T> {

    pub(crate) fn new(app_name: &str) -> Result<Self, EvlError> {
        Ok(Self {
            events: AwaitableVec::new(Vec::new()),
            proxy: proxy::EventProxyData::new(),
            wayland: wayland::Connection::new(app_name)?,
            signals: signals::SignalListener::new()?,
            app_name: app_name.into(),
            // dbus: dbus::Connection::new(app)?,
        })
    }

    pub fn run<R, H>(handler: H, app: &str) -> Result<R, EvlError>
        where T: 'static + Send,
              H: FnOnce(Self) -> R {

        let target = Self::new(app)?;
        Ok(handler(target))

    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
        self.signals.next() // signals are the most important
            .or(self.wayland.next())
            .or(self.proxy.next())
            .or(self.events.next())
            // .or(self.dbus.next())
            .await
    }

    /// Write pending requests. Call this during cleanup
    /// if you are no longer going to call `next`.
    pub async fn flush(&mut self) -> Result<(), EvlError> {
        // eg. close a notification
        // self.dbus.flush().await
        Ok(())
    }

    pub fn push_redraw_test<R: Send>(&mut self, window: &BaseWindow<R>) {
        self.events.push(Event::Window { id: window.id, event: WindowEvent::Redraw });
    }

    pub fn app_name(&self) -> &str {
        &self.app_name
    }

    /// On linux, this is a no-op.
    pub fn on_main_thread<R>(&mut self, func: impl FnOnce() -> R) -> R {
        func() // on linux the event loop runs on the main thread
    }

    pub fn suspend(&mut self) {
        self.events.push(Event::Suspend);
    }

    pub fn resume(&mut self) {
        self.events.push(Event::Resume);
    }

    pub fn quit(&mut self) {
        self.events.push(Event::Quit { reason: QuitReason::Program });
    }

    // TODO: make it be Notif::new(&mut evl) instead
    // pub fn send_notification(&mut self, notif: &NotifBuilder<'_>) -> Notif {
        // self.dbus.send_notification(notif)
        // }

}

unsafe impl<T: Send + 'static> egl::IsDisplay for EventLoop<T> {
    fn ptr(&self) -> *mut void {
        self.wayland.state.con.get_ref()
            .display().id().as_ptr().cast()
    }
}

struct AwaitableVec<T> {
    pub inner: Vec<T>,
}

impl<T> AwaitableVec<T> {

    pub fn new(inner: Vec<T>) -> Self {
        Self { inner }
    }

    pub fn push(&mut self, value: T) {
        self.inner.push(value)
    }

    pub async fn next(&mut self) -> Result<T, EvlError> { // TODO: infallible as error type?
        if let Some(val) = self.inner.pop() {
            Ok(val)
        } else {
            future::pending().await
        }
    }

}

pub use proxy::*;
pub mod proxy {

    use async_channel::{Sender as AsyncSender, Receiver as AsyncReceiver};

    use crate::*;

    pub(crate) struct EventProxyData<T> {
        pub(crate) sender: AsyncSender<Event<T>>,
        pub(crate) receiver: AsyncReceiver<Event<T>>,
    }

    impl<T> EventProxyData<T> {

        pub fn new() -> Self {
            let (sender, receiver) = async_channel::unbounded();
            Self { sender, receiver }
        }

        pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
            Ok(self.receiver.recv().await.unwrap())
        }

    }

    pub struct EventProxy<T: Send> {
        sender: AsyncSender<Event<T>>,
    }

    impl<T: Send> Clone for EventProxy<T> {
        fn clone(&self) -> Self {
            Self { sender: self.sender.clone() }
        }
    }

    impl<T: Send> EventProxy<T> {

        pub fn new(evl: &EventLoop<T>) -> Self {
            Self { sender: evl.proxy.sender.clone() }
        }

        #[track_caller]
        pub fn send(&self, event: Event<T>) {
            // the event loop exiting should be the last thing to happen, so this
            // isn't meant to fail
            self.sender.try_send(event)
                .expect("event loop dead")
        }

    }

}

// pub use signals::*; // NOTE: this module has no public items right now
pub mod signals {

    use std::io;

    use futures_lite::StreamExt;
    use nix::sys::signal::Signal;

    use crate::*;

    /// Listens to SIGTERM and SIGINT to emit the apropriate events
    pub(crate) struct SignalListener {
        #[cfg(feature = "signals")]
        signals: async_signals::Signals,
    }

    impl SignalListener {

        pub fn new() -> io::Result<Self> {

            #[cfg(feature = "signals")]
            let signals = async_signals::Signals::new([
                Signal::SIGTERM as i32,
                Signal::SIGINT as i32
            ]).map_err(io::Error::from)?;

            Ok(Self {
                #[cfg(feature = "signals")]
                signals
            })

        }

        pub async fn next<T>(&mut self) -> Result<Event<T>, EvlError> {

            #[cfg(feature = "signals")]
            loop {
                let signal = self.signals.next().await.unwrap_or(0);
                if signal == Signal::SIGTERM as i32 {
                    return Ok(Event::Quit { reason: QuitReason::System })
                } else if signal == Signal::SIGINT as i32 {
                    return Ok(Event::Quit { reason: QuitReason::CtrlC })
                }
            }

            #[cfg(not(feature = "signals"))]
            future::pending().await


        }

    }

}
