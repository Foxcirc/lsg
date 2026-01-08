
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

// TODO: add better and more unit-tests

// TODO: implement cleanup for the event loop, eg. the dbus connection should be flushed
pub struct EventLoop<T: 'static + Send = ()> {
    proxy: proxy::EventProxyData<T>,
    wayland: wayland::Connection<T>,
    signals: signals::SignalListener,
    config: EventLoopConfig,
    // dbus: dbus::Connection,
}

impl<T: 'static + Send> EventLoop<T> {

    fn new(config: EventLoopConfig) -> Result<Self, EvlError> {
        Ok(Self {
            proxy: proxy::EventProxyData::new(),
            wayland: wayland::Connection::new(&config.appid)?,
            signals: signals::SignalListener::new()?,
            config,
            // dbus: dbus::Connection::new(app)?,
        })
    }

    pub fn run<R, H>(config: EventLoopConfig, handler: H) -> Result<R, EvlError>
        where H: FnOnce(Self) -> R {

        let target = Self::new(config)?;
        Ok(handler(target))

    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
        self.signals.next() // signals are the most important
            .or(self.wayland.next())
            .or(self.proxy.recv())
            // .or(self.dbus.next())
            .await
    }

    // /// Write pending requests. Call this during cleanup
    // /// if you are no longer going to call `next`.
    // pub async fn flush(&mut self) -> Result<(), EvlError> {
    //     // eg. close a notification
    //     // self.dbus.flush().await
    //     Ok(())
    // }

    pub fn config(&self) -> &EventLoopConfig {
        &self.config
    }

    pub fn suspend(&mut self) {
        self.proxy.sender
            .try_send(Event::Suspend)
            .unwrap();
    }

    pub fn resume(&mut self) {
        self.proxy.sender
            .try_send(Event::Resume)
            .unwrap();
    }

    pub fn quit(&mut self) {
        self.proxy.sender
            .try_send(Event::Quit { reason: QuitReason::Program })
            .unwrap();
    }

    // TODO: make it be Notif::new(&mut evl) instead
    // pub fn send_notification(&mut self, notif: &NotifBuilder<'_>) -> Notif {
    //     self.dbus.send_notification(notif)
    // }

}

unsafe impl<T: Send + 'static> egl::IsDisplay for EventLoop<T> {
    fn ptr(&self) -> *mut void {
        self.wayland.state.con.get_ref()
            .display().id().as_ptr().cast()
    }
}

#[derive(Default)]
pub struct EventLoopConfig {
    pub appid: String,
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

        pub async fn recv(&mut self) -> Result<Event<T>, EvlError> {
            Ok(self.receiver.recv().await.unwrap())
        }

    }

    #[derive(Clone)]
    pub struct EventProxy<T: Send> {
        sender: AsyncSender<Event<T>>,
    }

    // impl<T: Send> Clone for EventProxy<T> {
    //     fn clone(&self) -> Self {
    //         Self { sender: self.sender.clone() }
    //     }
    // }

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

mod signals {

    use std::io;

    use futures_lite::StreamExt;
    use nix::sys::signal::Signal;

    use crate::*;

    /// Listens to SIGTERM and SIGINT to emit the apropriate events
    pub(crate) struct SignalListener {
        signals: async_signals::Signals,
    }

    impl SignalListener {

        pub fn new() -> io::Result<Self> {

            let signals = async_signals::Signals::new([
                Signal::SIGTERM as i32,
                Signal::SIGINT as i32
            ]).map_err(io::Error::from)?;

            Ok(Self {
                signals
            })

        }

        pub async fn next<T>(&mut self) -> Result<Event<T>, EvlError> {

            loop {

                let signal = self.signals.next().await.unwrap_or(0);

                if signal == Signal::SIGTERM as i32 {
                    return Ok(Event::Quit { reason: QuitReason::System })
                } else if signal == Signal::SIGINT as i32 {
                    return Ok(Event::Quit { reason: QuitReason::CtrlC })
                }

            }

        }

    }

}
