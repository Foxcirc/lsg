
// windowing, ...
pub mod wayland;
pub use wayland::*;

pub mod signals;

use crate::shared::*;
use common::SmartMutex;

use std::{ffi::c_void as void, future, task};

// TODO: add better and more unit-tests

struct EventLoopState {
    proxy: common::EventChannel<Event>,
    wayland: wayland::Connection,
    signals: signals::SignalListener,
    // dbus: dbus::Connection,
    config: EventLoopConfig,
}

pub struct EventLoop {
    state: SmartMutex<EventLoopState>,
}

// TODO: implement cleanup for the event loop, eg. the dbus connection should be flushed

impl EventLoop {

    fn new(config: EventLoopConfig) -> Result<Self, EvlError> {
        Ok(Self {
            state: SmartMutex::new(EventLoopState {
                proxy: common::EventChannel::new(),
                wayland: wayland::Connection::new(&config.appid)?,
                signals: signals::SignalListener::new()?,
                config,
            }),
        })
    }

    pub fn run<R, H>(config: EventLoopConfig, handler: H) -> Result<R, EvlError>
        where H: FnOnce(Self) -> R {

        let target = Self::new(config)?;
        Ok(handler(target))

    }

    pub async fn next(&self) -> Result<Event, EvlError> {

        future::poll_fn(|cx| {
            let mut state = self.state.lock(); // only lock briefly during polling
            if let task::Poll::Ready(ev) = state.signals.poll(cx) { return task::Poll::Ready(ev) }
            if let task::Poll::Ready(ev) = state.proxy  .poll(cx) { return task::Poll::Ready(Ok(ev)) }
            if let task::Poll::Ready(ev) = state.wayland.poll(cx) { return task::Poll::Ready(ev) }
            task::Poll::Pending
        }).await

    }

    // /// Write pending requests. Call this during cleanup
    // /// if you are no longer going to call `next`.
    // pub async fn flush(&mut self) -> Result<(), EvlError> {
    //     // eg. close a notification
    //     // self.dbus.flush().await
    //     Ok(())
    // }

    pub fn config(&self) -> EventLoopConfig {
        let guard = self.state.lock();
        guard.config.clone()
    }

    pub fn suspend(&self) {
        let guard = self.state.lock();
        guard.proxy.send(Event::Suspend);
    }

    pub fn resume(&self) {
        let guard = self.state.lock();
        guard.proxy.send(Event::Resume);
    }

    pub fn quit(&self) {
        let guard = self.state.lock();
        guard.proxy.send(Event::Quit { reason: QuitReason::Program });
    }

    // TODO: make it be Notif::new(&mut evl) instead
    // pub fn send_notification(&mut self, notif: &NotifBuilder<'_>) -> Notif {
    //     self.dbus.send_notification(notif)
    // }

}

unsafe impl egl::IsDisplay for EventLoop {
    fn ptr(&self) -> *mut void {
        self.state.lock().wayland.display()
    }
}

#[derive(Default, Clone)]
pub struct EventLoopConfig {
    pub appid: String,
}
