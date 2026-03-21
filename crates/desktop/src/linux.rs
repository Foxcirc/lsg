
// windowing, ...
pub mod wayland;
use futures_lite::FutureExt;
pub use wayland::*;

pub mod signals;

use crate::shared::*;
use common::SmartMutex;

use std::{ffi::{CStr, CString, c_void as void}, future, sync::{Arc, MutexGuard}, task, time::Instant};

// TODO: add better and more unit-tests

#[derive(Clone)]
pub struct EventLoopConfig {
    pub appid: String,
    /// If `true` relevant signals will be intercepted and
    /// turned into `Quit` events. Otherwise signals
    /// will never be intercepted.
    pub intercept: bool,
}

impl Default for EventLoopConfig {
    fn default() -> Self {
        Self {
            appid: format!("lsg-{:?}", Instant::now()),
            intercept: true,
        }
    }
}

pub struct EventLoop {
    state: SmartMutex<EventLoopState>,
}

struct EventLoopState {
    wayland: wayland::Connection,
    signals: signals::SignalListener,
    injected: Vec<Event>,
    // dbus: dbus::Connection,
    config: EventLoopConfig,
}

// TODO: implement cleanup for the event loop, eg. the dbus connection should be flushed

impl EventLoop {

    #[track_caller]
    fn new(config: EventLoopConfig) -> Result<Arc<Self>, EvlError> {

        // We do some setup here, which should be nice
        // for the user, since this is a framework after all.

        let name = CString::new(config.appid.clone())
            .expect("appid cannot contain NUL byte");

        // Set the process' name to the appid.
        nix::sys::prctl::set_name(&name)?;

        Ok(Arc::new(Self {
            state: SmartMutex::new(EventLoopState {
                wayland: wayland::Connection::new(&config.appid)?,
                signals: signals::SignalListener::new(config.intercept)?,
                injected: Vec::with_capacity(1),
                config,
            }),
        }))

    }

    #[track_caller]
    pub fn run<R, H>(config: EventLoopConfig, handler: H) -> Result<R, EvlError>
        where H: FnOnce(Arc<Self>) -> R {

        let target = Self::new(config)?;
        Ok(handler(target))

    }

    pub async fn next(&self) -> Result<Event, EvlError> {

        use task::Poll::*;

        future::poll_fn(|cx| {
            let mut state = self.state.lock(); // only lock briefly during polling
                 if let Ready(ev) = state.wayland.poll(cx) { Ready(ev) }
            else if let Ready(ev) = state.signals.poll(cx) { Ready(ev) }
            else if let Some(ev)  = state.injected.pop()   { Ready(Ok(ev)) }
            else { Pending }
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
        let mut guard = self.state.lock();
        guard.injected.push(Event::Suspend);
    }

    pub fn resume(&self) {
        let mut guard = self.state.lock();
        guard.injected.push(Event::Resume);
    }

    pub fn quit(&self) {
        let mut guard = self.state.lock();
        guard.injected.push(Event::Quit { reason: QuitReason::Program });
    }

    // TODO: make it be Notif::new(&mut evl) instead
    // pub fn send_notification(&mut self, notif: &NotifBuilder<'_>) -> Notif {
    //     self.dbus.send_notification(notif)
    // }

}

unsafe impl common::IsDisplay for EventLoop {
    fn ptr(&self) -> *mut void {
        self.state.lock().wayland.display()
    }
}
