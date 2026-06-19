
pub mod wayland;
pub mod signals;

pub use wayland::*;

use crate::*;
use common::*;

use std::{ffi::{CString, c_void as void}, sync::Arc, task};

// TODO: add better and more unit-tests

pub struct EventLoopBackend {
    state: SmartMutex<EventLoopState>,
}

struct EventLoopState {
    wayland: wayland::Connection,
    signals: signals::SignalListener,
    injected: Vec<Event>,
    // dbus: dbus::Connection,
}

// TODO: implement cleanup for the event loop, eg. the dbus connection should be flushed

impl EventLoopBackend {

    #[track_caller]
    fn new(config: EvlConfig) -> Result<Self, EvlError> {

        // We do some setup here, which should be nice
        // for the user, since this is a framework after all.

        let name = CString::new(config.appid.clone())
            .expect("appid cannot contain NUL byte");

        // Set the process' name to the appid.
        nix::sys::prctl::set_name(&name)?;

        Ok(Self {
            state: SmartMutex::new(EventLoopState {
                wayland: wayland::Connection::new(&config.appid)?,
                signals: signals::SignalListener::new(config.intercept)?,
                injected: Vec::with_capacity(1),
            }),
        })

    }

    #[track_caller]
    pub fn run<R, H>(config: EvlConfig, handler: H) -> Result<R, EvlError>
        where H: FnOnce(Arc<EventLoop>) -> R {

        let evl = EventLoop {
            backend: Self::new(config)?
        };

        Ok(handler(Arc::new(evl)))

    }

    pub fn poll(&self, cx: &mut task::Context<'_>) -> task::Poll<Result<Event, EvlError>> {

        use task::Poll::*;

        let mut state = self.state.lock(); // only lock briefly during polling
             if let Ready(ev) = state.wayland.poll(cx) { Ready(ev) }
        else if let Ready(ev) = state.signals.poll(cx) { Ready(ev) }
        else if let Some(ev)  = state.injected.pop()   { Ready(Ok(ev)) }
        else { Pending }

    }

    // /// Write pending requests. Call this during cleanup
    // /// if you are no longer going to call `next`.
    // pub async fn flush(&mut self) -> Result<(), EvlError> {
    //     // eg. close a notification
    //     // self.dbus.flush().await
    //     Ok(())
    // }

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

    pub fn ptr(&self) -> *const void {
        self.state.lock().wayland.display()
    }

}
