
// the main logic: windowing, egl, ...
 pub mod wayland;
 pub use wayland::*;

// egl
pub mod egl;
pub use egl::*;

// needed for notifications, status icons, ...
pub mod dbus;
pub use dbus::*;

use crate::*;

pub struct EventLoop<T: 'static + Send> {
    events: Vec<Event<T>>,
    wayland: wayland::EventLoop<T>, // TODO: move evl proxies from wayland.rs to here
}

impl<T: 'static + Send> EventLoop<T> {

    pub(crate) fn new(application: &str) -> Result<Self, EvlError> {
        Ok(Self {
            events: Vec::new(),
            wayland: wayland::EventLoop::new(application)?,
        })
    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
        // todo: from self.events
        self.wayland.next().await
    }
    
    pub fn on_main_thread<R>(&mut self, func: impl FnOnce() -> R) -> R {
        // on linux the event loop runs on the main thread
        func()
    }

    pub fn new_proxy(&mut self) -> EventProxy<T> {
        todo!()
    }

    pub fn suspend(&mut self) {
        self.events.push(Event::Suspend);
    }

    pub fn resume(&mut self) {
        self.events.push(Event::Resume);
    }

    pub fn quit(&mut self) {
        self.events.push(Event::Quit { reason: QuitReason::User });
    }

}

pub fn run<E: 'static + Send, T, H: FnOnce(EventLoop<E>) -> T>(handler: H, application: &str) -> Result<T, EvlError> {
    // TODO: use where clause

    let target = EventLoop::new(application)?;
    Ok(handler(target))

}

