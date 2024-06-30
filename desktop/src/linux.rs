
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

use async_channel::{Sender as AsyncSender, Receiver as AsyncReceiver};
use std::{fmt, error::Error as StdError};

pub fn run<E: 'static + Send, T, H: FnOnce(EventLoop<E>) -> T>(handler: H, application: &str) -> Result<T, EvlError> {
    // TODO: use where clause

    let target = EventLoop::new(application)?;
    Ok(handler(target))

}

pub struct EventLoop<T: 'static + Send> {
    // events produced by us
    events: Vec<Event<T>>,
    // event proxy data
    proxy: EventProxy<T>,
    receiver: AsyncReceiver<Event<T>>,
    // wayland connection & events
    wayland: wayland::EventLoop<T>, // TODO: move evl proxies from wayland.rs to here

}

impl<T: 'static + Send> EventLoop<T> {

    pub(crate) fn new(application: &str) -> Result<Self, EvlError> {

        let (sender, receiver) = async_channel::unbounded();

        Ok(Self {
            events: Vec::new(),
            proxy: EventProxy { sender },
            receiver,
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

    pub fn new_proxy(& self) -> EventProxy<T> {
        self.proxy.clone()
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

pub struct EventProxy<T> {
    sender: AsyncSender<Event<T>>,
}

impl<T> Clone for EventProxy<T> {
    fn clone(&self) -> Self {
        Self { sender: self.sender.clone() }
    }
}

impl<T> EventProxy<T> {

    pub fn send(&self, event: Event<T>) -> Result<(), SendError<Event<T>>> {
        self.sender.send_blocking(event)
            .map_err(|err| SendError { inner: err.into_inner() })
    }

}

pub struct SendError<T> {
    pub inner: T
}

impl<T> fmt::Debug for SendError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SendError: EventLoop dead")
    }
}

impl<T> fmt::Display for SendError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<T: fmt::Debug> StdError for SendError<T> {}

