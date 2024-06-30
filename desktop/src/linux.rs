
// the main logic: windowing, egl, ...
 pub mod wayland;
 use futures_lite::{FutureExt, StreamExt};
use nix::sys::signal::Signal;
pub use wayland::*;

// egl
pub mod egl;
pub use egl::*;

// needed for notifications, status icons, ...
pub mod dbus;
pub use dbus::*;

use crate::*;

use async_channel::{Sender as AsyncSender, Receiver as AsyncReceiver};
use std::{error::Error as StdError, fmt, io};

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
    wayland: wayland::Connection<T>, // TODO: move evl proxies from wayland.rs to here
    // unix signal events
    #[cfg(feature = "signals")]
    signals: async_signals::Signals, // listens to sigterm and emits a quit event

}

impl<T: 'static + Send> EventLoop<T> {

    pub(crate) fn new(application: &str) -> Result<Self, EvlError> {

        #[cfg(feature = "signals")]
        let signals = async_signals::Signals::new([
            Signal::SIGTERM as i32,
            Signal::SIGINT as i32
        ]).map_err(io::Error::from)?;

        // proxy sender & receiver
        let (sender, receiver) = async_channel::unbounded();

        Ok(Self {
            events: Vec::new(),
            proxy: EventProxy { sender },
            receiver,
            wayland: wayland::Connection::new(application)?,
            #[cfg(feature = "signals")]
            signals,
        })

    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
            // #[cfg(feature = "signals")]
            // let signals = async {
            //     use futures_lite::StreamExt;
            //     let signal = self.state.signals.next().await
            //         .unwrap_or(0);
            //     Ok(Either::Signal(signal))
            // };

                // #[cfg(feature = "signals")]
                // Ok(Either::Signal(signal)) => {
                //     if signal == Signal::SIGTERM as i32 {
                //         self.state.events.push(Event::Quit { reason: QuitReason::System })
                //     } else if signal == Signal::SIGINT as i32 {
                //         self.state.events.push(Event::Quit { reason: QuitReason::CtrlC })
                //     }
                // }
        // todo: from self.events

        let wayland = async {
            self.wayland.next().await
        };

        let proxy = async {
            Ok(self.receiver.recv().await.unwrap())
        };

        let signals = async {
            loop {
                let signal = self.signals.next().await.unwrap_or(0);
                if signal == Signal::SIGTERM as i32 {
                    return Ok(Event::Quit { reason: QuitReason::System })
                } else if signal == Signal::SIGINT as i32 {
                    return Ok(Event::Quit { reason: QuitReason::CtrlC })
                }
            }
        };

        wayland
            .or(proxy)
            .or(signals)
            .await

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

    pub fn get_clip_board(&mut self) -> Option<DataOffer> {
        self.wayland.get_clip_board()
    }

    pub fn set_clip_board(&mut self, src: Option<&DataSource>) {
        self.wayland.set_clip_board(src)
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

