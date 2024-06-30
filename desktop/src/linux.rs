
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

use futures_lite::FutureExt;

pub fn run<T, R, H>(handler: H, application: &str) -> Result<R, EvlError>
    where T: 'static + Send,
          H: FnOnce(EventLoop<T>) -> R {

    let target = EventLoop::new(application)?;
    Ok(handler(target))

}

pub struct EventLoop<T: 'static + Send> {
    events: Vec<Event<T>>,
    proxy: proxy::InnerProxy<T>,
    wayland: wayland::Connection<T>,
    signals: signals::SignalListener,
}

impl<T: 'static + Send> EventLoop<T> {

    pub(crate) fn new(application: &str) -> Result<Self, EvlError> {
        Ok(Self {
            events: Vec::new(),
            proxy: proxy::InnerProxy::new(),
            wayland: wayland::Connection::new(application)?,
            signals: signals::SignalListener::new()?,
        })
    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
        self.wayland.next()
            .or(self.proxy.next())
            .or(self.signals.next())
            .await
    }
    
    /// On linux, this is a no-op.
    pub fn on_main_thread<R>(&mut self, func: impl FnOnce() -> R) -> R {
        func() // on linux the event loop runs on the main thread
    }

    pub fn new_proxy(& self) -> EventProxy<T> {
        self.proxy.new_proxy()
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

pub use proxy::*;
pub mod proxy {

    use std::{error::Error as StdError, fmt};

    use async_channel::{Sender as AsyncSender, Receiver as AsyncReceiver};

    use crate::*;

    pub(crate) struct InnerProxy<T> {
        sender: AsyncSender<Event<T>>,
        receiver: AsyncReceiver<Event<T>>,
    }

    impl<T> InnerProxy<T> {

        pub fn new() -> Self {
            let (sender, receiver) = async_channel::unbounded();
            Self { sender, receiver }
        }

        pub fn new_proxy(&self) -> EventProxy<T> {
            EventProxy {
                sender: self.sender.clone(),
            }
        }

        pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
            Ok(self.receiver.recv().await.unwrap())
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

        #[track_caller]
        pub fn send(&self, event: Event<T>) {
            // the event loop exiting should be the last thing to happen
            // anyways
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

    pub(crate) struct SignalListener {
        #[cfg(feature = "signals")]
        signals: async_signals::Signals, // listens to sigterm and emits a quit event
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

// 170

