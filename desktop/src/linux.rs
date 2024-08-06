
// windowing, ...
pub mod wayland;
pub use wayland::*;

// egl
pub mod egl;
pub use egl::*;

// notifs, status icons, ...
pub mod dbus;
pub use dbus::*;

use crate::shared::*;

use std::future;
use futures_lite::FutureExt;

// TODO: add tracing

pub fn run<T, R, H>(handler: H, app: &str) -> Result<R, EvlError>
    where T: 'static + Send,
          H: FnOnce(EventLoop<T>) -> R {

    let target = EventLoop::new(app)?;
    Ok(handler(target))

}

// TODO: implement cleanup for the event loop, eg. the dbus connection should be flushed
pub struct EventLoop<T: 'static + Send> {
    events: Vec<Event<T>>,
    proxy: proxy::InnerProxy<T>,
    wayland: wayland::Connection<T>,
    signals: signals::SignalListener,
    dbus: dbus::Connection,
}

impl<T: 'static + Send> EventLoop<T> {

    pub(crate) fn new(app: &str) -> Result<Self, EvlError> {
        Ok(Self {
            events: Vec::new(),
            proxy: proxy::InnerProxy::new(),
            wayland: wayland::Connection::new(app)?,
            signals: signals::SignalListener::new()?,
            dbus: dbus::Connection::new(app)?,
        })
    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
        pop_event(&mut self.events)
            .or(self.wayland.next())
            .or(self.proxy.next())
            .or(self.signals.next())
            .or(self.dbus.next())
            .await
    }
    
    /// Write pending requests. Call this during cleanup
    /// if you are no longer going to call `next`.
    pub async fn flush(&mut self) -> Result<(), EvlError> {
        // eg. close a notification
        self.dbus.flush().await
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
        self.events.push(Event::Quit { reason: QuitReason::User });
    }

    pub fn get_selection(&mut self) -> Option<DataOffer> {
        // TODO: maybe we should also emit a ClipBoardChanged event instead of this
        self.wayland.get_clip_board()
    }

    // TODO: docs-rs alias to "clipboard" or smth
    pub fn set_selection(&mut self, src: Option<&DataSource>) {
        self.wayland.set_clip_board(src)
    }

    // TODO: to mimic other api  (eg. set_clip_board ^^) this should take &Notif and return nothing
    //       but what would happen if this is called multiple times then?
    pub fn send_notification(&mut self, notif: &NotifBuilder<'_>) -> Notif {
        self.dbus.send_notification(notif)
    }

}

// TODO: make pop_event not a function but make a type "Queue" or smth that does this
async fn pop_event<T>(events: &mut Vec<Event<T>>) -> Result<Event<T>, EvlError> {
    if let Some(val) = events.pop() {
        Ok(future::ready(val).await)
    } else {
        future::pending().await
    }
}

pub use proxy::*;
pub mod proxy {

    use async_channel::{Sender as AsyncSender, Receiver as AsyncReceiver};

    use crate::*;

    pub(crate) struct InnerProxy<T> {
        pub(crate) sender: AsyncSender<Event<T>>,
        pub(crate) receiver: AsyncReceiver<Event<T>>,
    }

    impl<T> InnerProxy<T> {

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

