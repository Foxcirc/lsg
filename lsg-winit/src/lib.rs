
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/lsg-colored.png")]

//! `lsg-winit` is an async [`winit`](https://docs.rs/winit) wrapper written for the `lsg` library.
//! However this crate is also meant to be used as a standalone.
//!
//! `lsg-winit` provides a simple abstraction over `winit` that allows you to receive all
//! events produced by your application in an async context.
//! There are some challenges implementing this tough. On some platforms the event loop has to
//! be run on the main thread, which makes this a little bit more un-ergonomic.
//!
//! You are also not gonna get around spawning a thread for your application logic,
//! but [`spawn`](Runner::spawn) makes this easier and manages joining the thread for you.
//!
//! # Example
//! This crate can be used with any async runtime, or none at all.
//!
//! ```ignore
//! let (evl, mut runner) = AsyncEventLoop::new()?;
//!
//! runner.spawn(move || { // slightly altered thread::spawn
//!     block_on(async move {
//!         let monitor = evl.primary_monitor()?; // exposes normal winit functions
//!         while let event = evl.next().await { // get the next event
//!             // handle events here
//!             if should_close { return } // return to terminate the event loop
//!         }
//!     });
//! });
//! 
//! runner.run()?; // run the event loop on the main thread
//! println!("event loop terminated");
//! ```
//!
//! Please also consult the regular `winit` docs as only new functionality is documented here.
//! To be in sync with the versions, this crate exposes it's own `winit` that you can use.
//! ```
//! use lsg_winit::winit;
//! ```

use std::thread;
use winit::{event_loop::{EventLoop, EventLoopBuilder, EventLoopProxy, DeviceEvents, ControlFlow}, error::EventLoopError, event::Event, monitor::MonitorHandle};

pub use winit;

enum Request {
    Exit, // only ever sent when the spawned thread returns
    Exiting            { resp: oneshot::Sender<bool> },
    AvailableMonitors  { resp: oneshot::Sender<AvailableMonitorsIterator> },
    PrimaryMonitor     { resp: oneshot::Sender<Option<MonitorHandle>> },
    ListenDeviceEvents { arg: DeviceEvents },
    SetControlFlow     { arg: ControlFlow },
    ControlFlow        { resp: oneshot::Sender<ControlFlow> }
}

type AvailableMonitorsIterator = Box<dyn Iterator<Item = MonitorHandle> + Send>;

/// Handle that allows interacting with the `winit` event loop
/// 
/// This struct allows you to interact with your event loop in an async context.
/// It mirrors the methods of a regular [`EventLoop`](https://docs.rs/winit/latest/winit/event_loop/struct.EventLoop.html).
/// However calling them is more expensive then normally, since a channel has to be
/// created to communicate with the event loop.
#[derive(Debug)]
pub struct AsyncEventLoop {
    events: flume::Receiver<Event<()>>,
    proxy: EventLoopProxy<Request>,
}

impl AsyncEventLoop {

    /// Returns a new event loop and a handle that is used to run the event loop on the main thread.
    pub fn new<T>() -> Result<(Self, Runner<T>), EventLoopError> {

        let inner = EventLoopBuilder::with_user_event().build()?;
        let proxy = inner.create_proxy();
        let (sender, receiver) = flume::unbounded();

        Ok((
            Self { events: receiver, proxy },
            Runner { inner, events: sender, thread: None }
        ))

    }

    /// Receive the next event.inside your async task.
    pub async fn next(&self) -> Event<()> {
        self.events.recv_async().await.unwrap() // this can only fail if the main thread is dead
    }

    pub async fn available_monitors(&self) -> impl Iterator<Item = MonitorHandle> {
        let (resp, result) = oneshot::channel();
        self.proxy.send_event(Request::AvailableMonitors { resp }).map_err(drop).unwrap();
        result.await.unwrap()
    }
    
    pub async fn primary_monitor(&self) -> Option<MonitorHandle> {
        let (resp, result) = oneshot::channel();
        self.proxy.send_event(Request::PrimaryMonitor { resp }).map_err(drop).unwrap();
        result.await.unwrap()
    }
    
    pub fn listen_device_events(&self, arg: DeviceEvents) {
        self.proxy.send_event(Request::ListenDeviceEvents { arg }).map_err(drop).unwrap();
    }
    
    pub async fn control_flow(&self) -> ControlFlow {
        let (resp, result) = oneshot::channel();
        self.proxy.send_event(Request::ControlFlow { resp }).map_err(drop).unwrap();
        result.await.unwrap()
    }
    
    pub fn set_control_flow(&self, arg: ControlFlow) {
        self.proxy.send_event(Request::SetControlFlow { arg }).map_err(drop).unwrap();
    }

    pub async fn exiting(&self) -> bool {
        let (resp, result) = oneshot::channel();
        self.proxy.send_event(Request::Exiting { resp }).map_err(drop).unwrap();
        result.await.unwrap()
    }
    
}

/// Erases the custom UserData type from the event
fn convert(event: Event<Request>) -> Event<()> {
    use Event::*;
    match event {
        NewEvents(start_cause) => NewEvents(start_cause),
        WindowEvent { window_id, event } => WindowEvent { window_id, event },
        DeviceEvent { device_id, event } => DeviceEvent { device_id, event },
        UserEvent(..) => unreachable!(),
        Suspended => Suspended,
        Resumed => Resumed,
        AboutToWait => AboutToWait,
        LoopExiting => LoopExiting,
        MemoryWarning => MemoryWarning,
    }
}

/// Wrapper for the [`EventLoop`](https://docs.rs/winit/latest/winit/event_loop/struct.EventLoop.html)
///
/// This struct is used to manage a thread for you and in the meantime run the
/// `winit` event loop on the main thread.
/// Also see the [`spawn`](Runner::spawn) function.
#[derive(Debug)]
pub struct Runner<T> {
    inner: EventLoop<Request>,
    events: flume::Sender<Event<()>>,
    thread: Option<thread::JoinHandle<T>>
}

impl<T> Runner<T> {

    /// Spawns a thread for you and manages joining it.
    ///
    /// This thread will act just like the main thread. When you return from it the event loop
    /// will terminate and [`run`](Runner::run) will return the same value.
    pub fn spawn<F>(&mut self, f: F)
    where
        F: FnOnce() -> T,
        F: Send + 'static,
        T: Send + 'static,
    {
        let proxy = self.inner.create_proxy();
        let thread = thread::spawn(move || {
            let result = f();
            proxy.send_event(Request::Exit).map_err(drop).unwrap();
            result
        });
        self.thread = Some(thread);
    }

    /// Runs the `winit` event loop. Only call on the main thread.
    /// 
    /// Also see [`spawn`](Runner::spawn).
    pub fn run(mut self) -> Result<T, EventLoopError> {

        let thread = self.thread.take();

        self.inner.run(move |event, evl| {

            if let Event::UserEvent(request) = event {
                match request {
                    Request::Exit => evl.exit(),
                    Request::Exiting            { resp } => resp.send(evl.exiting()).unwrap(),
                    Request::AvailableMonitors  { resp } => resp.send(Box::new(evl.available_monitors())).unwrap(),
                    Request::PrimaryMonitor     { resp } => resp.send(evl.primary_monitor()).unwrap(),
                    Request::ControlFlow        { resp } => resp.send(evl.control_flow()).unwrap(),
                    Request::SetControlFlow     { arg }  => evl.set_control_flow(arg),
                    Request::ListenDeviceEvents { arg }  => evl.listen_device_events(arg),
                }
            } else {
                // the other side could be dead before we receive the exit request
                let erased = convert(event);
                let _ignored = self.events.send(erased);
            }

        })?;

        Ok(
            thread.unwrap().join().unwrap()
        )
        
    }
    
}

