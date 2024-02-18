
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
//!         let window = lsg_winit::WindowBuilder::new().build(&evl)?; // create a window
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

use std::{thread, ops::{Deref, DerefMut}, any::Any, panic::{catch_unwind, resume_unwind, UnwindSafe}};
use winit::{
    event_loop::{EventLoop, EventLoopBuilder, EventLoopProxy, DeviceEvents, ControlFlow},
    window::{WindowBuilder as WinitWindowBuilder, Window as WinitWindow, WindowAttributes, WindowButtons, Fullscreen, WindowLevel, Icon, Theme},
    event::Event,
    monitor::MonitorHandle,
    dpi::{Size as WinitSize, Position as WinitPosition},
    error::{EventLoopError, OsError},
};

pub use winit;

enum Request {
    Exit, // only ever sent when the spawned thread returns
    ExecOnMainThread   { f: SendableFunction, resp: oneshot::Sender<()> },
    Exiting            { resp: oneshot::Sender<bool> },
    AvailableMonitors  { resp: oneshot::Sender<AvailableMonitorsIterator> },
    PrimaryMonitor     { resp: oneshot::Sender<Option<MonitorHandle>> },
    ListenDeviceEvents { arg: DeviceEvents },
    SetControlFlow     { arg: ControlFlow },
    ControlFlow        { resp: oneshot::Sender<ControlFlow> },
    BuildWindow        { arg: WinitWindowBuilder, resp: oneshot::Sender<Result<WinitWindow, OsError>> },
}

type SendableFunction = Box<dyn FnOnce() + Send>;
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
        self.events.recv_async().await.unwrap()
    }

    /// Run something on the main thread
    ///
    /// This is useful as some platforms require specific things to be done on the main thread.
    /// The closure will be run immediatly and the event loop will not run until it is done.
    pub async fn exec_on_main_thread<F>(&mut self, f: F) -> ()
    where
        F: FnOnce() -> (),
        F: Send + 'static,
    {
        let (resp, result) = oneshot::channel();
        self.proxy.send_event(Request::ExecOnMainThread { f: Box::new(f), resp }).map_err(drop).unwrap();
        result.await.unwrap()
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

    async fn build_window(&self, arg: WinitWindowBuilder) -> Result<WinitWindow, OsError> {
        let (resp, result) = oneshot::channel();
        self.proxy.send_event(Request::BuildWindow { arg, resp }).map_err(drop).unwrap();
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
        F: Send + UnwindSafe + 'static,
        T: Send + 'static,
    {
        let proxy = self.inner.create_proxy();
        let thread = thread::spawn(move || {
            let result = catch_unwind(move || f());
            proxy.send_event(Request::Exit).map_err(drop).unwrap();
            match result {
                Ok(val) => val,
                Err(err) => resume_unwind(err),
            }
        });
        self.thread = Some(thread);
    }

    /// Runs the `winit` event loop. Only call on the main thread.
    /// 
    /// Also see [`spawn`](Runner::spawn).
    pub fn run(self) -> Result<T, EventLoopError> {

        let thread = self.thread.expect("cannot run without calling spawn");

        self.inner.run(move |event, evl| {

            if let Event::UserEvent(request) = event {
                match request {
                    Request::ExecOnMainThread { f, resp } => { f(); resp.send(()).unwrap() },
                    Request::Exit => evl.exit(),
                    Request::Exiting            { resp } => resp.send(evl.exiting()).unwrap(),
                    Request::AvailableMonitors  { resp } => resp.send(Box::new(evl.available_monitors())).unwrap(),
                    Request::PrimaryMonitor     { resp } => resp.send(evl.primary_monitor()).unwrap(),
                    Request::ControlFlow        { resp } => resp.send(evl.control_flow()).unwrap(),
                    Request::SetControlFlow     { arg  } => evl.set_control_flow(arg),
                    Request::ListenDeviceEvents { arg  } => evl.listen_device_events(arg),
                    Request::BuildWindow   { arg, resp } => resp.send(arg.build(&evl)).unwrap(),
                }
            } else {
                // the other side could be dead before we receive the exit request
                let erased = convert(event);
                let _ignored = self.events.send(erased);
            }

        })?;

        Ok(thread.join().expect("lsg_winit child thread panicked"))
        
    }
    
}

pub struct WindowBuilder {
    inner: WinitWindowBuilder
}

/// Simple wrapper around a [`WindowBuilder`](https://docs.rs/winit/latest/winit/window/struct.WindowBuilder.html)
/// using an [`AsyncEventLoop`] to build the window.
impl WindowBuilder {
    
    /// Create a new `WindowBuilder`.
    /// Don't use the deref method here.
    pub fn new() -> Self {
        Self { inner: WinitWindowBuilder::new() }
    }

    pub fn window_attributes(&self) -> &WindowAttributes {
        self.inner.window_attributes()
    }

    pub fn with_inner_size<S: Into<WinitSize>>(mut self, size: S) -> Self {
        self.inner = self.inner.with_inner_size(size);
        self
    }

    pub fn with_min_inner_size<S: Into<WinitSize>>(mut self, size: S) -> Self {
        self.inner = self.inner.with_min_inner_size(size);
        self
    }

    pub fn with_max_inner_size<S: Into<WinitSize>>(mut self, size: S) -> Self {
        self.inner = self.inner.with_max_inner_size(size);
        self
    }

    pub fn with_position(mut self, resizable: bool) -> Self {
        self.inner = self.inner.with_resizable(resizable);
        self
    }

    pub fn with_enabled_buttons(mut self, buttons: WindowButtons) -> Self {
        self.inner = self.inner.with_enabled_buttons(buttons);
        self
    }

    pub fn with_title<T: Into<String>>(mut self, title: T) -> Self {
        self.inner = self.inner.with_title(title);
        self
    }

    pub fn with_fullscreen(mut self, fullscreen: Option<Fullscreen>) -> Self {
        self.inner = self.inner.with_fullscreen(fullscreen);
        self
    }

    pub fn with_maximized(mut self, maximized: bool) -> Self {
        self.inner = self.inner.with_maximized(maximized);
        self
    }

    pub fn with_visible(mut self, visible: bool) -> Self {
        self.inner = self.inner.with_visible(visible);
        self
    }

    pub fn with_transparent(mut self, transparent: bool) -> Self {
        self.inner = self.inner.with_transparent(transparent);
        self
    }

    pub fn with_blur(mut self, blur: bool) -> Self {
        self.inner = self.inner.with_blur(blur);
        self
    }

    pub fn transparent(&self) -> bool {
        self.inner.transparent()
    }

    pub fn with_decorations(mut self, decorations: bool) -> Self {
        self.inner = self.inner.with_decorations(decorations);
        self
    }

    pub fn with_window_level(mut self, level: WindowLevel) -> Self {
        self.inner = self.inner.with_window_level(level);
        self
    }

    pub fn with_window_icon(mut self, icon: Option<Icon>) -> Self {
        self.inner = self.inner.with_window_icon(icon);
        self
    }

    pub fn with_theme(mut self, theme: Option<Theme>) -> Self {
        self.inner = self.inner.with_theme(theme);
        self
    }

    pub fn with_resize_increments<S: Into<WinitSize>>(mut self, resize_increment: S) -> Self {
        self.inner = self.inner.with_resize_increments(resize_increment);
        self
    }

    pub fn with_content_protected(mut self, protected: bool) -> Self {
        self.inner = self.inner.with_content_protected(protected);
        self
    }

    pub fn with_active(mut self, active: bool) -> Self {
        self.inner = self.inner.with_active(active);
        self
    }

    /// This returns an actual `winit` [`Window`](https://docs.rs/winit/latest/winit/window/struct.Window.html).
    /// Don't use the deref method here.
    pub async fn build(self, evl: &AsyncEventLoop) -> Result<WinitWindow, OsError> {
        let arg = self.inner;
        evl.build_window(arg).await
    }

}
