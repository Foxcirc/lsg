
use wayland_client::{
    protocol::{
        wl_registry::{WlRegistry, Event as WlRegistryEvent},
        wl_compositor::WlCompositor,
        // wl_shm::WlShm,
        wl_seat::{WlSeat, Event as WlSeatEvent, Capability as WlSeatCapability},
        wl_surface::WlSurface,
        wl_callback::{WlCallback, Event as WlCallbackEvent},
        wl_keyboard::WlKeyboard,
        wl_pointer::WlPointer, wl_region::WlRegion,
    },
    WEnum, Proxy, QueueHandle, EventQueue
};

use wayland_protocols::xdg::shell::client::{
    xdg_wm_base::{XdgWmBase, Event as XdgWmBaseEvent},
    xdg_surface::{XdgSurface, Event as XdgSurfaceEvent},
    xdg_toplevel::{XdgToplevel, Event as XdgToplevelEvent},
};

use khronos_egl as egl;
use rustix::event::{PollFd, PollFlags, poll};
use std::{mem, fmt, ffi::c_void as void, error::Error as StdError, sync::{mpsc::{Sender, Receiver, channel, SendError}, atomic::{AtomicBool, Ordering}, Arc}, io, os::fd::{RawFd, BorrowedFd}};

pub struct EventLoop<T: 'static = ()> {
    running: bool,
    ids: usize,
    con: wayland_client::Connection,
    qh: QueueHandle<Self>,
    queue: Option<EventQueue<Self>>, // TODO: used where?
    wayland: WaylandState,
    events: Vec<Event<T>>, // used to push events from inside the dispatch impl
    channel: Channel<Event<T>>, // used to send events to the evl from anywhere else
}

struct Channel<T> {
    eventfd: RawFd,
    sender: Sender<T>,
    receiver: Receiver<T>
}

impl<T> Channel<T> {
    pub fn new() -> Self {
        let (sender, receiver) = channel();
        let eventfd = unsafe { libc::eventfd(0, libc::EFD_NONBLOCK) }; // TODO: use rustix eventfd when it is fixed
        Channel { eventfd, sender, receiver }
    }
}

impl<T> Drop for Channel<T> {
    fn drop(&mut self) {
        unsafe { libc::close(self.eventfd) };
    }
}

impl<T: 'static> EventLoop<T> {

    pub fn new() -> Result<Self, EvlError> {

        let con = wayland_client::Connection::connect_to_env()?;

        // initial setup
        let globals = {
            
            let mut queue = con.new_event_queue();
            let qh = queue.handle();

            let mut globals = UninitWaylandGlobals::default();

            // register wayland globals
            let display = con.display();
            display.get_registry(&qh, ());

            // wait for the request to be processed
            let mut count = 20; // try max 20 rounds
            while globals.partial() && count > 0 {
                queue.roundtrip(&mut globals)?;
                queue.dispatch_pending(&mut globals)?;
                count -= 1;
            }

            // assert that all the globals are actually bound
            globals.finalize().ok_or(EvlError::Unsupported)?

        };

        let queue = con.new_event_queue();
        let qh = queue.handle();

        let this = EventLoop {
            running: true,
            ids: 0,
            con,
            queue: Some(queue),
            qh,
            wayland: globals,
            events: Vec::with_capacity(8),
            channel: Channel::new(),
        };

        Ok(this)
        
    }

    pub fn run<F: for<'a> FnMut(&'a mut Self, Event<T>)>(mut self, mut cb: F) -> Result<(), EvlError> {

        let Some(mut queue) = self.queue.take() else { unreachable!() };
        let qh = queue.handle();

        // connect keyboard and pointer input
        if self.wayland.capabilities.contains(WlSeatCapability::Keyboard) {
            self.wayland.seat.get_keyboard(&qh, ());
        } else if self.wayland.capabilities.contains(WlSeatCapability::Pointer) {
            self.wayland.seat.get_pointer(&qh, ());
        }

        self.events.push(Event::Resume);

        let mut events = Vec::with_capacity(8);

        // main event loop
        loop {

            queue.dispatch_pending(&mut self)?;

            // we `swap` the events so we can pass the event loop into the closure
            mem::swap(&mut self.events, &mut events);

            // foreward the events to the callback
            for event in events.drain(..) { cb(&mut self, event) }
            
            if !self.running { break } // stop if `exit` was called
            if !self.events.is_empty() { continue } // the callback could've generated more events

            // prepare to wait for new events
            let Some(guard) = queue.prepare_read() else { continue };

            let queue_fd = guard.connection_fd();
            let channel_fd = unsafe { BorrowedFd::borrow_raw(self.channel.eventfd) };

            let mut fds = [
                PollFd::new(&queue_fd, PollFlags::IN | PollFlags::ERR),
                PollFd::new(&channel_fd, PollFlags::IN),
            ];

            // println!("right before");
            poll(&mut fds, -1).map_err(|err| io::Error::from(err))?;

            let wl_events = fds[0].revents();
            let channel_events = fds[1].revents();

            // new events from the wayland connection
            if wl_events.contains(PollFlags::IN) {
                guard.read()?;
            } else if wl_events.contains(PollFlags::ERR) {
                return Err(EvlError::Io(io::Error::last_os_error()));
            }

            // new user events
            if channel_events.contains(PollFlags::IN) {
                while let Ok(event) = self.channel.receiver.try_recv() {
                    self.events.push(event)
                }
                unsafe { libc::eventfd_read(self.channel.eventfd, &mut 0) };
            }

        }

        Ok(())
        
    }

    pub fn proxy(&self) -> EventLoopProxy<T> {

        EventLoopProxy {
            sender: self.channel.sender.clone(),
            eventfd: self.channel.eventfd, // we can share the fd, no problemo
        }
        
    }

    pub fn exit(&mut self) {
        self.running = false
    }

    pub(crate) fn id(&mut self) -> usize {
        self.ids += 1;
        self.ids
    }
    
}

pub struct EventLoopProxy<T> {
    sender: Sender<Event<T>>,
    eventfd: RawFd,
}

impl<T> EventLoopProxy<T> {

    pub fn send(&self, event: T) -> Result<(), SendError<Event<T>>> {

        self.sender.send(Event::User(event))?;

        // the fd will be alive since the `send` would have failed if the EventLoop was destroyed
        unsafe { libc::eventfd_write(self.eventfd, 1) };

        Ok(())

    }
}

struct WaylandState {
    compositor: WlCompositor,
    // shm: WlShm,
    seat: WlSeat,
    capabilities: WlSeatCapability,
    wm: XdgWmBase,
}

#[derive(Default, Debug)]
struct UninitWaylandGlobals {
    compositor: Option<WlCompositor>,
    // shm: Option<WlShm>,
    seat: Option<WlSeat>,
    capabilities: Option<WlSeatCapability>,
    wm: Option<XdgWmBase>,
}

impl UninitWaylandGlobals {
    pub(crate) fn partial(&self) -> bool {
        self.compositor.is_none() ||
        // self.shm.is_none() ||
        self.seat.is_none() ||
        self.capabilities.is_none() ||
        self.wm.is_none()
    }
    pub(crate) fn finalize(self) -> Option<WaylandState> {
        Some(WaylandState {
            compositor: self.compositor?,
            // shm: self.shm?,
            seat: self.seat?,
            capabilities: self.capabilities?,
            wm: self.wm?
        })
    }
}

pub type WindowId = usize;

pub struct Window<T: 'static> {
    // our data
    shared: Arc<WindowShared>, // needs to be accessed by some callbacks
    configured: bool,
    width: i32, // TODO: can this possibly be removed?
    height: i32, // TODO: this aswell ^^
    // wayland state
    channel: Sender<Event<T>>,
    qh: QueueHandle<EventLoop<T>>,
    compositor: WlCompositor,
    surface: WlSurface,
    xdg_surface: XdgSurface,
    xdg_toplevel: XdgToplevel,
}

struct WindowShared {
    id: usize,
    frame_callback_registered: AtomicBool, // needs to be modified
    redraw_requested: AtomicBool, // needs to be modified
}

impl<T> Window<T> {
    
    /// The window will initially be hidden, so you can setup title etc.
    pub fn new(evl: &mut EventLoop<T>) -> Result<Self, EvlError> {

        let shared = Arc::new(WindowShared {
            id: evl.id(),
            frame_callback_registered: AtomicBool::new(false),
            redraw_requested: AtomicBool::new(false)
        });

        let mut queue = evl.con.new_event_queue();
        let qh = queue.handle();

        let surface = evl.wayland.compositor.create_surface(&qh, ());
        let xdg_surface = evl.wayland.wm.get_xdg_surface(&surface, &qh, ());
        let xdg_toplevel = xdg_surface.get_toplevel(&evl.qh, Arc::clone(&shared)); // <- use the main event queue here!

        surface.commit(); // commit the initial setup, the compositor will now send a `configure` event

        let mut this = Self {
            shared,
            configured: false,
            width: 100, // a default of 100x100
            height: 100,
            channel: evl.channel.sender.clone(),
            qh: evl.qh.clone(),
            compositor: evl.wayland.compositor.clone(),
            surface,
            xdg_surface,
            xdg_toplevel,
        };

        // wait for the configure event
        while !this.configured {
            queue.blocking_dispatch(&mut this)?;
        }

        Ok(this)
        
    }

    pub fn request_redraw(&mut self) {
         if self.shared.frame_callback_registered.load(Ordering::Relaxed) {
            self.shared.redraw_requested.store(true, Ordering::Relaxed); // wait for the frame callback to send the redraw event
        } else {
            self.channel.send(Event::Window { id: self.shared.id, event: WindowEvent::Redraw }).unwrap(); // immediatly send the event
        }
    }

    pub fn pre_present_notify(&mut self) -> PresentToken { // TODO: store QH
         if self.shared.frame_callback_registered.compare_exchange(
            false, true, Ordering::Relaxed, Ordering::Relaxed
        ).is_ok() {
            self.surface.frame(&self.qh, Arc::clone(&self.shared)); // TODO: make this cheaper
            self.surface.commit();
        }
        PresentToken(())
    }

    pub fn title<S: Into<String>>(&self, text: S) {
        self.xdg_toplevel.set_title(text.into());
    }

    pub fn class<S: Into<String>>(&self, text: S) {
        self.xdg_toplevel.set_app_id(text.into());
    }

    pub fn transparency(&self, value: bool) {
        if value {
            self.surface.set_opaque_region(None);
        } else {
            let region = self.compositor.create_region(&self.qh, ());
            region.add(0, 0, i32::MAX, i32::MAX);
            self.surface.set_opaque_region(Some(&region));
        }
    }

    pub fn set_fullscreen(&self) {
        // self.xdg_toplevel.set_fullscreen()
        todo!();
    }

}

pub struct PresentToken(());

pub struct EglInstance {
    lib: egl::Instance<egl::Static>,
    display: egl::Display,
    config: egl::Config,
}

impl EglInstance {

    /// Should be only be called once. Although initializing multiple instances is not a hard error.
    pub fn new<T>(evh: &mut EventLoop<T>) -> Result<EglInstance, EvlError> {
        
        let lib = egl::Instance::new(egl::Static);

        let wl_display = evh.con.display().id().as_ptr();
        let egl_display = unsafe {
            lib.get_display(wl_display.cast())
        }.ok_or(EvlError::NoDisplay)?;

        lib.initialize(egl_display)?;

        let config = {
            let attribs = [
                egl::SURFACE_TYPE, egl::WINDOW_BIT,
                egl::RENDERABLE_TYPE, egl::OPENGL_ES3_BIT,
                egl::RED_SIZE, 8,
                egl::GREEN_SIZE, 8,
                egl::BLUE_SIZE, 8,
                egl::NONE
            ];
            lib.choose_first_config(egl_display, &attribs)?
                .ok_or(EvlError::EglUnsupported)?
        };

        Ok(Self {
            lib,
            display: egl_display,
            config,
        })
        
    }

    pub fn get_proc_address(&self, name: &str) -> Option<extern "system" fn()> {
        self.lib.get_proc_address(name)
    }
    
}

/// One-per-window egl context.
pub struct EglContext {
    wl_egl_surface: wayland_egl::WlEglSurface, // needs to be kept alive
    egl_surface: egl::Surface,
    egl_context: egl::Context,
}

impl EglContext {

    /// Create a new egl context that will draw onto the given window.
    pub fn new<T>(instance: &EglInstance, window: &Window<T>) -> Result<Self, EvlError> {

        let context = {
            let attribs = [
                egl::CONTEXT_MAJOR_VERSION, 4,
                egl::CONTEXT_MINOR_VERSION, 0,
                egl::CONTEXT_CLIENT_VERSION, 3,
                egl::CONTEXT_OPENGL_DEBUG, if cfg!(debug) { 1 } else { 0 },
                egl::NONE,
            ];
            instance.lib.create_context(instance.display, instance.config, None, &attribs).unwrap()
        };

        let wl_egl_surface = wayland_egl::WlEglSurface::new(window.surface.id(), window.width, window.height)?;

        let surface = {
            unsafe { instance.lib.create_window_surface(
                instance.display,
                instance.config,
                wl_egl_surface.ptr() as *mut void,
                None
            )? }
        };

        Ok(Self {
            wl_egl_surface,
            egl_surface: surface,
            egl_context: context,
        })
        
    }

    /// Make this context current.
    pub fn bind(&self, instance: &EglInstance) -> Result<(), egl::Error> {

        instance.lib.make_current(
            instance.display,
            Some(self.egl_surface), // note: it is an error to only specify one of the two (read/draw) surfaces
            Some(self.egl_surface),
            Some(self.egl_context)
        )
        
    }

    /// Returns an error if this context is not the current one.
    pub fn swap_buffers(&self, instance: &EglInstance, _token: PresentToken) -> Result<(), EvlError> {

        instance.lib.swap_buffers(instance.display, self.egl_surface)?;

        Ok(())

    }

    /// Don't forget to also resize your opengl Viewport!
    pub fn resize(&self, width: u32, height: u32) {

        self.wl_egl_surface.resize(width as i32, height as i32, 0, 0);
        
    }
    
}

pub enum Event<T> {
    Resume,
    User(T),
    Window { id: WindowId, event: WindowEvent },
}

pub enum WindowEvent {
    Close,
    Resize { width: u32, height: u32 },
    Redraw,
}

impl wayland_client::Dispatch<WlRegistry, ()> for UninitWaylandGlobals {
    fn event(
        evh: &mut Self,
        registry: &WlRegistry,
        event: WlRegistryEvent,
        _data: &(),
        _con: &wayland_client::Connection,
        qh: &wayland_client::QueueHandle<Self>
    ) {
        if let WlRegistryEvent::Global { name, interface, .. } = event {
            match &interface[..] {
                "wl_compositor" => evh.compositor = Some(registry.bind(name, 1, qh, ())),
                // "wl_shm"        => evh.shm        = Some(registry.bind(name, 1, qh, ())),
                "wl_seat"       => evh.seat       = Some(registry.bind(name, 1, qh, ())),
                "xdg_wm_base"   => evh.wm         = Some(registry.bind(name, 1, qh, ())),
                _ => ()
            }
        }
    }
}

impl wayland_client::Dispatch<WlSeat, ()> for UninitWaylandGlobals {
    fn event(
        evh: &mut Self,
        _seat: &WlSeat,
        event: WlSeatEvent,
        _data: &(),
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if let WlSeatEvent::Capabilities { capabilities: WEnum::Value(capabilities) } = event {
            evh.capabilities = Some(capabilities);
            // we will get the keyboard and pointer later
        }
    }
}

impl wayland_client::Dispatch<XdgWmBase, ()> for UninitWaylandGlobals {
    fn event(
        _: &mut Self,
        wm: &XdgWmBase,
        event: XdgWmBaseEvent,
        _: &(),
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if let XdgWmBaseEvent::Ping { serial } = event {
            wm.pong(serial);
        }
    }
}

impl<T> wayland_client::Dispatch<XdgSurface, ()> for Window<T> {
    fn event(
        window: &mut Self,
        _surface: &XdgSurface,
        event: XdgSurfaceEvent,
        _data: &(),
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if let XdgSurfaceEvent::Configure { serial } = event {
            window.xdg_surface.ack_configure(serial);
            window.configured = true;
        }
    }
}

impl<T> wayland_client::Dispatch<XdgToplevel, Arc<WindowShared>> for EventLoop<T> {
    fn event(
        evl: &mut Self,
        _surface: &XdgToplevel,
        event: XdgToplevelEvent,
        shared: &Arc<WindowShared>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if let XdgToplevelEvent::Configure { width, height, .. } = event {
            evl.events.push(Event::Window { id: shared.id, event: WindowEvent::Resize { width: width as u32, height: height as u32 } });
            evl.events.push(Event::Window { id: shared.id, event: WindowEvent::Redraw });
        } else if let XdgToplevelEvent::Close = event {
            evl.events.push(Event::Window { id: shared.id, event: WindowEvent::Close });
        }
    }
}

impl<T> wayland_client::Dispatch<WlCallback, Arc<WindowShared>> for EventLoop<T> {
    fn event(
        evl: &mut Self,
        _cb: &WlCallback,
        _event: WlCallbackEvent,
        shared: &Arc<WindowShared>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if shared.redraw_requested.compare_exchange(
            true, false, Ordering::Relaxed, Ordering::Relaxed
        ).is_ok() {
            shared.frame_callback_registered.store(false, Ordering::Relaxed);
            evl.events.push(Event::Window { id: shared.id, event: WindowEvent::Redraw });
        }
    }
}

macro_rules! ignore {
    ($prxy:ident) => {
        fn event(
            _: &mut Self,
            _: &$prxy,
            _: <$prxy as wayland_client::Proxy>::Event,
            _: &(),
            _: &wayland_client::Connection,
            _: &wayland_client::QueueHandle<Self>
        ) { () }
    };
}

// global events
impl wayland_client::Dispatch<WlCompositor, ()> for UninitWaylandGlobals { ignore!(WlCompositor); }

// surface events
impl<T> wayland_client::Dispatch<WlSurface, ()> for Window<T> { ignore!(WlSurface); }

// region events
impl<T> wayland_client::Dispatch<WlRegion, ()> for EventLoop<T> { ignore!(WlRegion); }

// input events
impl<T> wayland_client::Dispatch<WlKeyboard, ()> for EventLoop<T> { ignore!(WlKeyboard); }
impl<T> wayland_client::Dispatch<WlPointer, ()> for EventLoop<T> { ignore!(WlPointer); }

#[derive(Debug)]
pub enum EvlError {
    Connect(wayland_client::ConnectError),
    Wayland(wayland_client::backend::WaylandError),
    Dispatch(wayland_client::DispatchError),
    Egl(egl::Error),
    NoDisplay,
    WaylandEgl(wayland_egl::Error),
    Io(io::Error),
    Unsupported,
    EglUnsupported,
}

impl fmt::Display for EvlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "windowing error: {}", match self {
            Self::Connect(value)    => value.to_string(),
            Self::Wayland(value)    => value.to_string(),
            Self::Dispatch(value)   => value.to_string(),
            Self::Egl(value)        => value.to_string(),
            Self::NoDisplay         => "cannot get egl display".to_string(),
            Self::WaylandEgl(value) => value.to_string(),
            Self::Io(value)         => value.to_string(),
            Self::Unsupported       => "required wayland features not present".to_string(),
            Self::EglUnsupported    => "required egl features not present".to_string(),
        })
    }
}

impl StdError for EvlError {}

impl From<wayland_client::ConnectError> for EvlError {
    fn from(value: wayland_client::ConnectError) -> Self {
        Self::Connect(value)
    }
}

impl From<wayland_client::backend::WaylandError> for EvlError {
    fn from(value: wayland_client::backend::WaylandError) -> Self {
        Self::Wayland(value)
    }
}

impl From<wayland_client::DispatchError> for EvlError {
    fn from(value: wayland_client::DispatchError) -> Self {
        Self::Dispatch(value)
    }
}

impl From<egl::Error> for EvlError {
    fn from(value: egl::Error) -> Self {
        Self::Egl(value)
    }
}

impl From<wayland_egl::Error> for EvlError {
    fn from(value: wayland_egl::Error) -> Self {
        Self::WaylandEgl(value)
    }
}

impl From<io::Error> for EvlError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

