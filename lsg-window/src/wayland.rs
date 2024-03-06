
use wayland_client::{
    protocol::{
        wl_registry::{WlRegistry, Event as WlRegistryEvent},
        wl_compositor::WlCompositor,
        // wl_shm::WlShm,
        wl_seat::{WlSeat, Event as WlSeatEvent, Capability as WlSeatCapability},
        wl_surface::WlSurface, wl_keyboard::WlKeyboard, wl_pointer::WlPointer,
    },
    WEnum, Proxy
};

use wayland_protocols::xdg::shell::client::{
    xdg_wm_base::XdgWmBase,
    xdg_surface::{XdgSurface, Event as XdgSurfaceEvent},
    xdg_toplevel::{XdgToplevel, Event as XdgToplevelEvent},
};

use khronos_egl as egl;

use std::{mem, fmt, ffi::c_void as void, error::Error as StdError};

pub struct EventLoop {
    running: bool,
    con: wayland_client::Connection,
    wayland: WaylandState,
    events: Vec<Event>, // ^^ also `taken` out but doesn't need to be an option
    ids: usize,
}

impl EventLoop {

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

        let this = EventLoop {
            running: true,
            con,
            wayland: globals,
            events: Vec::new(),
            ids: 0,
        };

        Ok(this)
        
    }

    pub fn run<F: for<'a> FnMut(&'a mut EventLoop, Event)>(mut self, mut cb: F) -> Result<(), EvlError> {

        let mut queue = self.con.new_event_queue();
        let qh = queue.handle();

        // connect keyboard and pointer input
        if self.wayland.capabilities.contains(WlSeatCapability::Keyboard) {
            self.wayland.seat.get_keyboard(&qh, ());
        } else if self.wayland.capabilities.contains(WlSeatCapability::Pointer) {
            self.wayland.seat.get_pointer(&qh, ());
        }

        self.events.push(Event::Init);

        // main event loop
        loop {

            // foreward the events to the callback
            // we `take` the events so we can pass the dispatcher into the closure
            let mut events = mem::take(&mut self.events); // this is cheap because Vec::default doesn't allocate
            for event in events.drain(..) { cb(&mut self, event) }
            self.events = events;

            if !self.running { break }

            // wait for new events
            // we have to take the queue out every time here because the queue has to be present when handling
            // events (above)
            // for example, the queue is used when a new window is created to wait for the `configure` event
            queue.blocking_dispatch(&mut self)?;

        }

        Ok(())
        
    }

    pub fn exit(&mut self) {
        self.running = false
    }

    pub(crate) fn id(&mut self) -> usize {
        self.ids += 1;
        self.ids
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

pub struct Window {
    // our data
    id: usize, // only used for some assertions so I don't fuck up accessing the windows
    configured: bool,
    width: i32,
    height: i32,
    // wayland state
    surface: WlSurface,
    xdg_surface: XdgSurface,
    xdg_toplevel: XdgToplevel,
    // egl state
    // egl: Option<InternalEgl>,
}

// struct InternalEgl {
//     _wl_egl_surface: wayland_egl::WlEglSurface, // needs to be kept alive
//     egl_surface: egl::Surface,
//     egl_context: egl::Context,
// }

impl Window {
    
    /// The window will initially be hidden, so you can setup title etc.
    pub fn new(evh: &mut EventLoop) -> Result<Window, EvlError> {

        let id = evh.id(); // the window id is the position in the array

        let mut queue = evh.con.new_event_queue();
        let qh = queue.handle();

        let surface = evh.wayland.compositor.create_surface(&qh, ());
        let xdg_surface = evh.wayland.wm.get_xdg_surface(&surface, &qh, ());
        let xdg_toplevel = xdg_surface.get_toplevel(&qh, ());

        surface.commit(); // commit the initial setup, the compositor will now send a `configure` event

        let mut this = Self {
            id,
            configured: false,
            width: 100, // a default of 100x100
            height: 100,
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

}

pub struct EglInstance {
    lib: egl::Instance<egl::Static>,
    display: egl::Display,
    config: egl::Config,
}

impl EglInstance {

    /// Should be only be called once. Although initializing multiple instances is not a hard error.
    pub fn new(evh: &mut EventLoop) -> Result<EglInstance, EvlError> {
        
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
                .ok_or(EvlError::Unsupported)? // todo: use different error
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
    _wl_egl_surface: wayland_egl::WlEglSurface, // needs to be kept alive
    egl_surface: egl::Surface,
    egl_context: egl::Context,
}

impl EglContext {

    /// Create a new egl context that will draw onto the given window.
    pub fn new(instance: &EglInstance, window: &Window) -> Result<Self, EvlError> {

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
            _wl_egl_surface: wl_egl_surface,
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

    pub fn swap_buffers(&self, instance: &EglInstance) -> Result<(), EvlError> {

        instance.lib.swap_buffers(instance.display, self.egl_surface)?;

        Ok(())

    }
    
}

pub enum Event {
    Init,
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

impl wayland_client::Dispatch<XdgSurface, ()> for Window {
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

impl wayland_client::Dispatch<XdgToplevel, ()> for Window {
    fn event(
        window: &mut Self,
        _surface: &XdgToplevel,
        event: XdgToplevelEvent,
        _data: &(),
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if let XdgToplevelEvent::Configure { width, height, .. } = event {
            if width + height != 0 {
                window.width = width;
                window.height = height;
            }
        }
    }
}

// global events
wayland_client::delegate_noop!(UninitWaylandGlobals: ignore WlCompositor);
// wayland_client::delegate_noop!(UninitWaylandGlobals: ignore WlShm);
wayland_client::delegate_noop!(UninitWaylandGlobals: ignore XdgWmBase);

// surface events
wayland_client::delegate_noop!(Window: ignore WlSurface);

// input events
wayland_client::delegate_noop!(EventLoop: ignore WlKeyboard);
wayland_client::delegate_noop!(EventLoop: ignore WlPointer);

#[derive(Debug)]
pub enum EvlError {
    Connect(wayland_client::ConnectError),
    Wayland(wayland_client::backend::WaylandError),
    Dispatch(wayland_client::DispatchError),
    Egl(egl::Error),
    NoDisplay,
    WaylandEgl(wayland_egl::Error),
    Unsupported,
}

impl fmt::Display for EvlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "windowing error: {}", match self {
            Self::Connect(value)    => value.to_string(),
            Self::Wayland(value)    => value.to_string(),
            Self::Dispatch(value)   => value.to_string(),
            Self::Egl(value)        => value.to_string(),
            Self::NoDisplay         => "cannot get egl display".to_string(), // TODO: maybe this error occurs if trying to initialize egl twice?
            Self::WaylandEgl(value) => value.to_string(),
            Self::Unsupported       => "required wayland or egl features not present".to_string(),
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
