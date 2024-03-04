
use wayland_client::{protocol::{
    wl_registry::{WlRegistry, Event as WlRegistryEvent},
    wl_compositor::{WlCompositor, Event as WlCompositorEvent},
    wl_shm::{WlShm, Event as WlShmEvent},
    wl_shm_pool::{WlShmPool, Event as WlShmPoolEvent},
    wl_seat::{WlSeat, Event as WlSeatEvent, Capability as WlSeatCapability},
    wl_shell::{WlShell, Event as WlShellEvent},
    wl_surface::WlSurface, wl_keyboard::WlKeyboard, wl_pointer::WlPointer,
}, QueueHandle, WEnum};

use wayland_protocols::xdg::shell::client::{
    xdg_wm_base::{XdgWmBase, Event as XdgWmBaseEvent},
    xdg_surface::{XdgSurface, Event as XdgSurfaceEvent},
    xdg_toplevel::{XdgToplevel, Event as XdgTopLevelEvent},
};

use std::mem;

pub struct EventLoop {
}

impl EventLoop {

    pub fn new() -> Self {

        Self {}
        
    }

    pub fn run<F: for<'a> FnMut(&'a mut Dispatcher, Event)>(mut self, mut cb: F) {

        let con = wayland_client::Connection::connect_to_env().expect("todo");

        // initial setup
        let globals = {
            
            let mut queue = con.new_event_queue();
            let qh = queue.handle();

            let mut empty_globals = UninitWaylandState::default();

            // register wayland globals
            let display = con.display();
            display.get_registry(&qh, ());

            // wait for the request to be processed
            con.roundtrip().expect("todo");
            queue.dispatch_pending(&mut empty_globals).expect("todo");

            // assert that all the globals are actually bound
            empty_globals.finalize().expect("todo")

        };

        // main event queue
        let mut queue = con.new_event_queue();

        let mut dispatcher = Dispatcher {
            running: true,
            qh: queue.handle(),
            wayland: globals,
            windows: Vec::new(),
            events: Vec::new(),
        };

        dispatcher.events.push(Event::Init);
        
        // main event loop
        while dispatcher.running {

            // foreward the events to the callback
            // we take out the events so we can pass the dispatcher into the closure

            let mut events = mem::take(&mut dispatcher.events); // this is cheap because Vec::default doesn't allocate

            for event in events.drain(..) {
                cb(&mut dispatcher, event);
            }

            dispatcher.events = events;

            queue.blocking_dispatch(&mut dispatcher).expect("todo");

        }
        
    }
    
}

/// Dispatches wayland events and creates wayland windows, etc.
pub struct Dispatcher {
    running: bool,
    qh: QueueHandle<Self>,
    wayland: WaylandState,
    windows: Vec<Window>,
    events: Vec<Event>,
}

struct WaylandState {
    compositor: WlCompositor,
    shm: WlShm,
    seat: WlSeat,
    wm: XdgWmBase,
}

#[derive(Default)]
struct UninitWaylandState {
    compositor: Option<WlCompositor>,
    shm: Option<WlShm>,
    seat: Option<WlSeat>,
    wm: Option<XdgWmBase>,
}

impl UninitWaylandState {
    pub(crate) fn finalize(self) -> Option<WaylandState> {
        Some(WaylandState {
            compositor: self.compositor?,
            shm: self.shm?,
            seat: self.seat?,
            wm: self.wm?
        })
    }
}

type WindowId = usize;

/// A cloned `Window` will refer to the same underlying window.
#[derive(Clone)]
pub struct Window {
    id: WindowId, // only used for some assertions so I don't fuck up accessing the windows
    surface: WlSurface,
    xdg_surface: XdgSurface,
    xdg_toplevel: XdgToplevel
}

impl Window {
    
    /// The window will initially be hidden, so you can setup title etc.
    pub fn new(self, evh: &mut Dispatcher) -> Window {

        let id = evh.windows.len(); // the window id is the position in the array

        let surface = evh.wayland.compositor.create_surface(&evh.qh, ());
        let xdg_surface = evh.wayland.wm.get_xdg_surface(&surface, &evh.qh, id);
        let xdg_toplevel = xdg_surface.get_toplevel(&evh.qh, ());

        surface.commit(); // commit the initial setup, now we have to handle the `configure` event

        let window = Window {
            id,
            surface,
            xdg_surface,
            xdg_toplevel
        };

        // let id = evh.windows.len();
        // evh.windows.push()

        window
        
    }

    pub fn title<S: Into<String>>(&self, text: S) {
        self.xdg_toplevel.set_title(text.into());
        self.surface.commit();
    }

}

pub enum Event {
    Init,
}

impl wayland_client::Dispatch<WlRegistry, ()> for UninitWaylandState {
    fn event(
        this: &mut Self,
        registry: &WlRegistry,
        event: WlRegistryEvent,
        _data: &(),
        _con: &wayland_client::Connection,
        qh: &wayland_client::QueueHandle<Self>
    ) {
        if let WlRegistryEvent::Global { name, interface, .. } = event {
            match &interface[..] {
                "wl_compositor" => this.compositor = Some(registry.bind(name, 1, qh, ())),
                "wl_shm"        => this.shm        = Some(registry.bind(name, 1, qh, ())),
                "wl_seat"       => this.seat       = Some(registry.bind(name, 1, qh, ())),
                "xdg_wm_base"   => this.wm         = Some(registry.bind(name, 1, qh, ())),
                _ => ()
            }
        }
    }
}

impl wayland_client::Dispatch<XdgSurface, WindowId> for Dispatcher {
    fn event(
        this: &mut Self,
        surface: &XdgSurface,
        event: XdgSurfaceEvent,
        id: &usize,
        _con: &wayland_client::Connection,
        qh: &wayland_client::QueueHandle<Self>
    ) {
        let XdgSurfaceEvent::Configure { serial } = event else { unreachable!() };
        this.windows[*id].xdg_surface.ack_configure(serial);
    }
}

impl wayland_client::Dispatch<WlSeat, ()> for Dispatcher {
    fn event(
        this: &mut Self,
        seat: &WlSeat,
        event: WlSeatEvent,
        _data: &(),
        _con: &wayland_client::Connection,
        qh: &wayland_client::QueueHandle<Self>
    ) {
        if let WlSeatEvent::Capabilities { capabilities: WEnum::Value(capabilities) } = event {
            if      capabilities.contains(WlSeatCapability::Keyboard) { seat.get_keyboard(qh, ()); }
            else if capabilities.contains(WlSeatCapability::Pointer)  { seat.get_pointer(qh, ()); }
        }
    }
}

// global events
wayland_client::delegate_noop!(UninitWaylandState: ignore WlCompositor);
wayland_client::delegate_noop!(UninitWaylandState: ignore WlShm);
wayland_client::delegate_noop!(UninitWaylandState: ignore XdgWmBase);

// surface events
wayland_client::delegate_noop!(Dispatcher: ignore WlSurface);
wayland_client::delegate_noop!(Dispatcher: ignore XdgToplevel);

// input events
wayland_client::delegate_noop!(Dispatcher: ignore WlKeyboard);
wayland_client::delegate_noop!(Dispatcher: ignore WlPointer);
