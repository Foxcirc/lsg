
// ### imports ###

use wayland_client::{
    EventQueue, Proxy, QueueHandle, WEnum, backend::WaylandError, globals::{BindError, GlobalList, GlobalListContents, registry_queue_init}, protocol::{
        wl_buffer::WlBuffer, wl_callback::{Event as WlCallbackEvent, WlCallback}, wl_compositor::WlCompositor, wl_data_device::{Event as WlDataDeviceEvent, WlDataDevice}, wl_data_device_manager::{DndAction, WlDataDeviceManager}, wl_data_offer::{Event as WlDataOfferEvent, WlDataOffer}, wl_data_source::{Event as WlDataSourceEvent, WlDataSource}, wl_keyboard::{Event as WlKeyboardEvent, KeyState, WlKeyboard}, wl_output::{Event as WlOutputEvent, Mode as WlOutputMode, WlOutput}, wl_pointer::{Axis, ButtonState, Event as WlPointerEvent, WlPointer}, wl_region::WlRegion, wl_registry::{Event as WlRegistryEvent, WlRegistry}, wl_seat::{Capability as WlSeatCapability, Event as WlSeatEvent, WlSeat}, wl_shm::{Format as WlFormat, WlShm}, wl_shm_pool::WlShmPool, wl_surface::WlSurface
    }
};

use wayland_protocols::xdg::{
    shell::client::{
        xdg_wm_base::{XdgWmBase, Event as XdgWmBaseEvent},
        xdg_surface::{XdgSurface, Event as XdgSurfaceEvent},
        xdg_toplevel::{XdgToplevel, Event as XdgToplevelEvent, State as XdgToplevelState},
        // xdg_popup::{XdgPopup, Event as XdgPopupEvent},
        xdg_positioner::XdgPositioner,
    },
    decoration::zv1::client::{zxdg_decoration_manager_v1::ZxdgDecorationManagerV1, zxdg_toplevel_decoration_v1::{ZxdgToplevelDecorationV1, Event as ZxdgDecorationEvent, Mode as ZxdgDecorationMode}},
    activation::v1::client::{xdg_activation_v1::XdgActivationV1, xdg_activation_token_v1::{XdgActivationTokenV1, Event as XdgActivationTokenEvent}},
};

use wayland_protocols::wp::{
    fractional_scale::v1::client::{wp_fractional_scale_manager_v1::WpFractionalScaleManagerV1, wp_fractional_scale_v1::{WpFractionalScaleV1, Event as WpFractionalScaleV1Event}},
    viewporter::client::{wp_viewporter::WpViewporter, wp_viewport::WpViewport},
    cursor_shape::v1::client::{wp_cursor_shape_manager_v1::WpCursorShapeManagerV1, wp_cursor_shape_device_v1::{WpCursorShapeDeviceV1, Shape as WlCursorShape}},
};

use wayland_protocols_wlr::layer_shell::v1::client::{
    zwlr_layer_shell_v1::ZwlrLayerShellV1,
    // zwlr_layer_surface_v1::{ZwlrLayerSurfaceV1, Event as ZwlrLayerSurfaceEvent, Anchor, KeyboardInteractivity},
};

use xkbcommon::xkb;

use async_io::{Async, Timer};
use futures_lite::FutureExt;

use std::{
    env, io, mem, task,
    collections::{HashMap, VecDeque},
    ffi::c_void as void,
    os::fd::{AsFd, OwnedFd},
    sync::Arc,
    time::{Duration, Instant}
};

use crate::*;
use common::*;

// ### base event loop ###

pub(crate) struct ConnectionState {
    appid: String,
    pub(crate) con: Async<wayland_client::Connection>,
    qh: QueueHandle<Self>,
    globals: WaylandGlobals,
    // -- outputs --
    events: VecDeque<crate::Event>, // used to push events from inside dispatch
    errors: VecDeque<crate::EvlError>, // used to push errors from inside dispatch
    // -- windowing state --
    windows: WindowData,
    mouse: MouseData,
    keyboard: KeyboardData,
    offer: OfferData, // drag-and-drop / selection data
    monitors: MonitorData,
    last_serial: u32, // used to sign some events
}

#[derive(Default)]
struct WindowData {
    inner: HashMap<crate::Id, WindowState>,
}

impl WindowData {
    pub fn get(&mut self, id: crate::Id) -> &mut WindowState {
        self.inner.get_mut(&id).expect("tried to access invalid window")
    }
}

#[derive(Default)]
struct MonitorData {
    inner: HashMap<Id, (MonitorInfo, WlOutput)>,
}

/// Used for handling drag-and-drop and selections.
#[derive(Default)]
struct OfferData {
    advertised_data_kinds: Vec<DataKind>,
    dnd: DndOfferData,
}

#[derive(Default)]
struct DndOfferData {
    focused: Option<WlSurface>,
    current: Option<WlDataOffer>,
    x: f64,
    y: f64,
    ours: bool, // set if we started a drag and drop
    icon: Option<CustomIcon>, // set if we started a drag and drop
}

#[derive(Default)]
struct MouseData {
    focused: Option<WlSurface>,
    pos: common::PhysicalPoint,
    last_enter_serial: u32,
}

struct KeyboardData {
    focused: Option<WlSurface>,
    xkb_context: xkb::Context,
    keymap_specific: Option<KeymapSpecificData>, // (re)initialized when a keymap is loaded
    repeat_timer: Timer,
    repeat_key: u32, // raw key
    repeat_rate: Duration,
    repeat_delay: Duration,
}

impl KeyboardData {
    pub fn new() -> Self {
        Self {
            focused: None,
            xkb_context: xkb::Context::new(xkb::CONTEXT_NO_FLAGS),
            keymap_specific: None,
            repeat_timer: Timer::never(),
            repeat_key: 0,
            repeat_rate: Duration::from_millis(60),
            repeat_delay: Duration::from_millis(450),
        }
    }
}

struct KeymapSpecificData {
    xkb_state: xkb::State,
    compose_state: xkb::compose::State,
    pressed_keys: PressedKeys,
}

// ### pressed keys ###

struct PressedKeys {
    min: u32,
    keys: Vec<bool>
}

impl PressedKeys {

    pub fn new(keymap: &xkb::Keymap) -> Self {
        let min = keymap.min_keycode();
        let max = keymap.max_keycode();
        let len = max.raw() - min.raw();
        let mut keys = Vec::new();
        keys.resize(len as usize, false);
        Self {
            min: min.raw(),
            keys,
        }
    }

    pub fn update_key_state(&mut self, key: xkb::Keycode, state: KeyState) {
        let pressed = state == KeyState::Pressed;
        let idx = key.raw() - self.min;
        self.keys[idx as usize] = pressed;
    }

    pub fn write_currently_pressed(&self, out: &mut [xkb::Keycode]) -> usize {

        let mut written = 0;

        for idx in 0..self.keys.len() {
            if self.keys[idx] == true {
                let keycode = xkb::Keycode::from(self.min + idx as u32);
                out[written] = keycode;
                written += 1;
            }
        }

        written

    }

}

// ### async event loop ### TODO: rework these comments

pub(crate) struct Connection {
    state: ConnectionState,
    queue: EventQueue<ConnectionState>,
}

impl Connection {

    pub fn new(application: &str) -> Result<Self, crate::EvlError> {

        let con = Async::new(
            wayland_client::Connection::connect_to_env()?
        )?;

        let (globals, queue) = registry_queue_init::<ConnectionState>(con.get_ref())?;
        let qh = queue.handle();

        let globals = WaylandGlobals::from_globals(globals, &qh)?;

        let state = ConnectionState {
            appid: application.to_string(),
            con, qh, globals,
            events: VecDeque::from([crate::Event::Resume]),
            errors: VecDeque::new(),
            windows: WindowData::default(),
            mouse: MouseData::default(),
            keyboard: KeyboardData::new(),
            offer: OfferData::default(),
            monitors: MonitorData::default(),
            last_serial: 0,
        };

        Ok(Self {
            state,
            queue,
        })

    }

    pub fn poll(&mut self, cx: &mut task::Context<'_>) -> task::Poll<Result<crate::Event, crate::EvlError>> {

        // 1.
        // try to read new data from the connection

        loop {
            if let Some(guard) = self.queue.prepare_read() {
                ignore_wouldblock(guard.read_without_dispatch())?;
            }
            if let task::Poll::Ready(res) = self.state.con.poll_readable(cx) {
                res?;
            } else {
                break
            }
        }

        self.queue.flush()?;
        self.queue.dispatch_pending(&mut self.state)?;

        // 2.
        // check if the key-repeat timer is ready

        if let task::Poll::Ready(..) = self.state.keyboard.repeat_timer.poll(cx) {
            // insert the synthetic key-repeat event
            let key = self.state.keyboard.repeat_key;
            process_key_event(&mut self.state, key, KeyDirection::Down, KeySource::Repeat);
        }

        // 4.
        // foreward events

        if let Some(event) = self.state.events.pop_front() {
            task::Poll::Ready(Ok(event))
        } else if let Some(error) = self.state.errors.pop_front() {
            task::Poll::Ready(Err(error))
        } else {
            task::Poll::Pending
        }


    }

    pub fn display(&self) -> *mut void {
        self.state.con.get_ref()
            .display().id().as_ptr().cast()
    }

}

fn ignore_wouldblock<T>(result: Result<T, WaylandError>) -> Result<(), WaylandError> {
    match result {
        Ok(..) => Ok(()),
        Err(WaylandError::Io(ref err)) if err.kind() == io::ErrorKind::WouldBlock => Ok(()),
        Err(other) => Err(other),
    }
}

struct WaylandGlobals {
    compositor: WlCompositor,
    wm: XdgWmBase,
    shm: WlShm,
    seat: WlSeat,
    pointer: Option<WlPointer>,
    shape_device: Option<WpCursorShapeDeviceV1>,
    data_device_mgr: WlDataDeviceManager,
    data_device: WlDataDevice,
    frac_scale_mgrs: Option<FracScaleMgrs>,
    decoration_mgr: Option<ZxdgDecorationManagerV1>,
    // layer_shell_mgr: Option<ZwlrLayerShellV1>,
    activation_mgr: Option<XdgActivationV1>,
    cursor_shape_mgr: Option<WpCursorShapeManagerV1>,
}

impl WaylandGlobals {

    pub fn from_globals(globals: GlobalList, qh: &QueueHandle<ConnectionState>) -> Result<Self, BindError> {

        // we don't support processing multiple seats
        let seat: WlSeat = globals.bind(qh, 1..=4, ())?;

        // bind the data device, for this seat
        let data_device_mgr: WlDataDeviceManager = globals.bind(qh, 1..=3, ())?; // < v3 doesn't emit cancelled events
        let data_device = data_device_mgr.get_data_device(&seat, qh, ());

        let this = Self {
            compositor: globals.bind(qh, 4..=6, ())?,
            wm: globals.bind(qh, 1..=1, ())?,
            shm: globals.bind(qh, 1..=1, ())?,
            seat,
            pointer: None,
            shape_device: None, // TODO: should this be an Option? we could wait for this thing to be received using `roundtrip`... what do we even do if this is not present rn?
            data_device_mgr,
            data_device,
            // frac_scale_mgrs: globals.bind(qh, 1..=1, ()).ok().and_then( // only Some if both are present
            //     |vp| Some((vp, globals.bind(qh, 1..=1, ()).ok()?)))
            //     .map(|(vp, frc)| FracScaleMgrs { viewport_mgr: vp, frac_scaling_mgr: frc }),
            frac_scale_mgrs: globals.bind(qh, 1..=1, ()).ok()
                .map(|it| FracScaleMgrs { frac_scaling_mgr: it }),
            decoration_mgr: globals.bind(qh, 1..=1, ()).ok(),
            // layer_shell_mgr: globals.bind(qh, 1..=1, ()).ok(),
            activation_mgr: globals.bind(qh, 1..=1, ()).ok(),
            cursor_shape_mgr: globals.bind(qh, 1..=1, ()).ok(),
        };

        Ok(this)

    }
}

struct FracScaleMgrs {
    // viewport_mgr: WpViewporter,
    frac_scaling_mgr: WpFractionalScaleManagerV1,
}

struct FracScaleData {
    // viewport: WpViewport,
    frac_scale: WpFractionalScaleV1,
}

// ### monitor info ###

fn get_object_id<T: Proxy>(object: &T) -> u32 {
    object.id().protocol_id()
}

pub struct MonitorBackend {
    wl_output: WlOutput,
}

// ### window ###

struct WindowState {
    wl_surface: WlSurface,
    xdg_surface: XdgSurface,
    xdg_toplevel: XdgToplevel,
    xdg_decoration: Option<ZxdgToplevelDecorationV1>,
    frac_scale_data: Option<FracScaleData>,
    scale: f64, // used to convert between logical and physical sizes
    size: common::PhysicalSize, // set by the xdg-toplevel configure event
    fullscreen: bool,
    hidden: bool,
    redraw: WindowRedrawStateV2,
}

#[derive(Default)]
struct WindowRedrawStateV2 {
    /// Used to check if a frame event was already requested.
    should_emit_event: bool,
    /// Used to check if a frame callback is currently in-flight or if a redraw event
    /// has to be "force generated".
    frame_callback_registered: bool,
    /// This flag is set everytime a redraw event is finally pushed onto the event queue
    /// and used to assure only a single redraw event will be generated each frame.
    already_got_event: bool,
}

impl Drop for WindowState {
    fn drop(&mut self) {

        self.xdg_surface.destroy();
        self.xdg_toplevel.destroy();

        self.xdg_decoration.as_ref()
            .inspect(|val| val.destroy());

        self.frac_scale_data.as_ref().inspect(|it| {
            // it.viewport.destroy();
            it.frac_scale.destroy();
        });

    }
}

pub struct WindowBackend {
    pub id: Id,
    evl: Arc<EventLoop>,
}

impl Drop for WindowBackend {
    fn drop(&mut self) {
        // Remove the stored window from the event loop.
        // Further cleanup is done by WindowState::drop.
        let evb = &mut self.evl.backend.state.lock().wayland.state;
        evb.windows.inner.remove(&self.id);
    }
}

impl WindowBackend {

    pub fn new(evl: &Arc<EventLoop>) -> Self {

        let evb = &mut evl.backend.state.lock().wayland.state;

        let surface = evb.globals.compositor.create_surface(&evb.qh, ());
        let id = get_object_id(&surface);

        // enable fractional scaling, if supported
        let frac_scale_data = evb.globals.frac_scale_mgrs.as_ref().map(|val| {
            // let viewport = val.viewport_mgr.get_viewport(&surface, &evb.qh, ());
            let frac_scale = val.frac_scaling_mgr.get_fractional_scale(&surface, &evb.qh, Id(id));
            FracScaleData { /* viewport, */ frac_scale }
        });

        // assign xdg-top-level role (+ init decoration manager)
        let xdg_surface = evb.globals.wm.get_xdg_surface(&surface, &evb.qh, Id(id));
        let xdg_toplevel = xdg_surface.get_toplevel(&evb.qh, Id(id));
        let xdg_decoration = evb.globals.decoration_mgr.as_ref()
            .map(|it| it.get_toplevel_decoration(&xdg_toplevel, &evb.qh, Id(id)));

        xdg_decoration.as_ref().map(|val| val.set_mode(ZxdgDecorationMode::ServerSide));
        xdg_toplevel.set_app_id(evb.appid.clone());

        // The window will be hidden, since it does not have a buffer attached yet.
        surface.commit();

        let state = WindowState {
            wl_surface: surface,
            xdg_surface,
            xdg_toplevel,
            xdg_decoration,
            frac_scale_data,
            scale: 1.0,
            size: PhysicalSize::new(500, 500), // bigger isn't always better <3
            fullscreen: false,
            hidden: false,
            redraw: WindowRedrawStateV2::default(),
        };

        evb.windows.inner.insert(Id(id), state);

        Self {
            id: Id(id),
            evl: Arc::clone(evl)
        }

    }

    pub fn id(&self) -> Id {
        self.id
    }

    pub fn present(&self) {

        // TODO: with the current logic I think we could call this from withing the event loop
        // everytime a redraw event is received! TRY  THAT SHIT
        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        // we are now processing the redraw event, so we can receive another one later
        // note: it is important that resetting this is not done inside the if-check below, since this might
        //       happen when a frame callback is still in-flight due to a redraw being triggered by a configure event
        //       that arrived before the frame callback completed, but we ALWAYS have to reset the variable
        state.redraw.already_got_event = false;

        // you have to request the frame callback before swapping buffers.
        // really, the frame callback will start counting from the moment the buffers are swapped
        if !state.redraw.frame_callback_registered { // make sure to only request a frame callback once
            state.redraw.frame_callback_registered = true;
            state.wl_surface.frame(&evb.qh, self.id);
            state.wl_surface.commit();
        }

    }

    /// Tells the windowing systen to redraw the window.
    ///
    /// Don't forget to call [`present`](Self::present).
    ///
    /// The next redraw will automatically be throttled to align with the "desired"
    /// framerate that may be chosen by the system. In most cases, this is the refresh
    /// rate of the monitor.
    ///
    /// In practice this means you can call this function as often or as rarely as you want and
    /// it will always generate at most one redraw event for every monitor frame.
    #[track_caller]
    pub fn redraw(&self) {

        let mut guard = self.evl.backend.state.lock();
        let evb = &mut guard.wayland.state;
        let window = evb.windows.get(self.id);

        if window.redraw.frame_callback_registered {
            // since a frame callback is currently in-flight which means we are wanting
            // to redraw faster then the monitor refresh rate, we will wait for vsync
            window.redraw.should_emit_event = true;
        } else if !window.redraw.already_got_event {
            // force-redraw, since we are apperently drawing slower then the monitor refresh rate
            window.redraw.already_got_event = true; // will be reset next frame by `pre_present_notify`
            evb.events.push_back(Event::Window { id: self.id, event: WindowEvent::Redraw });
        } else {
            // it would be nice to get a friendly panic here, but this
            // case is possible when a redraw event is dispatched along with
            // another event, but the other event is handeled first and causes
            // a call to redraw.
            // panic!("forgot to call `window.present` before drawing");
        }

    }

    pub fn transparency(&self, value: bool) {

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        if value {
            state.wl_surface.set_opaque_region(None);
        } else {
            let region = evb.globals.compositor.create_region(&evb.qh, ());
            region.add(0, 0, i32::MAX, i32::MAX);
            state.wl_surface.set_opaque_region(Some(&region));
        }

        state.wl_surface.commit();

    }

    pub fn decorations(&self, value: bool) {

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        let mode = if value { ZxdgDecorationMode::ServerSide } else { ZxdgDecorationMode::ClientSide };
        state.xdg_decoration.as_ref().map(|val| val.set_mode(mode));

    }

    pub fn title(&self, text: &str) {

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        state.xdg_toplevel.set_title(text.to_string());
    }

    pub fn maximize(&self, value: bool) {

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        if value {
            state.xdg_toplevel.set_maximized();
        } else {
            state.xdg_toplevel.unset_maximized();
        };

        state.wl_surface.commit();

    }

    pub fn fullscreen(&self, value: bool, monitor: Option<&Monitor>) {

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        if value {
            let wl_output = monitor.map(|val| &val.backend.wl_output);
            state.xdg_toplevel.set_fullscreen(wl_output);
        } else {
            state.xdg_toplevel.unset_fullscreen();
        };

    }

    /// If this is called before the window is first shown, the provided
    /// size will be used if the system doesn't provide another.
    #[track_caller]
    pub fn sizehint(&self, size: PhysicalSize) {

        // TODO: make sure this is asserted/handeled/documented in all places:
        assert!(size.w > 0 && size.h > 0);

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        // TODO: make sure the size is not updated inappropriatly. right now if you call this randomly the size property will be put in a "wrong" state

        // Scale to window-geometry-space:
        state.size = size.scale(1.0 / state.scale);

    }

    pub fn minsize(&self, size: Option<PhysicalSize>) {

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        // Scale to window-geometry-space:
        let size = size.unwrap_or_default()
            .scale(1.0 / state.scale);

        state.xdg_toplevel.set_min_size(size.w as i32, size.h as i32);
        state.wl_surface.commit();

    }

    pub fn maxsize(&self, size: Option<PhysicalSize>) {

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        // Scale to window-geometry-space:
        let size = size.unwrap_or_default()
            .scale(1.0 / state.scale);

        state.xdg_toplevel.set_max_size(size.w as i32, size.h as i32);
        state.wl_surface.commit();

    }

    /// Aka. request-user-attention
    pub fn alert(&self, urgency: Urgency) {

        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);

        if let Urgency::Info = urgency {
            // we don't wanna switch focus, but on wayland just showing a
            // blinking icon is not possible
            return
        }

        if let Some(ref activation_mgr) = evb.globals.activation_mgr {

            let token = activation_mgr.get_activation_token(&evb.qh, state.wl_surface.clone());

            token.set_app_id(evb.appid.clone());
            token.set_serial(evb.last_serial, &evb.globals.seat);

            if let Some(ref surface) = evb.keyboard.focused {
                token.set_surface(surface);
            }

            token.commit();

        }

    }

    pub fn ptr(&self) -> *mut void {
        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);
        state.wl_surface.id().as_ptr().cast()
    }

    pub fn size(&self) -> PhysicalSize {
        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);
        state.size
    }

    pub fn scale(&self) -> f64 {
        let evb = &mut self.evl.backend.state.lock().wayland.state;
        let state = evb.windows.get(self.id);
        state.scale
    }

}

impl CursorStyle {
    pub fn apply(&self, evl: &EventLoop) {

        let evb = &evl.backend.state.lock().wayland.state;

        let serial = evb.mouse.last_enter_serial;

        match self {
            CursorStyle::Hidden => {
                if let Some(ref wl_pointer) = evb.globals.pointer {
                    wl_pointer.set_cursor(serial, None, 0, 0);
                }
            },
            CursorStyle::Custom { icon, hotspot } => {
                if let Some(ref wl_pointer) = evb.globals.pointer {
                    wl_pointer.set_cursor(
                        serial, Some(&icon.backend.wl_surface),
                        hotspot.x as i32, hotspot.y as i32
                    )
                }
            },
            CursorStyle::Predefined { shape } => {
                if let Some(ref wp_shape_device) = evb.globals.shape_device {
                    let wl_shape = shape.to_wl();
                    wp_shape_device.set_shape(serial, wl_shape);
                }
            }
        }

    }
}

impl CursorShape {
    pub(crate) fn to_wl(&self) -> WlCursorShape {
        match self {
            Self::Default => WlCursorShape::Default,
            Self::ContextMenu => WlCursorShape::ContextMenu,
            Self::Help => WlCursorShape::Help,
            Self::Pointer => WlCursorShape::Pointer,
            Self::Progress => WlCursorShape::Progress,
            Self::Wait => WlCursorShape::Wait,
            Self::Cell => WlCursorShape::Cell,
            Self::Crosshair => WlCursorShape::Crosshair,
            Self::Text => WlCursorShape::Text,
            Self::VerticalText => WlCursorShape::VerticalText,
            Self::Alias => WlCursorShape::Alias,
            Self::Copy => WlCursorShape::Copy,
            Self::Move => WlCursorShape::Move,
            Self::NoDrop => WlCursorShape::NoDrop,
            Self::NotAllowed => WlCursorShape::NotAllowed,
            Self::Grab => WlCursorShape::Grab,
            Self::Grabbing => WlCursorShape::Grabbing,
            Self::EResize => WlCursorShape::EResize,
            Self::NResize => WlCursorShape::NResize,
            Self::NeResize => WlCursorShape::NeResize,
            Self::NwResize => WlCursorShape::NwResize,
            Self::SResize => WlCursorShape::SResize,
            Self::SeResize => WlCursorShape::SeResize,
            Self::SwResize => WlCursorShape::SwResize,
            Self::WResize => WlCursorShape::WResize,
            Self::EwResize => WlCursorShape::EwResize,
            Self::NsResize => WlCursorShape::NsResize,
            Self::NeswResize => WlCursorShape::NeswResize,
            Self::NwseResize => WlCursorShape::NwseResize,
            Self::ColResize => WlCursorShape::ColResize,
            Self::RowResize => WlCursorShape::RowResize,
            Self::AllScroll => WlCursorShape::AllScroll,
            Self::ZoomIn => WlCursorShape::ZoomIn,
            Self::ZoomOut => WlCursorShape::ZoomOut,
        }
    }
}

// ### drag and drop ###

impl DataKind {
    pub(crate) fn to_mime_type(&self) -> &'static str {
        match *self {
            DataKind::Text  => "text/plain",
            DataKind::Xml   => "application/xml",
            DataKind::Html  => "application/html",
            DataKind::Zip   => "application/zip",
            DataKind::Json  => "text/json",
            DataKind::Jpeg  => "image/jpeg",
            DataKind::Png   => "image/png",
            DataKind::Other => "application/octet-stream",
        }
    }
    pub(crate) fn from_mime_type(mime_type: &str) -> Option<Self> {
        match mime_type {
            "text/plain"       => Some(DataKind::Text),
            "application/xml"  => Some(DataKind::Xml),
            "application/html" => Some(DataKind::Html),
            "application/zip"  => Some(DataKind::Zip),
            "text/json"        => Some(DataKind::Json),
            "image/jpeg"       => Some(DataKind::Jpeg),
            "image/png"        => Some(DataKind::Png),
            "UTF8_STRING" |
            "STRING" |
            "TEXT" => Some(DataKind::Text), // apparently used in some X11 apps
            "application/octet-stream" => Some(DataKind::Other),
            _ => None,
        }
    }
}

/// Don't hold onto it. You should immediatly decide if you want to receive something or not.
pub struct DataReadableBackend {
    wl_data_offer: WlDataOffer,
    data_kinds: Vec<DataKind>,
    dnd: bool, // checked in the destructor to determine how wl_data_offer should be destroyed
}

/// Dropping this will cancel drag-and-drop.
impl Drop for DataReadableBackend {
    fn drop(&mut self) {
        if self.dnd { self.wl_data_offer.finish() };
        self.wl_data_offer.destroy();
    }
}

impl DataReadableBackend {

    pub fn kinds(&self) -> &[DataKind] {
        &self.data_kinds
    }

    /// A `DataOffer` can be read multiple times. Also using different `DataKinds`.
    #[track_caller]
    pub fn receive(&self, evl: &EventLoop, kind: DataKind) -> DataReaderBackend {

        let (reader, writer) = io::pipe().expect("cannot create pipe");

        // receive the data
        let mime_type = kind.to_mime_type();
        self.wl_data_offer.receive(mime_type.to_string(), writer.as_fd());

         // This is important! We need the compositor to inform the other side
         // that we want to read now, otherwise reading immediatly would deadlock.
        evl.backend.state.lock().wayland.state.con.get_ref().flush()
            .expect("cannot flush wayland socket");

        DataReaderBackend {
            inner: reader,
        }

    }

}

pub struct DataReaderBackend {
    inner: io::PipeReader,
}

impl DataReaderBackend {
    // fn as_fd(&self) -> std::os::fd::BorrowedFd<'_> {
    //     self.inner.as_fd()
    // }
    pub fn read(&mut self, buf: &mut [u8]) -> Result<usize, ()> {
        io::Read::read(&mut self.inner, buf)
            .map_err(|_| ())
    }
}

#[derive(Debug)]
pub struct DataWriterBackend {
    inner: io::PipeWriter,
}

impl DataWriterBackend {
    // fn as_fd(&self) -> std::os::fd::BorrowedFd<'_> {
    //     self.inner.as_fd()
    // }
    pub fn write(&mut self, buf: &[u8]) -> Result<usize, ()> {
        let result = io::Write::write(&mut self.inner, buf).map_err(|_| ())?;
        // Make sure to flush, so we actually send the data.
        io::Write::flush(&mut self.inner).map_err(|_| ())?;
        Ok(result)
    }
    // fn flush(&mut self) -> io::Result<()> {
    //     self.inner.flush()
    // }
}

/// A handle that let's you send data to other clients. Used for clipboard and drag-and-drop.
///
/// You will receive events for this DataSource when another client
/// or the system wants to read from the selection.
pub struct DataWritableBackend {
    pub id: Id,
    wl_data_source: WlDataSource,
}

/// Dropping this will cancel a drag-and-drop operation.
impl Drop for DataWritableBackend {
    fn drop(&mut self) {
        self.wl_data_source.destroy();
    }
}

impl DataWritableBackend {

    pub fn id(&self) -> Id {
        self.id
    }

    /// Create a DataSource that will be the new selection.
    ///
    /// In other words this "sets the selection (clipboard)". You will receive events for this DataSource when another client
    /// wants to read from the selection.
    // TODO + DOCS: docs-rs alias to "clipboard" or smth
    pub fn selection(evl: &EventLoop, offers: &[DataKind]) -> Self {

        let this = Self::new(evl, offers);

        let evb = &evl.backend.state.lock().wayland.state;

        evb.globals.data_device.set_selection(
            Some(&this.wl_data_source),
            evb.last_serial
        );

        this

    }

    /// You should only start a drag-and-drop when the left mouse button is held down
    /// *and* the user then moves the mouse.
    /// Otherwise the request may be denied or visually broken.
    #[track_caller]
    pub fn dnd(handle: &Window, offers: &[DataKind], icon: CustomIcon) -> Self {

        let this = Self::new(&handle.backend.evl, offers);

        let evb = &mut handle.backend.evl.backend.state.lock().wayland.state;
        let window = evb.windows.get(handle.backend.id);

        // actually start the drag and drop
        evb.globals.data_device.start_drag(
            Some(&this.wl_data_source),
            &window.wl_surface,
            Some(&icon.backend.wl_surface),
            evb.last_serial
        );

        evb.offer.dnd.ours = true;
        evb.offer.dnd.icon = Some(icon);

        this

    }

    fn new(evl: &EventLoop, offers: &[DataKind]) -> Self {

        let evb = &evl.backend.state.lock().wayland.state;

        debug_assert!(!offers.len() != 0, "must offer at least one DataKind");

        let wl_data_source = evb.globals.data_device_mgr.create_data_source(&evb.qh, ());

        for offer in offers {
            let mime_type = offer.to_mime_type();
            wl_data_source.offer(mime_type.to_string()); // why do all wayland methods take String's and not &str?
        }

        // actions are not implemented right now
        wl_data_source.set_actions(DndAction::Move | DndAction::Copy);

        Self {
            id: Id(get_object_id(&wl_data_source)),
            wl_data_source,
        }

    }

}

// ### custom icon ###

pub struct CustomIconBackend {
    wl_shm_pool: WlShmPool,
    wl_buffer: WlBuffer,
    wl_surface: WlSurface,
    _mapping: OwnedFd,
}

/// The icon surface and all memory related to it, is destroyed on drop.
/// The `CustomIcon` needs to be alive as long as it is displayed.
impl Drop for CustomIconBackend {
    fn drop(&mut self) {
        self.wl_surface.destroy();
        self.wl_buffer.destroy();
        self.wl_shm_pool.destroy();
        // self._mapping will be closed automatically
    }
}

impl CustomIconBackend {

    /// # Panic (debug-only)
    /// This function assserts that the `size` is valid for `data.len()`
    /// and also that `size > 0`.
    #[track_caller]
    pub fn new(evl: &EventLoop, size: PhysicalSize, format: IconFormat, data: &[u8]) -> Self {

        use nix::{sys::{mman, stat}, fcntl, unistd};
        use std::num::NonZeroUsize;

        let evb = &evl.backend.state.lock().wayland.state;

        let (wl_format, bytes_per_pixel) = match format {
            IconFormat::Argb8 => (WlFormat::Argb8888, 4i32),
        };

        // some basic checks that the dimensions of the data match the specified size

        let len = NonZeroUsize::new(data.len())
            .expect("length of data must be > 0");

        debug_assert!(
            len.get() == size.w as usize * size.h as usize * bytes_per_pixel as usize,
            "length of data doesn't match specified dimensions and format"
        );

        // create the shared memory mapping

        let mapping = mman::shm_open(
            &evb.appid[..],
            fcntl::OFlag::O_CREAT | fcntl::OFlag::O_RDWR,
            stat::Mode::S_IRWXU
        ).expect("open shared memory");

        unistd::ftruncate(mapping.as_fd(), len.get() as i64)
            .expect("truncate file");

        unsafe {

            // Map the region as shared memory and write our data.

            let ptr = mman::mmap(
                None, len,
                mman::ProtFlags::PROT_WRITE,
                mman::MapFlags::MAP_SHARED,
                mapping.as_fd(),
                0 /* offset */
            ).expect("map shared memory");

            ptr.as_ptr().cast::<u8>()
                .copy_from_nonoverlapping(data.as_ptr(), len.get());

        };

        let wl_shm_pool = evb.globals.shm.create_pool(
            mapping.as_fd(),
            len.get() as i32,
            &evb.qh, ()
        );

        let wl_buffer = wl_shm_pool.create_buffer(
            0, size.w as i32, size.h as i32,
            size.w as i32 * bytes_per_pixel, wl_format,
            &evb.qh, ()
        );

        let wl_surface = evb.globals.compositor.create_surface(&evb.qh, ());
        wl_surface.attach(Some(&wl_buffer), 0, 0);
        wl_surface.commit();

        Self {
            // for more info about the lifetimes
            // of these objects see `drop`
            wl_shm_pool,
            wl_buffer,
            wl_surface,
            _mapping: mapping,
        }

    }

}

// ### (wayland) popup and layer window ###

// pub struct PopupWindow {
//     base: BaseWindow,
//     xdg_surface: XdgSurface,
//     xdg_popup: XdgPopup,
// }

// impl ops::Deref for PopupWindow {
//     type Target = BaseWindow;
//     fn deref(&self) -> &Self::Target {
//         &self.base
//     }
// }

// /// The window is closed on drop.
// impl Drop for PopupWindow {
//     fn drop(&mut self) {
//         self.xdg_popup.destroy();
//         self.xdg_surface.destroy();
//     }
// }

// impl PopupWindow {

//     pub fn new(evl: &EventLoop, size: LogicalSize, parent: &Window) -> Self {

//         // TODO: this doesn't implement positioning of the popup window (where on the parent should it be)
//         //       this is implemented using xdg_positioner.set_anchor or smth

//         let base = BaseWindow::new(evl);

//         let evb = &evl.backend.state.lock().wayland.state;

//         // xdg-popup role
//         let xdg_surface = evb.globals.wm.get_xdg_surface(&base.wl_surface, &evb.qh, Arc::clone(&base.shared));
//         let xdg_positioner = evb.globals.wm.create_positioner(&evb.qh, ());

//         let parent_guard = parent.shared.lock().unwrap();
//         xdg_positioner.set_size(size.w as i32, size.h as i32);
//         xdg_positioner.set_anchor_rect(0, 0, size.w as i32, size.h as i32);
//         drop(parent_guard);

//         let xdg_popup = xdg_surface.get_popup(Some(&parent.xdg_surface), &xdg_positioner, &evb.qh, Arc::clone(&base.shared));

//         base.wl_surface.commit();

//         Self {
//             base,
//             xdg_surface,
//             xdg_popup,
//         }

//     }

//     pub fn destroy(self) {}

// }

// pub struct LayerWindow {
//     base: BaseWindow,
//     zwlr_surface: ZwlrLayerSurfaceV1,
// }

// impl ops::Deref for LayerWindow {
//     type Target = BaseWindow;
//     fn deref(&self) -> &Self::Target {
//         &self.base
//     }
// }

// /// The window is closed on drop.
// impl Drop for LayerWindow {
//     fn drop(&mut self) {
//         self.zwlr_surface.destroy();
//         self.base.wl_surface.destroy();
//     }
// }

// impl LayerWindow {

//     /// # Errors
//     /// Will return `Unsupported` if the neceserry extension (ZwlrLayerShellV1) is not present.
//     /// # Panics
//     /// `size` must be < u32::MAX
//     pub fn new(evl: &EventLoop, layer: WindowLayer, monitor: Option<&Monitor>) -> Result<Self, EvlError> {

//         let base = BaseWindow::new(evl);

//         let evb = &evl.backend.state.lock().wayland.state;

//         let wl_layer = match layer {
//             WindowLayer::Background => Layer::Background,
//             WindowLayer::Bottom     => Layer::Bottom,
//             WindowLayer::Top        => Layer::Top,
//             WindowLayer::Overlay    => Layer::Overlay,
//         };

//         let wl_output = monitor.map(|val| &val.wl_output);

//         // creating this kind of window requires some wayland extensions
//         let layer_shell_mgr = evb.globals.layer_shell_mgr.as_ref().ok_or(
//             EvlError::unsupported(ZwlrLayerShellV1::interface().name.into())
//         )?;

//         // layer-shell role
//         let zwlr_surface = layer_shell_mgr.get_layer_surface(
//             &base.wl_surface, wl_output, wl_layer, evb.appid.clone(),
//             &evb.qh, Arc::clone(&base.shared)
//         );

//         // zwlr_surface.set_size(size.w as u32, size.h as u32);
//         // TODO: check if this still works when size is not set explicitely (and anchoring is not set here)

//         base.wl_surface.commit();

//         Ok(Self {
//             base,
//             zwlr_surface,
//         })

//     }

//     pub fn destroy(self) {}

//     pub fn anchor(&self, anchor: WindowAnchor) {

//         let wl_anchor = match anchor {
//             WindowAnchor::Top => Anchor::Top,
//             WindowAnchor::Bottom => Anchor::Bottom,
//             WindowAnchor::Left => Anchor::Left,
//             WindowAnchor::Right => Anchor::Right,
//         };

//         self.zwlr_surface.set_anchor(wl_anchor);
//         self.base.wl_surface.commit();

//     }

//     pub fn margin(&self, value: u32) {

//         let n = value as i32;

//         self.zwlr_surface.set_margin(n, n, n, n);
//         self.base.wl_surface.commit();

//     }

//     pub fn interactivity(&self, value: KbInteractivity) {

//         let wl_intr = match value {
//             KbInteractivity::None => KeyboardInteractivity::None,
//             // KbInteractivity::Normal => KeyboardInteractivity::OnDemand,
//             KbInteractivity::Exclusive => KeyboardInteractivity::Exclusive,
//         };

//         self.zwlr_surface.set_keyboard_interactivity(wl_intr);
//         self.base.wl_surface.commit();

//     }

// }

// ### more stuff ###

pub struct HoveredItemBackend {
    last_serial: u32,
    wl_data_offer: Option<WlDataOffer>,
}

impl HoveredItemBackend {
    pub fn advertise(&self, kinds: &[DataKind]) {
        if let Some(ref wl_data_offer) = self.wl_data_offer {
            for kind in kinds {
                let mime_type = kind.to_mime_type();
                wl_data_offer.accept(self.last_serial, Some(mime_type.into()));
            }
        }
    }
}

fn translate_dead_to_normal_sym(xkb_sym: xkb::Keysym) -> Option<xkb::Keysym> {

    use xkb::Keysym;

    match xkb_sym {
        Keysym::dead_acute      => Some(Keysym::acute),
        Keysym::dead_grave      => Some(Keysym::grave),
        Keysym::dead_circumflex => Some(Keysym::asciicircum),
        Keysym::dead_tilde      => Some(Keysym::asciitilde),
        _ => None
    }

}

/// Look at the source code to see how keys are translated.
pub fn translate_xkb_sym(xkb_sym: xkb::Keysym) -> Key {

    use xkb::Keysym;

    match xkb_sym {

        Keysym::Escape    => Key::Special(SpecialKey::Escape),
        Keysym::Tab       => Key::Special(SpecialKey::Tab),
        Keysym::Caps_Lock => Key::Special(SpecialKey::CapsLock),
        Keysym::Shift_L   => Key::Special(SpecialKey::Shift),
        Keysym::Shift_R   => Key::Special(SpecialKey::Shift),
        Keysym::Control_L => Key::Special(SpecialKey::Control),
        Keysym::Control_R => Key::Special(SpecialKey::Control),
        Keysym::Alt_L     => Key::Special(SpecialKey::Alt),
        Keysym::Alt_R     => Key::Special(SpecialKey::Alt),
        Keysym::Super_L   => Key::Special(SpecialKey::Super),
        Keysym::Super_R   => Key::Special(SpecialKey::Super),
        Keysym::Menu      => Key::Special(SpecialKey::AppMenu),
        Keysym::Return    => Key::Special(SpecialKey::Return),
        Keysym::BackSpace => Key::Special(SpecialKey::Backspace),
        Keysym::space     => Key::Special(SpecialKey::Space),
        Keysym::Up        => Key::Special(SpecialKey::ArrowUp),
        Keysym::Down      => Key::Special(SpecialKey::ArrowDown),
        Keysym::Left      => Key::Special(SpecialKey::ArrowLeft),
        Keysym::Right     => Key::Special(SpecialKey::ArrowRight),
        Keysym::ISO_Level3_Shift => Key::Special(SpecialKey::AltGr),

        Keysym::F1  => Key::Special(SpecialKey::F1),
        Keysym::F2  => Key::Special(SpecialKey::F2),
        Keysym::F3  => Key::Special(SpecialKey::F3),
        Keysym::F4  => Key::Special(SpecialKey::F4),
        Keysym::F5  => Key::Special(SpecialKey::F5),
        Keysym::F6  => Key::Special(SpecialKey::F6),
        Keysym::F7  => Key::Special(SpecialKey::F7),
        Keysym::F8  => Key::Special(SpecialKey::F8),
        Keysym::F9  => Key::Special(SpecialKey::F9),
        Keysym::F10 => Key::Special(SpecialKey::F10),
        Keysym::F11 => Key::Special(SpecialKey::F11),
        Keysym::F12 => Key::Special(SpecialKey::F12),

        Keysym::_1 => Key::Char('1'),
        Keysym::_2 => Key::Char('2'),
        Keysym::_3 => Key::Char('3'),
        Keysym::_4 => Key::Char('4'),
        Keysym::_5 => Key::Char('5'),
        Keysym::_6 => Key::Char('6'),
        Keysym::_7 => Key::Char('7'),
        Keysym::_8 => Key::Char('8'),
        Keysym::_9 => Key::Char('9'),

        Keysym::a => Key::Char('a'),
        Keysym::A => Key::Char('A'),
        Keysym::b => Key::Char('b'),
        Keysym::B => Key::Char('B'),
        Keysym::c => Key::Char('c'),
        Keysym::C => Key::Char('C'),
        Keysym::d => Key::Char('d'),
        Keysym::D => Key::Char('D'),
        Keysym::e => Key::Char('e'),
        Keysym::E => Key::Char('E'),
        Keysym::f => Key::Char('f'),
        Keysym::F => Key::Char('F'),
        Keysym::g => Key::Char('g'),
        Keysym::G => Key::Char('G'),
        Keysym::h => Key::Char('h'),
        Keysym::H => Key::Char('H'),
        Keysym::i => Key::Char('i'),
        Keysym::I => Key::Char('I'),
        Keysym::j => Key::Char('j'),
        Keysym::J => Key::Char('J'),
        Keysym::k => Key::Char('k'),
        Keysym::K => Key::Char('K'),
        Keysym::l => Key::Char('l'),
        Keysym::L => Key::Char('L'),
        Keysym::m => Key::Char('m'),
        Keysym::M => Key::Char('M'),
        Keysym::n => Key::Char('n'),
        Keysym::N => Key::Char('N'),
        Keysym::o => Key::Char('o'),
        Keysym::O => Key::Char('O'),
        Keysym::p => Key::Char('p'),
        Keysym::P => Key::Char('P'),
        Keysym::q => Key::Char('q'),
        Keysym::Q => Key::Char('Q'),
        Keysym::r => Key::Char('r'),
        Keysym::R => Key::Char('R'),
        Keysym::s => Key::Char('s'),
        Keysym::S => Key::Char('S'),
        Keysym::t => Key::Char('t'),
        Keysym::T => Key::Char('T'),
        Keysym::u => Key::Char('u'),
        Keysym::U => Key::Char('U'),
        Keysym::v => Key::Char('v'),
        Keysym::V => Key::Char('V'),
        Keysym::w => Key::Char('w'),
        Keysym::W => Key::Char('W'),
        Keysym::x => Key::Char('x'),
        Keysym::X => Key::Char('X'),
        Keysym::y => Key::Char('y'),
        Keysym::Y => Key::Char('Y'),
        Keysym::z => Key::Char('z'),
        Keysym::Z => Key::Char('Z'),

        Keysym::question     => Key::Char('?'),
        Keysym::equal        => Key::Char('='),
        Keysym::exclam       => Key::Char('!'),
        Keysym::at           => Key::Char('@'),
        Keysym::numbersign   => Key::Char('#'),
        Keysym::dollar       => Key::Char('$'),
        Keysym::EuroSign     => Key::Char('€'),
        Keysym::percent      => Key::Char('%'),
        Keysym::section      => Key::Char('§'),
        Keysym::asciicircum  => Key::Char('^'),
        Keysym::degree       => Key::Char('°'),
        Keysym::ampersand    => Key::Char('&'),
        Keysym::asterisk     => Key::Char('*'),
        Keysym::parenleft    => Key::Char('('),
        Keysym::parenright   => Key::Char(')'),
        Keysym::underscore   => Key::Char('_'),
        Keysym::minus        => Key::Char('-'),
        Keysym::plus         => Key::Char('+'),
        Keysym::braceleft    => Key::Char('{'),
        Keysym::braceright   => Key::Char('}'),
        Keysym::bracketleft  => Key::Char('['),
        Keysym::bracketright => Key::Char(']'),
        Keysym::backslash    => Key::Char('\\'),
        Keysym::bar          => Key::Char('|'),
        Keysym::colon        => Key::Char(':'),
        Keysym::semicolon    => Key::Char(';'),
        Keysym::quotedbl     => Key::Char('"'),
        Keysym::apostrophe   => Key::Char('\''),
        Keysym::less         => Key::Char('<'),
        Keysym::greater      => Key::Char('>'),
        Keysym::comma        => Key::Char(','),
        Keysym::period       => Key::Char('.'),
        Keysym::slash        => Key::Char('/'),
        Keysym::asciitilde   => Key::Char('~'),

        Keysym::dead_acute      => Key::DeadChar('´'),
        Keysym::dead_grave      => Key::DeadChar('`'),
        Keysym::dead_circumflex => Key::DeadChar('^'),
        Keysym::dead_tilde      => Key::DeadChar('~'),

        Keysym::adiaeresis => Key::Char('ä'),
        Keysym::odiaeresis => Key::Char('ö'),
        Keysym::udiaeresis => Key::Char('ü'),
        Keysym::ssharp     => Key::Char('ß'),

        other => Key::Unknown(other.raw())

    }

}

// ### wayland client implementation ###

macro_rules! ignore {
    ($prxy:ident, $usr:tt) => {
        fn event(
            _: &mut Self,
            _prxy: &$prxy,
            _event: <$prxy as wayland_client::Proxy>::Event,
            _: &$usr,
            _: &wayland_client::Connection,
            _: &wayland_client::QueueHandle<Self>
        ) {}
    };
}

impl wayland_client::Dispatch<WlRegistry, GlobalListContents> for ConnectionState {
    fn event(
        evl: &mut Self,
        registry: &WlRegistry,
        event: WlRegistryEvent,
        _data: &GlobalListContents,
        _con: &wayland_client::Connection,
        qh: &wayland_client::QueueHandle<Self>
    ) {

        // TODO: test if this actually works with my second monitor

        if let WlRegistryEvent::Global { name, interface, .. } = event {

            if &interface == "wl_output" {

                let wl_output = registry.bind(name, 2, qh, ());
                let id = get_object_id(&wl_output);

                evl.monitors.inner.insert(Id(id), (MonitorInfo::default(), wl_output));

            }

            // note: the events describing the outputs are emitted in the WlOutput event handler

        }

        else if let WlRegistryEvent::GlobalRemove { name: id } = event {
            let id = Id(id);
            if evl.monitors.inner.contains_key(&id) {
                evl.monitors.inner.remove(&id);
                evl.events.push_back(Event::Monitor{ id, event: MonitorEvent::Remove })
            }
        }

    }
}

impl wayland_client::Dispatch<WlOutput, ()> for ConnectionState {
    fn event(
        evl: &mut Self,
        wl_output: &WlOutput,
        event: WlOutputEvent,
        _: &(),
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let id = get_object_id(wl_output);

        let (info, _wl_output) = evl.monitors.inner
            .get_mut(&Id(id))
            .unwrap();

        match event {
            WlOutputEvent::Name { name } => {
                if name.is_empty() { info.name = name };
            },
            WlOutputEvent::Description { description } => {
                info.description = description;
            },
            WlOutputEvent::Mode { flags, width, height, refresh } => {
                if flags.into_result().is_ok_and(|it| it.contains(WlOutputMode::Current)) {
                    info.size = PhysicalSize { w: width as u16, h: height as u16 };
                    info.refresh = refresh as u32;
                }
            },
            WlOutputEvent::Geometry { make, .. } => {
                if info.name.is_empty() { info.name = make };
            },
            WlOutputEvent::Done => {
                evl.events.push_back(Event::Monitor {
                    id: Id(id),
                    event: MonitorEvent::Update {
                        info: info.clone(),
                        monitor: Monitor { backend: MonitorBackend {
                            wl_output: wl_output.clone(),
                        } }
                    },
                });
            },
            _ => (),
        }

    }
}

impl wayland_client::Dispatch<WlShm, ()> for ConnectionState { ignore!(WlShm, ()); }
impl wayland_client::Dispatch<WlShmPool, ()> for ConnectionState { ignore!(WlShmPool, ()); }
impl wayland_client::Dispatch<WlBuffer, ()> for ConnectionState { ignore!(WlBuffer, ()); }

impl wayland_client::Dispatch<XdgPositioner, ()> for ConnectionState { ignore!(XdgPositioner, ()); }

impl wayland_client::Dispatch<WpViewporter, ()> for ConnectionState { ignore!(WpViewporter, ()); }
impl wayland_client::Dispatch<WpViewport, ()> for ConnectionState { ignore!(WpViewport, ()); }

impl wayland_client::Dispatch<WpCursorShapeManagerV1, ()> for ConnectionState { ignore!(WpCursorShapeManagerV1, ()); }
impl wayland_client::Dispatch<WpCursorShapeDeviceV1, ()> for ConnectionState { ignore!(WpCursorShapeDeviceV1, ()); }

impl wayland_client::Dispatch<WlDataDeviceManager, ()> for ConnectionState { ignore!(WlDataDeviceManager, ()); }
impl wayland_client::Dispatch<WlDataDevice, ()> for ConnectionState {
    fn event(
        evl: &mut Self,
        _data_device: &WlDataDevice,
        event: WlDataDeviceEvent,
        _data: &(),
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        if let WlDataDeviceEvent::Enter { surface, x, y, id: wl_data_offer, .. } = event {

            if let Some(ref val) = wl_data_offer {
                // specific actions are not implemented right now
                val.set_actions(DndAction::Copy | DndAction::Move, DndAction::Copy);
            }

            let id = get_object_id(&surface);
            let sameapp = evl.offer.dnd.ours;

            evl.offer.dnd.focused = Some(surface);
            evl.offer.dnd.current = wl_data_offer.clone();

            evl.offer.dnd.x = x;
            evl.offer.dnd.y = y;

            let item = HoveredItem { backend: HoveredItemBackend {
                last_serial: evl.last_serial,
                wl_data_offer,
            } };

            evl.events.push_back(Event::Window {
                id: Id(id),
                event: WindowEvent::Dnd {
                    event: DndEvent::Motion { x, y, item },
                    sameapp
                }
            });

        }

        else if let WlDataDeviceEvent::Motion { x, y, .. } = event {

            evl.offer.dnd.x = x;
            evl.offer.dnd.y = y;

            let surface = evl.offer.dnd.focused.as_ref().unwrap();
            let sameapp = evl.offer.dnd.ours;

            let item = HoveredItem { backend: HoveredItemBackend {
                last_serial: evl.last_serial,
                wl_data_offer: evl.offer.dnd.current.clone(),
            } };

            evl.events.push_back(Event::Window {
                id: Id(get_object_id(surface)),
                event: WindowEvent::Dnd {
                    event: DndEvent::Motion { x, y, item },
                    sameapp
                }
            });

        }

        else if let WlDataDeviceEvent::Drop = event {

            if let Some(wl_data_offer) = evl.offer.dnd.current.take() {

                let x = evl.offer.dnd.x;
                let y = evl.offer.dnd.y;

                // The offer will have been introduced with the advertised mime types already.
                let data_kinds = mem::take(&mut evl.offer.advertised_data_kinds);

                let surface = evl.offer.dnd.focused.as_ref().unwrap();
                let sameapp = evl.offer.dnd.ours;

                let readable = DataReadable {
                    backend: DataReadableBackend {
                        wl_data_offer,
                        data_kinds,
                        dnd: true
                    },
                };

                evl.events.push_back(Event::Window {
                    id: Id(get_object_id(surface)),
                    event: WindowEvent::Dnd {
                        event: DndEvent::Drop { x, y, readable },
                        sameapp
                    },
                });

            }
        }

        else if let WlDataDeviceEvent::Leave = event {

            // this maybe sent twice :(, so has_offer could be None
            if let Some(ref surface) = evl.offer.dnd.focused {

                evl.events.push_back(Event::Window {
                    id: Id(get_object_id(surface)),
                    event: WindowEvent::Dnd {
                        event: DndEvent::Cancel,
                        sameapp: evl.offer.dnd.ours,
                    },
                });

            }

            evl.offer.advertised_data_kinds.clear();
            evl.offer.dnd.focused = None;
            evl.offer.dnd.current = None;

        }

        else if let WlDataDeviceEvent::Selection { id: value /* not an id! */ } = event {

            if let Some(wl_data_offer) = value {

                // The offer will have been introduced with the advertised mime types already.
                let data_kinds = mem::take(&mut evl.offer.advertised_data_kinds);

                let readable = DataReadable { backend: DataReadableBackend {
                    wl_data_offer,
                    data_kinds,
                    dnd: false,
                } };

                evl.events.push_back(Event::SelectionUpdate { readable: Some(readable) });

            } else {

                evl.offer.advertised_data_kinds.clear();
                evl.events.push_back(Event::SelectionUpdate { readable: None });

            }

        }

    }

    wayland_client::event_created_child!(Self, WlDataDevice, [
        wayland_client::protocol::wl_data_device::EVT_DATA_OFFER_OPCODE => (WlDataOffer, ())
    ]);

}

impl wayland_client::Dispatch<WlDataOffer, ()> for ConnectionState {
    fn event(
        evl: &mut Self,
        _data_offer: &WlDataOffer,
        event: WlDataOfferEvent,
        _: &(),
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        // Data offers will send events about the mime types they offer
        // after they have been received by our client.

        if let WlDataOfferEvent::Offer { mime_type } = event {

            let data_kinds = &mut evl.offer.advertised_data_kinds;

            // Insert the advertised mime type which will be consumed later.
            if let Some(kind) = DataKind::from_mime_type(&mime_type) {
                data_kinds.push(kind)
            };

        }

    }
}

impl wayland_client::Dispatch<WlDataSource, ()> for ConnectionState {
    fn event(
        evl: &mut Self,
        data_source: &WlDataSource,
        event: WlDataSourceEvent,
        _: &(),
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let id = get_object_id(data_source);

        if let WlDataSourceEvent::Send { mime_type, fd } = event {

            let kind = DataKind::from_mime_type(&mime_type).unwrap();

            let writer = DataWriter { backend: DataWriterBackend {
                inner: io::PipeWriter::from(fd)
            } };

            evl.events.push_back(Event::DataSource {
                id: Id(id),
                event: DataSourceEvent::Send { kind, writer }
            });

        }

        else if let WlDataSourceEvent::DndFinished = event { // emitted on succesfull write

            evl.events.push_back(Event::DataSource {
                id: Id(id),
                event: DataSourceEvent::Success
            });

        }

        else if let WlDataSourceEvent::Cancelled = event { // emitted on termination of the operation

            evl.offer.dnd.ours = false;
            evl.offer.dnd.icon = None;

            evl.events.push_back(Event::DataSource {
                id: Id(id),
                event: DataSourceEvent::Close
            });

        }

    }
}

impl wayland_client::Dispatch<XdgActivationV1, ()> for ConnectionState { ignore!(XdgActivationV1, ()); }
impl wayland_client::Dispatch<XdgActivationTokenV1, WlSurface> for ConnectionState {
    fn event(
        evl: &mut Self,
        _token: &XdgActivationTokenV1,
        event: XdgActivationTokenEvent,
        surface: &WlSurface,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        // activate the token
        if let XdgActivationTokenEvent::Done { token } = event {
            let activation_mgr = evl.globals.activation_mgr.as_ref().unwrap();
            activation_mgr.activate(token, surface);
        }

    }
}

impl wayland_client::Dispatch<ZxdgDecorationManagerV1, ()> for ConnectionState { ignore!(ZxdgDecorationManagerV1, ()); }
impl wayland_client::Dispatch<ZxdgToplevelDecorationV1, Id> for ConnectionState {
    fn event(
        evl: &mut Self,
        _deco: &ZxdgToplevelDecorationV1,
        event: <ZxdgToplevelDecorationV1 as Proxy>::Event,
        data: &Id,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        if let ZxdgDecorationEvent::Configure { mode } = event {
            let event = match mode {
                WEnum::Value(ZxdgDecorationMode::ServerSide) => WindowEvent::Decorations { active: true },
                WEnum::Value(ZxdgDecorationMode::ClientSide) => WindowEvent::Decorations { active: false },
                _ => return,
            };
            evl.events.push_back(Event::Window { id: *data, event });
        }

    }
}

impl wayland_client::Dispatch<WpFractionalScaleManagerV1, ()> for ConnectionState { ignore!(WpFractionalScaleManagerV1, ()); }

impl wayland_client::Dispatch<WlSeat, ()> for ConnectionState {
    fn event(
        evl: &mut Self,
        seat: &WlSeat,
        event: WlSeatEvent,
        _data: &(),
        _con: &wayland_client::Connection,
        qh: &wayland_client::QueueHandle<Self>
    ) {

        if let WlSeatEvent::Capabilities { capabilities: WEnum::Value(capabilities) } = event {

            if capabilities.contains(WlSeatCapability::Keyboard) {
                // Get the global.
                seat.get_keyboard(qh, ());
            }

            if capabilities.contains(WlSeatCapability::Pointer) {

                // Get the global.
                let wl_pointer = seat.get_pointer(qh, ());

                if let Some(ref wp_cursor_shape_mgr) = evl.globals.cursor_shape_mgr {
                    let wl_shape_device = wp_cursor_shape_mgr.get_pointer(&wl_pointer, qh, ());
                    evl.globals.shape_device = Some(wl_shape_device);
                }

                evl.globals.pointer = Some(wl_pointer);

            }

        }

    }
}

impl wayland_client::Dispatch<XdgWmBase, ()> for ConnectionState {
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

impl wayland_client::Dispatch<XdgSurface, Id> for ConnectionState {
    fn event(
        evl: &mut Self,
        xdg_surface: &XdgSurface,
        event: XdgSurfaceEvent,
        id: &Id,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if let XdgSurfaceEvent::Configure { serial } = event {

            // Ack the configure.
            //
            // The events generated by this configure event will be handeled by the
            // user before the Ack is actually sent with the next call to `flush`.
            xdg_surface.ack_configure(serial);

            let window = evl.windows.get(*id);
            let size = window.size;

            // This should not be able to happen
            debug_assert_ne!(size.w, 0);
            debug_assert_ne!(size.h, 0);

            // Update the window's viewport destination. (Used for custom scaling? TODO: do we need viewport shit???)
            // if let Some(ref frac_scale_data) = window.frac_scale_data {
            //     frac_scale_data.viewport.set_destination(size.w as i32, size.h as i32);
            // };

            // Foreward the final configuration state to the user.
            evl.events.push_back(Event::Window { id: *id, event: WindowEvent::Resize {
                size, fullscreen: window.fullscreen
            } });

            if !window.redraw.already_got_event {
                window.redraw.already_got_event = true;
                evl.events.push_back(Event::Window { id: *id, event: WindowEvent::Redraw });
            }

        }
    }
}

impl wayland_client::Dispatch<XdgToplevel, Id> for ConnectionState {
    fn event(
        evl: &mut Self,
        _surface: &XdgToplevel,
        event: XdgToplevelEvent,
        id: &Id,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let window = evl.windows.get(*id);

        if let XdgToplevelEvent::Configure { width, height, states } = event {

            let zerosized = width <= 0 && height <= 0;

            // Overwrite the values, since the compositor
            // has told us a mandatory size now.
            if !zerosized {
                // SCALING: The provided dimensions are in window-geometry-space,
                    //      so we need to scale appropriatly.
                window.size = PhysicalSize::new(width as u16, height as u16)
                    .scale(window.scale);
                // NOTE: In floating window compositors on wayland, the final
                //       size is usually determined by the first render.
            }

            let flags = ConfigureFlags::parse(states);

            window.fullscreen = flags.fullscreen;
            window.hidden = flags.suspended || (window.hidden && zerosized);
            //                             ^^^^ If the window was programatically hidden, the
            //                                  `suspended` flag might not be set, so we check extra.

        }

        else if let XdgToplevelEvent::Close = event {
            evl.events.push_back(Event::Window { id: *id, event: WindowEvent::ShouldClose });
        }

    }
}

// impl wayland_client::Dispatch<XdgPopup, WindowId> for ConnectionState {
//     fn event(
//         evl: &mut Self,
//         _surface: &XdgPopup,
//         event: XdgPopupEvent,
//         shared: &(),
//         _con: &wayland_client::Connection,
//         _qh: &wayland_client::QueueHandle<Self>
//     ) {

//         let mut guard = shared.lock().unwrap();

//         if let XdgPopupEvent::Configure { width, height, .. } = event {
//             if width > 0 && height > 0 {
//                 guard.width  = width  as u32;
//                 guard.height = height as u32;
//             }
//         }

//         else if let XdgPopupEvent::PopupDone = event {
//             evl.events.push(Event::Window { id: guard.id, event: WindowEvent::ShouldClose });
//         }

//     }
// }

impl wayland_client::Dispatch<ZwlrLayerShellV1, ()> for ConnectionState {
    ignore!(ZwlrLayerShellV1, ());
}

// impl wayland_client::Dispatch<ZwlrLayerSurfaceV1, ()> for ConnectionState {
//     fn event(
//         evl: &mut Self,
//         zwlr_surface: &ZwlrLayerSurfaceV1,
//         event: ZwlrLayerSurfaceEvent,
//         shared: &(),
//         _con: &wayland_client::Connection,
//         _qh: &wayland_client::QueueHandle<Self>
//     ) {

//         let mut guard = shared.lock().unwrap();

//         if let ZwlrLayerSurfaceEvent::Configure { width, height, serial } = event {

//             // ack the configure
//             zwlr_surface.ack_configure(serial);

//             if width > 0 && height > 0 {
//                 guard.width  = width;
//                 guard.height = height;
//             }

//             process_configure(evl, guard, width, height);

//         }

//         else if let ZwlrLayerSurfaceEvent::Closed = event {
//             evl.events.push(Event::Window { id: guard.id, event: WindowEvent::ShouldClose });
//         }

//     }
// }

#[derive(Default)]
struct ConfigureFlags {
    fullscreen: bool,
    /// Suspended means the surface is not visible and should not
    /// continue redrawing / doing animations.
    suspended: bool,
}

impl ConfigureFlags {
    pub fn parse(states: Vec<u8>) -> Self {
        states.chunks_exact(4)
            .flat_map(|chunk| chunk.try_into())
            .map(|bytes| u32::from_ne_bytes(bytes))
            .flat_map(XdgToplevelState::try_from)
            .fold(Self::default(), |mut acc, state| {
                match state {
                    XdgToplevelState::Fullscreen => acc.fullscreen = true,
                    XdgToplevelState::Suspended => acc.suspended = true,
                    _ => (),
                };
                acc
            })
    }
}

impl wayland_client::Dispatch<WlCallback, Id> for ConnectionState {
    fn event(
        evl: &mut Self,
        _cb: &WlCallback,
        _event: WlCallbackEvent,
        id: &Id,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let window = evl.windows.get(*id);

        if !window.redraw.already_got_event && window.redraw.should_emit_event {
            window.redraw.already_got_event = true;
            evl.events.push_back(Event::Window { id: *id, event: WindowEvent::Redraw });
        }

        window.redraw.frame_callback_registered = false;
        window.redraw.should_emit_event = false;

    }
}

impl wayland_client::Dispatch<WlCompositor, ()> for ConnectionState { ignore!(WlCompositor, ()); }
impl wayland_client::Dispatch<WlSurface, ()> for ConnectionState { ignore!(WlSurface, ()); }
impl wayland_client::Dispatch<WlRegion, ()> for ConnectionState { ignore!(WlRegion, ()); }

impl wayland_client::Dispatch<WpFractionalScaleV1, Id> for ConnectionState {
    fn event(
            evl: &mut Self,
            _proxy: &WpFractionalScaleV1,
            event: WpFractionalScaleV1Event,
            id: &Id,
            _conn: &wayland_client::Connection,
            _qh: &QueueHandle<Self>,
        ) {

        if let WpFractionalScaleV1Event::PreferredScale { scale } = event {

            let window = evl.windows.get(*id);

            let new_scale = scale as f64 / 120.0;
            let old_scale = window.scale;

            // We need to update window scaling factor AND physical size:
            window.scale = new_scale;
            window.size = window.size
                .scale(new_scale / old_scale);

            evl.events.push_back(Event::Window {
                id: *id,
                event: WindowEvent::Rescale { scale: new_scale }
            });

            // Tell the user to resize their buffers and re-render:
            evl.events.push_back(Event::Window { id: *id, event: WindowEvent::Resize {
                size: window.size, fullscreen: window.fullscreen
            } });

            if !window.redraw.already_got_event {
                window.redraw.already_got_event = true;
                evl.events.push_back(Event::Window { id: *id, event: WindowEvent::Redraw });
            }

        }

    }
}

impl wayland_client::Dispatch<WlKeyboard, ()> for ConnectionState {
    fn event(
            evl: &mut Self,
            _proxy: &WlKeyboard,
            event: WlKeyboardEvent,
            _data: &(),
            _con: &wayland_client::Connection,
            _qh: &QueueHandle<Self>,
        ) {

        match event {

            WlKeyboardEvent::Keymap { fd, size, .. } => {

                // initialize keymap & keyboard state

                let xkb_keymap = {
                    match unsafe { xkb::Keymap::new_from_fd(
                        &evl.keyboard.xkb_context,
                        fd, size as usize,
                        xkb::FORMAT_TEXT_V1,
                        xkb::KEYMAP_COMPILE_NO_FLAGS
                    ) } {
                        Ok(Some(val)) => val,
                        Ok(None) => panic!("cannot load keymap"),
                        Err(err) => panic!("cannot load keymap, {}", err)
                    }
                };

                let xkb_state = xkb::State::new(&xkb_keymap);
                let pressed_keys = PressedKeys::new(&xkb_keymap);

                // initialize composition state

                let locale = env::var_os("LANG")
                    .expect("missing LOCALE environment variable");

                let compose_table = match xkb::Table::new_from_locale(
                    &evl.keyboard.xkb_context,
                    &locale,
                    xkb::COMPILE_NO_FLAGS
                ) {
                    Ok(val) => val,
                    Err(()) => {
                        // Currently this line is never reachable. if the locale is invalid libxkbcommon actually
                        // exits immediatly with an errors message (wonderful library design there...).
                        // It seems that Qt Apps like KWrite will actually not crash on an invalid locale, even though
                        // I think they use libxkbcommon aswell. They probably validate the locale beforehand.
                        panic!("invalid keymap locale, {:?}", locale);
                    }
                };

                let compose_state = xkb::compose::State::new(&compose_table, xkb::STATE_NO_FLAGS);

                evl.keyboard.keymap_specific = Some(KeymapSpecificData {
                    xkb_state, compose_state, pressed_keys
                });

            },

            WlKeyboardEvent::Enter { surface, keys, .. } => {

                let id = get_object_id(&surface);

                evl.keyboard.focused = Some(surface);

                let iter = keys.chunks_exact(4)
                    .flat_map(|chunk| chunk.try_into())
                    .map(|bytes| u32::from_ne_bytes(bytes));

                // emit the enter event
                evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::Enter });

                // emit a key-down event for all keys that are pressed when entering focus
                for raw_key in iter {
                    process_key_event(evl, raw_key, KeyDirection::Down, KeySource::Event);
                }

            },

            WlKeyboardEvent::Leave { .. } => {

                if let Some(ref mut keymap_specific) = evl.keyboard.keymap_specific {

                    let surface = evl.keyboard.focused.as_ref().unwrap();
                    let id = get_object_id(surface);

                    // We get these keys in a kind of weird way to avoid memory
                    // allocation and ownership problems.
                    let mut buf = [xkb::Keycode::default(); 10];
                    let count = keymap_specific.pressed_keys.write_currently_pressed(&mut buf);
                    let pressed = &buf[..count];

                    // emit a synthetic key-up event for all keys that are still pressed
                    for key in pressed {
                        process_key_event(evl, key.raw(), KeyDirection::Up, KeySource::Event);
                    }

                    // emit the leave event
                    evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::Leave });

                    // also invalidate selection, to be more correct
                    evl.events.push_back(Event::SelectionUpdate { readable: None });

                    evl.keyboard.focused = None;

                };

            },

            WlKeyboardEvent::Key { key: raw_key, state, serial, .. } => {

                let dir = match state {
                    WEnum::Value(KeyState::Pressed) => KeyDirection::Down,
                    WEnum::Value(KeyState::Released) => KeyDirection::Up,
                    WEnum::Value(..) => return,
                    WEnum::Unknown(..) => return
                };

                evl.last_serial = serial;

                process_key_event(evl, raw_key, dir, KeySource::Event);


            },

            WlKeyboardEvent::Modifiers { mods_depressed, mods_latched, mods_locked, group, .. } => {

                if let Some(ref mut keymap_specific) = evl.keyboard.keymap_specific {
                    keymap_specific.xkb_state.update_mask(mods_depressed, mods_latched, mods_locked, 0, 0, group);
                };

            },

            WlKeyboardEvent::RepeatInfo { rate, delay } => {

                if rate > 0 {
                    evl.keyboard.repeat_rate = Duration::from_millis(1000 / rate as u64);
                    evl.keyboard.repeat_delay = Duration::from_millis(delay as u64);
                } else {
                    evl.keyboard.repeat_rate = Duration::ZERO;
                    evl.keyboard.repeat_delay = Duration::ZERO;
                }

            },

            _ => (),

        }

    }
}

#[derive(PartialEq, Eq)]
enum KeyDirection {
    Down,
    Up,
}

#[derive(PartialEq, Eq)]
enum KeySource {
    Event,
    Repeat,
}

fn process_key_event(evl: &mut ConnectionState, raw_key: u32, dir: KeyDirection, source: KeySource) {

    // NOTE: uses evl.keyboard_data and evl.events

    let Some(ref mut keymap_specific) = evl.keyboard.keymap_specific else { return };

    let surface = evl.keyboard.focused.as_ref().unwrap();
    let id = get_object_id(surface);

    let xkb_key = xkb::Keycode::new(raw_key + 8); // "+8" says the wayland docs

    let repeat = source == KeySource::Repeat;

    if dir == KeyDirection::Down {

        /* KEY DOWN HANDLER */

        let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
        let modifier = xkb_sym.is_modifier_key(); // if this key is a modifier key

        // emit a generic key down event
        let key = translate_xkb_sym(xkb_sym);
        evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::KeyDown { key, repeat } });

        // turn this key into utf8 text and emit text input events
        keymap_specific.compose_state.feed(xkb_sym);
        match keymap_specific.compose_state.status() {
            xkb::Status::Nothing => {
                if let Some(chr) = xkb_sym.key_char() {
                    evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::TextInput { chr } })
                }
            },
            xkb::Status::Composing => {
                // sadly we can't just get the string repr of a dead-char
                if let Some(chr) = translate_dead_to_normal_sym(xkb_sym).and_then(xkb::Keysym::key_char) {
                    evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::TextCompose { chr } })
                }
            },
            xkb::Status::Composed => {
                if let Some(text) = keymap_specific.compose_state.utf8() {
                    for chr in text.chars() {
                        evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::TextInput { chr } })
                    }
                }
                keymap_specific.compose_state.reset();
            },
            xkb::Status::Cancelled => {
                // order is important, so that the cancel event is received first
                evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::TextComposeCancel });
                if let Some(chr) = xkb_sym.key_char() {
                    evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::TextInput { chr } })
                }
            },
        }

        // implement key repeat
        // only re-arm if this was NOT called from a repeated key event
        if !modifier && source != KeySource::Repeat {

            evl.keyboard.repeat_key = raw_key;

            // arm key-repeat timer with the correct delay and repeat rate
            evl.keyboard.repeat_timer.set_interval_at(
                Instant::now() + evl.keyboard.repeat_delay,
                evl.keyboard.repeat_rate
            );

            // update the key state
            keymap_specific.pressed_keys.update_key_state(xkb_key, KeyState::Pressed);

        }

    } else {

        /* KEY UP HANDLER */

        // unarm key-repeat timer
        evl.keyboard.repeat_timer.set_after(Duration::MAX);

        // update the key state
        keymap_specific.pressed_keys.update_key_state(xkb_key, KeyState::Released);

        let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
        let key = translate_xkb_sym(xkb_sym);
        evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::KeyUp { key } });

    };

}

impl wayland_client::Dispatch<WlPointer, ()> for ConnectionState {
    fn event(
            evl: &mut Self,
            _proxy: &WlPointer,
            event: WlPointerEvent,
            _data: &(),
            _con: &wayland_client::Connection,
            _qh: &QueueHandle<Self>,
        ) {

        match event {

             WlPointerEvent::Enter { surface, surface_x, surface_y, serial } => {

                let id = get_object_id(&surface);
                let window = evl.windows.get(Id(id));

                let (x, y) = (surface_x.max(0.) as i16,
                              surface_y.max(0.) as i16); // must not be negative

                // Convert to physical/mathematical coordinate space.
                evl.mouse.pos = PhysicalPoint::new(x, window.size.h as i16 - y)
                    .scale(window.scale);

                evl.mouse.focused = Some(surface);

                evl.events.push_back(Event::Window { id: Id(id), event:
                    WindowEvent::MouseEnter
                });

                evl.events.push_back(Event::Window { id: Id(id), event:
                    WindowEvent::MouseMotion { point: evl.mouse.pos }
                });

                evl.mouse.last_enter_serial = serial;

             },

             WlPointerEvent::Leave { surface, .. } => {

                evl.mouse.focused = None;

                let id = get_object_id(&surface);
                evl.events.push_back(Event::Window { id: Id(id), event: WindowEvent::MouseLeave });

             },

             WlPointerEvent::Motion { surface_x, surface_y, .. } => {

                 let surface = evl.mouse.focused.as_ref()
                     .expect("surface must have been entered");

                 let id = get_object_id(surface);
                 let window = evl.windows.get(Id(id));

                 let (x, y) = (surface_x.max(0.) as i16,
                               surface_y.max(0.) as i16); // must not be negative

                 // convert to mathematical coordinate space
                 evl.mouse.pos.x = x;
                 evl.mouse.pos.y = window.size.h as i16 - y;

                evl.events.push_back(Event::Window {
                    id: Id(id),
                    event: WindowEvent::MouseMotion { point: evl.mouse.pos }
                });

             },

            WlPointerEvent::Button { button: button_code, state, serial, .. } => {

                const BTN_LEFT: u32 = 0x110; // defined somewhere in the linux kernel
                const BTN_RIGHT: u32 = 0x111;
                const BTN_MIDDLE: u32 = 0x112;
                const BTN_SIDE: u32 = 0x113;
                const BTN_EXTRA: u32 = 0x114;
                const BTN_FORWARD: u32 = 0x115;
                const BTN_BACK: u32 = 0x116;

                let button = match button_code {
                    BTN_LEFT   => MouseButton::Left,
                    BTN_RIGHT  => MouseButton::Right,
                    BTN_MIDDLE => MouseButton::Middle,
                    BTN_BACK    | BTN_SIDE  => MouseButton::X1,
                    BTN_FORWARD | BTN_EXTRA => MouseButton::X2,
                    _ => MouseButton::Unknown,
                };

                let down = match state {
                    WEnum::Value(ButtonState::Pressed) => true,
                    WEnum::Value(ButtonState::Released) => false,
                    WEnum::Value(..) => unreachable!(), // fucking non-exhaustive enums
                    WEnum::Unknown(..) => unreachable!()
                };

                let event = if down {
                    WindowEvent::MouseDown { button, point: evl.mouse.pos }
                } else {
                    WindowEvent::MouseUp { button, point: evl.mouse.pos }
                };

                evl.last_serial = serial;

                let surface = evl.mouse.focused.as_ref()
                    .unwrap();

                let id = get_object_id(surface);

                evl.events.push_back(Event::Window { id: Id(id), event });

            },

            WlPointerEvent::Axis { axis, value, .. } => {

                let surface = evl.mouse.focused.as_ref().unwrap();
                let id = get_object_id(surface);

                let adjusted_value = (value * 1000.0) as i16;

                let vertical = match axis {
                    WEnum::Value(Axis::VerticalScroll) => true,
                    WEnum::Value(Axis::HorizontalScroll) => false,
                    WEnum::Value(..) => unreachable!(),
                    WEnum::Unknown(..) => unreachable!()
                };

                let mut dx = 0;
                let mut dy = 0;

                if vertical { dy = adjusted_value }
                else        { dx = adjusted_value };

                evl.events.push_back(Event::Window {
                    id: Id(id),
                    event: WindowEvent::MouseScroll { dx, dy }
                });

            },

            _ => ()

        }

    }
}

// ### error handling ###

impl From<wayland_client::ConnectError> for crate::EvlError {
    fn from(value: wayland_client::ConnectError) -> Self {
        Self::fatal(&format!("cannot connect to wayland, {}", value))
    }
}

impl From<wayland_client::globals::GlobalError> for crate::EvlError {
    fn from(value: wayland_client::globals::GlobalError) -> Self {
        Self::fatal(&format!("failed to get wayland globals, {}", value))
    }
}

impl From<BindError> for crate::EvlError {
    fn from(value: BindError) -> Self {
        Self::fatal(&format!("failed to get wayland global, {}", value))
    }
}

impl From<wayland_client::backend::WaylandError> for crate::EvlError {
    fn from(value: wayland_client::backend::WaylandError) -> Self {
        Self::fatal(&format!("failed wayland call, {}", value))
    }
}

impl From<wayland_client::DispatchError> for crate::EvlError {
    fn from(value: wayland_client::DispatchError) -> Self {
        Self::fatal(&format!("failed wayland dispatch, {}", value))
    }
}

impl From<nix::errno::Errno> for crate::EvlError {
    fn from(value: nix::errno::Errno) -> Self {
        Self::fatal(&format!("failed I/O, {}", value))
    }
}

impl From<io::Error> for crate::EvlError {
    fn from(value: io::Error) -> Self {
        Self::fatal(&format!("failed I/O, {}", value))
    }
}
