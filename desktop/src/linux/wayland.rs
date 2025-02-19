
// ### imports ###

use wayland_client::{
    protocol::{
        wl_registry::{WlRegistry, Event as WlRegistryEvent},
        wl_compositor::WlCompositor,
        wl_shm::{WlShm, Format as WlFormat},
        wl_shm_pool::WlShmPool,
        wl_seat::{WlSeat, Event as WlSeatEvent, Capability as WlSeatCapability},
        wl_surface::WlSurface,
        wl_callback::{WlCallback, Event as WlCallbackEvent},
        wl_keyboard::{WlKeyboard, Event as WlKeyboardEvent, KeyState},
        wl_pointer::{WlPointer, Event as WlPointerEvent, ButtonState, Axis},
        wl_region::WlRegion,
        wl_output::{WlOutput, Event as WlOutputEvent, Mode as WlOutputMode},
        wl_data_device_manager::{WlDataDeviceManager, DndAction},
        wl_data_device::{WlDataDevice, Event as WlDataDeviceEvent, EVT_DATA_OFFER_OPCODE},
        wl_data_source::{WlDataSource, Event as WlDataSourceEvent},
        wl_data_offer::{WlDataOffer, Event as WlDataOfferEvent}, wl_buffer::WlBuffer,
    },
    WEnum, Proxy, QueueHandle, EventQueue, globals::{registry_queue_init, GlobalList, GlobalListContents, BindError}, backend::WaylandError
};

use wayland_protocols::xdg::{
    shell::client::{
        xdg_wm_base::{XdgWmBase, Event as XdgWmBaseEvent},
        xdg_surface::{XdgSurface, Event as XdgSurfaceEvent},
        xdg_toplevel::{XdgToplevel, Event as XdgToplevelEvent, State as XdgToplevelState},
        xdg_popup::{XdgPopup, Event as XdgPopupEvent},
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
    zwlr_layer_shell_v1::{ZwlrLayerShellV1, Layer},
    zwlr_layer_surface_v1::{ZwlrLayerSurfaceV1, Event as ZwlrLayerSurfaceEvent, Anchor, KeyboardInteractivity},
};

use xkbcommon::xkb;

use nix::{
    fcntl::{self, OFlag}, unistd::pipe2
};

use async_io::{Async, Timer};
use futures_lite::FutureExt;

use std::{collections::{HashMap, HashSet}, env, error::Error as StdError, ffi::c_void as void, fmt, fs, io::{self, Write}, ops, os::fd::{AsFd, AsRawFd, FromRawFd}, sync::{Arc, Mutex, MutexGuard}, time::{Duration, Instant}};

use common::*;
use crate::*;

// ### base event loop ###

pub(crate) struct WaylandState<T: 'static + Send = ()> {
    app_name: String,
    pub(crate) con: Async<wayland_client::Connection>,
    qh: QueueHandle<Self>,
    globals: WaylandGlobals,
    events: Vec<Event<T>>, // used to push events from inside the dispatch impl
    // -- windowing state --
    mouse_data: MouseData,
    keyboard_data: KeyboardData,
    offer_data: OfferData, // drag-and-drop / selection data
    cursor_data: CursorData,
    monitor_list: HashSet<MonitorId>, // used to see which interface names belong to wl_outputs, vec is efficient here
    last_serial: u32,
}

#[derive(Default)]
struct CursorData {
    styles: HashMap<WindowId, CursorStyle>, // per-window cursor style
    last_enter_serial: u32, // last mouse enter serial
}

#[derive(Default)]
/// Used for handling drag-and-drop.
struct OfferData {
    has_offer: Option<WlSurface>,
    current_offer: Option<WlDataOffer>,
    x: f64, y: f64,
    dnd_active: bool,
    dnd_icon: Option<CustomIcon>, // set when Window::start_drag_and_drop is called
}

struct KeyboardData {
    has_focus: Option<WlSurface>,
    xkb_context: xkb::Context,
    keymap_specific: Option<KeymapSpecificData>, // (re)initialized when a keymap is loaded
    keymap_error: Option<EvlError>, // stored and handeled later
    repeat_timer: Timer,
    repeat_key: u32, // raw key
    repeat_rate: Duration,
    repeat_delay: Duration,
}

impl KeyboardData {
    pub fn new() -> io::Result<Self> {
        Ok(Self {
            has_focus: None,
            xkb_context: xkb::Context::new(xkb::CONTEXT_NO_FLAGS),
            keymap_specific: None,
            keymap_error: None,
            repeat_timer: Timer::never(),
            repeat_key: 0,
            repeat_rate: Duration::from_millis(60),
            repeat_delay: Duration::from_millis(450),
        })
    }
}

struct KeymapSpecificData {
    xkb_state: xkb::State,
    compose_state: xkb::compose::State,
    pressed_keys: PressedKeys,
}

#[derive(Default)]
struct MouseData {
    has_focus: Option<WlSurface>,
    x: u16,
    y: u16
}

// ### pressed keys ###

struct PressedKeys {
    min: u32,
    keys: bv::BitVec,
}

impl PressedKeys {

    pub fn new(keymap: &xkb::Keymap) -> Self {
        let min = keymap.min_keycode();
        let max = keymap.max_keycode();
        let len = max.raw() - min.raw();
        let mut keys = bv::BitVec::new();
        keys.resize(len as u64, false);
        Self {
            min: min.raw(),
            keys,
        }
    }

    pub fn update_key_state(&mut self, key: xkb::Keycode, state: KeyState) {
        let pressed = state == KeyState::Pressed;
        let idx = key.raw() - self.min;
        self.keys.set(idx as u64, pressed);
    }

    pub fn currently_pressed(&self) -> Vec<xkb::Keycode> {

        let mut down = Vec::new(); // we can't return anything that borrows self right now, TODO: still somehow update this, maybe use a generator

        for idx in 0..self.keys.len() {
            if self.keys.get(idx) == true {
                let keycode = xkb::Keycode::from(self.min + idx as u32);
                down.push(keycode)
            }
        }

        down

    }

}

// ### public async event loop ### TODO: rework these comments

pub(crate) struct Connection<T: 'static + Send> {
    pub(crate) state: WaylandState<T>,
    queue: EventQueue<WaylandState<T>>,
}

// TODO: don't use Deref<BaseWindow> for Window, since Deref can be confusing

impl<T: 'static + Send> Connection<T> {

    pub fn new(application: &str) -> Result<Self, EvlError> {

        let con = Async::new(
            wayland_client::Connection::connect_to_env()?
        )?;

        let (globals, queue) = registry_queue_init::<WaylandState<T>>(con.get_ref())?;
        let qh = queue.handle();

        let mut monitor_list = HashSet::with_capacity(1);
        let globals = WaylandGlobals::from_globals(&mut monitor_list, globals, &qh)?;

        let mut events = Vec::with_capacity(4);
        events.push(Event::Resume);

        let base = WaylandState {
            app_name: application.to_string(),
            con, qh, globals,
            events,
            mouse_data: MouseData::default(),
            keyboard_data: KeyboardData::new()?,
            offer_data: OfferData::default(),
            cursor_data: CursorData::default(),
            monitor_list,
            last_serial: 0, // we don't use an option here since an invalid serial may be a common case and is not treated as an error
        };

        Ok(Self {
            state: base,
            queue
        })

    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {

        loop {

            // flush all outgoing requests
            // (I forgot this and had to debug 10+ hours... fuckkk me)
            self.state.con.get_ref().flush()?;

            let guard = {
                let _span = tracing::span!(tracing::Level::TRACE, "lsg::wayland").entered();

                // process all events that we've stored
                loop {
                    self.queue.dispatch_pending(&mut self.state)?;
                    match self.queue.prepare_read() {
                        Some(val) => break val,
                        None => continue,
                    }
                }
            };

            if let Some(error) = self.state.keyboard_data.keymap_error.take() {
                return Err(error)
            };

            if let Some(event) = self.state.events.pop() {
                return Ok(event)
            }

            // wait for new events
            enum Either {
                Readable,
                Timer,
            }

            let readable = async {
                self.state.con.readable().await?;
                Ok(Either::Readable)
            };

            let timer = async {
                (&mut self.state.keyboard_data.repeat_timer).await;
                Ok(Either::Timer)
            };

            let future = readable.or(timer);

            match future.await {
                Ok(Either::Readable) => {
                    // read from the wayland connection
                    ignore_wouldblock(guard.read())?
                },
                Ok(Either::Timer)    => {
                    // emit the sysnthetic key-repeat event
                    let key = self.state.keyboard_data.repeat_key;
                    process_key_event(&mut self.state, key, Direction::Down, Source::KeyRepeat); // TODO: make it not take &mut self.state but only part of it and then remove the Either enum and process these directly in the `async` block
                    // TODO ^^^^ make this like self.state.keyboard_data.process_key_event(...)
                },
                Err(err) => return Err(err),
            }

        }

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
    layer_shell_mgr: Option<ZwlrLayerShellV1>,
    activation_mgr: Option<XdgActivationV1>,
    cursor_shape_mgr: Option<WpCursorShapeManagerV1>,
}

impl WaylandGlobals {

    pub fn from_globals<T: 'static + Send>(monitor_data: &mut HashSet<MonitorId>, globals: GlobalList, qh: &QueueHandle<WaylandState<T>>) -> Result<Self, BindError> {

        // bind the primary monitor we already retreived
        globals.contents().with_list(|list| for val in list {
            if &val.interface == "wl_output" {
                process_new_output(monitor_data, globals.registry(), val.name, qh);
            }
        });

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
            shape_device: None,
            data_device_mgr,
            data_device,
            frac_scale_mgrs: globals.bind(qh, 1..=1, ()).ok().and_then( // only Some if both are present
                |vp| Some((vp, globals.bind(qh, 1..=1, ()).ok()?)))
                .map(|(vp, frc)| FracScaleMgrs { viewport_mgr: vp, frac_scaling_mgr: frc }),
            decoration_mgr: globals.bind(qh, 1..=1, ()).ok(),
            layer_shell_mgr: globals.bind(qh, 1..=1, ()).ok(),
            activation_mgr: globals.bind(qh, 1..=1, ()).ok(),
            cursor_shape_mgr: globals.bind(qh, 1..=1, ()).ok(),
        };

        // TODO: what happens if cursor shape feature is not present?

        let _span = tracing::trace_span!("lsg::globals").entered();

        tracing::trace!(
            "wayland globals arquired, additional features:
                - fractional scaling: {},
                - server side decorations: {},
                - layer shell: {},
                - surface activations: {},
                - predefined cursor shapes: {}",
            this.frac_scale_mgrs.is_some(),
            this.decoration_mgr.is_some(),
            this.layer_shell_mgr.is_some(),
            this.activation_mgr.is_some(),
            this.cursor_shape_mgr.is_some()
        );

        Ok(this)

    }
}

struct FracScaleMgrs {
    viewport_mgr: WpViewporter,
    frac_scaling_mgr: WpFractionalScaleManagerV1,
}

struct FracScaleData {
    viewport: WpViewport,
    frac_scale: WpFractionalScaleV1,
}

// ### monitor info ###

pub type MonitorId = u32;

fn get_monitor_id(output: &WlOutput) -> MonitorId {
    output.id().protocol_id()
}

pub struct Monitor {
    /// Information about the monitor.
    info: MonitorInfo,
    wl_output: WlOutput,
}

impl Monitor {
    pub fn info(&self) -> &MonitorInfo {
        &self.info
    }
}

impl fmt::Debug for Monitor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Monitor {{ info: {:?}, ... }}", self.info)
    }
}

// ### base window ###

pub type WindowId = u32;

fn get_window_id(surface: &WlSurface) -> WindowId {
    surface.id().protocol_id()
}

pub struct BaseWindow<T: 'static + Send> {
    // our data
    pub(crate) id: WindowId, // also found in `shared`
    shared: Arc<Mutex<WindowShared>>, // needs to be accessed by some callbacks
    // wayland state
    qh: QueueHandle<WaylandState<T>>,
    // proxy: EventProxy<T>,
    compositor: WlCompositor, // used to create opaque regions
    pub(crate) wl_surface: WlSurface,
}

impl<T: 'static + Send> Drop for BaseWindow<T> {
    fn drop(&mut self) {
        self.wl_surface.destroy();
    }
}

impl<T: 'static + Send> BaseWindow<T> {

    pub(crate) fn new(evl: &mut EventLoop<T>, size: Size) -> Self {

        let evb = &mut evl.wayland.state;

        let surface = evb.globals.compositor.create_surface(&evb.qh, ());
        let id = get_window_id(&surface);

        // fractional scaling, if present
        let frac_scale_data = evb.globals.frac_scale_mgrs.as_ref().map(|val| {
            let viewport = val.viewport_mgr.get_viewport(&surface, &evb.qh, ());
            let frac_scale = val.frac_scaling_mgr.get_fractional_scale(&surface, &evb.qh, id);
            FracScaleData { viewport, frac_scale }
        });

        let shared = Arc::new(Mutex::new(WindowShared {
            id,
            new_width:  size.w,
            new_height: size.h,
            flags: ConfigureFlags::default(),
            redraw_requested: false,
            frame_callback_registered: false,
            already_got_redraw_event: false,
            // need to access some wayland objects
            frac_scale_data,
        }));

        Self {
            id,
            shared,
            qh: evb.qh.clone(),
            compositor: evb.globals.compositor.clone(),
            // proxy: EventProxy::new(evl),
            wl_surface: surface,
        }

    }

    pub fn id(&self) -> WindowId {
        self.id
    }

    /// Notify the windowing system that you are going to draw to the window now.
    /// This function is mandatory and you must call it, otherwise the window will behave weirdly.
    pub fn pre_present_notify(&self) {
        let mut guard = self.shared.lock().unwrap();
        // we are now processing the redraw event, so we can receive another one later
        // note: it is important that resetting this is not done inside the if-check below, since this might
        //       happen when a frame callback is still in-flight due to a redraw being triggered by a configure event
        //       that arrived before the frame callback completed, but we ALWAYS have to reset the variable
        guard.already_got_redraw_event = false;
        // you have to request the frame callback before swapping buffers.
        // really, the frame callback will start counting from the moment the buffers are swapped
        if !guard.frame_callback_registered { // make sure to only request a frame callback once
            guard.frame_callback_registered = true;
            self.wl_surface.frame(&self.qh, Arc::clone(&self.shared)); // TODO: every time an arc is cloned rn
            self.wl_surface.commit();
        }
    }

    /// Tells the windowing systen to redraw the window.
    ///
    /// Don't forget to call [`pre_present_notify`](Self::pre_present_notify).
    ///
    /// The next redraw will automatically be throttled to align with the "desired"
    /// framerate that may be chosen by the system. In most cases, this is the refresh rate of
    /// the monitor.
    ///
    /// In practice this means you can call this function as often or as rarely as you want and
    /// it will always generate at most one redraw event for every monitor frame.
    pub fn redraw_with_vsync(&self, evl: &mut EventLoop<T>) {
        let mut guard = self.shared.lock().unwrap();
        if guard.frame_callback_registered {
            // since a frame callback is currently in-flight which means we are wanting to redraw faster
            // then the monitor refresh rate, we will wait for vsync
            guard.redraw_requested = true;
        } else if !guard.already_got_redraw_event {
            // force-redraw, since we are apperently drawing slower then the monitor refresh rate
            guard.already_got_redraw_event = true; // will be reset next frame by `pre_present_notify`.
            evl.events.push(Event::Window { id: self.id, event: WindowEvent::Redraw });
        }
    }

    pub fn set_transparency(&self, value: bool) {
        if value {
            self.wl_surface.set_opaque_region(None);
            self.wl_surface.commit();
        } else {
            let region = self.compositor.create_region(&self.qh, ());
            region.add(0, 0, i32::MAX, i32::MAX);
            self.wl_surface.set_opaque_region(Some(&region));
            self.wl_surface.commit();
        }
    }

    /// This simply drops the window.
    pub fn close(self) {} // TODO: add close helper fns to everything to make closing things more explicit...

}

unsafe impl<T: Send + 'static> egl::IsSurface for BaseWindow<T> {
    fn ptr(&self) -> *mut void {
        self.wl_surface.id().as_ptr().cast()
    }
}

struct WindowShared {
    id: WindowId,
    new_width: u32,
    new_height: u32,
    flags: ConfigureFlags,
    /// Used to check if a frame event was already requested.
    redraw_requested: bool,
    /// Used to check if a frame callback is currently in-flight or if a redraw event
    /// has to be "force generated".
    frame_callback_registered: bool,
    /// This is set to `true` everytime a redraw event is finally pushed onto the event queue
    /// and used to assure only a single redraw event will be generated each frame.
    already_got_redraw_event: bool,
    frac_scale_data: Option<FracScaleData>,
}

impl Drop for WindowShared {
    fn drop(&mut self) {
        let frac_scale_data = self.frac_scale_data.as_ref().unwrap();
        frac_scale_data.viewport.destroy();
        frac_scale_data.frac_scale.destroy();
    }
}


// ### window ###

pub struct Window<T: 'static + Send> {
    base: BaseWindow<T>,
    xdg_surface: XdgSurface,
    xdg_toplevel: XdgToplevel,
    xdg_decoration: Option<ZxdgToplevelDecorationV1>,
}

impl<T: 'static + Send> ops::Deref for Window<T> {
    type Target = BaseWindow<T>;
    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

/// The window is closed on drop.
impl<T: 'static + Send> Drop for Window<T> {
    fn drop(&mut self) {
        self.xdg_decoration.as_ref().map(|val| val.destroy());
        self.xdg_toplevel.destroy();
        self.xdg_surface.destroy();
    }
}

impl<T: 'static + Send> Window<T> {

    pub fn new(evl: &mut EventLoop<T>, size: Size) -> Self {

        let base = BaseWindow::new(evl, size);

        let evb = &mut evl.wayland.state;

        // xdg-top-level role (+ init decoration manager)
        let xdg_surface = evb.globals.wm.get_xdg_surface(&base.wl_surface, &evb.qh, Arc::clone(&base.shared));
        let xdg_toplevel = xdg_surface.get_toplevel(&evb.qh, Arc::clone(&base.shared));

        let xdg_decoration = evb.globals.decoration_mgr.as_ref()
            .map(|val| val.get_toplevel_decoration(&xdg_toplevel, &evb.qh, base.id));

        xdg_decoration.as_ref().map(|val| val.set_mode(ZxdgDecorationMode::ServerSide));
        xdg_toplevel.set_app_id(evb.app_name.clone());

        base.wl_surface.commit();

        Self {
            base,
            xdg_surface,
            xdg_toplevel,
            xdg_decoration,
        }

    }

    pub fn destroy(self) {}

    pub fn set_decorations(&mut self, value: bool) {
        let mode = if value { ZxdgDecorationMode::ServerSide } else { ZxdgDecorationMode::ClientSide };
        self.xdg_decoration.as_ref().map(|val| val.set_mode(mode));
    }

    pub fn set_title<S: Into<String>>(&mut self, text: S) {
        self.xdg_toplevel.set_title(text.into());
    }

    pub fn set_maximized(&self, value: bool) {
        if value {
            self.xdg_toplevel.set_maximized();
        } else {
            self.xdg_toplevel.unset_maximized();
        };
        self.base.wl_surface.commit();
    }

    pub fn set_fullscreen(&mut self, value: bool, monitor: Option<&Monitor>) {
        if value {
            let wl_output = monitor.map(|val| &val.wl_output);
            self.xdg_toplevel.set_fullscreen(wl_output);
        } else {
            self.xdg_toplevel.unset_fullscreen();
        };
    }

    pub fn set_min_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_min_size(size.w as i32, size.h as i32);
        self.base.wl_surface.commit();
    }

    pub fn set_max_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_max_size(size.w as i32, size.h as i32);
        self.base.wl_surface.commit();
    }

    pub fn set_fixed_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_max_size(size.w as i32, size.h as i32);
        self.xdg_toplevel.set_min_size(size.w as i32, size.h as i32);
        self.base.wl_surface.commit();
    }

    pub fn request_user_attention(&mut self, evl: &mut EventLoop<T>, urgency: Urgency) {

        let evb = &mut evl.wayland.state;

        if let Urgency::Info = urgency {
            // we don't wanna switch focus, but on wayland just showing a
            // blinking icon is not possible
            return
        }

        if let Some(ref activation_mgr) = evb.globals.activation_mgr {

            let token = activation_mgr.get_activation_token(&evb.qh, self.base.wl_surface.clone());

            token.set_app_id(evb.app_name.clone());
            token.set_serial(evb.last_serial, &evb.globals.seat);

            if let Some(ref surface) = evb.keyboard_data.has_focus {
                token.set_surface(surface);
            }

            token.commit();

        }

    }

    // TODO: in theory all window funcs only need &self not &mut self, decide on this and make the API homogenous

    pub fn set_cursor(&mut self, evl: &mut EventLoop<T>, style: CursorStyle) {

        let evb = &mut evl.wayland.state;

        // note: the CustomIcon will also be kept alive by the styles as long as needed
        evb.cursor_data.styles.insert(self.base.id, style);

        // immediatly apply the style
        process_new_cursor_style(evb, self.base.id);

    }

    pub fn surface(&self) -> *mut std::ffi::c_void {
        use wayland_client::Proxy;
        self.wl_surface.id().as_ptr().cast()
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

impl DataKinds {
    pub(crate) fn to_mime_type(&self) -> &'static str {
        match *self {
            DataKinds::TEXT   => "text/plain",
            DataKinds::XML    => "application/xml",
            DataKinds::HTML   => "application/html",
            DataKinds::ZIP    => "application/zip",
            DataKinds::JSON   => "text/json",
            DataKinds::JPEG   => "image/jpeg",
            DataKinds::PNG    => "image/png",
            DataKinds::OTHER  => "application/octet-stream",
            _ => unreachable!(),
        }
    }
    pub(crate) fn from_mime_type(mime_type: &str) -> Option<Self> {
        match mime_type {
            "text/plain"       => Some(DataKinds::TEXT),
            "application/xml"  => Some(DataKinds::XML),
            "application/html" => Some(DataKinds::HTML),
            "application/zip"  => Some(DataKinds::ZIP),
            "text/json"        => Some(DataKinds::JSON),
            "image/jpeg"       => Some(DataKinds::JPEG),
            "image/png"        => Some(DataKinds::PNG),
            "application/octet-stream" => Some(DataKinds::OTHER),
            "UTF8_STRING" | "STRING" | "TEXT" => Some(DataKinds::TEXT), // apparently used in some X11 apps
            _ => None,
        }
    }
}

pub type DataOfferId = u32;

/// Don't hold onto it. You should immediatly decide if you want to receive something or not.
pub struct DataOffer {
    wl_data_offer: WlDataOffer,
    con: wayland_client::Connection, // needed to flush all events after accepting the offer
    kinds: DataKinds,
    dnd: bool, // checked in the destructor to determine how wl_data_offer should be destroyed
}

impl fmt::Debug for DataOffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DataOffer {{ kinds: {:?} }}", &self.kinds)
    }
}

/// Dropping this will cancel drag-and-drop.
impl Drop for DataOffer {
    fn drop(&mut self) {
        if self.dnd { self.wl_data_offer.finish() };
        self.wl_data_offer.destroy();
    }
}

impl DataOffer {

    pub fn kinds(&self) -> DataKinds {
        self.kinds
    }

    /// A `DataOffer` can be read multiple times. Also using different `DataKinds`.
    pub fn receive(&self, kind: DataKinds, mode: IoMode) -> Result<DataReader, EvlError> {

        let (reader, writer) = pipe2(OFlag::empty())?;

        // receive the data
        let mime_type = kind.to_mime_type();
        self.wl_data_offer.receive(mime_type.to_string(), writer.as_fd());

        self.con.flush()?; // <--- this is important, so we can immediatly read without deadlocking

        // set only the writing end to be nonblocking, if enabled
        if let IoMode::Nonblocking = mode {
            let old_flags = fcntl::fcntl(reader.as_raw_fd(), fcntl::FcntlArg::F_GETFL)?;
            let new_flags = OFlag::from_bits_retain(old_flags) | OFlag::O_NONBLOCK;
            fcntl::fcntl(reader.as_raw_fd(), fcntl::FcntlArg::F_SETFL(new_flags))?;
        }

        Ok(DataReader {
            reader: fs::File::from(reader),
        })

    }

    pub fn cancel(self) {}

}

pub struct DataReader {
    reader: fs::File,
}

impl io::Read for DataReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.reader.read(buf)
    }
}

#[derive(Debug)]
pub struct DataWriter {
    writer: fs::File,
}

impl io::Write for DataWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

pub type DataSourceId = u32;

fn get_data_source_id(ds: &WlDataSource) -> DataSourceId {
    ds.id().protocol_id()
}

/// A handle that let's you send data to other clients. Used for clipboard and drag-and-drop.
///
/// You will receive events for this DataSource when another client
/// or the system wants to read from the selection.
pub struct DataSource {
    pub id: DataSourceId,
    wl_data_source: WlDataSource,
}

/// Dropping this will cancel a drag-and-drop operation.
impl Drop for DataSource {
    fn drop(&mut self) {
        self.wl_data_source.destroy();
    }
}

impl DataSource {

    fn new<T: 'static + Send>(evl: &mut EventLoop<T>, offers: DataKinds, mode: IoMode) -> Self {

        let evb = &mut evl.wayland.state;

        debug_assert!(!offers.is_empty(), "must offer at least one DataKind");

        let wl_data_source = evb.globals.data_device_mgr.create_data_source(&evb.qh, mode);

        for offer in offers {
            let mime_type = offer.to_mime_type();
            wl_data_source.offer(mime_type.to_string()); // why do all wayland methods take String's and not &str?
        }

        // actions are not implemented right now
        wl_data_source.set_actions(DndAction::Move | DndAction::Copy);

        Self {
            id: get_data_source_id(&wl_data_source),
            wl_data_source,
        }

    }

    /// Create a DataSource that will be the new selection.
    ///
    /// In other words this "sets the selection (clipboard)". You will receive events for this DataSource when another client
    /// wants to read from the selection.
    // TODO + DOCS: docs-rs alias to "clipboard" or smth
    pub fn create_selection<T: 'static + Send>(evl: &mut EventLoop<T>, offers: DataKinds, mode: IoMode) -> Self {

        let this = Self::new(evl, offers, mode);

        evl.wayland.state.globals.data_device.set_selection(
            Some(&this.wl_data_source),
            evl.wayland.state.last_serial
        );

        this

    }

    /// You should only start a drag-and-drop when the left mouse button is held down
    /// *and* the user then moves the mouse.
    /// Otherwise the request may be denied or visually broken.
    #[track_caller]
    pub fn create_drag_and_drop<T: 'static + Send>(evl: &mut EventLoop<T>, window: &mut Window<T>, offers: DataKinds, mode: IoMode, icon: CustomIcon) -> Self {

        let this = Self::new(evl, offers, mode);

        let evb = &mut evl.wayland.state;

        // actually start the drag and drop
        evb.globals.data_device.start_drag(
            Some(&this.wl_data_source),
            &window.base.wl_surface,
            Some(&icon.wl_surface),
            evb.last_serial
        );

        evb.offer_data.dnd_active = true;
        evb.offer_data.dnd_icon = Some(icon);

        this

    }

    pub fn id(&self) -> DataSourceId {
        self.id
    }

    pub fn cancel(self) {}

}

// ### custom icon ###

pub struct CustomIcon {
    _file: fs::File,
    wl_shm_pool: WlShmPool,
    wl_buffer: WlBuffer,
    wl_surface: WlSurface,
}

impl fmt::Debug for CustomIcon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CustomIcon {{ ... }}")
    }
}

/// The icon surface is destroyed on drop.
impl Drop for CustomIcon {
    fn drop(&mut self) {
        self.wl_surface.destroy();
        self.wl_buffer.destroy();
        self.wl_shm_pool.destroy();
        // self._file will also be closed
    }
}

impl CustomIcon {

    /// Currently uses env::temp_dir() so the image content of your icon could be leaked to other users.
    #[track_caller]
    pub fn new<T: 'static + Send>(evl: &mut EventLoop<T>, size: Size, format: IconFormat, data: &[u8]) -> Result<Self, EvlError> {

        let evb = &mut evl.wayland.state;

        let len = data.len();

        let tmpdir = env::temp_dir();
        let file = fcntl::open(
            &tmpdir,
            OFlag::O_TMPFILE | OFlag::O_RDWR,
            nix::sys::stat::Mode::empty()
        )?;

        let mut file = unsafe { fs::File::from_raw_fd(file) };

        file.write_all(data)?;
        file.flush()?;

        let (wl_format, bytes_per_pixel) = match format {
            IconFormat::Argb8 => (WlFormat::Argb8888, 4i32),
        };

        // some basic checks that the dimensions of the data match the specified size

        debug_assert!(
            data.len() as u32 == size.w * size.h * bytes_per_pixel as u32,
            "length of data doesn't match specified dimensions and format"
        );

        debug_assert!(
            data.len() != 0,
            "length of data must be greater then 0"
        );

        let wl_shm_pool = evb.globals.shm.create_pool(file.as_fd(), len as i32, &evb.qh, ());
        let wl_buffer = wl_shm_pool.create_buffer(
            0, size.w as i32, size.h as i32,
            size.w as i32 * bytes_per_pixel, wl_format,
            &evb.qh, ()
        );

        let wl_surface = evb.globals.compositor.create_surface(&evb.qh, ());
        wl_surface.attach(Some(&wl_buffer), 0, 0);
        wl_surface.commit();

        Ok(Self {
            _file: file, // just keep the shared tempfile alive, since we ignore WlBuffer Release event
            wl_shm_pool,
            wl_buffer,
            wl_surface,
        })

    }

    pub fn destroy(self) {}

}

// ### (wayland) popup and layer window ###

pub struct PopupWindow<T: 'static + Send> {
    base: BaseWindow<T>,
    xdg_surface: XdgSurface,
    xdg_popup: XdgPopup,
}

impl<T: 'static + Send> ops::Deref for PopupWindow<T> {
    type Target = BaseWindow<T>;
    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

/// The window is closed on drop.
impl<T: 'static + Send> Drop for PopupWindow<T> {
    fn drop(&mut self) {
        self.xdg_popup.destroy();
        self.xdg_surface.destroy();
    }
}

impl<T: 'static + Send> PopupWindow<T> {

    pub fn new(evl: &mut EventLoop<T>, size: Size, parent: &Window<T>) -> Self {

        // TODO: this doesn't implement positioning of the popup window (where on the parent should it be)
        //       this is implemented using xdg_positioner.set_anchor or smth

        let base = BaseWindow::new(evl, size);

        let evb = &mut evl.wayland.state;

        // xdg-popup role
        let xdg_surface = evb.globals.wm.get_xdg_surface(&base.wl_surface, &evb.qh, Arc::clone(&base.shared));
        let xdg_positioner = evb.globals.wm.create_positioner(&evb.qh, ());

        let parent_guard = parent.shared.lock().unwrap();
        xdg_positioner.set_size(size.w as i32, size.h as i32);
        xdg_positioner.set_anchor_rect(0, 0, parent_guard.new_width as i32, parent_guard.new_height as i32);
        drop(parent_guard);

        let xdg_popup = xdg_surface.get_popup(Some(&parent.xdg_surface), &xdg_positioner, &evb.qh, Arc::clone(&base.shared));

        base.wl_surface.commit();

        Self {
            base,
            xdg_surface,
            xdg_popup,
        }

    }

    pub fn destroy(self) {}

}

pub struct LayerWindow<T: 'static + Send> {
    base: BaseWindow<T>,
    zwlr_surface: ZwlrLayerSurfaceV1,
}

impl<T: 'static + Send> ops::Deref for LayerWindow<T> {
    type Target = BaseWindow<T>;
    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

/// The window is closed on drop.
impl<T: 'static + Send> Drop for LayerWindow<T> {
    fn drop(&mut self) {
        self.zwlr_surface.destroy();
        self.base.wl_surface.destroy();
    }
}

impl<T: 'static + Send> LayerWindow<T> {

    /// # Errors
    /// Will return `Unsupported` if the neceserry extension (ZwlrLayerShellV1) is not present.
    pub fn new(evl: &mut EventLoop<T>, size: Size, layer: WindowLayer, monitor: Option<&Monitor>) -> Result<Self, EvlError> {

        let base = BaseWindow::new(evl, size);

        let evb = &mut evl.wayland.state;

        let wl_layer = match layer {
            WindowLayer::Background => Layer::Background,
            WindowLayer::Bottom     => Layer::Bottom,
            WindowLayer::Top        => Layer::Top,
            WindowLayer::Overlay    => Layer::Overlay,
        };

        let wl_output = monitor.map(|val| &val.wl_output);

        // creating this kind of window requires some wayland extensions
        let layer_shell_mgr = evb.globals.layer_shell_mgr.as_ref().ok_or(
            EvlError::Unsupported { name: ZwlrLayerShellV1::interface().name }
        )?;

        // layer-shell role
        let zwlr_surface = layer_shell_mgr.get_layer_surface(
            &base.wl_surface, wl_output, wl_layer, evb.app_name.clone(),
            &evb.qh, Arc::clone(&base.shared)
        );

        zwlr_surface.set_size(size.w, size.h);

        base.wl_surface.commit();

        Ok(Self {
            base,
            zwlr_surface,
        })

    }

    pub fn destroy(self) {}

    pub fn anchor(&self, anchor: WindowAnchor) {

        let wl_anchor = match anchor {
            WindowAnchor::Top => Anchor::Top,
            WindowAnchor::Bottom => Anchor::Bottom,
            WindowAnchor::Left => Anchor::Left,
            WindowAnchor::Right => Anchor::Right,
        };

        self.zwlr_surface.set_anchor(wl_anchor);
        self.base.wl_surface.commit();

    }

    pub fn margin(&self, value: u32) {

        let n = value as i32;

        self.zwlr_surface.set_margin(n, n, n, n);
        self.base.wl_surface.commit();

    }

    pub fn interactivity(&self, value: KbInteractivity) {

        let wl_intr = match value {
            KbInteractivity::None => KeyboardInteractivity::None,
            // KbInteractivity::Normal => KeyboardInteractivity::OnDemand,
            KbInteractivity::Exclusive => KeyboardInteractivity::Exclusive,
        };

        self.zwlr_surface.set_keyboard_interactivity(wl_intr);
        self.base.wl_surface.commit();

    }

}

// ### more stuff ###

#[derive(Debug)]
pub struct DndHandle {
    last_serial: u32,
    wl_data_offer: Option<WlDataOffer>,
}

impl DndHandle {
    pub fn advertise(&self, kinds: &[DataKinds]) {
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

        Keysym::Escape    => Key::Escape,
        Keysym::Tab       => Key::Tab,
        Keysym::Caps_Lock => Key::CapsLock,
        Keysym::Shift_L   => Key::Shift,
        Keysym::Shift_R   => Key::Shift,
        Keysym::Control_L => Key::Control,
        Keysym::Control_R => Key::Control,
        Keysym::Alt_L     => Key::Alt,
        Keysym::Alt_R     => Key::Alt,
        Keysym::ISO_Level3_Shift => Key::AltGr,
        Keysym::Super_L   => Key::Super,
        Keysym::Super_R   => Key::Super,
        Keysym::Menu      => Key::AppMenu,
        Keysym::Return    => Key::Return,
        Keysym::BackSpace => Key::Backspace,
        Keysym::space     => Key::Space,
        Keysym::Up        => Key::ArrowUp,
        Keysym::Down      => Key::ArrowDown,
        Keysym::Left      => Key::ArrowLeft,
        Keysym::Right     => Key::ArrowRight,

        Keysym::F1  => Key::F(1),
        Keysym::F2  => Key::F(2),
        Keysym::F3  => Key::F(3),
        Keysym::F4  => Key::F(4),
        Keysym::F5  => Key::F(5),
        Keysym::F6  => Key::F(6),
        Keysym::F7  => Key::F(7),
        Keysym::F8  => Key::F(8),
        Keysym::F9  => Key::F(9),
        Keysym::F10 => Key::F(10),
        Keysym::F11 => Key::F(11),
        Keysym::F12 => Key::F(12),

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
        // ) { println!("{}: {_event:?}", $prxy::interface().name); }
        ) {}
    };
}

impl<T: 'static + Send> wayland_client::Dispatch<WlRegistry, GlobalListContents> for WaylandState<T> {
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
                process_new_output(&mut evl.monitor_list, registry, name, qh);
            }
            // note: the event for new outputs is emitted in the `WlOutput` event handler
        }

        else if let WlRegistryEvent::GlobalRemove { name } = event {
            if evl.monitor_list.contains(&name) {
                evl.monitor_list.remove(&name);
                evl.events.push(Event::MonitorRemove { id: name })
            }
        }

    }
}

fn process_new_output<T: 'static + Send>(monitor_list: &mut HashSet<MonitorId>, registry: &WlRegistry, name: u32, qh: &QueueHandle<WaylandState<T>>) {
    let info = MonitorInfo::default();
    let output = registry.bind(name, 2, qh, Mutex::new(info)); // first time in my life using Mutex without an Arc
    let id = get_monitor_id(&output);
    monitor_list.insert(id);
}

impl<T: 'static + Send> wayland_client::Dispatch<WlOutput, Mutex<MonitorInfo>> for WaylandState<T> {
    fn event(
        evl: &mut Self,
        wl_output: &WlOutput,
        event: WlOutputEvent,
        data: &Mutex<MonitorInfo>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let mut guard = data.lock().unwrap();

        match event {
            WlOutputEvent::Name { name } => {
                if !name.is_empty() { guard.name = name };
            },
            WlOutputEvent::Description { description } => {
                guard.description = description;
            },
            WlOutputEvent::Mode { flags, width, height, refresh } => {
                if flags.into_result().is_ok_and(|it| it.contains(WlOutputMode::Current)) {
                        guard.size = Size { w: width as u32, h: height as u32 };
                    guard.refresh = refresh as u32;
                }
            },
            WlOutputEvent::Geometry { make, .. } => {
                if guard.name.is_empty() { guard.name = make };
            },
            WlOutputEvent::Done => {
                let id = get_monitor_id(wl_output);
                let state = Monitor {
                    info: guard.clone(),
                    wl_output: wl_output.clone(),
                };
                evl.events.push(Event::MonitorUpdate {
                    id, state,
                });
            },
            _ => (),
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<WlShm, ()> for WaylandState<T> { ignore!(WlShm, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlShmPool, ()> for WaylandState<T> { ignore!(WlShmPool, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlBuffer, ()> for WaylandState<T> { ignore!(WlBuffer, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<XdgPositioner, ()> for WaylandState<T> { ignore!(XdgPositioner, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpViewporter, ()> for WaylandState<T> { ignore!(WpViewporter, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WpViewport, ()> for WaylandState<T> { ignore!(WpViewport, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpCursorShapeManagerV1, ()> for WaylandState<T> { ignore!(WpCursorShapeManagerV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WpCursorShapeDeviceV1, ()> for WaylandState<T> { ignore!(WpCursorShapeDeviceV1, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WlDataDeviceManager, ()> for WaylandState<T> { ignore!(WlDataDeviceManager, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlDataDevice, ()> for WaylandState<T> {
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
                // actions are not implemented right now
                val.set_actions(DndAction::Copy | DndAction::Move, DndAction::Move);
            }

            let id = get_window_id(&surface);
            let sameapp = evl.offer_data.dnd_active;

            evl.offer_data.has_offer = Some(surface);
            evl.offer_data.current_offer = wl_data_offer.clone();

            evl.offer_data.x = x;
            evl.offer_data.y = y;

            let handle = DndHandle {
                last_serial: evl.last_serial,
                wl_data_offer,
            };

            evl.events.push(Event::Window {
                id,
                event: WindowEvent::Dnd {
                    event: DndEvent::Motion { x, y, handle },
                    sameapp
                }
            });

        }

        else if let WlDataDeviceEvent::Motion { x, y, .. } = event {

            evl.offer_data.x = x;
            evl.offer_data.y = y;

            let handle = DndHandle {
                last_serial: evl.last_serial,
                wl_data_offer: evl.offer_data.current_offer.clone(),
            };

            let surface = evl.offer_data.has_offer.as_ref().unwrap();
            let sameapp = evl.offer_data.dnd_active;

            evl.events.push(Event::Window {
                id: get_window_id(surface),
                event: WindowEvent::Dnd {
                    event: DndEvent::Motion { x, y, handle },
                    sameapp
                }
            });

        }

        else if let WlDataDeviceEvent::Drop = event {

            if let Some(wl_data_offer) = evl.offer_data.current_offer.take() {

                let x = evl.offer_data.x;
                let y = evl.offer_data.y;

                let data = wl_data_offer.data::<Mutex<DataKinds>>().unwrap();
                let kinds = data.lock().unwrap().clone();

                let offer = DataOffer {
                    wl_data_offer,
                    con: evl.con.get_ref().clone(),
                    kinds,
                    dnd: true,
                };

                let surface = evl.offer_data.has_offer.as_ref().unwrap();
                let sameapp = evl.offer_data.dnd_active;

                evl.events.push(Event::Window {
                    id: get_window_id(surface),
                    event: WindowEvent::Dnd {
                        event: DndEvent::Drop { x, y, offer },
                        sameapp
                    },
                });

            }
        }

        else if let WlDataDeviceEvent::Leave = event {

            // this maybe sent twice :(, so has_offer could be None
            if let Some(ref surface) = evl.offer_data.has_offer {

                evl.events.push(Event::Window {
                    id: get_window_id(surface),
                    event: WindowEvent::Dnd {
                        event: DndEvent::Cancel,
                        sameapp: evl.offer_data.dnd_active,
                    },
                });

            }

            evl.offer_data.has_offer = None;
            evl.offer_data.current_offer = None;

        }

        else if let WlDataDeviceEvent::Selection { id: value /* not an id! */ } = event {

            // the data offer will have already been "introduced", so we have received all
            // advertised data kinds

            if let Some(wl_data_offer) = value {

                let data = wl_data_offer.data::<Mutex<DataKinds>>().unwrap();
                let kinds = *data.lock().unwrap(); // copy the bitflags

                let offer = Some(DataOffer {
                    wl_data_offer,
                    con: evl.con.get_ref().clone(), // should be pretty cheap
                    kinds,
                    dnd: false,
                });

                evl.events.push(Event::SelectionUpdate { offer });

            } else {

                evl.events.push(Event::SelectionUpdate { offer: None });

            }

        }

    }

    wayland_client::event_created_child!(Self, WlDataDevice, [
        EVT_DATA_OFFER_OPCODE => (WlDataOffer, Mutex::new(DataKinds::empty()))
    ]);

}

impl<T: 'static + Send> wayland_client::Dispatch<WlDataOffer, Mutex<DataKinds>> for WaylandState<T> {
    fn event(
        _evl: &mut Self,
        _data_offer: &WlDataOffer,
        event: WlDataOfferEvent,
        info: &Mutex<DataKinds>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        if let WlDataOfferEvent::Offer { mime_type } = event {
            if let Some(kind) = DataKinds::from_mime_type(&mime_type) {
                let mut guard = info.lock().unwrap();
                if !guard.contains(kind) { guard.insert(kind) };
            };
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<WlDataSource, IoMode> for WaylandState<T> {
    fn event(
        evl: &mut Self,
        data_source: &WlDataSource,
        event: WlDataSourceEvent,
        mode: &IoMode,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let id = get_data_source_id(data_source);

        if let WlDataSourceEvent::Send { mime_type, fd } = event {

            // always set nonblocking mode explicitly
            let old_flags = fcntl::fcntl(fd.as_raw_fd(), fcntl::FcntlArg::F_GETFL).expect("handle error"); // TODO:!!!
            let mut new_flags = OFlag::from_bits_retain(old_flags);
            if let IoMode::Nonblocking = mode { new_flags.insert(OFlag::O_NONBLOCK) }
            else { new_flags.remove(OFlag::O_NONBLOCK) };
            fcntl::fcntl(fd.as_raw_fd(), fcntl::FcntlArg::F_SETFL(new_flags)).expect("handle error");

            let kind = DataKinds::from_mime_type(&mime_type).unwrap();
            let writer = DataWriter { writer: fs::File::from(fd) };

            evl.events.push(Event::DataSource {
                id, event: DataSourceEvent::Send { kind, writer }
            });

        }

        else if let WlDataSourceEvent::DndFinished = event { // emitted on succesfull write

            evl.events.push(Event::DataSource {
                id, event: DataSourceEvent::Success
            });

        }

        else if let WlDataSourceEvent::Cancelled = event { // emitted on termination of the operation

            evl.offer_data.dnd_active = false;
            evl.offer_data.dnd_icon = None;

            evl.events.push(Event::DataSource {
                id, event: DataSourceEvent::Close
            });

        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<XdgActivationV1, ()> for WaylandState<T> { ignore!(XdgActivationV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<XdgActivationTokenV1, WlSurface> for WaylandState<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<ZxdgDecorationManagerV1, ()> for WaylandState<T> { ignore!(ZxdgDecorationManagerV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<ZxdgToplevelDecorationV1, WindowId> for WaylandState<T> {
    fn event(
        evl: &mut Self,
        _deco: &ZxdgToplevelDecorationV1,
        event: <ZxdgToplevelDecorationV1 as Proxy>::Event,
        data: &WindowId,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        if let ZxdgDecorationEvent::Configure { mode } = event {
            let event = match mode {
                WEnum::Value(ZxdgDecorationMode::ServerSide) => WindowEvent::Decorations { active: true },
                WEnum::Value(ZxdgDecorationMode::ClientSide) => WindowEvent::Decorations { active: false },
                _ => return,
            };
            evl.events.push(Event::Window { id: *data, event });
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<WpFractionalScaleManagerV1, ()> for WaylandState<T> { ignore!(WpFractionalScaleManagerV1, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WlSeat, ()> for WaylandState<T> {
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
                seat.get_keyboard(qh, ());
            }
            if capabilities.contains(WlSeatCapability::Pointer) {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgWmBase, ()> for WaylandState<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgSurface, Arc<Mutex<WindowShared>>> for WaylandState<T> {
    fn event(
        evl: &mut Self,
        xdg_surface: &XdgSurface,
        event: XdgSurfaceEvent,
        shared: &Arc<Mutex<WindowShared>>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if let XdgSurfaceEvent::Configure { serial } = event {

            // ack the configure
            xdg_surface.ack_configure(serial);

            let guard = shared.lock().unwrap();

            let width  = guard.new_width;
            let height = guard.new_height;

            process_configure(evl, guard, width, height);

        }
    }
}

impl<T: 'static + Send> wayland_client::Dispatch<XdgToplevel, Arc<Mutex<WindowShared>>> for WaylandState<T> {
    fn event(
        evl: &mut Self,
        _surface: &XdgToplevel,
        event: XdgToplevelEvent,
        shared: &Arc<Mutex<WindowShared>>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let mut guard = shared.lock().unwrap();

        if let XdgToplevelEvent::Configure { width, height, states } = event {
            if width > 0 && height > 0 {
                guard.new_width  = width  as u32;
                guard.new_height = height as u32;
            }
            guard.flags = read_configure_flags(states);
        }

        else if let XdgToplevelEvent::Close = event {
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Close });
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<XdgPopup, Arc<Mutex<WindowShared>>> for WaylandState<T> {
    fn event(
        evl: &mut Self,
        _surface: &XdgPopup,
        event: XdgPopupEvent,
        shared: &Arc<Mutex<WindowShared>>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let mut guard = shared.lock().unwrap();

        if let XdgPopupEvent::Configure { width, height, .. } = event {
            if width > 0 && height > 0 {
                guard.new_width  = width  as u32;
                guard.new_height = height as u32;
            }
        }

        else if let XdgPopupEvent::PopupDone = event {
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Close });
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<ZwlrLayerShellV1, ()> for WaylandState<T> {
    ignore!(ZwlrLayerShellV1, ());
}

impl<T: 'static + Send> wayland_client::Dispatch<ZwlrLayerSurfaceV1, Arc<Mutex<WindowShared>>> for WaylandState<T> {
    fn event(
        evl: &mut Self,
        zwlr_surface: &ZwlrLayerSurfaceV1,
        event: ZwlrLayerSurfaceEvent,
        shared: &Arc<Mutex<WindowShared>>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {

        let mut guard = shared.lock().unwrap();

        if let ZwlrLayerSurfaceEvent::Configure { width, height, serial } = event {

            // ack the configure
            zwlr_surface.ack_configure(serial);

            if width > 0 && height > 0 {
                guard.new_width  = width;
                guard.new_height = height;
            }

            process_configure(evl, guard, width, height);

        }

        else if let ZwlrLayerSurfaceEvent::Closed = event {
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Close });
        }

    }
}

fn process_configure<T: 'static + Send>(evl: &mut WaylandState<T>, mut guard: MutexGuard<WindowShared>, width: u32, height: u32) {

    // update the window's viewport destination
    if let Some(ref frac_scale_data) = guard.frac_scale_data {
        frac_scale_data.viewport.set_destination(width as i32, height as i32);
    };

    if !guard.already_got_redraw_event {
        guard.already_got_redraw_event = true;
        evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Redraw });
    }

    // foreward the final configuration state to the user
    evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Resize {
        size: Size { w: width, h: height },
        flags: guard.flags
    } });

}

fn read_configure_flags(states: Vec<u8>) -> ConfigureFlags {
    states.chunks_exact(4)
        .flat_map(|chunk| chunk.try_into())
        .map(|bytes| u32::from_ne_bytes(bytes))
        .flat_map(XdgToplevelState::try_from)
        .fold(ConfigureFlags::default(), |mut acc, state| {
            if let XdgToplevelState::Fullscreen = state {
                acc.fullscreen = true;
            };
            acc
        })
}

impl<T: 'static + Send> wayland_client::Dispatch<WlCallback, Arc<Mutex<WindowShared>>> for WaylandState<T> {
    fn event(
        evl: &mut Self,
        _cb: &WlCallback,
        _event: WlCallbackEvent,
        shared: &Arc<Mutex<WindowShared>>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        let mut guard = shared.lock().unwrap();
        if !guard.already_got_redraw_event && guard.redraw_requested {
            guard.already_got_redraw_event = true;
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Redraw });
        }
        guard.frame_callback_registered = false;
        guard.redraw_requested = false;
    }
}

// global events
impl<T: 'static + Send> wayland_client::Dispatch<WlCompositor, ()> for WaylandState<T> { ignore!(WlCompositor, ()); }

// surface events (like moving onto an output etc.)
impl<T: 'static + Send> wayland_client::Dispatch<WlSurface, ()> for WaylandState<T> { ignore!(WlSurface, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpFractionalScaleV1, WindowId> for WaylandState<T> {
    fn event(
            evl: &mut Self,
            _proxy: &WpFractionalScaleV1,
            event: WpFractionalScaleV1Event,
            data: &WindowId,
            _conn: &wayland_client::Connection,
            _qh: &QueueHandle<Self>,
        ) {

        if let WpFractionalScaleV1Event::PreferredScale { scale } = event {

            evl.events.push(Event::Window {
                id: *data,
                event: WindowEvent::Rescale { scale: scale as f64 / 120.0 }
            });

        }

    }
}

// region events
impl<T: 'static + Send> wayland_client::Dispatch<WlRegion, ()> for WaylandState<T> { ignore!(WlRegion, ()); }

// input events
impl<T: 'static + Send> wayland_client::Dispatch<WlKeyboard, ()> for WaylandState<T> {
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
                        &evl.keyboard_data.xkb_context,
                        fd, size as usize,
                        xkb::FORMAT_TEXT_V1,
                        xkb::KEYMAP_COMPILE_NO_FLAGS
                    ) } {
                        Ok(Some(val)) => val,
                        Ok(None) => { evl.keyboard_data.keymap_error = Some("corrupt xkb keymap received".into()); return },
                        Err(err) => { evl.keyboard_data.keymap_error = Some(err.into()); return }
                    }
                };

                let xkb_state = xkb::State::new(&xkb_keymap);
                let pressed_keys = PressedKeys::new(&xkb_keymap);

                // initialize composition state

                let locale = env::var_os("LANG")
                    .unwrap_or_else(|| "en_US.UTF-8".into());

                let compose_table = match xkb::Table::new_from_locale(
                    &evl.keyboard_data.xkb_context,
                    &locale,
                    xkb::COMPILE_NO_FLAGS
                ) {
                    Ok(val) => val,
                    Err(..) => {
                        evl.keyboard_data.keymap_error = Some(EvlError::InvalidLocale { value: locale.to_string_lossy().into() });
                        return
                    }
                };

                let compose_state = xkb::compose::State::new(&compose_table, xkb::STATE_NO_FLAGS);

                evl.keyboard_data.keymap_specific = Some(KeymapSpecificData {
                    xkb_state, compose_state, pressed_keys
                });

                tracing::trace!("keymap set, locale: {:?}", &locale);

            },

            WlKeyboardEvent::Enter { surface, keys, .. } => {

                let id = get_window_id(&surface);

                evl.keyboard_data.has_focus = Some(surface);

                // emit the enter event
                evl.events.push(Event::Window { id, event: WindowEvent::Enter });

                let iter = keys.chunks_exact(4)
                    .flat_map(|chunk| chunk.try_into())
                    .map(|bytes| u32::from_ne_bytes(bytes));

                // emit a key-down event for all keys that are pressed when entering focus
                for raw_key in iter {
                    process_key_event(evl, raw_key, Direction::Down, Source::Event);
                }

            },

            WlKeyboardEvent::Leave { .. } => {

                if let Some(ref keymap_specific) = evl.keyboard_data.keymap_specific {

                    let surface = evl.keyboard_data.has_focus.as_ref().unwrap();
                    let id = get_window_id(&surface);

                    evl.events.push(Event::Window { id, event: WindowEvent::Leave });

                    // emit a synthetic key-up event for all keys that are still pressed
                    for key in keymap_specific.pressed_keys.currently_pressed() {
                        process_key_event(evl, key.raw(), Direction::Up, Source::Event);
                    }

                    evl.keyboard_data.has_focus = None;

                    // also invalidate selection, to be more correct
                    evl.events.push(Event::SelectionUpdate { offer: None });

                };

            },

            WlKeyboardEvent::Key { key: raw_key, state, serial, .. } => {

                let dir = match state {
                    WEnum::Value(KeyState::Pressed) => Direction::Down,
                    WEnum::Value(KeyState::Released) => Direction::Up,
                    WEnum::Value(..) => return,
                    WEnum::Unknown(..) => return
                };

                evl.last_serial = serial;

                process_key_event(evl, raw_key, dir, Source::Event);


            },

            WlKeyboardEvent::Modifiers { mods_depressed, mods_latched, mods_locked, group, .. } => {

                if let Some(ref mut keymap_specific) = evl.keyboard_data.keymap_specific {
                    keymap_specific.xkb_state.update_mask(mods_depressed, mods_latched, mods_locked, 0, 0, group);
                };

            },

            WlKeyboardEvent::RepeatInfo { rate, delay } => {

                tracing::trace!("key repeat info, rate: {}, delay: {}", rate, delay);

                if rate > 0 {
                    evl.keyboard_data.repeat_rate = Duration::from_millis(1000 / rate as u64);
                    evl.keyboard_data.repeat_delay = Duration::from_millis(delay as u64);
                } else {
                    evl.keyboard_data.repeat_rate = Duration::ZERO;
                    evl.keyboard_data.repeat_delay = Duration::ZERO;
                }

            },

            _ => (),

        }

    }
}

#[derive(PartialEq, Eq)]
enum Direction {
    Down,
    Up,
}

#[derive(PartialEq, Eq)]
enum Source {
    Event,
    KeyRepeat,
}

// TODO: make this function not take a &mut WaylandState, but more like &mut KeyState
fn process_key_event<T: 'static + Send>(evl: &mut WaylandState<T>, raw_key: u32, dir: Direction, source: Source) {

    // NOTE: uses evl.keyboard_data and evl.events

    let Some(ref mut keymap_specific) = evl.keyboard_data.keymap_specific else { return };

    let surface = evl.keyboard_data.has_focus.as_ref().unwrap();
    let id = get_window_id(&surface);

    let xkb_key = xkb::Keycode::new(raw_key + 8); // "+8" says the wayland docs

    let repeat = source == Source::KeyRepeat;

    if dir == Direction::Down {

        let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
        let modifier = xkb_sym.is_modifier_key(); // if this key is a modifier key

        // emit a generic key down event
        let key = translate_xkb_sym(xkb_sym);
        evl.events.push(Event::Window { id, event: WindowEvent::KeyDown { key, repeat } });

        // turn this key into utf8 text and emit text input events
        keymap_specific.compose_state.feed(xkb_sym);
        match keymap_specific.compose_state.status() {
            xkb::Status::Nothing => {
                if let Some(chr) = xkb_sym.key_char() {
                    evl.events.push(Event::Window { id, event: WindowEvent::TextInput { chr } })
                }
            },
            xkb::Status::Composing => {
                // sadly we can't just get the string repr of a dead-char
                if let Some(chr) = translate_dead_to_normal_sym(xkb_sym).and_then(xkb::Keysym::key_char) {
                    evl.events.push(Event::Window { id, event: WindowEvent::TextCompose { chr } })
                }
            },
            xkb::Status::Composed => {
                if let Some(text) = keymap_specific.compose_state.utf8() {
                    for chr in text.chars() {
                        evl.events.push(Event::Window { id, event: WindowEvent::TextInput { chr } })
                    }
                }
                keymap_specific.compose_state.reset();
            },
            xkb::Status::Cancelled => {
                // order is important, so that the cancel event is received first
                if let Some(chr) = xkb_sym.key_char() {
                    evl.events.push(Event::Window { id, event: WindowEvent::TextInput { chr } })
                }
                evl.events.push(Event::Window { id, event: WindowEvent::TextComposeCancel });
            },
            }

        // implement key repeat
        // only re-arm if this was NOT called from a repeated key event
        if !modifier && source != Source::KeyRepeat {

            evl.keyboard_data.repeat_key = raw_key;

            // arm key-repeat timer with the correct delay and repeat rate
            evl.keyboard_data.repeat_timer.set_interval_at(
                Instant::now() + evl.keyboard_data.repeat_delay,
                evl.keyboard_data.repeat_rate
            );

            // update the key state
            keymap_specific.pressed_keys.update_key_state(xkb_key, KeyState::Pressed);

    }

    } else {

        // unarm key-repeat timer
        evl.keyboard_data.repeat_timer.set_after(Duration::MAX);

        // update the key state
        keymap_specific.pressed_keys.update_key_state(xkb_key, KeyState::Released);

        let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
        let key = translate_xkb_sym(xkb_sym);
        evl.events.push(Event::Window { id, event: WindowEvent::KeyUp { key } });

    };

}

impl<T: 'static + Send> wayland_client::Dispatch<WlPointer, ()> for WaylandState<T> {
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

                let id = get_window_id(&surface);
                let (x, y) = (surface_x.max(0.) as u16,
                              surface_y.max(0.) as u16); // must not be negative

                evl.mouse_data.has_focus = Some(surface);
                evl.mouse_data.x = x;
                evl.mouse_data.y = y;

                evl.events.push(Event::Window { id, event: WindowEvent::MouseEnter });
                evl.events.push(Event::Window { id, event:
                    WindowEvent::MouseMotion { x, y }
                });

                // set the apropriate per-window pointer style
                // wayland by default only supports client-wide pointer styling

                evl.cursor_data.last_enter_serial = serial;

                process_new_cursor_style(evl, id);

             },

             WlPointerEvent::Leave { surface, .. } => {

                let id = get_window_id(&surface);

                evl.mouse_data.has_focus = None;

                evl.events.push(Event::Window { id, event: WindowEvent::MouseEnter });

             },

             WlPointerEvent::Motion { surface_x, surface_y, .. } => {

                 let (x, y) = (surface_x.max(0.) as u16,
                               surface_y.max(0.) as u16); // must not be negative

                evl.mouse_data.x = x;
                evl.mouse_data.y = y;

                let surface = evl.mouse_data.has_focus.as_ref().unwrap();
                let id = get_window_id(&surface);

                evl.events.push(Event::Window {
                    id,
                    event: WindowEvent::MouseMotion { x, y }
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
                    other => MouseButton::Unknown(other),
                };

                let down = match state {
                    WEnum::Value(ButtonState::Pressed) => true,
                    WEnum::Value(ButtonState::Released) => false,
                    WEnum::Value(..) => return, // fucking non-exhaustive enums
                    WEnum::Unknown(..) => return
                };

                let event = if down {
                    WindowEvent::MouseDown { button, x: evl.mouse_data.x, y: evl.mouse_data.y }
                } else {
                    WindowEvent::MouseUp { button, x: evl.mouse_data.x, y: evl.mouse_data.y }
                };

                evl.last_serial = serial;

                let surface = evl.mouse_data.has_focus.as_ref().unwrap();
                let id = get_window_id(&surface);

                evl.events.push(Event::Window {
                    id,
                    event
                });

            },

            WlPointerEvent::Axis { axis, value, .. } => {

                let axis = match axis {
                    WEnum::Value(Axis::VerticalScroll) => ScrollAxis::Vertical,
                    WEnum::Value(Axis::HorizontalScroll) => ScrollAxis::Horizontal,
                    WEnum::Value(..) => return, // TODO: raise more soft-errors in general, maybe there's an "error" event
                    WEnum::Unknown(..) => return
                };

                let surface = evl.mouse_data.has_focus.as_ref().unwrap();
                let id = get_window_id(&surface);

                let adjusted_value = (value * 1000.0) as i16;

                evl.events.push(Event::Window {
                    id,
                    event: WindowEvent::MouseScroll { axis, value: adjusted_value }
                });

            },

            _ => ()

        }

    }
}

/// there are different wayland protocols used to set different kinds of
/// cursor styles
fn process_new_cursor_style<T: 'static + Send>(evl: &mut WaylandState<T>, id: WindowId) {

    let style = evl.cursor_data.styles.get(&id)
        .unwrap_or(&CursorStyle::Predefined { shape: CursorShape::Default });

    if let Some(ref wl_pointer) = evl.globals.pointer {

        let serial = evl.cursor_data.last_enter_serial;

        match style {
            CursorStyle::Hidden => {
                wl_pointer.set_cursor(serial, None, 0, 0);
            },
            CursorStyle::Custom { icon, hotspot } => {
                wl_pointer.set_cursor(
                    serial, Some(&icon.wl_surface),
                    hotspot.x as i32, hotspot.y as i32
                )
            },
            CursorStyle::Predefined { shape } => {
                let wl_shape = shape.to_wl();
                if let Some(ref wp_shape_device) = evl.globals.shape_device {
                    wp_shape_device.set_shape(serial, wl_shape);
                }
            }
        }
        // mat
    }

}

// ### error handling ###

#[derive(Debug)]
pub enum EvlError {
    // TODO: document variants
    Unsupported { name: &'static str, },
    InvalidLocale { value: String },
    Dbus { msg: String },
    Fatal { msg: String },
}

impl fmt::Display for EvlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unsupported   { name }  => write!(f, "[missing feature] '{}'", name),
            Self::InvalidLocale { value } => write!(f, "[invalid locale] '{}'", value),
            Self::Dbus          { msg }   => write!(f, "[dbus call failed] '{}'", msg),
            Self::Fatal         { msg }   => write!(f, "[fatal] '{}'", msg)
        }
    }
}

impl StdError for EvlError {}

impl<'a> From<&'a str> for EvlError {
    fn from(value: &'a str) -> Self {
        Self::Fatal { msg: value.into() }
    }
}

impl From<wayland_client::ConnectError> for EvlError {
    fn from(value: wayland_client::ConnectError) -> Self {
        Self::Fatal { msg: format!("cannot connect to wayland, {}", value) }
    }
}

impl From<wayland_client::globals::GlobalError> for EvlError {
    fn from(value: wayland_client::globals::GlobalError) -> Self {
        Self::Fatal { msg: format!("failed to get wayland globals, {}", value) }
    }
}

impl From<BindError> for EvlError {
    fn from(value: BindError) -> Self {
        Self::Fatal { msg: format!("failed to get wayland global, {}", value) }
    }
}

impl From<wayland_client::backend::WaylandError> for EvlError {
    fn from(value: wayland_client::backend::WaylandError) -> Self {
        Self::Fatal { msg: format!("failed wayland call, {}", value) }
    }
}

impl From<wayland_client::DispatchError> for EvlError {
    fn from(value: wayland_client::DispatchError) -> Self {
        Self::Fatal { msg: format!("failed wayland dispatch, {}", value) }
    }
}

impl From<egl::EglError> for EvlError {
    fn from(value: egl::EglError) -> Self {
        Self::Fatal { msg: format!("failed egl call, {:?}", value) }
    }
}

impl From<nix::errno::Errno> for EvlError {
    fn from(value: nix::errno::Errno) -> Self {
        Self::Fatal { msg: format!("failed I/O, {}", value) }
    }
}

impl From<io::Error> for EvlError {
    fn from(value: io::Error) -> Self {
        Self::Fatal { msg: format!("failed I/O, {}", value) }
    }
}

impl From<dbus::MethodError> for EvlError {
    fn from(value: dbus::MethodError) -> Self {
        // description handeled inside EvlError::Display::fmt
        Self::Dbus { msg: format!("{:?}", value) }
    }
}

impl From<dbus::ArgError> for EvlError {
    fn from(value: dbus::ArgError) -> Self {
        // description handeled inside EvlError::Display::fmt
        Self::Dbus { msg: format!("{:?}", value) }
    }
}
