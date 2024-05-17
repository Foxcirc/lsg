
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

use khronos_egl as egl;

use xkbcommon::xkb;

use nix::{
    fcntl::{self, OFlag}, unistd::pipe2
};

#[cfg(feature = "signals")]
use nix::sys::signal::Signal;

use async_io::{Async, Timer};
use async_channel::{Sender as AsyncSender, Receiver as AsyncReceiver};
use futures_lite::{future::block_on, FutureExt};

use std::{
    mem, fmt, env, ops, fs,
    ffi::c_void as void,
    error::Error as StdError,
    sync::{Arc, Mutex, MutexGuard},
    io::{self, Write},
    os::fd::{AsFd, FromRawFd, AsRawFd},
    time::{Duration, Instant},
    collections::{HashSet, HashMap},
};

use crate::window::{
    self,
    Event, WindowEvent, DndEvent,
    SendError,
    CursorStyle, CursorShape,
    IoMode, InputMode, QuitReason, Urgency, IconFormat, WindowLayer, WindowAnchor, KbInteractivity, Rect, Size, Key, DataSourceEvent, DataKinds, ConfigureFlags, MouseButton, ScrollAxis, PresentToken
};

// ### base event loop ###

struct BaseLoop<T: 'static + Send = ()> {
    appid: String,
    con: Async<wayland_client::Connection>,
    #[cfg(feature = "signals")]
    signals: async_signals::Signals, // listens to sigterm and emits a quit event
    qh: QueueHandle<Self>,
    wl: WaylandState,
    events: Vec<Event<T>>, // used to push events from inside the dispatch impl
    proxy_data: ProxyData<T>,
    // -- state --
    mouse_data: MouseData,
    keyboard_data: KeyboardData,
    offer_data: OfferData, // drag-and-drop / selection data
    cursor_data: CursorData,
    monitor_list: HashSet<MonitorId>, // used to see which interface names belong to wl_outputs, vec is efficient here
    last_serial: u32,
}

struct ProxyData<T: 'static + Send> {
    sender: AsyncSender<Event<T>>,
    receiver: AsyncReceiver<Event<T>>
}

impl<T: 'static + Send> ProxyData<T> {
    fn new() -> Self {
        let (sender, receiver) = async_channel::unbounded();
        Self { sender, receiver }
    }
}

#[derive(Default)]
struct CursorData {
    styles: HashMap<WindowId, CursorStyle>, // per-window cursor style
    last_enter_serial: u32, // last mouse enter serial
}

#[derive(Default)]
struct OfferData {
    has_offer: Option<WlSurface>,
    current_offer: Option<WlDataOffer>,
    current_selection: Option<WlDataOffer>,
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
    input_modes: HashMap<WindowId, InputMode>, // per-window input mode
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
            input_modes: HashMap::new(),
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
    x: f64,
    y: f64
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
        Self {
            min: min.raw(),
            keys: bv::bit_vec![false; len as u64],
        }
    }

    pub fn key_down(&mut self, key: xkb::Keycode) {
        let idx = key.raw() - self.min;
        self.keys.set(idx as u64, true);
    }

    pub fn key_up(&mut self, key: xkb::Keycode) {
        let idx = key.raw() - self.min;
        self.keys.set(idx as u64, false);
    }

    pub fn keys_down(&self) -> Vec<xkb::Keycode> {
        let mut down = Vec::new(); // we can't return anything that borrows self
        for idx in 0..self.keys.len() {
            if self.keys.get(idx) == true {
                let keycode = xkb::Keycode::from(self.min + idx as u32);
                down.push(keycode)
            }
        }
        down
    }

}

// ### event proxy ###

pub struct EventProxy<T> {
    sender: AsyncSender<Event<T>>,
}

impl<T> EventProxy<T> {

    pub fn send(&self, event: Event<T>) -> Result<(), SendError<Event<T>>> {
        block_on(self.sender.send(event))
            .map_err(|err| SendError { inner: err.into_inner() })
    }

}

// ### public async event loop ###

pub struct EventLoop<T: 'static + Send> {
    base: BaseLoop<T>,
    queue: EventQueue<BaseLoop<T>>,
}

impl<T: 'static + Send> EventLoop<T> {

    pub fn new(application: &str) -> Result<Self, EvlError> {

        let con = Async::new(
            wayland_client::Connection::connect_to_env()?
        )?;

        let (globals, queue) = registry_queue_init::<BaseLoop<T>>(con.get_ref())?;
        let qh = queue.handle();

        let mut monitor_list = HashSet::with_capacity(1);
        let wl = WaylandState::from_globals(&mut monitor_list, globals, &qh)?;

        #[cfg(feature = "signals")]
        let signals = async_signals::Signals::new([
            Signal::SIGTERM as i32,
            Signal::SIGINT as i32
        ]).map_err(|err| io::Error::from(err))?;

        let mut events = Vec::with_capacity(4);
        events.push(Event::Resume);

        let base = BaseLoop {
            appid: application.to_string(),
            con, qh, wl,
            #[cfg(feature = "signals")]
            signals,
            events,
            proxy_data: ProxyData::new(),
            mouse_data: MouseData::default(),
            keyboard_data: KeyboardData::new()?,
            offer_data: OfferData::default(),
            cursor_data: CursorData::default(),
            monitor_list,
            last_serial: 0, // we don't use an option here since an invalid serial may be a common case and is not treated as an error
        };

        Ok(Self {
            base,
            queue
        })
        
    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {

        loop {
            
            self.base.con.get_ref().flush()?; // nah, I forgot this and had to debug 10+ hours... fuckkk me

            let guard = loop {

                self.queue.dispatch_pending(&mut self.base).unwrap();

                match self.queue.prepare_read() {
                    Some(val) => break val,
                    None => continue,
                };

            };

            if let Some(error) = self.base.keyboard_data.keymap_error.take() {
                return Err(error)
            };

            // process all new events
            if let Some(event) = self.base.events.pop() {
                return Ok(event)
            }

            // let timeout = if self.events.is_empty() { PollTimeout::NONE } else { PollTimeout::ZERO }; // maybe cb generated events TOOD: <-----

            enum Either<T: 'static + Send> {
                Readable,
                Timer,
                Channel(Event<T>),
                #[cfg(feature = "signals")] Signal(i32),
            }

            let readable = async {
                self.base.con.readable().await?;
                Ok(Either::Readable)
            };

            let timer = async {
                (&mut self.base.keyboard_data.repeat_timer).await;
                Ok(Either::Timer)
            };

            let channel = async {
                let event = self.base.proxy_data.receiver.recv().await.unwrap();
                Ok(Either::Channel(event))
            };

            #[cfg(feature = "signals")]
            let signals = async {
                use futures_lite::StreamExt;
                let signal = self.base.signals.next().await
                    .unwrap_or(0);
                Ok(Either::Signal(signal))
            };

            #[cfg(feature = "signals")]
            let future = readable.or(timer).or(channel).or(signals);
            #[cfg(not(feature = "signals"))]
            let future = readable.or(timer).or(channel);

            match future.await {
                Ok(Either::Readable) => {
                    // read from the wayland connection
                    ignore_wouldblock(guard.read())?
                },
                Ok(Either::Timer)    => {
                    // emit the sysnthetic key-repeat event
                    let key = self.base.keyboard_data.repeat_key;
                    process_key_event(&mut self.base, key, Direction::Down, Source::KeyRepeat);
                },
                Ok(Either::Channel(event)) => {
                    // just return the event
                    return Ok(event)
                },
                #[cfg(feature = "signals")]
                Ok(Either::Signal(signal)) => {
                    if signal == Signal::SIGTERM as i32 {
                        self.base.events.push(Event::QuitRequested { reason: QuitReason::System })
                    } else if signal == Signal::SIGINT as i32 {
                        self.base.events.push(Event::QuitRequested { reason: QuitReason::CtrlC })
                    }
                }
                Err(err) => return Err(err),
            }

        }
        
    }

    pub fn on_dispatch_thread<R>(&mut self, func: impl FnOnce() -> R) -> R {
        // on linux, this code should run on the main thread anyways
        func()
    }

    pub fn new_proxy(&mut self) -> EventProxy<T> {
        EventProxy {
            sender: self.base.proxy_data.sender.clone()
        }
    }

    pub fn suspend(&mut self) {
        self.base.events.push(Event::Suspend);
    }

    pub fn resume(&mut self) {
        self.base.events.push(Event::Resume);
    }

    pub fn request_quit(&mut self) {
        self.base.events.push(Event::QuitRequested { reason: QuitReason::User });
    }

    #[track_caller]
    pub fn get_clip_board(&mut self) -> Option<DataOffer> {

        let wl_data_offer = self.base.offer_data.current_selection.clone()?;
        let data = wl_data_offer.data::<Mutex<DataKinds>>().unwrap();
        let kinds = *data.lock().unwrap(); // copy the bitflags

        Some(DataOffer {
            wl_data_offer,
            con: self.base.con.get_ref().clone(), // should be pretty cheap
            kinds,
            dnd: false,
        })

    }

    pub fn set_clip_board(&mut self, ds: Option<&DataSource>) {

        self.base.wl.data_device.set_selection(
            ds.map(|val| &val.wl_data_source),
            self.base.last_serial
        );

    }

}

fn ignore_wouldblock<T>(result: Result<T, WaylandError>) -> Result<(), WaylandError> {
    match result {
        Ok(..) => Ok(()),
        Err(WaylandError::Io(ref err)) if err.kind() == io::ErrorKind::WouldBlock => Ok(()),
        Err(other) => Err(other),
    }
}

pub fn run<E: 'static + Send, T, H: FnOnce(EventLoop<E>) -> T>(handler: H, application: &str) -> Result<T, EvlError> {

    let target = EventLoop::new(application)?;
    Ok(handler(target))

}

struct WaylandState {
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

impl WaylandState {
    pub fn from_globals<T: 'static + Send>(monitor_data: &mut HashSet<MonitorId>, globals: GlobalList, qh: &QueueHandle<BaseLoop<T>>) -> Result<Self, BindError> {

        // bind the primary monitor we already retreived
        globals.contents().with_list(|list| for val in list {
            if &val.interface == "wl_output" {
                process_new_output(monitor_data, globals.registry(), val.name, qh);
            }
        });

        // TODO: multiple seats?, switching and updating seats? how to handle it
        let seat: WlSeat = globals.bind(qh, 1..=4, ())?;

        // bind the data device, for this seat
        let data_device_mgr: WlDataDeviceManager = globals.bind(qh, 1..=3, ())?; // < v3 doesn't emit cancelled events
        let data_device = data_device_mgr.get_data_device(&seat, qh, ());

        Ok(Self {
            compositor: globals.bind(qh, 4..=6, ())?,
            wm: globals.bind(qh, 1..=1, ())?,
            shm: globals.bind(qh, 1..=1, ())?,
            seat,
            pointer: None,
            shape_device: None,
            data_device_mgr,
            data_device,
            frac_scale_mgrs: globals.bind(qh, 1..=1, ()).ok() .and_then( // only Some if both are present
                |vp| Some((vp, globals.bind(qh, 1..=1, ()).ok()?)))
                .map(|(vp, frc)| FracScaleMgrs { viewport_mgr: vp, frac_scaling_mgr: frc }),
            decoration_mgr: globals.bind(qh, 1..=1, ()).ok(),
            layer_shell_mgr: globals.bind(qh, 1..=1, ()).ok(),
            activation_mgr: globals.bind(qh, 1..=1, ()).ok(),
            cursor_shape_mgr: globals.bind(qh, 1..=1, ()).ok(),
        })

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
    info: window::MonitorInfo,
    wl_output: WlOutput,
}

impl Monitor {
    pub fn info(&self) -> &window::MonitorInfo {
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
    id: WindowId, // also found in `shared`
    shared: Arc<Mutex<WindowShared>>, // needs to be accessed by some callbacks
    // wayland state
    qh: QueueHandle<BaseLoop<T>>,
    compositor: WlCompositor, // used to create opaque regions
    wl_surface: WlSurface,
}

impl<T: 'static + Send> Drop for BaseWindow<T> {
    fn drop(&mut self) {
        self.wl_surface.destroy();
    }
}

impl<T: 'static + Send> BaseWindow<T> {

    pub(crate) fn new(evl: &mut EventLoop<T>, size: Size) -> Self {

        let evb = &mut evl.base;

        let surface = evb.wl.compositor.create_surface(&evb.qh, ());
        let id = get_window_id(&surface);

        // fractional scaling, if present
        let frac_scale_data = evb.wl.frac_scale_mgrs.as_ref().map(|val| {
            let viewport = val.viewport_mgr.get_viewport(&surface, &evb.qh, ());
            let frac_scale = val.frac_scaling_mgr.get_fractional_scale(&surface, &evb.qh, id);
            FracScaleData { viewport, frac_scale }
        });

        let shared = Arc::new(Mutex::new(WindowShared {
            id,
            new_width:  size.width,
            new_height: size.height,
            flags: ConfigureFlags::default(),
            redraw_requested: false,
            frame_callback_registered: false,
            already_redrawing: false,
            // need to access some wayland objects
            frac_scale_data,
        }));

        Self {
            id,
            shared,
            qh: evb.qh.clone(),
            compositor: evb.wl.compositor.clone(),
            wl_surface: surface,
        }
        
    }

    pub fn id(&self) -> WindowId {
        self.id
    }

    #[track_caller]
    pub fn request_redraw(&self, token: PresentToken) {
        debug_assert!(self.id == token.id, "token for another window, see PresentToken docs");
        let mut guard = self.shared.lock().unwrap();
        guard.redraw_requested = true;
    }

    /// You must always redraw if asked to.
    pub fn pre_present_notify(&self) -> PresentToken {
        // note: it seems you have request the frame callback before swapping buffers
        //       otherwise the callback will never fire because the compositor thinks the content didn't change
        let mut guard = self.shared.lock().unwrap();
        if !guard.frame_callback_registered {
            guard.frame_callback_registered = true;
            guard.already_redrawing = false; // reset it here, because this must be invoked after a frame event was received
            self.wl_surface.frame(&self.qh, Arc::clone(&self.shared));
            self.wl_surface.commit();
        }
        PresentToken { id: self.id }
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
    
}

struct WindowShared {
    id: WindowId,
    new_width: u32,
    new_height: u32,
    flags: ConfigureFlags,
    redraw_requested: bool,
    frame_callback_registered: bool,
    already_redrawing: bool,
    // need to access some wayland objects
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
    base: window::BaseWindow<T>,
    xdg_surface: XdgSurface,
    xdg_toplevel: XdgToplevel,
    xdg_decoration: Option<ZxdgToplevelDecorationV1>,
}

impl<T: 'static + Send> ops::Deref for Window<T> {
    type Target = window::BaseWindow<T>;
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

        let evb = &mut evl.base;

        // xdg-top-level role (+ init decoration manager)
        let xdg_surface = evb.wl.wm.get_xdg_surface(&base.wl_surface, &evb.qh, Arc::clone(&base.shared));
        let xdg_toplevel = xdg_surface.get_toplevel(&evb.qh, Arc::clone(&base.shared));

        let xdg_decoration = evb.wl.decoration_mgr.as_ref()
            .map(|val| val.get_toplevel_decoration(&xdg_toplevel, &evb.qh, base.id));

        xdg_decoration.as_ref().map(|val| val.set_mode(ZxdgDecorationMode::ServerSide));
        xdg_toplevel.set_app_id(evb.appid.clone());

        base.wl_surface.commit();

        Self {
            base: window::BaseWindow { platform: base },
            xdg_surface,
            xdg_toplevel,
            xdg_decoration,
        }
        
    }

    pub fn destroy(self) {}

    pub fn set_input_mode(&self, evl: &mut EventLoop<T>, mode: InputMode) {
        evl.base.keyboard_data.input_modes.insert(self.base.platform.id, mode);
    }

    pub fn set_decorations(&mut self, value: bool) {
        let mode = if value { ZxdgDecorationMode::ServerSide } else { ZxdgDecorationMode::ClientSide };
        self.xdg_decoration.as_ref().map(|val| val.set_mode(mode));
    }

    pub fn set_title<S: Into<String>>(&self, text: S) {
        self.xdg_toplevel.set_title(text.into());
    }

    pub fn set_maximized(&self, value: bool) {
        if value {
            self.xdg_toplevel.set_maximized();
        } else {
            self.xdg_toplevel.unset_maximized();
        };
        self.base.platform.wl_surface.commit();
    }

    pub fn set_fullscreen(&self, value: bool, monitor: Option<&Monitor>) {
        if value {
            let wl_output = monitor.map(|val| &val.wl_output);
            self.xdg_toplevel.set_fullscreen(wl_output);
        } else {
            self.xdg_toplevel.unset_fullscreen();
        };
    }

    pub fn min_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_min_size(size.width as i32, size.height as i32);
        self.base.platform.wl_surface.commit();
    }

    pub fn max_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_max_size(size.width as i32, size.height as i32);
        self.base.platform.wl_surface.commit();
    }

    pub fn force_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_max_size(size.width as i32, size.height as i32);
        self.xdg_toplevel.set_min_size(size.width as i32, size.height as i32);
        self.base.platform.wl_surface.commit();
    }

    pub fn request_user_attention(&self, evl: &mut EventLoop<T>, urgency: Urgency) {

        let evb = &mut evl.base;

        if let Urgency::Info = urgency {
            // we don't wanna switch focus, but on wayland just showing a
            // blinking icon is not possible
            return
        }

        if let Some(ref activation_mgr) = evb.wl.activation_mgr {

            let token = activation_mgr.get_activation_token(&evb.qh, self.base.platform.wl_surface.clone());

            token.set_app_id(evb.appid.clone());
            token.set_serial(evb.last_serial, &evb.wl.seat);

            if let Some(ref surface) = evb.keyboard_data.has_focus {
                token.set_surface(surface);
            }

            token.commit();

        }

    }

    /// You should only start a drag-and-drop when the left mouse button is held down
    /// *and* the user then moves the mouse.
    /// Otherwise the request may be denied or visually broken.
    pub fn start_drag_and_drop(&self, evl: &mut EventLoop<T>, icon: CustomIcon, ds: &DataSource) {

        let evb = &mut evl.base;

        evb.wl.data_device.start_drag(
            Some(&ds.wl_data_source),
            &self.base.platform.wl_surface,
            Some(&icon.wl_surface),
            evb.last_serial
        );

        evb.offer_data.dnd_active = true;
        evb.offer_data.dnd_icon = Some(icon);

    }

    pub fn set_cursor(&self, evl: &mut EventLoop<T>, style: CursorStyle) {

        let evb = &mut evl.base;

        // note: the CustomIcon will also be kept alive by the styles as long as needed
        evb.cursor_data.styles.insert(self.base.platform.id, style);

        // immediatly apply the style
        process_new_cursor_style(evb, self.base.platform.id);

    }

}

impl window::CursorShape {
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
    dnd: bool, // checked on drop to determine how wl_data_offer should be destroyed
}

impl fmt::Debug for DataOffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DataOffer {{ kinds: {:?} }}", &self.kinds)
    }
}

/// Dropping this will cancel drag-and-drop / invalidate the clipboard.
impl Drop for DataOffer {
    fn drop(&mut self) {
        if self.dnd {
            self.wl_data_offer.finish();
            self.wl_data_offer.destroy();
        };
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

pub struct DataSource {
    id: DataSourceId,
    wl_data_source: WlDataSource,
}

/// Dropping this will cancel the drag-and-drop operation.
impl Drop for DataSource {
    fn drop(&mut self) {
        self.wl_data_source.destroy();
    }
}

impl DataSource {

    #[track_caller]
    pub fn new<T: 'static + Send>(evl: &mut EventLoop<T>, offers: DataKinds, mode: IoMode) -> Self {

        let evb = &mut evl.base;

        debug_assert!(!offers.is_empty(), "must offer at least one DataKind");

        let wl_data_source = evb.wl.data_device_mgr.create_data_source(&evb.qh, mode);

        for offer in offers {

            let mime_type = offer.to_mime_type();
            wl_data_source.offer(mime_type.to_string()); // why do all wayland methods take String's and not &str

        }

        // actions are not implemented right now
        wl_data_source.set_actions(DndAction::Move | DndAction::Copy);

        Self {
            id: get_data_source_id(&wl_data_source),
            wl_data_source,
        }
        
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

        let evb = &mut evl.base;

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
            data.len() as u32 == size.width * size.height * bytes_per_pixel as u32,
            "length of data doesn't match specified dimensions and format"
        );

        debug_assert!(
            data.len() != 0,
            "length of data must be greater then 0"
        );

        let wl_shm_pool = evb.wl.shm.create_pool(file.as_fd(), len as i32, &evb.qh, ());
        let wl_buffer = wl_shm_pool.create_buffer(
            0, size.width as i32, size.height as i32,
            size.width as i32 * bytes_per_pixel, wl_format,
            &evb.qh, ()
        );

        let wl_surface = evb.wl.compositor.create_surface(&evb.qh, ());
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

        let base = BaseWindow::new(evl, size);

        let evb = &mut evl.base;

        // xdg-popup role
        let xdg_surface = evb.wl.wm.get_xdg_surface(&base.wl_surface, &evb.qh, Arc::clone(&base.shared));
        let xdg_positioner = evb.wl.wm.create_positioner(&evb.qh, ());

        let parent_guard = parent.platform.shared.lock().unwrap();
        xdg_positioner.set_size(size.width as i32, size.height as i32);
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

    pub fn new(evl: &mut EventLoop<T>, size: Size, layer: WindowLayer, monitor: Option<&Monitor>) -> Result<Self, Unsupported> {

        let base = BaseWindow::new(evl, size);

        let evb = &mut evl.base;

        let wl_layer = match layer {
            WindowLayer::Background => Layer::Background,
            WindowLayer::Bottom     => Layer::Bottom,
            WindowLayer::Top        => Layer::Top,
            WindowLayer::Overlay    => Layer::Overlay,
        };

        let wl_output = monitor.map(|val| &val.wl_output);

        // creating this kind of window requires some wayland extensions 
        let layer_shell_mgr = evb.wl.layer_shell_mgr.as_ref().ok_or(
            Unsupported(ZwlrLayerShellV1::interface().name)
        )?;

        // layer-shell role
        let zwlr_surface = layer_shell_mgr.get_layer_surface(
            &base.wl_surface, wl_output, wl_layer, evb.appid.clone(),
            &evb.qh, Arc::clone(&base.shared)
        );

        zwlr_surface.set_size(size.width, size.height);

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

    pub fn set_interactivity(&self, value: KbInteractivity) {

        let wl_intr = match value {
            KbInteractivity::None => KeyboardInteractivity::None,
            // KbInteractivity::Normal => KeyboardInteractivity::OnDemand,
            KbInteractivity::Exclusive => KeyboardInteractivity::Exclusive,
        };

        self.zwlr_surface.set_keyboard_interactivity(wl_intr);
        self.base.wl_surface.commit();
        
    }
    
}

// ### egl api ###

type FnSwapBuffersWithDamage = fn(
    khronos_egl::EGLDisplay,
    khronos_egl::EGLSurface,
    *const void /* damage rect array */,
    khronos_egl::Int
) -> khronos_egl::Int;

pub struct EglInstance {
    lib: Arc<egl::DynamicInstance<egl::EGL1_0>>,
    swap_buffers_with_damage: Option<FnSwapBuffersWithDamage>,
    display: egl::Display,
}

impl EglInstance {

    /// Should be only be called once. Although initializing multiple instances is not a hard error.
    pub fn new<T: 'static + Send>(evh: &mut EventLoop<T>) -> Result<Arc<Self>, EvlError> {
        
        let loaded = unsafe {
            egl::DynamicInstance::<egl::EGL1_0>::load_required()
                .map_err(|_| EvlError::EglUnsupported)?
        };

        let lib = Arc::new(loaded);

        let wl_display = evh.base.con.get_ref().display().id().as_ptr();
        let egl_display = unsafe {
            lib.get_display(wl_display.cast())
        }.ok_or(EvlError::NoDisplay)?;

        lib.initialize(egl_display)?;

    	// side note: const EGL_EXTENSIONS = 0x3055

        let func = lib.get_proc_address("eglSwapBuffersWithDamageKHR");
        let swap_buffers_with_damage: Option<FnSwapBuffersWithDamage> =
            unsafe { mem::transmute(func) };

        Ok(Arc::new(Self {
            lib,
            swap_buffers_with_damage,
            display: egl_display,
        }))
        
    }

    pub fn get_proc_address(&self, name: &str) -> Option<extern "system" fn()> {
        self.lib.get_proc_address(name)
    }
    
}

struct EglBase {
    instance: Arc<EglInstance>,
    egl_surface: egl::Surface,
    egl_context: egl::Context,
    damage_rects: Vec<Rect>, // only here to save some allocations
    size: Size, // updated in resize
}

impl Drop for EglBase {
    fn drop(&mut self) {
        self.instance.lib.destroy_surface(self.instance.display, self.egl_surface).unwrap();
        self.instance.lib.destroy_context(self.instance.display, self.egl_context).unwrap();
    }
}

impl EglBase {

    /// Create a new egl context that will draw onto the given window.
    pub(crate) fn new(instance: &Arc<EglInstance>, surface: egl::Surface, config: egl::Config, size: Size) -> Result<Self, EvlError> {

        let context = {
            let attribs = [
                egl::CONTEXT_MAJOR_VERSION, 4,
                egl::CONTEXT_MINOR_VERSION, 0,
                egl::CONTEXT_CLIENT_VERSION, 3,
                egl::CONTEXT_OPENGL_DEBUG, if cfg!(debug) { 1 } else { 0 },
                egl::NONE,
            ];
            instance.lib.create_context(instance.display, config, None, &attribs).unwrap()
        };

        Ok(Self {
            instance: Arc::clone(&instance),
            egl_surface: surface,
            egl_context: context,
            damage_rects: Vec::with_capacity(2),
            size,
        })
        
    }

    /// Make this context current.
    pub(crate) fn bind(&self) -> Result<(), egl::Error> {

        self.instance.lib.make_current(
            self.instance.display,
            Some(self.egl_surface), // note: it is an error to only specify one of the two (read/draw) surfaces
            Some(self.egl_surface),
            Some(self.egl_context)
        )
        
    }

    /// Unbind this context.
    pub(crate) fn unbind(&self) -> Result<(), egl::Error> {

        self.instance.lib.make_current(
            self.instance.display,
            None, None, None
        )
        
    }

    /// Returns an error if this context is not the current one.
    pub(crate) fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EvlError> {

        // recalculate the origin of the rects to be in the top left

        let damage = damage.unwrap_or(&[]);

        self.damage_rects.clear();
        self.damage_rects.extend_from_slice(damage);

        for rect in self.damage_rects.iter_mut() {
            rect.y = self.size.height as i32 - rect.y - rect.h;
        }

        if let Some(func) = self.instance.swap_buffers_with_damage {
            // swap with damage, if the fn could be found
            (func)(self.instance.display.as_ptr(), self.egl_surface.as_ptr(), self.damage_rects.as_ptr().cast(), damage.len() as khronos_egl::Int);
        } else {
            // normal swap (if the extension is unsupported)
            self.instance.lib.swap_buffers(self.instance.display, self.egl_surface)?;
        }

        Ok(())

    }
   
}

pub struct EglContext {
    inner: EglBase,
    id: WindowId,
    wl_egl_surface: wayland_egl::WlEglSurface, // note: needs to be kept alive
}

impl EglContext {

    /// Create a new egl context that will draw onto the given window.
    pub fn new<T: 'static + Send>(instance: &Arc<EglInstance>, window: &window::BaseWindow<T>, size: Size) -> Result<Self, EvlError> {

        let config = {
            let attribs = [
                egl::SURFACE_TYPE, egl::WINDOW_BIT,
                egl::RENDERABLE_TYPE, egl::OPENGL_ES3_BIT,
                egl::RED_SIZE, 8,
                egl::GREEN_SIZE, 8,
                egl::BLUE_SIZE, 8,
                egl::ALPHA_SIZE, 8,
                egl::NONE
            ];
            instance.lib.choose_first_config(instance.display, &attribs)?
                .ok_or(EvlError::EglUnsupported)?
        };

        let wl_egl_surface = wayland_egl::WlEglSurface::new(
            window.platform.wl_surface.id(),
            size.width as i32,
            size.height as i32
        )?;

        let attrs = [
            egl::RENDER_BUFFER, egl::BACK_BUFFER,
            egl::NONE,
        ];

        let surface = unsafe {
            instance.lib.create_window_surface(
                instance.display,
                config,
                wl_egl_surface.ptr() as *mut void,
                Some(&attrs),
            )?
        };

        let inner = EglBase::new(instance, surface, config, size)?;

        Ok(Self {
            inner,
            id: window.platform.id,
            wl_egl_surface,
        })
        
    }

    /// Make this context current.
    pub fn bind(&self) -> Result<(), egl::Error> {
        self.inner.bind()
    }

    /// Unbind this context.
    pub fn unbind(&self) -> Result<(), egl::Error> {
        self.inner.unbind()
    }

    /// Returns an error if this context is not the current one.
    #[track_caller]
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>, token: PresentToken) -> Result<(), EvlError> {

        debug_assert!(self.id == token.id, "present token for another window");

        self.inner.swap_buffers(damage)

    }

    /// Don't forget to also resize your opengl viewport!
    pub fn resize(&mut self, size: Size) {

        self.inner.size = size;
        self.wl_egl_surface.resize(size.width as i32, size.height as i32, 0, 0);
        
    }

}

pub struct EglPixelBuffer {
    inner: EglBase,
}

impl EglPixelBuffer {

    /// Create a new egl context that will draw onto the given window.
    pub fn new(instance: &Arc<EglInstance>, size: Size) -> Result<Self, EvlError> {

        let config = {
            let attribs = [
                egl::SURFACE_TYPE, egl::PBUFFER_BIT,
                egl::RENDERABLE_TYPE, egl::OPENGL_ES3_BIT,
                egl::RED_SIZE, 8,
                egl::GREEN_SIZE, 8,
                egl::BLUE_SIZE, 8,
                egl::ALPHA_SIZE, 8,
                egl::NONE
            ];
            instance.lib.choose_first_config(instance.display, &attribs)?
                .ok_or(EvlError::EglUnsupported)?
        };

        let surface = instance.lib.create_pbuffer_surface(
            instance.display,
            config,
            &[]
        )?;

        let inner = EglBase::new(instance, surface, config, size)?;

        Ok(Self {
            inner,
        })
        
    }

    /// Make this context current.
    pub fn bind(&self) -> Result<(), egl::Error> {
        self.inner.bind()
    }

    /// Unbind this context.
    pub fn unbind(&self) -> Result<(), egl::Error> {
        self.inner.unbind()
    }

    /// Returns an error if this context is not the current one.
    #[track_caller]
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EvlError> {
        self.inner.swap_buffers(damage)
    }

}

// ### events ###

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

/// Look at the source and see how keys are translated.
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

impl<T: 'static + Send> wayland_client::Dispatch<WlRegistry, GlobalListContents> for BaseLoop<T> {
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

fn process_new_output<T: 'static + Send>(monitor_list: &mut HashSet<MonitorId>, registry: &WlRegistry, name: u32, qh: &QueueHandle<BaseLoop<T>>) {
    let info = window::MonitorInfo::default();
    let output = registry.bind(name, 2, qh, Mutex::new(info)); // first time in my life using Mutex without an Arc
    let id = get_monitor_id(&output);
    monitor_list.insert(id);
}

impl<T: 'static + Send> wayland_client::Dispatch<WlOutput, Mutex<window::MonitorInfo>> for BaseLoop<T> {
    fn event(
        evl: &mut Self,
        wl_output: &WlOutput,
        event: WlOutputEvent,
        data: &Mutex<window::MonitorInfo>,
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
                if flags == WEnum::Value(WlOutputMode::Current) {
                    guard.size = Size { width: width as u32, height: height as u32 };
                    guard.refresh = refresh as u32;
                }
            },
            WlOutputEvent::Geometry { make, .. } => {
                if guard.name.is_empty() { guard.name = make };
            },
            WlOutputEvent::Done => {
                let id = get_monitor_id(wl_output);
                let platform = Monitor {
                    info: guard.clone(),
                    wl_output: wl_output.clone(),
                };
                evl.events.push(Event::MonitorUpdate {
                    id, state: window::Monitor { platform }
                });
            },
            _ => (),
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<WlShm, ()> for BaseLoop<T> { ignore!(WlShm, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlShmPool, ()> for BaseLoop<T> { ignore!(WlShmPool, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlBuffer, ()> for BaseLoop<T> { ignore!(WlBuffer, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<XdgPositioner, ()> for BaseLoop<T> { ignore!(XdgPositioner, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpViewporter, ()> for BaseLoop<T> { ignore!(WpViewporter, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WpViewport, ()> for BaseLoop<T> { ignore!(WpViewport, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpCursorShapeManagerV1, ()> for BaseLoop<T> { ignore!(WpCursorShapeManagerV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WpCursorShapeDeviceV1, ()> for BaseLoop<T> { ignore!(WpCursorShapeDeviceV1, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WlDataDeviceManager, ()> for BaseLoop<T> { ignore!(WlDataDeviceManager, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlDataDevice, ()> for BaseLoop<T> {
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

            let platform = DndHandle {
                last_serial: evl.last_serial,
                wl_data_offer,
            };

            evl.events.push(Event::Window {
                id,
                event: WindowEvent::Dnd {
                    event: DndEvent::Motion { x, y, handle: window::DndHandle { platform } },
                    sameapp
                }
            });

        }

        else if let WlDataDeviceEvent::Motion { x, y, .. } = event {

            evl.offer_data.x = x;
            evl.offer_data.y = y;

            let platform = DndHandle {
                last_serial: evl.last_serial,
                wl_data_offer: evl.offer_data.current_offer.clone(),
            };

            let surface = evl.offer_data.has_offer.as_ref().unwrap();
            let sameapp = evl.offer_data.dnd_active;

            evl.events.push(Event::Window {
                id: get_window_id(surface),
                event: WindowEvent::Dnd {
                    event: DndEvent::Motion { x, y, handle: window::DndHandle { platform } },
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

                let platform = DataOffer {
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
                        event: DndEvent::Drop { x, y, offer: window::DataOffer { platform } },
                        sameapp
                    },
                });

            }
        }

        else if let WlDataDeviceEvent::Leave = event {

            let surface = evl.offer_data.has_offer.as_ref().unwrap();
            let sameapp = evl.offer_data.dnd_active;

            evl.events.push(Event::Window {
                id: get_window_id(surface),
                event: WindowEvent::Dnd { event: DndEvent::Cancel, sameapp },
            });

            evl.offer_data.has_offer = None;
            evl.offer_data.current_offer = None;

        }
    
        else if let WlDataDeviceEvent::Selection { id: wl_data_offer } = event {

            evl.offer_data.current_selection = wl_data_offer;

        }

    }

    wayland_client::event_created_child!(Self, WlDataDevice, [
        EVT_DATA_OFFER_OPCODE => (WlDataOffer, Mutex::new(DataKinds::empty()))
    ]);

}

impl<T: 'static + Send> wayland_client::Dispatch<WlDataOffer, Mutex<DataKinds>> for BaseLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<WlDataSource, IoMode> for BaseLoop<T> {
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
            let old_flags = fcntl::fcntl(fd.as_raw_fd(), fcntl::FcntlArg::F_GETFL).expect("handle error");
            let mut new_flags = OFlag::from_bits_retain(old_flags);
            if let IoMode::Nonblocking = mode { new_flags.insert(OFlag::O_NONBLOCK) }
            else { new_flags.remove(OFlag::O_NONBLOCK) };
            fcntl::fcntl(fd.as_raw_fd(), fcntl::FcntlArg::F_SETFL(new_flags)).expect("handle error");

            let kind = DataKinds::from_mime_type(&mime_type).unwrap();
            let platform = DataWriter { writer: fs::File::from(fd) };

            evl.events.push(Event::DataSource {
                id, event: DataSourceEvent::Send { kind, writer: window::DataWriter { platform } }
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgActivationV1, ()> for BaseLoop<T> { ignore!(XdgActivationV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<XdgActivationTokenV1, WlSurface> for BaseLoop<T> {
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
            let activation_mgr = evl.wl.activation_mgr.as_ref().unwrap();
            activation_mgr.activate(token, surface);
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<ZxdgDecorationManagerV1, ()> for BaseLoop<T> { ignore!(ZxdgDecorationManagerV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<ZxdgToplevelDecorationV1, WindowId> for BaseLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<WpFractionalScaleManagerV1, ()> for BaseLoop<T> { ignore!(WpFractionalScaleManagerV1, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WlSeat, ()> for BaseLoop<T> {
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
                if let Some(ref wp_cursor_shape_mgr) = evl.wl.cursor_shape_mgr {
                    let wl_shape_device = wp_cursor_shape_mgr.get_pointer(&wl_pointer, qh, ());
                    evl.wl.shape_device = Some(wl_shape_device);
                }
                evl.wl.pointer = Some(wl_pointer);
            }
        }
    }
}

impl<T: 'static + Send> wayland_client::Dispatch<XdgWmBase, ()> for BaseLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgSurface, Arc<Mutex<WindowShared>>> for BaseLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgToplevel, Arc<Mutex<WindowShared>>> for BaseLoop<T> {
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
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::CloseRequested });
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<XdgPopup, Arc<Mutex<WindowShared>>> for BaseLoop<T> {
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
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::CloseRequested });
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<ZwlrLayerShellV1, ()> for BaseLoop<T> {
    ignore!(ZwlrLayerShellV1, ());
}

impl<T: 'static + Send> wayland_client::Dispatch<ZwlrLayerSurfaceV1, Arc<Mutex<WindowShared>>> for BaseLoop<T> {
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
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::CloseRequested });
        }
    
    }
}

fn process_configure<T: 'static + Send>(evl: &mut BaseLoop<T>, guard: MutexGuard<WindowShared>, width: u32, height: u32) {

    // update the window's viewport destination
    if let Some(ref frac_scale_data) = guard.frac_scale_data {
        frac_scale_data.viewport.set_destination(width as i32, height as i32);
    };

    // foreward the final configuration state to the user
    evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Resize {
        size: Size { width, height },
        flags: guard.flags
    } });

    if !guard.redraw_requested && !guard.already_redrawing {
        evl.events.push(Event::Window { id: guard.id, event: WindowEvent::RedrawRequested });
    }

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

impl<T: 'static + Send> wayland_client::Dispatch<WlCallback, Arc<Mutex<WindowShared>>> for BaseLoop<T> {
    fn event(
        evl: &mut Self,
        _cb: &WlCallback,
        _event: WlCallbackEvent,
        shared: &Arc<Mutex<WindowShared>>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        let mut guard = shared.lock().unwrap();
        if guard.redraw_requested {
            guard.redraw_requested = false;
            guard.frame_callback_registered = false;
            guard.already_redrawing = true; // prevent another redraw event from getting sent in case a Configure event arrives just after this
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::RedrawRequested });
        }
    }
}

// global events
impl<T: 'static + Send> wayland_client::Dispatch<WlCompositor, ()> for BaseLoop<T> { ignore!(WlCompositor, ()); }

// surface events (like moving onto an output etc.)
impl<T: 'static + Send> wayland_client::Dispatch<WlSurface, ()> for BaseLoop<T> { ignore!(WlSurface, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpFractionalScaleV1, WindowId> for BaseLoop<T> {
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
impl<T: 'static + Send> wayland_client::Dispatch<WlRegion, ()> for BaseLoop<T> { ignore!(WlRegion, ()); }

// input events
impl<T: 'static + Send> wayland_client::Dispatch<WlKeyboard, ()> for BaseLoop<T> {
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
                        Ok(None) => { evl.keyboard_data.keymap_error = Some(EvlError::InvalidKeymap); return },
                        Err(err) => { evl.keyboard_data.keymap_error = Some(EvlError::Io(err));       return }
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
                    Err(..) => { evl.keyboard_data.keymap_error = Some(EvlError::InvalidLocale); return }
                };

                let compose_state = xkb::compose::State::new(&compose_table, xkb::STATE_NO_FLAGS);

                evl.keyboard_data.keymap_specific = Some(KeymapSpecificData {
                    xkb_state, compose_state, pressed_keys
                })
                
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
                    for key in keymap_specific.pressed_keys.keys_down() {
                        process_key_event(evl, key.raw(), Direction::Up, Source::Event);
                    }

                    evl.keyboard_data.has_focus = None;

                    // also clear out selection WlDataOffer

                    evl.offer_data.current_selection = None;
                    
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

fn process_key_event<T: 'static + Send>(evl: &mut BaseLoop<T>, raw_key: u32, dir: Direction, source: Source) {

    let Some(ref mut keymap_specific) = evl.keyboard_data.keymap_specific else { return };

    let surface = evl.keyboard_data.has_focus.as_ref().unwrap();
    let id = get_window_id(&surface);

    let input_mode = evl.keyboard_data.input_modes.get(&id)
        .unwrap_or(&InputMode::SingleKey);

    let xkb_key = xkb::Keycode::new(raw_key + 8); // says the wayland docs

    let repeat = source == Source::KeyRepeat;

    if dir == Direction::Down {
        
        let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
        let modifier = xkb_sym.is_modifier_key(); // if this key is a modifier key

        match input_mode {
            InputMode::SingleKey => {
                let key = translate_xkb_sym(xkb_sym);
                evl.events.push(Event::Window { id, event: WindowEvent::KeyDown { key, repeat } });
            },
            InputMode::Text => {
                keymap_specific.compose_state.feed(xkb_sym);
                match keymap_specific.compose_state.status() {
                    xkb::Status::Nothing => {
                        if let Some(chr) = xkb_sym.key_char() {
                            evl.events.push(Event::Window { id, event: WindowEvent::TextInput { chr } })
                        } else {
                            let key = translate_xkb_sym(xkb_sym);
                            evl.events.push(Event::Window { id, event: WindowEvent::KeyDown { key, repeat } });
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
                    xkb::Status::Cancelled => {},
                }
            }
        }

        if !modifier {

            // only re-arm if this was NOT called from a repeated key event
            if source == Source::Event {

                evl.keyboard_data.repeat_key = raw_key;

                // arm key-repeat timer with the correct delay and repeat rate
                evl.keyboard_data.repeat_timer.set_interval_at(
                    Instant::now() + evl.keyboard_data.repeat_delay,
                    evl.keyboard_data.repeat_rate
                );
                
                // update the key state
                keymap_specific.pressed_keys.key_down(xkb_key);
            }
        }

    } else {

        // unarm key-repeat timer
        evl.keyboard_data.repeat_timer.set_after(Duration::MAX);

        // update the key state
        keymap_specific.pressed_keys.key_up(xkb_key);

        if let InputMode::SingleKey = input_mode {
            let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
            let key = translate_xkb_sym(xkb_sym);
            evl.events.push(Event::Window { id, event: WindowEvent::KeyUp { key } });
        }

    };

}

impl<T: 'static + Send> wayland_client::Dispatch<WlPointer, ()> for BaseLoop<T> {
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

                evl.mouse_data.has_focus = Some(surface);
                evl.mouse_data.x = surface_x;
                evl.mouse_data.y = surface_y;

                evl.events.push(Event::Window { id, event:
                    WindowEvent::MouseMotion { x: surface_x, y: surface_y }
                });

                // set the apropriate per-window pointer style
                // wayland by default only supports client-wide pointer styling

                evl.cursor_data.last_enter_serial = serial;

                process_new_cursor_style(evl, id);

             },

             WlPointerEvent::Leave { .. } => {
                evl.mouse_data.has_focus = None;
             },

             WlPointerEvent::Motion { surface_x, surface_y, .. } => {

                evl.mouse_data.x = surface_x;
                evl.mouse_data.y = surface_x;

                let surface = evl.mouse_data.has_focus.as_ref().unwrap();
                let id = get_window_id(&surface);

                evl.events.push(Event::Window {
                    id,
                    event: WindowEvent::MouseMotion { x: surface_x, y: surface_y }
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
                    WEnum::Value(..) => return,
                    WEnum::Unknown(..) => return
                };

                let surface = evl.mouse_data.has_focus.as_ref().unwrap();
                let id = get_window_id(&surface);

                evl.events.push(Event::Window {
                    id,
                    event: WindowEvent::MouseScroll { axis, value }
                });
                
            },

            _ => ()
            
        }
        
    }
}

fn process_new_cursor_style<T: 'static + Send>(evl: &mut BaseLoop<T>, id: WindowId) {

    let style = evl.cursor_data.styles.get(&id)
        .unwrap_or(&CursorStyle::Predefined { shape: CursorShape::Default });

    if let Some(ref wl_pointer) = evl.wl.pointer {

        let serial = evl.cursor_data.last_enter_serial;

        match style {
            CursorStyle::Hidden => {
                wl_pointer.set_cursor(serial, None, 0, 0);
            },
            CursorStyle::Custom { icon, hotspot } => {
                wl_pointer.set_cursor(
                    serial, Some(&icon.platform.wl_surface),
                    hotspot.x as i32, hotspot.y as i32
                )
            },
            CursorStyle::Predefined { shape } => {
                let wl_shape = shape.to_wl();
                if let Some(ref wp_shape_device) = evl.wl.shape_device {
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
    Connect(wayland_client::ConnectError),
    Wayland(wayland_client::backend::WaylandError),
    WaylandGlobals(wayland_client::globals::GlobalError),
    BindGlobals(BindError),
    Dispatch(wayland_client::DispatchError),
    Egl(egl::Error),
    NoDisplay,
    WaylandEgl(wayland_egl::Error),
    Io(io::Error),
    InvalidKeymap,
    InvalidLocale,
    Unsupported,
    EglUnsupported,
}

impl fmt::Display for EvlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "windowing error: {}", match self {
            Self::Connect(value)    => value.to_string(),
            Self::Wayland(value)    => value.to_string(),
            Self::WaylandGlobals(value) => value.to_string(),
            Self::BindGlobals(value) => value.to_string(),
            Self::Dispatch(value)   => value.to_string(),
            Self::Egl(value)        => value.to_string(),
            Self::NoDisplay         => "cannot get egl display".to_string(),
            Self::WaylandEgl(value) => value.to_string(),
            Self::Io(value)         => value.to_string(),
            Self::InvalidKeymap     => "invalid/unknown keymap".to_string(),
            Self::InvalidLocale     => "invalid/unknown locale".to_string(),
            Self::Unsupported       => "required wayland features not present".to_string(),
            Self::EglUnsupported    => "required egl features not present".to_string(),
        })
    }
}

impl StdError for EvlError {}

#[derive(Debug, Clone, Copy)]
pub struct Unsupported(&'static str);

impl fmt::Display for Unsupported {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "required wayland features not present: {}", self.0)
    }
}

impl StdError for Unsupported {}

impl From<nix::errno::Errno> for EvlError {
    fn from(value: nix::errno::Errno) -> Self {
        Self::Io(value.into())
    }
}

impl From<wayland_client::ConnectError> for EvlError {
    fn from(value: wayland_client::ConnectError) -> Self {
        Self::Connect(value)
    }
}

impl From<wayland_client::globals::GlobalError> for EvlError {
    fn from(value: wayland_client::globals::GlobalError) -> Self {
        Self::WaylandGlobals(value)
    }
}

impl From<wayland_client::backend::WaylandError> for EvlError {
    fn from(value: wayland_client::backend::WaylandError) -> Self {
        Self::Wayland(value)
    }
}

impl From<BindError> for EvlError {
    fn from(value: BindError) -> Self {
        Self::BindGlobals(value)
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

