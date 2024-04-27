
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
    sys::{timerfd::{TimerFd, ClockId, TimerFlags, TimerSetTimeFlags, Expiration}, eventfd::{EventFd, EfdFlags}, signalfd::{SignalFd, SfdFlags, SigSet}, signal::Signal},
    poll::{PollFd, PollFlags, poll, PollTimeout}, fcntl::{self, OFlag}, unistd::pipe2
};

use bitflags::bitflags;

use std::{mem, fmt, ffi::c_void as void, error::Error as StdError, sync::{mpsc::{Sender, Receiver, channel, SendError}, Arc, Mutex, MutexGuard}, io::{self, Write}, os::fd::{AsFd, FromRawFd, AsRawFd}, time::Duration, env, ops, collections::{HashSet, HashMap}, fs};

pub struct EventLoop<T: 'static + Send = ()> {
    appid: String,
    running: bool,
    con: wayland_client::Connection,
    qh: QueueHandle<Self>,
    queue: Option<EventQueue<Self>>,
    wl: WaylandState,
    events: Vec<Event<T>>, // used to push events from inside the dispatch impl
    quit_signalfd: SignalFd, // listens to sigterm and emits a quit event
    proxy_data: EvlProxyData<T>,
    mouse_data: MouseData,
    keyboard_data: KeyboardData,
    offer_data: OfferData, // drag-and-drop / selection data
    cursor_data: CursorData,
    monitor_list: HashSet<MonitorId>, // used to see which interface names belong to wl_outputs, vec is efficient here
    last_serial: u32,
}

struct CursorData {
    styles: HashMap<WindowId, CursorStyle>, // per-window cursor style
    last_enter_serial: u32, // last mouse enter serial
}

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
    repeat_timer: TimerFd,
    repeat_key: u32, // raw key
    repeat_rate: Duration,
    repeat_delay: Duration,
    input_modes: HashMap<WindowId, InputMode>, // per-window input mode
}

struct KeymapSpecificData {
    xkb_state: xkb::State,
    compose_state: xkb::compose::State,
    pressed_keys: PressedKeys,
}

struct MouseData {
    has_focus: Option<WlSurface>,
    x: f64,
    y: f64
}

struct PressedKeys {
    min: xkb::Keycode,
    keys: Vec<u8>, // a bit map containing every possible key
}

impl PressedKeys {
    pub fn new(keymap: &xkb::Keymap) -> Self {
        let min = keymap.min_keycode();
        let max = keymap.max_keycode();
        let len = max.raw() - min.raw();
        let mut keys = Vec::new();
        keys.resize(len as usize, 0);
        Self {
            min,
            keys
        }
    }
    pub fn key_down(&mut self, key: xkb::Keycode) {
        let (byte, bit) = self.get_indicies(key);
        self.keys[byte] |= 1 << bit
    }
    pub fn key_up(&mut self, key: xkb::Keycode) {
        let (byte, bit) = self.get_indicies(key);
        self.keys[byte] &= !(1 << bit)
    }
    pub fn keys_down(&self) -> Vec<xkb::Keycode> {
        let mut down = Vec::new(); // we can't return anything that borrows self
        for (byte, val) in self.keys.iter().enumerate() {
            for bit in 0..7 {
                if (*val & (1 << bit)) > 0 { // > 0 is important here
                    let key = byte as u32 + bit as u32 + self.min.raw();
                    down.push(xkb::Keycode::from(key));
                }
            }
        }
        down
    }
    fn get_indicies(&self, key: xkb::Keycode) -> (usize, usize) {
        let index = key.raw() - self.min.raw();
        (index as usize / 8, index as usize % 8)
    }
}

struct EvlProxyData<T> {
    pub eventfd: Arc<EventFd>, // Arc is just easier then duping it
    pub sender: Sender<Event<T>>,
    pub receiver: Receiver<Event<T>>
}

impl<T: 'static + Send> EventLoop<T> {

    pub fn new(application: &str) -> Result<Self, EvlError> {

        let con = wayland_client::Connection::connect_to_env()?;

        let mut monitor_list = HashSet::with_capacity(1);

        let (globals, queue) = registry_queue_init::<Self>(&con)?;
        let qh = queue.handle();
        let wayland = WaylandState::from_globals(&mut monitor_list, globals, &qh)?;

        let quit_signalfd = {
            let mut mask = SigSet::empty();
            mask.add(Signal::SIGTERM); // graceful termination, system shutdown
            mask.add(Signal::SIGINT); // ctrl-c
            mask.thread_block()?;
            SignalFd::with_flags(&mask, SfdFlags::SFD_NONBLOCK)?
        };

        let proxy_data = {
            let (sender, receiver) = channel();
            let eventfd = Arc::new(EventFd::from_flags(EfdFlags::EFD_NONBLOCK)?);
            EvlProxyData { eventfd, sender, receiver }
        };

        let repeat_timer = TimerFd::new(ClockId::CLOCK_MONOTONIC, TimerFlags::TFD_NONBLOCK)?;

        let xkb_context = xkb::Context::new(xkb::CONTEXT_NO_FLAGS);

        Ok(EventLoop {
            appid: application.to_string(),
            running: true,
            con,
            queue: Some(queue),
            qh,
            wl: wayland,
            events: Vec::with_capacity(4),
            quit_signalfd,
            proxy_data,
            mouse_data: MouseData {
                has_focus: None,
                x: 0.0, y: 0.0
            },
            keyboard_data: KeyboardData {
                has_focus: None,
                xkb_context,
                keymap_specific: None,
                keymap_error: None,
                repeat_timer,
                repeat_key: 0,
                repeat_rate: Duration::from_millis(60),
                repeat_delay: Duration::from_millis(450), // sensible defaults if no RepeatInfo event is provided
                input_modes: HashMap::new(),
            },
            offer_data: OfferData {
                has_offer: None,
                current_offer: None,
                current_selection: None,
                x: 0.0, y: 0.0,
                dnd_active: false,
                dnd_icon: None,
            },
            cursor_data: CursorData {
                styles: HashMap::new(),
                last_enter_serial: 0,
            },
            monitor_list,
            last_serial: 0, // we don't use an option here since an invalid serial may be a common case and is not treated as an error
        })
        
    }

    pub fn run<F: for<'a> FnMut(&'a mut Self, Event<T>)>(mut self, mut cb: F) -> Result<(), EvlError> {

        let Some(mut queue) = self.queue.take() else { unreachable!() };

        self.events.push(Event::Resume);

        let mut events = Vec::with_capacity(4);

        // main event loop
        loop {

            queue.dispatch_pending(&mut self)?;

            // we `swap` the events so we can pass the event loop into the closure
            mem::swap(&mut self.events, &mut events);

            // foreward the events to the callback
            for event in events.drain(..) {
                cb(&mut self, event)
            }
            
            if !self.running { break } // stop if `exit` was called
            let timeout = if self.events.is_empty() { PollTimeout::NONE } else { PollTimeout::ZERO }; // maybe cb generated events

            self.con.flush()?; // nah, I forgot this and had to debug 10+ hours... fuckkk me

            // prepare to wait for new events
            let Some(guard) = queue.prepare_read() else { continue };

            let queue_fd = guard.connection_fd();
            let channel_fd = self.proxy_data.eventfd.as_fd();
            let timer_fd = self.keyboard_data.repeat_timer.as_fd();
            let quit_signal_fd = self.quit_signalfd.as_fd();

            let mut fds = [
                PollFd::new(queue_fd,       PollFlags::POLLIN | PollFlags::POLLERR),
                PollFd::new(channel_fd,     PollFlags::POLLIN),
                PollFd::new(timer_fd,       PollFlags::POLLIN),
                PollFd::new(quit_signal_fd, PollFlags::POLLIN),
            ];

            // wait for new events
            retry_on_intr(|| poll(&mut fds, timeout))?;

            let wl_events          = fds[0].revents().unwrap_or(PollFlags::empty());
            let channel_events     = fds[1].revents().unwrap_or(PollFlags::empty());
            let timer_events       = fds[2].revents().unwrap_or(PollFlags::empty());
            let quit_signal_events = fds[3].revents().unwrap_or(PollFlags::empty());

            // new events from the wayland connection
            if wl_events.contains(PollFlags::POLLIN) {
                ignore_wouldblock(guard.read())?;
                if let Some(error) = self.keyboard_data.keymap_error { return Err(error) };
            } else if wl_events.contains(PollFlags::POLLERR) {
                return Err(EvlError::Io(io::Error::last_os_error()));
            }

            // new user events
            if channel_events.contains(PollFlags::POLLIN) {
                self.proxy_data.eventfd.read()?; // make sure to read
                while let Ok(event) = self.proxy_data.receiver.try_recv() {
                    self.events.push(event)
                }
            }

            // new key-repeat timer events
            if timer_events.contains(PollFlags::POLLIN) {
                self.keyboard_data.repeat_timer.wait()?; // make sure to read, doesn't actually wait
                // emit the sysnthetic key-repeat event
                let key = self.keyboard_data.repeat_key;
                process_key_event(&mut self, key, Direction::Down, Source::KeyRepeat);
            }

            // signal that a quit event should be emitted for
            if quit_signal_events.contains(PollFlags::POLLIN) {
                for signal in &mut self.quit_signalfd {
                    let reason = if signal.ssi_signo == Signal::SIGTERM as u32 { QuitReason::System }
                            else if signal.ssi_signo == Signal::SIGINT  as u32 { QuitReason::Ctrlc }
                            else { unreachable!() };
                    self.events.push(Event::QuitRequested { reason });
                }
            }

        }

        Ok(())
        
    }

    pub fn new_proxy(&self) -> EvlProxy<T> {

        EvlProxy {
            sender: self.proxy_data.sender.clone(),
            eventfd: Arc::clone(&self.proxy_data.eventfd),
        }
        
    }

    pub fn suspend(&mut self) {
        self.events.push(Event::Suspend);
    }

    pub fn resume(&mut self) {
        self.events.push(Event::Resume);
    }

    pub fn request_quit(&mut self) {
        self.events.push(Event::QuitRequested { reason: QuitReason::User });
    }

    pub fn exit(&mut self) {
        self.running = false
    }

    #[track_caller]
    pub fn get_clip_board(&mut self) -> Option<DataOffer> {

        let wl_data_offer = self.offer_data.current_selection.clone()?;
        let data = wl_data_offer.data::<Mutex<DataKinds>>().unwrap();
        let kinds = *data.lock().unwrap(); // copy the bitflags

        Some(DataOffer {
            wl_data_offer,
            con: self.con.clone(), // should be pretty cheap
            kinds,
            dnd: false,
        })

    }

    pub fn set_clip_board(&mut self, ds: Option<&DataSource>) {

        self.wl.data_device.set_selection(
            ds.map(|val| &val.wl_data_source),
            self.last_serial
        );

    }

}

fn retry_on_intr(mut func: impl FnMut() -> Result<i32, nix::errno::Errno>) -> Result<i32, nix::errno::Errno> {
    loop {
        match func() {
            Ok(val) => return Ok(val),
            Err(nix::errno::Errno::EINTR) => continue,
            Err(other) => return Err(other)
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

pub type MonitorId = u32;

fn get_monitor_id(output: &WlOutput) -> MonitorId {
    output.id().protocol_id()
}

#[derive(Debug, Default, Clone)]
pub struct MonitorInfo {
    pub name: String,
    pub description: String,
    pub size: Size,
    /// Refresh rate in mHz. You can use the [`fps`](Monitor::fps) method to convert it to Hz.
    pub refresh: u32,
}

impl MonitorInfo {
    /// Trimmed conversion.
    pub fn fps(&self) -> u32 {
        self.refresh / 1000
    }    
}

pub struct Monitor {
    /// Information about the monitor.
    pub info: MonitorInfo,
    wl_output: WlOutput,
}

impl fmt::Debug for Monitor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Monitor {{ info: {:?}, ... }}", self.info)
    }
}

pub struct EvlProxy<T> {
    sender: Sender<Event<T>>,
    eventfd: Arc<EventFd>,
}

impl<T> EvlProxy<T> {

    pub fn send(&self, event: Event<T>) -> Result<(), SendError<Event<T>>> {

        self.sender.send(event)?;

        // the fd will be alive since the `send` would have failed if the EventLoop was destroyed
        self.eventfd.write(1).unwrap();

        Ok(())
    }

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
    pub fn from_globals<T: 'static + Send>(monitor_data: &mut HashSet<MonitorId>, globals: GlobalList, qh: &QueueHandle<EventLoop<T>>) -> Result<Self, BindError> {

        // bind the primary monitor we already retreived
        globals.contents().with_list(|list| for val in list {
            if &val.interface == "wl_output" { process_new_output(monitor_data, globals.registry(), val.name, qh); }
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

pub trait Drawable {
    // pub 
}

pub struct PixMap {
    
}

pub type WindowId = u32;

fn get_window_id(surface: &WlSurface) -> WindowId {
    surface.id().protocol_id()
}

pub struct BaseWindow<T: 'static + Send> {
    // our data
    id: WindowId, // also found in `shared`
    shared: Arc<Mutex<WindowShared>>, // needs to be accessed by some callbacks
    // wayland state
    qh: QueueHandle<EventLoop<T>>,
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

        let surface = evl.wl.compositor.create_surface(&evl.qh, ());
        let id = get_window_id(&surface);

        // fractional scaling, if present
        let frac_scale_data = evl.wl.frac_scale_mgrs.as_ref().map(|val| {
            let viewport = val.viewport_mgr.get_viewport(&surface, &evl.qh, ());
            let frac_scale = val.frac_scaling_mgr.get_fractional_scale(&surface, &evl.qh, id);
            FracScaleData { viewport, frac_scale }
        });

        let shared = Arc::new(Mutex::new(WindowShared {
            id,
            new_width:  size.width,
            new_height: size.height,
            flags: ConfigureFlags::default(),
            frame_callback_registered: false,
            redraw_requested: false,
            // need to access some wayland objects
            frac_scale_data,
        }));

        Self {
            id,
            shared,
            qh: evl.qh.clone(),
            compositor: evl.wl.compositor.clone(),
            wl_surface: surface,
        }
        
    }

    pub fn id(&self) -> WindowId {
        self.id
    }

    #[track_caller]
    pub fn request_redraw(&self, token: PresentToken) {
        assert!(self.id == token.id, "token for another window, see PresentToken docs");
        let mut guard = self.shared.lock().unwrap();
        guard.redraw_requested = true;
    }

    pub fn pre_present_notify(&self) -> PresentToken {
        // note: it seems you have request the frame callback before swapping buffers
        //       otherwise the callback will never fire because the compositor thinks the content didn't change
        let mut guard = self.shared.lock().unwrap();
        if !guard.frame_callback_registered {
            guard.frame_callback_registered = true;
            self.wl_surface.frame(&self.qh, Arc::clone(&self.shared));
            self.wl_surface.commit();
        }
        PresentToken { id: self.id }
    }

    pub fn transparency(&self, value: bool) {
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
    frame_callback_registered: bool,
    redraw_requested: bool,
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

        // xdg-top-level role (+ init decoration manager)
        let xdg_surface = evl.wl.wm.get_xdg_surface(&base.wl_surface, &evl.qh, Arc::clone(&base.shared));
        let xdg_toplevel = xdg_surface.get_toplevel(&evl.qh, Arc::clone(&base.shared));

        let xdg_decoration = evl.wl.decoration_mgr.as_ref()
            .map(|val| val.get_toplevel_decoration(&xdg_toplevel, &evl.qh, base.id));

        xdg_decoration.as_ref().map(|val| val.set_mode(ZxdgDecorationMode::ServerSide));
        xdg_toplevel.set_app_id(evl.appid.clone());

        base.wl_surface.commit();

        Self {
            base,
            xdg_surface,
            xdg_toplevel,
            xdg_decoration,
        }
        
    }

    pub fn close(self) {
        drop(self);
    }

    pub fn input_mode(&self, evl: &mut EventLoop<T>, mode: InputMode) {
        evl.keyboard_data.input_modes.insert(self.id, mode);
    }

    pub fn decorations(&mut self, value: bool) {
        let mode = if value { ZxdgDecorationMode::ServerSide } else { ZxdgDecorationMode::ClientSide };
        self.xdg_decoration.as_ref().map(|val| val.set_mode(mode));
    }

    pub fn title<S: Into<String>>(&self, text: S) {
        self.xdg_toplevel.set_title(text.into());
    }

    pub fn maximized(&self, value: bool) {
        if value {
            self.xdg_toplevel.set_maximized();
        } else {
            self.xdg_toplevel.unset_maximized();
        };
        self.wl_surface.commit();
    }

    pub fn fullscreen(&self, value: bool, monitor: Option<&Monitor>) {
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
        self.wl_surface.commit();
    }

    pub fn max_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_max_size(size.width as i32, size.height as i32);
        self.wl_surface.commit();
    }

    pub fn force_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_max_size(size.width as i32, size.height as i32);
        self.xdg_toplevel.set_min_size(size.width as i32, size.height as i32);
        self.wl_surface.commit();
    }

    pub fn request_user_attention(&self, evl: &EventLoop<T>, urgency: Urgency) {

        if let Urgency::Info = urgency {
            // we don't wanna switch focus, but on wayland just showing a
            // blinking icon is not possible
            return
        }

        if let Some(ref activation_mgr) = evl.wl.activation_mgr {

            let token = activation_mgr.get_activation_token(&evl.qh, self.base.wl_surface.clone());

            token.set_app_id(evl.appid.clone());
            token.set_serial(evl.last_serial, &evl.wl.seat);

            if let Some(ref surface) = evl.keyboard_data.has_focus {
                token.set_surface(surface);
            }

            token.commit();

        }

    }

    /// You should only start a drag-and-drop when the left mouse button is held down
    /// *and* the user then moves the mouse.
    /// Otherwise the request may be denied or visually broken.
    pub fn start_drag_and_drop(&self, evl: &mut EventLoop<T>, icon: CustomIcon, ds: &DataSource) {

        evl.wl.data_device.start_drag(
            Some(&ds.wl_data_source),
            &self.base.wl_surface,
            Some(&icon.wl_surface),
            evl.last_serial
        );

        evl.offer_data.dnd_active = true;
        evl.offer_data.dnd_icon = Some(icon);

    }

    pub fn set_cursor(&self, evl: &mut EventLoop<T>, style: CursorStyle) {

        // note: the CustomIcon will also be kept alive by the styles as long as needed
        evl.cursor_data.styles.insert(self.id, style);

        // immediatly apply the style
        process_new_cursor_style(evl, self.id);

    }

}

#[derive(Debug)]
pub enum CursorStyle {
    Hidden,
    Custom { icon: CustomIcon, hotspot: Pos },
    Predefined { shape: CursorShape }
}

impl Default for CursorStyle {
    fn default() -> Self {
        Self::Predefined { shape: CursorShape::default() }
    }
}

#[derive(Debug, Default)]
pub enum CursorShape {
    #[default]
    Default,
    ContextMenu,
    Help,
    Pointer,
    Progress,
    Wait,
    Cell,
    Crosshair,
    Text,
    VerticalText,
    Alias,
    Copy,
    Move,
    NoDrop,
    NotAllowed,
    Grab,
    Grabbing,
    EResize,
    NResize,
    NeResize,
    NwResize,
    SResize,
    SeResize,
    SwResize,
    WResize,
    EwResize,
    NsResize,
    NeswResize,
    NwseResize,
    ColResize,
    RowResize,
    AllScroll,
    ZoomIn,
    ZoomOut,
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

#[derive(Debug, Default)]
pub enum Urgency {
    /// Should display a hint. Might do nothing.
    #[default]
    Info,
    /// Will likely switch window focus or display an urgent hint.
    Switch,
}

bitflags! {
    #[derive(Debug, Clone, Copy,PartialEq, Eq)]
    pub struct DataKinds: u64 {
        const TEXT   = 1;
        const XML    = 1 << 1;
        const HTML   = 1 << 2;
        const ZIP    = 1 << 3;
        const JSON   = 1 << 4;
        const JPEG   = 1 << 5;
        const PNG    = 1 << 6;
        const OTHER  = 1 << 7;
    }
}

impl Default for DataKinds {
    fn default() -> Self {
        Self::OTHER
    }
}

impl DataKinds {
    pub fn to_mime_type(&self) -> &'static str {
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
    pub fn from_mime_type(mime_type: &str) -> Option<Self> {
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

    // TODO: write some better result types in general, eg also EvlResult
    /// A `DataOffer` can be read multiple times. Also using different `DataKinds`.
    pub fn receive(&self, kind: DataKinds, nonblocking: bool) -> Result<DataReader, EvlError> {

        let (reader, writer) = pipe2(OFlag::empty())?;

        // receive the data
        let mime_type = kind.to_mime_type();
        self.wl_data_offer.receive(mime_type.to_string(), writer.as_fd());

        self.con.flush()?; // <--- this is important, so we can immediatly read without deadlocking

        // set only the writing end to be nonblocking, if enabled
        if nonblocking {
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

#[derive(Debug, Clone, Copy, Default)]
pub enum IoMode {
    #[default]
    Blocking,
    Nonblocking,
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

        assert!(!offers.is_empty(), "must offer at least one DataKind");

        let wl_data_source = evl.wl.data_device_mgr.create_data_source(&evl.qh, mode);

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

pub enum IconFormat {
    Argb8,
}

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

        assert!(
            data.len() as u32 == size.width * size.height * bytes_per_pixel as u32,
            "length of data doesn't match specified dimensions and format"
        );

        assert!(
            data.len() != 0,
            "length of data must be greater then 0"
        );

        let wl_shm_pool = evl.wl.shm.create_pool(file.as_fd(), len as i32, &evl.qh, ());
        let wl_buffer = wl_shm_pool.create_buffer(
            0, size.width as i32, size.height as i32,
            size.width as i32 * bytes_per_pixel, wl_format,
            &evl.qh, ()
        );

        let wl_surface = evl.wl.compositor.create_surface(&evl.qh, ());
        wl_surface.attach(Some(&wl_buffer), 0, 0);
        wl_surface.commit();

        Ok(Self {
            _file: file, // just keep the shared tempfile alive, since we ignore WlBuffer Release event
            wl_shm_pool,
            wl_buffer,
            wl_surface,
        })
        
    }

}

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

        // xdg-popup role
        let xdg_surface = evl.wl.wm.get_xdg_surface(&base.wl_surface, &evl.qh, Arc::clone(&base.shared));
        let xdg_positioner = evl.wl.wm.create_positioner(&evl.qh, ());

        let parent_guard = parent.shared.lock().unwrap();
        xdg_positioner.set_size(size.width as i32, size.height as i32);
        xdg_positioner.set_anchor_rect(0, 0, parent_guard.new_width as i32, parent_guard.new_height as i32);
        drop(parent_guard);

        let xdg_popup = xdg_surface.get_popup(Some(&parent.xdg_surface), &xdg_positioner, &evl.qh, Arc::clone(&base.shared));

        base.wl_surface.commit();

        Self {
            base,
            xdg_surface,
            xdg_popup,
        }
        
    }

    pub fn close(self) {
        drop(self);
    }

}

/// The layers are ordered from bottom most to top most.
pub enum WindowLayer {
    // Below everything.
    // eg. desktop widgets, file icons
    Background,
    // Always below normal programs.
    Bottom,
    // Always above normal programs.
    // eg. fullscreen windows, windows task manager
    Top,
    // Above everything.
    // eg. key-press display, fps counter, notifications
    Overlay
}

pub enum WindowAnchor {
    Top,
    Bottom,
    Left,
    Right
}

/// Keyboard window interactivity.
pub enum KbInteractivity {
    /// Window can't have keyboard focus.
    None,
    /// Top/Overlay windows will grab keyboard focus
    /// Can be buggy. I advise against using it.
    Exclusive
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

        let wl_layer = match layer {
            WindowLayer::Background => Layer::Background,
            WindowLayer::Bottom     => Layer::Bottom,
            WindowLayer::Top        => Layer::Top,
            WindowLayer::Overlay    => Layer::Overlay,
        };

        let wl_output = monitor.map(|val| &val.wl_output);

        // creating this kind of window requires some wayland extensions 
        let layer_shell_mgr = evl.wl.layer_shell_mgr.as_ref().ok_or(
            Unsupported(ZwlrLayerShellV1::interface().name)
        )?;

        // layer-shell role
        let zwlr_surface = layer_shell_mgr.get_layer_surface(
            &base.wl_surface, wl_output, wl_layer, evl.appid.clone(),
            &evl.qh, Arc::clone(&base.shared)
        );

        zwlr_surface.set_size(size.width, size.height);

        base.wl_surface.commit();

        Ok(Self {
            base,
            zwlr_surface,
        })
        
    }

    pub fn close(self) {
        drop(self);
    }

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

#[derive(Debug, Default, Clone, Copy)]
pub struct Size {
    pub width: u32,
    pub height: u32
}

impl Size {
    pub const INFINITE: Size = Size { width: u32::MAX, height: u32::MAX };
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Pos {
    pub x: u32,
    pub y: u32
}

impl Pos {
    pub const ORIGIN: Pos = Pos { x: 0, y: 0 };
}

/// A rectangular region on a surface.
///
/// The origin is in the top left of the surface.
/// Normally EGL specifies the origin in the bottom left of the surface but this is **NOT**
/// what this library does. We recalculate the origin for consistency with windowing systems.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub w: i32,
    pub h: i32,
}

impl Rect {
    pub const INFINITE: Self = Self::new(0, 0, i32::MAX, i32::MAX);
    pub const fn new(x: i32, y: i32, w: i32, h: i32) -> Self {
        Self { x, y, w, h}
    }
}

#[derive(Clone, Copy)]
pub struct PresentToken { id: WindowId }

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

        let wl_display = evh.con.display().id().as_ptr();
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

/// Egl context and surface.
/// Can render to Windows. You should create one of these per window and bind
/// the one you want to render to.
/// To render to a pixel buffer see [`EglPixelBuffer`].
pub struct EglBase {
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
    pub fn new<T: 'static + Send, W: ops::Deref<Target = BaseWindow<T>>>(instance: &Arc<EglInstance>, window: &W, size: Size) -> Result<Self, EvlError> {

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
            window.wl_surface.id(),
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
            id: window.id,
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

        assert!(self.id == token.id, "present token for another window");

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

#[derive(Debug)]
pub enum QuitReason {
    /// Quit requested via `request_quit`.
    User,
    /// SIGTERM received.
    System,
    /// SIGINT received.
    Ctrlc,
}

pub enum Event<T> {
    /// Your own events. See [`EvlProxy`].
    User(T),
    /// Your app was resumed from the background or started and should show it's view.
    Resume,
    /// Your app's view should be destroyed but it can keep running in the background.
    Suspend,
    /// Your app should quit.
    QuitRequested { reason: QuitReason },
    /// A monitor was discovered or updated.
    MonitorUpdate { id: MonitorId, state: Monitor },
    /// A monitor was removed.
    MonitorRemove { id: MonitorId },
    // /// A  mouse, keyboard or touch device was discovered or removed.
    // /// If you never get this event, there is no keyboard or mouse attached.
    // Device { exists: bool, device: () }, // TODO: implement this (wl_seat) also implement controller support through this
    // Device { exists: bool, device: () }, // TODO: implement this (wl_seat)
    /// An event that belongs to a specific window. (eg. focus change, mouse movement)
    Window { id: WindowId, event: WindowEvent },
    /// Requests you sending data to another client.
    DataSource { id: DataSourceId, event: DataSourceEvent },
}

#[derive(Debug)]
pub enum WindowEvent {
    CloseRequested,
    RedrawRequested,
    Resize { size: Size, flags: ConfigureFlags },
    Rescale { scale: f64 },
    Decorations { active: bool },
    Enter,
    Leave,
    MouseMotion { x: f64, y: f64 },
    MouseDown { x: f64, y: f64, button: MouseButton },
    MouseUp { x: f64, y: f64, button: MouseButton },
    MouseScroll { axis: ScrollAxis, value: f64 },
    KeyDown { key: Key, repeat: bool },
    KeyUp { key: Key },
    TextCompose { chr: char },
    TextInput { chr: char },
    /// A Drag-and-drop event.
    Dnd { event: DndEvent, internal: bool },
}

#[derive(Debug)]
/// Events for a [`DataSource`].
pub enum DataSourceEvent {
    /// Data of the specific [`DataKind`] you advertised was requested to be transferred.
    /// Could be send multiple times.
    Send { kind: DataKinds, writer: DataWriter },
    /// Data was successfully transfarred.
    /// Could be send multiple times, one per `Send`.
    Success,
    /// Your data source is no longer used and can be dropped.
    /// *This event may never be sent in rare cases.*
    Close,
}

#[derive(Debug)]
pub enum DndEvent {
    Motion { x: f64, y: f64, handle: DndHandle },
    Drop { x: f64, y: f64, offer: DataOffer },
    Cancel,
}

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

#[derive(Debug, Clone, Copy)]
pub enum InputMode {
    SingleKey,
    Text,
}

#[derive(Debug, Clone, Copy)]
pub enum MouseButton {
    Left,
    Right,
    Middle,
    X1,
    X2,
    Unknown(u32),
}

#[derive(Debug, Clone, Copy)]
pub enum Key {
    Escape,
    Tab,
    CapsLock,
    Shift,
    Control,
    Alt,
    AltGr,
    /// Windows key.
    Super, // windows key
    /// Application menu key.
    AppMenu,
    Return,
    Backspace,
    Space,
    ArrowUp,
    ArrowDown,
    ArrowLeft,
    ArrowRight,
    F(u32), // f1, f2, f3, etc.
    Char(char), // a-z, A-Z, 1-9, + special chars
    DeadChar(char),
    Unknown(u32),
}

impl Key {
    pub fn modifier(&self) -> bool {
        matches!(
            self,
            Self::Shift | Self::Control | Self::CapsLock |
            Self::Alt | Self::AltGr | Self::Super
        )
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

impl<T: 'static + Send> wayland_client::Dispatch<WlRegistry, GlobalListContents> for EventLoop<T> {
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

fn process_new_output<T: 'static + Send>(monitor_list: &mut HashSet<MonitorId>, registry: &WlRegistry, name: u32, qh: &QueueHandle<EventLoop<T>>) {
    let info = MonitorInfo::default();
    let output = registry.bind(name, 2, qh, Mutex::new(info)); // first time in my life using Mutex without an Arc
    let id = get_monitor_id(&output);
    monitor_list.insert(id);
}

impl<T: 'static + Send> wayland_client::Dispatch<WlOutput, Mutex<MonitorInfo>> for EventLoop<T> {
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
                evl.events.push(Event::MonitorUpdate { id, state: Monitor {
                    info: guard.clone(),
                    wl_output: wl_output.clone(),
                } });
            },
            _ => (),
        }

    }
}

impl<T: 'static + Send> wayland_client::Dispatch<WlShm, ()> for EventLoop<T> { ignore!(WlShm, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlShmPool, ()> for EventLoop<T> { ignore!(WlShmPool, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlBuffer, ()> for EventLoop<T> { ignore!(WlBuffer, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<XdgPositioner, ()> for EventLoop<T> { ignore!(XdgPositioner, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpViewporter, ()> for EventLoop<T> { ignore!(WpViewporter, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WpViewport, ()> for EventLoop<T> { ignore!(WpViewport, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpCursorShapeManagerV1, ()> for EventLoop<T> { ignore!(WpCursorShapeManagerV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WpCursorShapeDeviceV1, ()> for EventLoop<T> { ignore!(WpCursorShapeDeviceV1, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WlDataDeviceManager, ()> for EventLoop<T> { ignore!(WlDataDeviceManager, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<WlDataDevice, ()> for EventLoop<T> {
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
            let internal = evl.offer_data.dnd_active;

            evl.offer_data.has_offer = Some(surface);
            evl.offer_data.current_offer = wl_data_offer.clone();

            evl.offer_data.x = x;
            evl.offer_data.y = y;

            let advertisor = DndHandle {
                last_serial: evl.last_serial,
                wl_data_offer,
            };

            evl.events.push(Event::Window {
                id,
                event: WindowEvent::Dnd { event: DndEvent::Motion { x, y, handle: advertisor }, internal }
            });

        }

        else if let WlDataDeviceEvent::Motion { x, y, .. } = event {

            evl.offer_data.x = x;
            evl.offer_data.y = y;

            let advertisor = DndHandle {
                last_serial: evl.last_serial,
                wl_data_offer: evl.offer_data.current_offer.clone(),
            };

            let surface = evl.offer_data.has_offer.as_ref().unwrap();
            let internal = evl.offer_data.dnd_active;

            evl.events.push(Event::Window {
                id: get_window_id(surface),
                event: WindowEvent::Dnd { event: DndEvent::Motion { x, y, handle: advertisor }, internal }
            });

        }

        else if let WlDataDeviceEvent::Drop = event {

            if let Some(wl_data_offer) = evl.offer_data.current_offer.take() {

                let x = evl.offer_data.x;
                let y = evl.offer_data.y;

                let data = wl_data_offer.data::<Mutex<DataKinds>>().unwrap();
                let kinds = data.lock().unwrap().clone();

                let data = DataOffer {
                    wl_data_offer,
                    con: evl.con.clone(),
                    kinds,
                    dnd: true,
                };

                let surface = evl.offer_data.has_offer.as_ref().unwrap();
                let internal = evl.offer_data.dnd_active;

                evl.events.push(Event::Window {
                    id: get_window_id(surface),
                    event: WindowEvent::Dnd { event: DndEvent::Drop { x, y, offer: data }, internal },
                });

            }
        }

        else if let WlDataDeviceEvent::Leave = event {

            let surface = evl.offer_data.has_offer.as_ref().unwrap();
            let internal = evl.offer_data.dnd_active;

            evl.events.push(Event::Window {
                id: get_window_id(surface),
                event: WindowEvent::Dnd { event: DndEvent::Cancel, internal },
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

impl<T: 'static + Send> wayland_client::Dispatch<WlDataOffer, Mutex<DataKinds>> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<WlDataSource, IoMode> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgActivationV1, ()> for EventLoop<T> { ignore!(XdgActivationV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<XdgActivationTokenV1, WlSurface> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<ZxdgDecorationManagerV1, ()> for EventLoop<T> { ignore!(ZxdgDecorationManagerV1, ()); }
impl<T: 'static + Send> wayland_client::Dispatch<ZxdgToplevelDecorationV1, WindowId> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<WpFractionalScaleManagerV1, ()> for EventLoop<T> { ignore!(WpFractionalScaleManagerV1, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WlSeat, ()> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgWmBase, ()> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgSurface, Arc<Mutex<WindowShared>>> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgToplevel, Arc<Mutex<WindowShared>>> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgPopup, Arc<Mutex<WindowShared>>> for EventLoop<T> {
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

impl<T: 'static + Send> wayland_client::Dispatch<ZwlrLayerShellV1, ()> for EventLoop<T> {
    ignore!(ZwlrLayerShellV1, ());
}

impl<T: 'static + Send> wayland_client::Dispatch<ZwlrLayerSurfaceV1, Arc<Mutex<WindowShared>>> for EventLoop<T> {
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

fn process_configure<T: 'static + Send>(evl: &mut EventLoop<T>, guard: MutexGuard<WindowShared>, width: u32, height: u32) {

    // update the window's viewport destination
    if let Some(ref frac_scale_data) = guard.frac_scale_data {
        frac_scale_data.viewport.set_destination(width as i32, height as i32);
    };

    // foreward the final configuration state to the user
    evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Resize {
        size: Size { width, height },
        flags: guard.flags
    } });

    if !guard.redraw_requested {
        evl.events.push(Event::Window { id: guard.id, event: WindowEvent::RedrawRequested });
    }

}

#[derive(Debug, Default, Clone, Copy)]
pub struct ConfigureFlags {
    pub fullscreen: bool
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

impl<T: 'static + Send> wayland_client::Dispatch<WlCallback, Arc<Mutex<WindowShared>>> for EventLoop<T> {
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
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::RedrawRequested });
        }
    }
}

// global events
impl<T: 'static + Send> wayland_client::Dispatch<WlCompositor, ()> for EventLoop<T> { ignore!(WlCompositor, ()); }

// surface events (like moving onto an output etc.)
impl<T: 'static + Send> wayland_client::Dispatch<WlSurface, ()> for EventLoop<T> { ignore!(WlSurface, ()); }

impl<T: 'static + Send> wayland_client::Dispatch<WpFractionalScaleV1, WindowId> for EventLoop<T> {
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
impl<T: 'static + Send> wayland_client::Dispatch<WlRegion, ()> for EventLoop<T> { ignore!(WlRegion, ()); }

// input events
impl<T: 'static + Send> wayland_client::Dispatch<WlKeyboard, ()> for EventLoop<T> {
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

fn process_key_event<T: 'static + Send>(evl: &mut EventLoop<T>, raw_key: u32, dir: Direction, source: Source) {

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
            // arm key-repeat timer with the correct delay and repeat rate
            if source == Source::Event { // only re-arm if this was NOT called from a repeated key event

                evl.keyboard_data.repeat_key = raw_key;

                evl.keyboard_data.repeat_timer.set(Expiration::IntervalDelayed(
                    evl.keyboard_data.repeat_delay.into(), evl.keyboard_data.repeat_rate.into()
                ), TimerSetTimeFlags::empty()).unwrap();
                
                // update the key state
                keymap_specific.pressed_keys.key_down(xkb_key);
            }
        }

    } else {

        // unarm key-repeat timer
        evl.keyboard_data.repeat_timer.unset().unwrap();

        // update the key state
        keymap_specific.pressed_keys.key_up(xkb_key);

        if let InputMode::SingleKey = input_mode {
            let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
            let key = translate_xkb_sym(xkb_sym);
            evl.events.push(Event::Window { id, event: WindowEvent::KeyUp { key } });
        }

    };

}

impl<T: 'static + Send> wayland_client::Dispatch<WlPointer, ()> for EventLoop<T> {
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

fn process_new_cursor_style<T: 'static + Send>(evl: &mut EventLoop<T>, id: WindowId) {

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
                    serial, Some(&icon.wl_surface),
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

#[derive(Debug)]
pub enum ScrollAxis {
    Vertical,
    Horizontal
}

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
            Self::WaylandGlobals(value)    => value.to_string(),
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

