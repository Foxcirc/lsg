
use wayland_client::{
    protocol::{
        wl_registry::{WlRegistry, Event as WlRegistryEvent},
        wl_compositor::WlCompositor,
        wl_seat::{WlSeat, Event as WlSeatEvent, Capability as WlSeatCapability},
        wl_surface::{WlSurface, Event as WlSurfaceEvent},
        wl_callback::{WlCallback, Event as WlCallbackEvent},
        wl_keyboard::{WlKeyboard, Event as WlKeyboardEvent, KeyState},
        wl_pointer::{WlPointer, Event as WlPointerEvent, ButtonState, Axis},
        wl_region::WlRegion,
    },
    WEnum, Proxy, QueueHandle, EventQueue, globals::{registry_queue_init, GlobalList, GlobalListContents, BindError}
};

use wayland_protocols::xdg::shell::client::{
    xdg_wm_base::{XdgWmBase, Event as XdgWmBaseEvent},
    xdg_surface::{XdgSurface, Event as XdgSurfaceEvent},
    xdg_toplevel::{XdgToplevel, Event as XdgToplevelEvent, State as XdgToplevelState},
};

use khronos_egl as egl;
use rustix::{event::{PollFd, PollFlags, poll}, fd::AsFd};
use xkbcommon::xkb;
use std::{mem, fmt, ffi::c_void as void, error::Error as StdError, sync::{mpsc::{Sender, Receiver, channel, SendError}, Arc, Mutex}, io, os::fd::{RawFd, BorrowedFd}, time::Duration, env};

pub struct EventLoop<T: 'static + Send /*  = () */> { // TODO: reenable the default type param (= ()) in the end
    running: bool,
    con: wayland_client::Connection,
    qh: QueueHandle<Self>,
    queue: Option<EventQueue<Self>>,
    wayland: WaylandState,
    events: Vec<Event<T>>, // used to push events from inside the dispatch impl
    nah_what_the_fuck: Option<EvlError>,
    proxy_data: EvlProxyData<T>,
    mouse_data: MouseData,
    keyboard_data: KeyboardData,
}

struct KeyboardData {
    has_focus: WindowId,
    xkb_context: xkb::Context,
    keymap_specific: Option<KeymapSpecificData>, // (re)initialized when a keymap is loaded
    repeat_timer: timerfd::TimerFd,
    repeat_key: u32, // raw key
    repeat_rate: Duration,
    repeat_delay: Duration,
    input_mode: InputMode,
}

struct KeymapSpecificData {
    xkb_state: xkb::State,
    compose_state: xkb::compose::State,
    pressed_keys: PressedKeys,
}

struct MouseData {
    has_focus: WindowId,
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
    pub eventfd: RawFd,
    pub sender: Sender<Event<T>>,
    pub receiver: Receiver<Event<T>>
}

impl<T> Drop for EvlProxyData<T> {
    fn drop(&mut self) {
        unsafe { libc::close(self.eventfd) };
    }
}

impl<T: 'static + Send> EventLoop<T> {

    pub fn new() -> Result<Self, EvlError> {

        let con = wayland_client::Connection::connect_to_env()?;

        let (globals, queue) = registry_queue_init::<Self>(&con).unwrap();
        let qh = queue.handle();
        let wayland = WaylandState::from_globals(globals, &qh)?;

        let proxy_data = {
            let (sender, receiver) = channel();
            let eventfd = unsafe { libc::eventfd(0, libc::EFD_NONBLOCK) }; // TODO: use rustix eventfd when it is fixed
            EvlProxyData { eventfd, sender, receiver }
        };

        let xkb_context = xkb::Context::new(xkb::CONTEXT_NO_FLAGS);

        let this = EventLoop {
            running: true,
            con,
            queue: Some(queue),
            qh,
            wayland,
            events: Vec::with_capacity(4),
            nah_what_the_fuck: None,
            proxy_data,
            mouse_data: MouseData {
                has_focus: 0,
                x: 0.0,
                y: 0.0
            },
            keyboard_data: KeyboardData {
                has_focus: 0,
                xkb_context,
                keymap_specific: None,
                repeat_timer: timerfd::TimerFd::new()?,
                repeat_key: 0,
                repeat_rate: Duration::ZERO,
                repeat_delay: Duration::ZERO,
                input_mode: InputMode::SingleKey,
            },
        };

        Ok(this)
        
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
            let timeout = if self.events.is_empty() { -1 } else { 0 }; // maybe cb generated events

            // prepare to wait for new events
            let Some(guard) = queue.prepare_read() else { continue };

            let queue_fd = guard.connection_fd();
            let channel_fd = unsafe { BorrowedFd::borrow_raw(self.proxy_data.eventfd) };
            let timer_fd = self.keyboard_data.repeat_timer.as_fd();

            let mut fds = [
                PollFd::new(&queue_fd, PollFlags::IN | PollFlags::ERR),
                PollFd::new(&channel_fd, PollFlags::IN),
                PollFd::new(&timer_fd, PollFlags::IN),
            ];

            // wait for new events
            poll(&mut fds, timeout).map_err(|err| io::Error::from(err))?;

            let wl_events = fds[0].revents();
            let channel_events = fds[1].revents();
            let timer_events = fds[2].revents();

            // new events from the wayland connection
            if wl_events.contains(PollFlags::IN) {
                guard.read()?;
            } else if wl_events.contains(PollFlags::ERR) {
                return Err(EvlError::Io(io::Error::last_os_error()));
            }

            // new user events
            if channel_events.contains(PollFlags::IN) {
                unsafe { libc::eventfd_read(self.proxy_data.eventfd, &mut 0) }; // make sure to read
                while let Ok(event) = self.proxy_data.receiver.try_recv() {
                    self.events.push(event)
                }
            }

            // new key-repeat timer events
            if timer_events.contains(PollFlags::IN) {
                self.keyboard_data.repeat_timer.read(); // make sure to read
                // emit the sysnthetic key-repeat event
                let key = self.keyboard_data.repeat_key;
                process_key_event(&mut self, key, Direction::Down, Source::KeyRepeat);
            }

        }

        Ok(())
        
    }

    pub fn new_proxy(&self) -> EvlProxy<T> {

        EvlProxy {
            sender: self.proxy_data.sender.clone(),
            eventfd: self.proxy_data.eventfd, // we can share the fd, no problemo
        }
        
    }

    pub fn suspend(&mut self) {
        self.events.push(Event::Suspend);
    }

    pub fn resume(&mut self) {
        self.events.push(Event::Resume);
    }

    pub fn quit(&mut self) {
        self.events.push(Event::Quit);
    }

    pub fn exit(&mut self) {
        self.running = false
    }

    pub fn set_input_mode(&mut self, mode: InputMode) {
        self.keyboard_data.input_mode = mode;
    }

}

pub struct EvlProxy<T> {
    sender: Sender<Event<T>>,
    eventfd: RawFd,
}

impl<T> EvlProxy<T> {

    pub fn send(&self, event: Event<T>) -> Result<(), SendError<Event<T>>> {

        self.sender.send(event)?;

        // the fd will be alive since the `send` would have failed if the EventLoop was destroyed
        unsafe { libc::eventfd_write(self.eventfd, 1) };

        Ok(())
    }

}

struct WaylandState {
    compositor: WlCompositor,
    wm: XdgWmBase,
}

impl WaylandState {
    pub fn from_globals<T: 'static + Send>(globals: GlobalList, qh: &QueueHandle<EventLoop<T>>) -> Result<Self, BindError> {
        Ok(Self {
            compositor: globals.bind(qh, 4..=6, ())?,
            wm: globals.bind(qh, 1..=1, ())?,
        })
    }
}

pub type WindowId = u32;
fn get_window_id(surface: &WlSurface) -> WindowId {
    surface.id().protocol_id()
}

pub struct Window<T: 'static + Send> {
    // our data
    shared: Arc<Mutex<WindowShared<T>>>, // needs to be accessed by some callbacks
    // wayland state
    qh: QueueHandle<EventLoop<T>>,
    compositor: WlCompositor,
    surface: WlSurface,
    _xdg_surface: XdgSurface,
    xdg_toplevel: XdgToplevel,
}

struct WindowShared<T> {
    id: WindowId,
    proxy: EvlProxy<T>,
    new_width: u32,
    new_height: u32,
    flags: ConfigureFlags,
    frame_callback_registered: bool,
    redraw_requested: bool,
}

impl<T: Send> Window<T> {
    
    pub fn new(evl: &mut EventLoop<T>, size: Size) -> Self {

        let shared = Arc::new(Mutex::new(WindowShared {
            id: 0,
            proxy: evl.new_proxy(),
            new_width:  size.width,
            new_height: size.height,
            flags: ConfigureFlags::default(),
            frame_callback_registered: false,
            redraw_requested: false
        }));

        let surface = evl.wayland.compositor.create_surface(&evl.qh, Arc::clone(&shared));
        let xdg_surface = evl.wayland.wm.get_xdg_surface(&surface, &evl.qh, Arc::clone(&shared));
        let xdg_toplevel = xdg_surface.get_toplevel(&evl.qh, Arc::clone(&shared));

        let mut guard = shared.lock().unwrap();
        guard.id = get_window_id(&surface);
        drop(guard);

        surface.commit();

        Self {
            shared,
            qh: evl.qh.clone(),
            compositor: evl.wayland.compositor.clone(),
            surface,
            _xdg_surface: xdg_surface,
            xdg_toplevel
        }
        
    }

    pub fn request_redraw(&mut self, _token: PresentToken) {
        let mut guard = self.shared.lock().unwrap();
        guard.redraw_requested = true;
    }

    pub fn pre_present_notify(&mut self) -> PresentToken {
        // note: it seems you have request the frame callback before swapping buffers
        //       otherwise the callback will never fire because the compositor thinks the content didn't change
        let mut guard = self.shared.lock().unwrap();
        if !guard.frame_callback_registered {
            guard.frame_callback_registered = true;
            self.surface.frame(&self.qh, Arc::clone(&self.shared));
            self.surface.commit();
        }
        PresentToken { _inner: () }
    }

    pub fn title<S: Into<String>>(&self, text: S) {
        self.xdg_toplevel.set_title(text.into());
    }

    pub fn application<S: Into<String>>(&self, text: S) {
        self.xdg_toplevel.set_app_id(text.into());
    }

    pub fn set_transparent(&self, value: bool) {
        if value {
            self.surface.set_opaque_region(None);
            self.surface.commit();
        } else {
            let region = self.compositor.create_region(&self.qh, ());
            region.add(0, 0, i32::MAX, i32::MAX);
            self.surface.set_opaque_region(Some(&region));
            self.surface.commit();
        }
    }

    pub fn maximized(&self, value: bool) {
        if value {
            self.xdg_toplevel.set_maximized();
        } else {
            self.xdg_toplevel.unset_maximized();
        };
        self.surface.commit();
    }

    pub fn fullscreen(&self, value: bool) {
        if value {
            self.xdg_toplevel.set_fullscreen(None);
        } else {
            self.xdg_toplevel.unset_fullscreen();
        };
    }

    pub fn min_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_min_size(size.width as i32, size.height as i32);
        self.surface.commit();
    }

    pub fn max_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_max_size(size.width as i32, size.height as i32);
        self.surface.commit();
    }

    pub fn force_size(&mut self, optional_size: Option<Size>) {
        let size = optional_size.unwrap_or_default();
        self.xdg_toplevel.set_max_size(size.width as i32, size.height as i32);
        self.xdg_toplevel.set_min_size(size.width as i32, size.height as i32);
        self.surface.commit();
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
pub struct PresentToken { _inner: () }

type FnSwapBuffersWithDamage = fn(
    khronos_egl::EGLDisplay,
    khronos_egl::EGLSurface,
    *const void /* damage rect array */,
    khronos_egl::Int
) -> khronos_egl::Int;

pub struct EglInstance {
    lib: Arc<egl::DynamicInstance>,
    swap_buffers_with_damage: Option<FnSwapBuffersWithDamage>,
    display: egl::Display,
    config: egl::Config,
}

impl EglInstance {

    /// Should be only be called once. Although initializing multiple instances is not a hard error.
    pub fn new<T: 'static + Send>(evh: &mut EventLoop<T>) -> Result<EglInstance, EvlError> {
        
        let loaded = unsafe {
            egl::DynamicInstance::<egl::EGL1_5>::load_required()
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
            lib.choose_first_config(egl_display, &attribs)?
                .ok_or(EvlError::EglUnsupported)?
        };

        Ok(Self {
            lib,
            swap_buffers_with_damage,
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
    wl_egl_surface: wayland_egl::WlEglSurface, // note: needs to be kept alive
    width: u32, // updated in resize
    height: u32,
    damage_rects: Vec<Rect>, // only here to save some allocations
    egl_surface: egl::Surface,
    egl_context: egl::Context,
}

impl EglContext {

    /// Create a new egl context that will draw onto the given window.
    pub fn new<T: 'static + Send>(instance: &EglInstance, window: &Window<T>, size: Size) -> Result<Self, EvlError> {

        let context = {
            let attribs = [
                egl::CONTEXT_MAJOR_VERSION, 4,
                egl::CONTEXT_MINOR_VERSION, 0, // TODO: add context options so the version etc. can be configured
                egl::CONTEXT_CLIENT_VERSION, 3,
                egl::CONTEXT_OPENGL_DEBUG, if cfg!(debug) { 1 } else { 0 },
                egl::NONE,
            ];
            instance.lib.create_context(instance.display, instance.config, None, &attribs).unwrap()
        };

        let wl_egl_surface = wayland_egl::WlEglSurface::new(window.surface.id(), size.width as i32, size.height as i32)?;

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
            width: size.width,
            height: size.height,
            damage_rects: Vec::with_capacity(2),
            egl_surface: surface,
            egl_context: context,
        })
        
    }

    /// Make this context current.
    pub fn bind(&mut self, instance: &EglInstance) -> Result<(), egl::Error> {

        instance.lib.make_current(
            instance.display,
            Some(self.egl_surface), // note: it is an error to only specify one of the two (read/draw) surfaces
            Some(self.egl_surface),
            Some(self.egl_context)
        )
        
    }

    /// Returns an error if this context is not the current one.
    pub fn swap_buffers(&mut self, instance: &EglInstance, damage: &[Rect], _token: PresentToken) -> Result<(), EvlError> {

        // YES IT WORKS, EVEN W/ DAMAGE TRACKING!

        // recalculate the origin of the rects to be in the top left

        self.damage_rects.clear();
        self.damage_rects.extend_from_slice(damage);

        for rect in self.damage_rects.iter_mut() {
            rect.y = self.width as i32 - rect.y - rect.h;
        }

        if let Some(func) = instance.swap_buffers_with_damage {
            // swap with damage
            (func)(instance.display.as_ptr(), self.egl_surface.as_ptr(), self.damage_rects.as_ptr().cast(), damage.len() as khronos_egl::Int);
        } else {
            // normal swap (if the extension is unsupported)
            instance.lib.swap_buffers(instance.display, self.egl_surface)?;
        }

        Ok(())

    }

    /// Don't forget to also resize your opengl viewport!
    pub fn resize(&mut self, size: Size) {

        self.width = size.width;
        self.height = size.height;

        self.wl_egl_surface.resize(size.width as i32, size.height as i32, 0, 0);
        
    }
    
}

pub enum Event<T> {
    /// Your app was resumed from the background or started and should show it's view.
    Resume,
    /// Your app's view should be destroyed but it can keep running in the background.
    Suspend,
    /// Your app should quit.
    Quit,
    /// Your own events. See [`EvlProxy`].
    User(T),
    /// An event that belongs to a specific window. (Eg. focus, mouse movement)
    Window { id: WindowId, event: WindowEvent },
}

#[derive(Debug)]
pub enum WindowEvent {
    Close,
    Resize { size: Size, flags: ConfigureFlags },
    Redraw,
    Rescale { scale: f64 },
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
    Super, // windows key
    AppMenu, // application menu key
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
            _: &$prxy,
            _: <$prxy as wayland_client::Proxy>::Event,
            _: &$usr,
            _: &wayland_client::Connection,
            _: &wayland_client::QueueHandle<Self>
        ) { () }
    };
}

impl<T: 'static + Send> wayland_client::Dispatch<WlRegistry, GlobalListContents> for EventLoop<T> {
    fn event(
        _evl: &mut Self,
        registry: &WlRegistry,
        event: WlRegistryEvent,
        _data: &GlobalListContents,
        _con: &wayland_client::Connection,
        qh: &wayland_client::QueueHandle<Self>
    ) {

        if let WlRegistryEvent::Global { name, interface, .. } = event {

            if &interface == "wl_seat" {
                let _seat: WlSeat = registry.bind(name, 1, qh, ());
            }
            
        }

    }

}

impl<T: 'static + Send> wayland_client::Dispatch<WlSeat, ()> for EventLoop<T> {
    fn event(
        _evl: &mut Self,
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
                seat.get_pointer(qh, ());
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

impl<T: 'static + Send> wayland_client::Dispatch<XdgSurface, Arc<Mutex<WindowShared<T>>>> for EventLoop<T> {
    fn event(
        evl: &mut Self,
        xdg_surface: &XdgSurface,
        event: XdgSurfaceEvent,
        shared: &Arc<Mutex<WindowShared<T>>>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        if let XdgSurfaceEvent::Configure { serial } = event {

            // ack the configure
            xdg_surface.ack_configure(serial);

            let guard = shared.lock().unwrap();

            // foreward the final configuration state to the user
            let width  = guard.new_width;
            let height = guard.new_height;
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Resize {
                size: Size { width, height },
                flags: guard.flags
            } });
            
            if !guard.redraw_requested {
                evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Redraw });
            }

        }
    }
}

impl<T: 'static + Send> wayland_client::Dispatch<XdgToplevel, Arc<Mutex<WindowShared<T>>>> for EventLoop<T> {
    fn event(
        evl: &mut Self,
        _surface: &XdgToplevel,
        event: XdgToplevelEvent,
        shared: &Arc<Mutex<WindowShared<T>>>,
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

impl<T: 'static + Send> wayland_client::Dispatch<WlCallback, Arc<Mutex<WindowShared<T>>>> for EventLoop<T> {
    fn event(
        evl: &mut Self,
        _cb: &WlCallback,
        _event: WlCallbackEvent,
        shared: &Arc<Mutex<WindowShared<T>>>,
        _con: &wayland_client::Connection,
        _qh: &wayland_client::QueueHandle<Self>
    ) {
        let mut guard = shared.lock().unwrap();
        if guard.redraw_requested {
            guard.redraw_requested = false;
            guard.frame_callback_registered = false;
            evl.events.push(Event::Window { id: guard.id, event: WindowEvent::Redraw });
        }
    }
}

// global events
impl<T: 'static + Send> wayland_client::Dispatch<WlCompositor, ()> for EventLoop<T> { ignore!(WlCompositor, ()); }

// surface events
impl<T: 'static + Send> wayland_client::Dispatch<WlSurface, Arc<Mutex<WindowShared<T>>>> for EventLoop<T> {
    fn event(
            _evl: &mut Self,
            _proxy: &WlSurface,
            event: WlSurfaceEvent,
            data: &Arc<Mutex<WindowShared<T>>>,
            _conn: &wayland_client::Connection,
            _qhandle: &QueueHandle<Self>,
        ) {

        if let WlSurfaceEvent::PreferredBufferScale { factor } = event {

            let guard = data.lock().unwrap();
            guard.proxy.send(Event::Window {
                id: guard.id,
                event: WindowEvent::Rescale { scale: factor as f64 }
            }).unwrap();
            
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
                        Ok(None) => { evl.nah_what_the_fuck = Some(EvlError::InvalidKeymap); return },
                        Err(err) => { evl.nah_what_the_fuck = Some(EvlError::Io(err));       return }
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
                    Err(..) => { evl.nah_what_the_fuck = Some(EvlError::InvalidLocale); return }
                };

                let compose_state = xkb::compose::State::new(&compose_table, xkb::STATE_NO_FLAGS);

                evl.keyboard_data.keymap_specific = Some(KeymapSpecificData {
                    xkb_state, compose_state, pressed_keys
                })
                
            },

            WlKeyboardEvent::Enter { surface, keys, .. } => {

                let id = get_window_id(&surface);
                evl.keyboard_data.has_focus = id;

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

                    evl.keyboard_data.has_focus = 0;

                    evl.events.push(Event::Window { id: evl.keyboard_data.has_focus, event: WindowEvent::Leave});

                    // emit a synthetic key-up event for all keys that are still pressed
                    for key in keymap_specific.pressed_keys.keys_down() {
                        process_key_event(evl, key.raw(), Direction::Up, Source::Event);
                    }
                    
                };

            },

            WlKeyboardEvent::Key { key: raw_key, state, .. } => {

                let dir = match state {
                    WEnum::Value(KeyState::Pressed) => Direction::Down,
                    WEnum::Value(KeyState::Released) => Direction::Up,
                    WEnum::Value(..) => return,
                    WEnum::Unknown(..) => return
                };

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

    let xkb_key = xkb::Keycode::new(raw_key + 8); // says the wayland docs
    let mut events = Vec::with_capacity(1); // TODO: use a smallvec OR use smallstring and only one event

    let repeat = source == Source::KeyRepeat;

    if dir == Direction::Down {

        // arm key-repeat timer with the correct delay and repeat rate
        if source == Source::Event { // only re-arm if this was NOT called from a repeated key event
            evl.keyboard_data.repeat_key = raw_key;
            evl.keyboard_data.repeat_timer.set_state(timerfd::TimerState::Periodic {
                current: evl.keyboard_data.repeat_delay,
                interval: evl.keyboard_data.repeat_rate
            }, timerfd::SetTimeFlags::Default);

            // update the key state
            keymap_specific.pressed_keys.key_down(xkb_key);
        }

        match evl.keyboard_data.input_mode {
            InputMode::SingleKey => {
                let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
                let key = translate_xkb_sym(xkb_sym);
                events.push(WindowEvent::KeyDown { key, repeat });
            },
            InputMode::Text => {
                let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
                keymap_specific.compose_state.feed(xkb_sym);
                match keymap_specific.compose_state.status() {
                    xkb::Status::Nothing => {
                        if let Some(chr) = xkb_sym.key_char() {
                            events.push(WindowEvent::TextInput { chr })
                        } else {
                            let key = translate_xkb_sym(xkb_sym);
                            events.push(WindowEvent::KeyDown { key, repeat });
                        }
                    },
                    xkb::Status::Composing => {
                        // sadly we can't just get the string repr of a dead-char
                        if let Some(chr) = translate_dead_to_normal_sym(xkb_sym).and_then(xkb::Keysym::key_char) {
                            events.push(WindowEvent::TextCompose { chr })
                        }
                    },
                    xkb::Status::Composed => {
                        if let Some(text) = keymap_specific.compose_state.utf8() {
                            for chr in text.chars() {
                                events.push(WindowEvent::TextInput { chr })
                            }
                        }
                        keymap_specific.compose_state.reset();
                    },
                    xkb::Status::Cancelled => {},
                }
            }
        }

    } else {

        // unarm key-repeat timer
        evl.keyboard_data.repeat_timer.set_state(timerfd::TimerState::Disarmed, timerfd::SetTimeFlags::Default);

        // update the key state
        keymap_specific.pressed_keys.key_up(xkb_key);

        if let InputMode::SingleKey = evl.keyboard_data.input_mode {
            let xkb_sym = keymap_specific.xkb_state.key_get_one_sym(xkb_key);
            let key = translate_xkb_sym(xkb_sym);
            events.push(WindowEvent::KeyUp { key });
        }

    };

    for event in events {
        evl.events.push(Event::Window { id: evl.keyboard_data.has_focus, event })
    }

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

             WlPointerEvent::Enter { surface, surface_x, surface_y, .. } => {

                let id = get_window_id(&surface);
                evl.mouse_data.has_focus = id;
                evl.mouse_data.x = surface_x;
                evl.mouse_data.y = surface_y;

                evl.events.push(Event::Window { id, event:
                    WindowEvent::MouseMotion { x: surface_x, y: surface_y }
                });

             },

             WlPointerEvent::Leave { .. } => {
                evl.mouse_data.has_focus = 0;
             },

             WlPointerEvent::Motion { surface_x, surface_y, .. } => {

                evl.mouse_data.x = surface_x;
                evl.mouse_data.y = surface_x;

                evl.events.push(Event::Window {
                    id: evl.mouse_data.has_focus,
                    event: WindowEvent::MouseMotion { x: surface_x, y: surface_y }
                });

             },

            WlPointerEvent::Button { button, state, .. } => {

                const BTN_LEFT: u32 = 0x110;
                const BTN_RIGHT: u32 = 0x111;
                const BTN_MIDDLE: u32 = 0x112;
                const BTN_SIDE: u32 = 0x113;
                const BTN_EXTRA: u32 = 0x114;
                const BTN_FORWARD: u32 = 0x115;
                const BTN_BACK: u32 = 0x116;

                let converted = match button {
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
                    WindowEvent::MouseDown { button: converted, x: evl.mouse_data.x, y: evl.mouse_data.y }
                } else {
                    WindowEvent::MouseUp { button: converted, x: evl.mouse_data.x, y: evl.mouse_data.y }
                };

                evl.events.push(Event::Window {
                    id: evl.mouse_data.has_focus,
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

                evl.events.push(Event::Window {
                    id: evl.mouse_data.has_focus,
                    event: WindowEvent::MouseScroll { axis, value }
                });
                
            },

            _ => ()
            
        }
        
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

