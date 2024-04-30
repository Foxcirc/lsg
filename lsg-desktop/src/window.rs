
use std::{
    fmt, ops,
    error::Error as StdError, io, sync::Arc,
};

use bitflags::bitflags;
use khronos_egl as egl;

use crate::wayland as platform;

macro_rules! foreward_debug {
    () => {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self.platform)
        }
    };
}

// ### event proxy ###

pub struct EventProxy<T> {
    pub(crate) platform: platform::EventProxy<T>,
}

impl<T> EventProxy<T> {

    pub fn send(&self, event: Event<T>) -> Result<(), SendError<Event<T>>> {
        self.platform.send(event)
    }

}

pub struct SendError<T> {
    pub inner: T
}

impl<T> fmt::Debug for SendError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SendError: EventLoop dead")
    }
}

impl<T> fmt::Display for SendError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<T: fmt::Debug> StdError for SendError<T> {}

// ### public async event loop ###

pub struct EventLoop<T: 'static + Send> {
    pub(crate) platform: platform::EventLoop<T>,
}

impl<T: 'static + Send> EventLoop<T> {

    pub fn new(application: &str) -> Result<Self, EvlError> {
        Ok(Self {
            platform: platform::EventLoop::new(application)?,
        })
    }

    pub async fn next(&mut self) -> Result<Event<T>, EvlError> {
        self.platform.next().await
    }

    pub fn on_dispatch_thread<R>(&mut self, func: impl FnOnce(&mut Self) -> R) -> R {
        self.platform.on_dispatch_thread(func)
    }

    pub fn new_proxy(&mut self) -> EventProxy<T> {
        EventProxy {
            platform: self.platform.new_proxy()
        }
    }

    pub fn suspend(&mut self) {
        self.platform.suspend()
    }

    pub fn resume(&mut self) {
        self.platform.resume()
    }

    pub fn request_quit(&mut self) {
        self.platform.request_quit()
    }

    #[track_caller]
    pub fn get_clip_board(&mut self) -> Option<DataOffer> {
        Some(DataOffer {
            platform: self.platform.get_clip_board()?
        })
    }

    pub fn set_clip_board(&mut self, ds: Option<&DataSource>) {
        self.platform.set_clip_board(ds.map(|it| &it.platform))
    }

}

pub fn run<E: 'static + Send, T, H: FnOnce(EventLoop<E>) -> T>(handler: H, application: &str) -> Result<T, EvlError> {
    let target = EventLoop::new(application)?;
    Ok(handler(target))
}

pub type MonitorId = u32;

#[derive(Debug, Default, Clone)] // TODO: foreward-implement all debug impls using the macro
pub struct MonitorInfo {
    pub(crate) platform: platform::MonitorInfo,
}

impl MonitorInfo {
    /// Trimmed conversion.
    pub fn fps(&self) -> u32 {
        self.platform.fps()
    }    
}

pub struct Monitor {
    pub(crate) platform: platform::Monitor,
}

impl fmt::Debug for Monitor {
    foreward_debug!();
}

// #### base window ####

pub struct BaseWindow<T: 'static + Send> {
    pub(crate) platform: platform::BaseWindow<T>
}

impl<T: 'static + Send> BaseWindow<T> {

    pub fn id(&self) -> WindowId {
        self.platform.id()
    }

    #[track_caller]
    pub fn request_redraw(&self, token: PresentToken) {
        self.platform.request_redraw(token)
    }

    pub fn pre_present_notify(&self) -> PresentToken {
        self.platform.pre_present_notify()
    }

    pub fn set_transparency(&self, value: bool) {
        self.platform.set_transparency(value)
    }
    
}

// ### window ###

pub type WindowId = u32;

pub struct Window<T: 'static + Send> {
    pub(crate) platform: platform::Window<T>
}

impl<T: 'static + Send> ops::Deref for Window<T> {
    type Target = BaseWindow<T>;
    fn deref(&self) -> &Self::Target {
        ops::Deref::deref(&self.platform)
    }
}

impl<T: 'static + Send> Window<T> {
    
    pub fn new(evl: &mut EventLoop<T>, size: Size) -> Self {
        Window {
            platform: platform::Window::new(&mut evl.platform, size)
        }
    }

    pub fn destroy(self) {}

    pub fn set_input_mode(&self, evl: &mut EventLoop<T>, mode: InputMode) {
        self.platform.set_input_mode(&mut evl.platform, mode)
    }

    pub fn set_decorations(&mut self, value: bool) {
        self.platform.set_decorations(value)
    }

    pub fn set_title<S: Into<String>>(&self, text: S) {
        self.platform.set_title(text)
    }

    pub fn set_maximized(&self, value: bool) {
        self.platform.set_maximized(value)
    }

    pub fn set_fullscreen(&self, value: bool, monitor: Option<&Monitor>) {
        self.platform.set_fullscreen(value, monitor.map(|it| &it.platform))
    }

    pub fn min_size(&mut self, optional_size: Option<Size>) {
        self.platform.min_size(optional_size)
    }

    pub fn max_size(&mut self, optional_size: Option<Size>) {
        self.platform.max_size(optional_size)
    }

    pub fn force_size(&mut self, optional_size: Option<Size>) {
        self.platform.force_size(optional_size)
    }

    pub fn request_user_attention(&self, evl: &mut EventLoop<T>, urgency: Urgency) {
        self.platform.request_user_attention(&mut evl.platform, urgency)
    }

    /// You should only start a drag-and-drop when the left mouse button is held down
    /// *and* the user then moves the mouse.
    /// Otherwise the request may be denied or visually broken.
    pub fn start_drag_and_drop(&self, evl: &mut EventLoop<T>, icon: CustomIcon, ds: &DataSource) {
        self.platform.start_drag_and_drop(&mut evl.platform, icon.platform, &ds.platform)
    }

    pub fn set_cursor(&self, evl: &mut EventLoop<T>, style: CursorStyle) {
        self.platform.set_cursor(&mut evl.platform, style)
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

#[derive(Debug, Default)]
pub enum Urgency {
    /// Should display a hint. Might do nothing.
    #[default]
    Info,
    /// Will likely switch window focus or display an urgent hint.
    Switch,
}

// ### drag and drop ###

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

pub type DataOfferId = u32;

/// Don't hold onto it. You should immediatly decide if you want to receive something or not.
pub struct DataOffer {
    pub(crate) platform: platform::DataOffer,
}

impl fmt::Debug for DataOffer {
    foreward_debug!();
}

impl DataOffer {

    pub fn kinds(&self) -> DataKinds {
        self.platform.kinds()
    }

    /// A `DataOffer` can be read multiple times. Also using different `DataKinds`.
    pub fn receive(&self, kind: DataKinds, mode: IoMode) -> Result<DataReader, EvlError> {
        Ok(DataReader {
            platform: self.platform.receive(kind, mode)?
        })
    }

    pub fn cancel(self) {}
    
}

pub struct DataReader {
    pub(crate) platform: platform::DataReader
}

impl io::Read for DataReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.platform.read(buf)
    }
}

#[derive(Debug)]
pub struct DataWriter {
    pub(crate) platform: platform::DataWriter,
}

impl io::Write for DataWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.platform.write(buf)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.platform.flush()
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub enum IoMode {
    #[default]
    Blocking,
    Nonblocking,
}

pub type DataSourceId = u32;

pub struct DataSource {
    pub(crate) platform: platform::DataSource,
}

impl DataSource {

    #[track_caller]
    pub fn new<T: 'static + Send>(evl: &mut EventLoop<T>, offers: DataKinds, mode: IoMode) -> Self {
        DataSource {
            platform: platform::DataSource::new(&mut evl.platform, offers, mode)
        }
    }

    pub fn id(&self) -> DataSourceId {
        self.platform.id()
    }

    pub fn cancel(self) {}

}

// ### custom icon ###

pub enum IconFormat {
    Argb8,
}

pub struct CustomIcon {
    pub(crate) platform: platform::CustomIcon,
}

impl fmt::Debug for CustomIcon { foreward_debug!(); }

impl CustomIcon {
    
    /// Currently uses env::temp_dir() so the image content of your icon could be leaked to other users.
    #[track_caller]
    pub fn new<T: 'static + Send>(evl: &mut EventLoop<T>, size: Size, format: IconFormat, data: &[u8]) -> Result<Self, EvlError> {
        Ok(CustomIcon {
            platform: platform::CustomIcon::new(&mut evl.platform, size, format, data)?
        })
    }

    pub fn destroy(self) {}

}

// ### (wayland) popup and layer window ###

pub struct PopupWindow<T: 'static + Send> {
    pub(crate) platform: platform::PopupWindow<T>,
}

impl<T: 'static + Send> ops::Deref for PopupWindow<T> {
    type Target = platform::BaseWindow<T>;
    fn deref(&self) -> &Self::Target {
        ops::Deref::deref(&self.platform)
    }
}

impl<T: 'static + Send> PopupWindow<T> {
    
    pub fn new(evl: &mut EventLoop<T>, size: Size, parent: &Window<T>) -> Self {
        Self {
            platform: platform::PopupWindow::new(&mut evl.platform, size, &parent.platform)
        }
    }

    pub fn destroy(self) {}

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
    pub(crate) platform: platform::LayerWindow<T>
}

impl<T: 'static + Send> ops::Deref for LayerWindow<T> {
    type Target = platform::BaseWindow<T>;
    fn deref(&self) -> &Self::Target {
        ops::Deref::deref(&self.platform)
    }
}

impl<T: 'static + Send> LayerWindow<T> {

    pub fn new(evl: &mut EventLoop<T>, size: Size, layer: WindowLayer, monitor: Option<&Monitor>) -> Result<Self, Unsupported> {
        Ok(Self {
            platform: platform::LayerWindow::new(&mut evl.platform, size, layer, monitor.map(|val| &val.platform))?,
        })
    }

    pub fn destroy(self) {}

    pub fn anchor(&self, anchor: WindowAnchor) {
        self.platform.anchor(anchor)
    }

    pub fn margin(&self, value: u32) {
        self.platform.margin(value)
    }

    pub fn set_interactivity(&self, value: KbInteractivity) {
        self.platform.set_interactivity(value)
    }
    
}

// ### general purpose types ###

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

// ### egl api ###

#[derive(Clone, Copy)]
pub struct PresentToken { pub(crate) id: WindowId }

pub struct EglInstance {
    pub(crate) platform: Arc<platform::EglInstance>,
}

impl EglInstance {

    /// Should be only be called once. Although initializing multiple instances is not a hard error.
    pub fn new<T: 'static + Send>(evh: &mut EventLoop<T>) -> Result<Self, EvlError> {
        Ok(Self {
            platform: platform::EglInstance::new(&mut evh.platform)?,
        })
    }

    pub fn get_proc_address(&self, name: &str) -> Option<extern "system" fn()> {
        self.platform.get_proc_address(name)
    }
    
}

pub struct EglContext {
    pub(crate) platform: platform::EglContext,
}

impl EglContext {

    /// Create a new egl context that will draw onto the given window.
    pub fn new<T: 'static + Send, W: ops::Deref<Target = BaseWindow<T>>>(instance: &Arc<EglInstance>, window: &W, size: Size) -> Result<Self, EvlError> {
        Ok(Self {
            platform: platform::EglContext::new(&instance.platform, &window, size)?
        })
    }

    /// Make this context current.
    pub fn bind(&self) -> Result<(), egl::Error> {
        self.platform.bind()
    }

    /// Unbind this context.
    pub fn unbind(&self) -> Result<(), egl::Error> {
        self.platform.unbind()
    }

    /// Returns an error if this context is not the current one.
    #[track_caller]
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>, token: PresentToken) -> Result<(), EvlError> {
        self.platform.swap_buffers(damage, token)
    }

    /// Don't forget to also resize your opengl viewport!
    pub fn resize(&mut self, size: Size) {
        self.platform.resize(size)
    }

}

pub struct EglPixelBuffer {
    pub(crate) platform: platform::EglPixelBuffer,
}

impl EglPixelBuffer {

    /// Create a new egl context that will draw onto the given window.
    pub fn new(instance: &EglInstance, size: Size) -> Result<Self, EvlError> {
        Ok(Self {
            platform: platform::EglPixelBuffer::new(&instance.platform, size)?,
        })
    }

    /// Make this context current.
    pub fn bind(&self) -> Result<(), egl::Error> {
        self.platform.bind()
    }

    /// Unbind this context.
    pub fn unbind(&self) -> Result<(), egl::Error> {
        self.platform.unbind()
    }

    /// Returns an error if this context is not the current one.
    #[track_caller]
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EvlError> {
        self.platform.swap_buffers(damage)
    }

}

// ### events ###

#[derive(Debug)]
pub enum QuitReason {
    /// Quit requested via `request_quit`.
    User,
    /// SIGTERM received. For example on shutdown. Only generated when `signals` feature is enabled.
    System,
    /// SIGINT received. Only generated when `signals` feature is enabled.
    CtrlC,
}

#[derive(Debug)]
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
    Dnd { event: DndEvent, sameapp: bool },
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
    pub(crate) platform: platform::DndHandle,
}

impl DndHandle {
    pub fn advertise(&self, kinds: &[DataKinds]) {
        self.platform.advertise(kinds)
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

#[derive(Debug, Default, Clone, Copy)]
pub struct ConfigureFlags {
    pub fullscreen: bool
}
 
#[derive(Debug)]
pub enum ScrollAxis {
    Vertical,
    Horizontal
}

// ### error handling ###

pub type EvlError = platform::EvlError;
pub type Unsupported = platform::Unsupported;

