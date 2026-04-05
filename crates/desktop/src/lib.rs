
use core::{error::Error as StdError, fmt, future, task};

// TODO: use alloc instead of std to possibly support non-std targets
use std::{io, os::fd::AsFd, sync::Arc};

#[cfg(feature = "import")] mod import;
#[cfg(feature = "import")] use import as backend2;

#[cfg(all(target_os = "linux", feature = "export"))] mod linux;
#[cfg(all(target_os = "linux", feature = "export"))] use linux as backend2;

#[cfg(feature = "export")] mod export;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(u32);

#[derive(Debug)]
pub enum Event {
    /// Your app was resumed from the background or started and should show it's view.
    Resume,
    /// Your app's view should be destroyed but it can keep running in the background.
    Suspend,
    /// Your app should quit.
    Quit { reason: QuitReason },
    /// A monitor was discovered or updated.
    MonitorUpdate { id: Id, state: Monitor },
    /// A monitor was removed.
    MonitorRemove { id: Id },
    /// An event that belongs to a specific window. (eg. focus change, mouse movement)
    Window { id: Id, event: WindowEvent },
    /// Requests you sending data to another client.
    Sender { id: Id, event: SenderEvent },
    /// The selection changed. This event will not be send if your app isn't in focus.
    /// `None` indicates that the current selection was invalidated.
    SelectionUpdate { recv: Option<Receiver> },
    // ///  Notification event. (eg. an action was invoked)
    // Notif { id: NotifId, event: NotifEvent },
}

#[derive(Debug)]
pub enum WindowEvent {
    ShouldClose,
    /// You must always redraw if asked to.
    Redraw,
    Resize { size: common::PhysicalSize, fullscreen: bool },
    Rescale { scale: f64 },
    Decorations { active: bool },
    Enter,
    Leave,
    MouseEnter,
    MouseLeave,
    MouseMotion { point: common::LogicalPoint },
    MouseDown { point: common::LogicalPoint, button: MouseButton },
    MouseUp { point: common::LogicalPoint, button: MouseButton },
    MouseScroll { axis: ScrollAxis, value: i16 },
    KeyDown { key: Key, repeat: bool },
    KeyUp { key: Key },
    TextInput { chr: char },
    TextCompose { chr: char },
    TextComposeCancel,
    /// A Drag-and-drop event.
    Dnd { event: DndEvent, sameapp: bool },
}

/// Events for a [`DataSource`].
#[derive(Debug)]
pub enum SenderEvent {
    /// Data of the specific [`DataKind`] you advertised was requested to be transferred.
    /// Could be send multiple times.
    Send { kind: DataKind, send: Sender },
    /// Data was successfully transfarred.
    /// Could be send multiple times, one per `Send`.
    Success,
    /// Your data source is no longer used and can be dropped.
    /// *This event may never be sent in rare cases.*
    Close,
}

#[derive(Debug)]
/// Events for a notification.
pub enum NotifEvent {
    // ActionInvoked { action: InvokedNotifAction }
}

#[derive(Debug)]
pub enum DndEvent {
    Motion { x: f64, y: f64, item: HoveredItem },
    Drop { x: f64, y: f64, recv: Receiver },
    Cancel,
}

#[derive(Debug)]
pub enum CursorStyle {
    Hidden,
    Custom { icon: CustomIcon, hotspot: common::LogicalPoint },
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

#[derive(Debug, Clone, Copy)]
pub enum InputMode {
    SingleKey,
    Text,
}

#[derive(Debug, Clone, Copy)]
pub enum QuitReason {
    /// Quit requested programatically.
    Program,
    /// SIGTERM received. For example on shutdown.
    /// Only generated when `signals` feature is enabled.
    System,
    /// SIGINT received.
    /// Only generated when `signals` feature is enabled.
    CtrlC,
}

#[derive(Debug, Default)]
pub enum Urgency {
    /// Should display a hint or might do nothing.
    #[default]
    Info,
    /// Should switch window focus or display an urgent hint.
    Switch,
}

pub enum IconFormat {
    Argb8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // TODO: derive the right traits to all those classes
pub enum Key {
    Escape,
    Tab,
    CapsLock,
    Shift,
    Control,
    Alt,
    AltGr,
    /// Windows key.
    Super,
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

#[derive(Debug, Clone, Copy)]
pub enum MouseButton {
    Left,
    Right,
    Middle,
    X1,
    X2,
    Unknown(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataKind {
    Text,
    Xml,
    Html,
    Zip,
    Json,
    Jpeg,
    Png,
    Other
}

impl Default for DataKind {
    fn default() -> Self {
        Self::Other
    }
}

#[derive(Debug)]
pub enum ScrollAxis {
    Vertical,
    Horizontal
}

#[derive(Debug, Default, Clone)]
pub struct MonitorInfo {
    pub name: String,
    pub description: String,
    /// Size in physical millimeters.
    pub size: common::PhysicalSize,
    /// Refresh rate in mHz. You can use the [`fps`](Monitor::fps) method to convert it to Hz.
    pub refresh: u32,
}

impl MonitorInfo {
    /// Trimmed conversion.
    pub fn fps(&self) -> u32 {
        self.refresh / 1000
    }
}

#[derive(Debug)]
pub struct EvlError {
    severity: Severity,
    message: String,
}

impl EvlError {
    pub fn new(severity: Severity, message: String) -> Self {
        Self { severity, message }
    }
    pub fn fatal(message: String) -> Self {
        Self::new(Severity::Fatal, message)
    }
    pub fn warning(message: String) -> Self {
        Self::new(Severity::Warning, message)
    }
    pub fn unsupported(message: String) -> Self {
        Self::new(Severity::Unsupported, message)
    }
    pub fn anyerror<T: StdError>(t: T) -> Self {
        Self::fatal(t.to_string())
    }
}

#[derive(Debug)]
pub enum Severity {
    /// Can likely not be recovered from.
    Fatal,
    /// Something has gone wrong but the application may continue to run.
    Warning,
    /// Some environments may not support a feature. Especially in a diverse ecosystem like wayland.
    Unsupported,
}

impl fmt::Display for EvlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.severity, self.message)
    }
}

impl StdError for EvlError {}

// Now the items which need integration with
// the backend are defined.

pub struct EventLoop {
    backend: backend2::EventLoopBackend,
}

impl EventLoop {

    #[track_caller]
    pub fn run<R, H>(config: EvlConfig, handler: H) -> Result<R, EvlError>
        where H: FnOnce(Arc<Self>) -> R {

            backend2::EventLoopBackend::run(config, handler)

    }

    pub async fn next(&self) -> Result<Event, EvlError> {
        future::poll_fn(|cx| self.poll(cx)).await
    }

    pub fn poll(&self, cx: &mut task::Context<'_>) -> task::Poll<Result<Event, EvlError>> {
        self.backend.poll(cx)
    }

    pub fn suspend(&self) { self.backend.suspend() }
    pub fn resume(&self)  { self.backend.resume() }
    pub fn quit(&self)    { self.backend.quit() }

}

unsafe impl common::IsDisplay for EventLoop {
    fn ptr(&self) -> *const std::ffi::c_void {
        self.backend.ptr()
    }
}

#[derive(Debug, Clone)]
pub struct EvlConfig {
    pub appid: String,
    /// If `true` relevant signals will be intercepted and
    /// turned into `Quit` events. Otherwise signals
    /// will never be intercepted.
    pub intercept: bool,
}

impl Default for EvlConfig {
    fn default() -> Self {
        Self {
            appid: "lsg-unknown".into(),
            intercept: false,
        }
    }
}

pub struct Window {
    backend: backend2::WindowBackend,
}

impl Window {
    pub fn new(evl: &Arc<EventLoop>) -> Self {
        let backend = backend2::WindowBackend::new(evl);
        Self { backend }
    }
    pub fn id(&self) -> Id {
        self.backend.id()
    }
    /// Notify the windowing system that you are going to draw to the window now.
    /// This function is mandatory and you must call it, otherwise the window will behave weirdly.
    // TODO: on wayland call present automatically when emitting a redraw event (cause
    // you HAVE to present if asked to)
    pub fn present(&self) {
        self.backend.present();
    }
    #[track_caller]
    pub fn redraw(&self) {
        self.backend.redraw();
    }
    pub fn transparency(&self, value: bool) {
        self.backend.transparency(value);
    }
    pub fn decorations(&self, value: bool) {
        self.backend.decorations(value);
    }
    pub fn title<S: Into<String>>(&self, text: S) {
        self.backend.title(text);
    }
    pub fn maximize(&self, value: bool) {
        self.backend.maximize(value);
    }
    pub fn fullscreen(&self, value: bool, monitor: Option<&Monitor>) {
        self.backend.fullscreen(value, monitor);
    }
    pub fn sizehint(&self, size: common::PhysicalSize) {
        self.backend.sizehint(size);
    }
    pub fn minsize(&self, size: Option<common::LogicalSize>) {
        self.backend.minsize(size);
    }
    pub fn maxsize(&self, size: Option<common::LogicalSize>) {
        self.backend.maxsize(size);
    }
    pub fn alert(&self, urgency: Urgency) {
        self.backend.alert(urgency);
    }
}

unsafe impl common::IsSurface for Window {
    fn ptr(&self) -> *mut std::ffi::c_void {
        self.backend.ptr()
    }
    fn size(&self) -> common::PhysicalSize {
        self.backend.size()
    }
}

pub struct Monitor {
    pub info: MonitorInfo,
    backend: backend2::MonitorBackend,
}

pub struct Receiver {
    backend: backend2::ReceiverBackend,
}

impl Receiver {

    pub fn kinds(&self) -> &[DataKind] {
        self.backend.kinds()
    }

    /// A [`Receiver`] can be read multiple times. Also using different [`DataKind`].
    pub fn receive(&self, evl: &EventLoop, kind: DataKind) -> Result<impl io::Read, EvlError> {
        self.backend.receive(evl, kind)
    }

}

pub struct Sender {
    backend: backend2::SenderBackend,
}

impl AsFd for Sender {
    fn as_fd(&self) -> std::os::fd::BorrowedFd<'_> {
        self.backend.as_fd()
    }
}

impl io::Write for Sender {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.backend.write(buf)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.backend.flush()
    }
}

pub struct HoveredItem {
    backend: backend2::HoveredItemBackend,
}

impl HoveredItem {
    pub fn advertise(&self, kinds: &[DataKind]) {
        self.backend.advertise(kinds);
    }
}

pub struct CustomIcon {
    backend: backend2::CustomIconBackend,
}

impl CustomIcon {
    pub fn new(evl: &EventLoop, size: common::LogicalSize, format: IconFormat, data: &[u8]) -> Result<Self, EvlError> {
        let backend = backend2::CustomIconBackend::new(evl, size, format, data)?;
        Ok(Self { backend })
    }
}

// /// The layers are ordered from bottom most to top most.
// pub enum WindowLayer {
//     /// Below everything. (Eg. Desktop Widgets)
//     Background,
//     /// Below normal programs.
//     Bottom,
//     /// Above normal programs. (Eg. Always-on-top Window)
//     Top,
//     /// Above everything. (Eg. Fps Counter)
//     Overlay
// }

// pub enum WindowAnchor {
//     Top,
//     Bottom,
//     Left,
//     Right
// }

// /// Keyboard window interactivity.
// pub enum KbInteractivity {
//     /// Window can't have keyboard focus.
//     None,
//     /// Top/Overlay windows will completely grab keyboard focus.
//     Exclusive
// }

macro_rules! impl_debug_opaque {
    ($($name:ty),* $(,)?) => {
        $(impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!(stringify!($ty), " {{ ... }}"))
            }
        })*
    };
}

impl_debug_opaque!(
    Monitor,
    Receiver,
    Sender,
    HoveredItem,
    CustomIcon
);
