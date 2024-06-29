
use crate::InvokedNotifAction;

use super::{CustomIcon, DataOffer, DataWriter, DndHandle, Monitor, WindowId, MonitorId, DataSourceId, NotifId, NotifAction};

use bitflags::bitflags;

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
    ///  Notification event. (eg. an action was invoked)
    Notif { id: NotifId, event: NotifEvent }
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
/// Events for a notification.
pub enum NotifEvent {
    ActionInvoked { action: InvokedNotifAction }
}

#[derive(Debug)]
pub enum DndEvent {
    Motion { x: f64, y: f64, handle: DndHandle },
    Drop { x: f64, y: f64, offer: DataOffer },
    Cancel,
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

#[derive(Debug, Clone, Copy, Default)]
pub enum IoMode {
    #[default]
    Blocking,
    Nonblocking,
}

#[derive(Debug, Clone, Copy)]
pub enum InputMode {
    SingleKey,
    Text,
}

#[derive(Debug)]
pub enum QuitReason {
    /// Quit requested via `request_quit`.
    User,
    /// SIGTERM received. For example on shutdown. Only generated when `signals` feature is enabled.
    System,
    /// SIGINT received. Only generated when `signals` feature is enabled.
    CtrlC,
}

#[derive(Debug, Default)]
pub enum Urgency {
    /// Should display a hint. Might do nothing.
    #[default]
    Info,
    /// Will likely switch window focus or display an urgent hint.
    Switch,
}

pub enum IconFormat {
    Argb8,
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
    pub fn modifier(&self) -> bool { // TODO: rename is_modifier?
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

#[derive(Debug, Default, Clone, Copy)]
pub struct ConfigureFlags {
    pub fullscreen: bool
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

#[derive(Clone, Copy)]
pub struct PresentToken { pub(crate) id: WindowId }
