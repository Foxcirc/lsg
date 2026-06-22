
use std::ffi::c_void as void;

#[repr(C)]
pub struct EventLoopConfig {
    pub appid: *const i8,
    pub intercept: bool,
}

pub type EventLoopHandler = unsafe extern "C" fn(*const EventLoop, *mut void);

#[repr(C)]
pub struct EventLoop;

#[repr(C)]
pub enum EvlResult {
    Ok,
    Err
}

#[repr(C)]
pub struct MonitorInfo {
    pub name: *mut i8,
    pub description: *mut i8,
    pub size: common::PhysicalSize,
    pub refresh: u32,
}

#[repr(C)]
pub struct Window;

#[repr(C)]
pub struct Monitor;

#[repr(C)]
pub struct CustomIcon;

#[repr(C)]
pub struct HoveredItem;

#[repr(C)]
pub struct DataReadable;

#[repr(C)]
pub struct DataKindsSlice {
    pub ptr: *const crate::DataKind,
    pub len: usize,
}

#[repr(C)]
pub struct ReadSlice {
    pub ptr: *mut u8,
    pub len: usize,
}

#[repr(C)]
pub struct WriteSlice {
    pub ptr: *const u8,
    pub len: usize,
}

#[repr(C)]
pub struct DataReader;

#[repr(C)]
pub struct DataWritable;

#[repr(C)]
pub struct DataWriter;

#[repr(C)]
pub struct EvlHandlers {
    pub resume: extern "C" fn(*mut void),
    pub suspend: extern "C" fn(*mut void),
    pub quit: extern "C" fn(*mut void, crate::QuitReason),

    pub monitor_update: extern "C" fn(*mut void, crate::Id, MonitorInfo, *mut Monitor),
    pub monitor_remove: extern "C" fn(*mut void, crate::Id),

    pub window_should_close: extern "C" fn(*mut void, crate::Id),
    pub window_redraw: extern "C" fn(*mut void, crate::Id),
    pub window_resize: extern "C" fn(*mut void, crate::Id, common::PhysicalSize, bool),
    pub window_rescale: extern "C" fn(*mut void, crate::Id, f64),
    pub window_decorations: extern "C" fn(*mut void, crate::Id, bool),
    pub window_enter: extern "C" fn(*mut void, crate::Id),
    pub window_leave: extern "C" fn(*mut void, crate::Id),

    pub window_mouse_enter: extern "C" fn(*mut void, crate::Id),
    pub window_mouse_leave: extern "C" fn(*mut void, crate::Id),
    pub window_mouse_motion: extern "C" fn(*mut void, crate::Id, common::PhysicalPoint),
    pub window_mouse_down: extern "C" fn(*mut void, crate::Id, common::PhysicalPoint, crate::MouseButton),
    pub window_mouse_up: extern "C" fn(*mut void, crate::Id, common::PhysicalPoint, crate::MouseButton),
    pub window_mouse_scroll: extern "C" fn(*mut void, crate::Id, i16, i16),

    pub window_key_down_special: extern "C" fn(*mut void, crate::Id, crate::SpecialKey, bool),
    pub window_key_down_char: extern "C" fn(*mut void, crate::Id, u32, bool, bool),
    pub window_key_down_unknown: extern "C" fn(*mut void, crate::Id, u32, bool),

    pub window_key_up_special: extern "C" fn(*mut void, crate::Id, crate::SpecialKey),
    pub window_key_up_char: extern "C" fn(*mut void, crate::Id, u32, bool),
    pub window_key_up_unknown: extern "C" fn(*mut void, crate::Id, u32),

    pub window_text_input: extern "C" fn(*mut void, crate::Id, u32),
    pub window_text_compose: extern "C" fn(*mut void, crate::Id, u32),
    pub window_text_compose_cancel: extern "C" fn(*mut void, crate::Id),

    pub window_dnd_motion: extern "C" fn(*mut void, crate::Id, bool, f64, f64, *mut HoveredItem),
    pub window_dnd_drop: extern "C" fn(*mut void, crate::Id, bool, f64, f64, *mut DataReadable),
    pub window_dnd_cancel: extern "C" fn(*mut void, crate::Id, bool),

    pub data_source_send: extern "C" fn(*mut void, crate::Id, crate::DataKind, *mut DataWriter),
    pub data_source_success: extern "C" fn(*mut void, crate::Id),
    pub data_source_close: extern "C" fn(*mut void, crate::Id),

    pub selection_update: extern "C" fn(*mut void, *mut DataReadable),
}

#[repr(C)]
pub enum Poll {
    Ready,
    Pending,
    Err,
}
