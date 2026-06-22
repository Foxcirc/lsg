
//! When running in the browser, we need some extra
//! helper functions to make the glue-layer work.

use std::ffi::c_void as void;
use crate::ffi::types::*;

#[unsafe(no_mangle)]
unsafe extern "C" fn call_event_loop_handler(
    ptr: EventLoopHandler,
    evl: *const EventLoop,
    state: *mut void
) {
    unsafe { (ptr)(evl, state) }
}

// These call wrappers take only primitive types as arguments, since JS cannot pass structs to WASM.

#[unsafe(no_mangle)] unsafe extern "C" fn call_resume               (handlers: *const EvlHandlers, state: *mut void)                            { (unsafe { &*handlers }.resume)(state) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_suspend              (handlers: *const EvlHandlers, state: *mut void)                            { (unsafe { &*handlers }.suspend)(state) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_quit                 (handlers: *const EvlHandlers, state: *mut void, reason: crate::QuitReason) { (unsafe { &*handlers }.quit)(state, reason) }

// Unsupported.
// #[unsafe(no_mangle)] unsafe extern "C" fn call_monitor_update (
//     handlers: *const EvlHandlers, state: *mut void, id: crate::Id,
//     monitor_info_name: *mut i8,
//     monitor_info_description: *mut i8,
//     monitor_info_size_w: u16,
//     monitor_info_size_h: u16,
//     monitor_info_refresh: u32,
//     monitor: *mut Monitor
// ) {
//     (unsafe { &*handlers }.monitor_update)(
//         state, id,
//         MonitorInfo {
//             name: monitor_info_name,
//             description: monitor_info_description,
//             size: common::PhysicalSize { w: monitor_info_size_w, h: monitor_info_size_h },
//             refresh: monitor_info_refresh
//         },
//         monitor
//     )
// }
// #[unsafe(no_mangle)] unsafe extern "C" fn call_monitor_remove       (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.monitor_remove)(state, id) }

// #[unsafe(no_mangle)] unsafe extern "C" fn call_window_should_close  (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.window_should_close)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_redraw        (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.window_redraw)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_enter         (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.window_enter)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_leave         (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.window_leave)(state, id) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_resize        (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, size_w: u16, size_h: u16, fullscreen: bool) { (unsafe { &*handlers }.window_resize)(state, id, common::PhysicalSize { w: size_w, h: size_h }, fullscreen) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_rescale       (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, scale: f64)                                 { (unsafe { &*handlers }.window_rescale)(state, id, scale) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_decorations   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, enabled: bool)                              { (unsafe { &*handlers }.window_decorations)(state, id, enabled) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_enter   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                                          { (unsafe { &*handlers }.window_mouse_enter)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_leave   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                                          { (unsafe { &*handlers }.window_mouse_leave)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_motion  (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, point_x: i16, point_y: i16)                             { (unsafe { &*handlers }.window_mouse_motion)(state, id, common::PhysicalPoint { x: point_x, y: point_y }) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_down    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, point_x: i16, point_y: i16, button: crate::MouseButton) { (unsafe { &*handlers }.window_mouse_down)  (state, id, common::PhysicalPoint { x: point_x, y: point_y }, button) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_up      (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, point_x: i16, point_y: i16, button: crate::MouseButton) { (unsafe { &*handlers }.window_mouse_up)    (state, id, common::PhysicalPoint { x: point_x, y: point_y }, button) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_scroll  (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, dx: i16, dy: i16)                                       { (unsafe { &*handlers }.window_mouse_scroll)(state, id, dx, dy) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_down_special (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, key: crate::SpecialKey, repeat: bool) { (unsafe { &*handlers }.window_key_down_special)(state, id, key, repeat) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_down_char    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, chr: u32, dead: bool, repeat: bool)   { (unsafe { &*handlers }.window_key_down_char)(state, id, chr, dead, repeat) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_down_unknown (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, key: u32, repeat: bool)               { (unsafe { &*handlers }.window_key_down_unknown)(state, id, key, repeat) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_up_special   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, key: crate::SpecialKey) { (unsafe { &*handlers }.window_key_up_special)(state, id, key) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_up_char      (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, chr: u32, dead: bool)   { (unsafe { &*handlers }.window_key_up_char)(state, id, chr, dead) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_up_unknown   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, key: u32)               { (unsafe { &*handlers }.window_key_up_unknown)(state, id, key) }

// #[unsafe(no_mangle)] unsafe extern "C" fn call_window_text_input          (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, chr: u32) { (unsafe { &*handlers }.window_text_input)(state, id, chr) }
// #[unsafe(no_mangle)] unsafe extern "C" fn call_window_text_compose        (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, chr: u32) { (unsafe { &*handlers }.window_text_compose)(state, id, chr) }
// #[unsafe(no_mangle)] unsafe extern "C" fn call_window_text_compose_cancel (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)           { (unsafe { &*handlers }.window_text_compose_cancel)(state, id) }

// #[unsafe(no_mangle)] unsafe extern "C" fn call_window_dnd_motion    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, sameapp: bool, x: f64, y: f64, item: *mut HoveredItem)      { (unsafe { &*handlers }.window_dnd_motion)(state, id, sameapp, x, y, item) }
// #[unsafe(no_mangle)] unsafe extern "C" fn call_window_dnd_drop      (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, sameapp: bool, x: f64, y: f64, readable: *mut DataReadable) { (unsafe { &*handlers }.window_dnd_drop)(state, id, sameapp, x, y, readable) }
// #[unsafe(no_mangle)] unsafe extern "C" fn call_window_dnd_cancel    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, sameapp: bool)                                              { (unsafe { &*handlers }.window_dnd_cancel)(state, id, sameapp) }

// #[unsafe(no_mangle)] unsafe extern "C" fn call_data_source_send     (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, kind: crate::DataKind, writer: *mut DataWriter) { (unsafe { &*handlers }.data_source_send)(state, id, kind, writer) }
// #[unsafe(no_mangle)] unsafe extern "C" fn call_data_source_success  (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                                 { (unsafe { &*handlers }.data_source_success)(state, id) }
// #[unsafe(no_mangle)] unsafe extern "C" fn call_data_source_close    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                                 { (unsafe { &*handlers }.data_source_close)(state, id) }

// #[unsafe(no_mangle)] unsafe extern "C" fn call_selection_update     (handlers: *const EvlHandlers, state: *mut void, readable: *mut DataReadable) { (unsafe { &*handlers }.selection_update)(state, readable) }
