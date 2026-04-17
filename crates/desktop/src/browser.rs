
//! When running in the browser, we need some extra
//! helper functions to make the glue-layer work.

use std::{task, mem::ManuallyDrop, ffi::c_void as void};
use crate::export::*;

#[unsafe(no_mangle)]
unsafe extern "C" fn call_event_loop_handler(
    ptr: EventLoopHandler,
    evl: *const EventLoop,
    state: *mut void
) {
    unsafe { (ptr)(evl, state) }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn call_poll_context_wake(
    rawcx0: *const EvlPollContextRust,
) {
    let rawcx = unsafe { &*rawcx0 };
    let waker = unsafe { ManuallyDrop::new(task::Waker::new(
        rawcx.waker.state.cast(),
        &*rawcx.waker.vtable.cast()
    )) };
    waker.wake_by_ref();
}

#[unsafe(no_mangle)] unsafe extern "C" fn call_resume               (handlers: *const EvlHandlers, state: *mut void)                            { (unsafe { &*handlers }.resume)(state) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_suspend              (handlers: *const EvlHandlers, state: *mut void)                            { (unsafe { &*handlers }.suspend)(state) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_quit                 (handlers: *const EvlHandlers, state: *mut void, reason: crate::QuitReason) { (unsafe { &*handlers }.quit)(state, reason) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_monitor_update       (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, info: MonitorInfo, monitor: *mut Monitor) { (unsafe { &*handlers }.monitor_update)(state, id, info, monitor) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_monitor_remove       (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                           { (unsafe { &*handlers }.monitor_remove)(state, id) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_should_close  (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.window_should_close)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_redraw        (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.window_redraw)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_enter         (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.window_enter)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_leave         (handlers: *const EvlHandlers, state: *mut void, id: crate::Id) { (unsafe { &*handlers }.window_leave)(state, id) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_resize        (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, point: common::PhysicalSize, fullscreen: bool) { (unsafe { &*handlers }.window_resize)(state, id, point, fullscreen) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_rescale       (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, scale: f64)                                    { (unsafe { &*handlers }.window_rescale)(state, id, scale) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_decorations   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, enabled: bool)                                 { (unsafe { &*handlers }.window_decorations)(state, id, enabled) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_enter   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                                          { (unsafe { &*handlers }.window_mouse_enter)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_leave   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                                          { (unsafe { &*handlers }.window_mouse_leave)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_motion  (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, point: common::LogicalPoint)                             { (unsafe { &*handlers }.window_mouse_motion)(state, id, point) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_down    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, point: common::LogicalPoint, button: crate::MouseButton) { (unsafe { &*handlers }.window_mouse_down)(state, id, point, button) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_up      (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, point: common::LogicalPoint, button: crate::MouseButton) { (unsafe { &*handlers }.window_mouse_up)(state, id, point, button) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_mouse_scroll  (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, axis: crate::ScrollAxis, value: i16)                     { (unsafe { &*handlers }.window_mouse_scroll)(state, id, axis, value) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_down_special (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, key: crate::SpecialKey, repeat: bool) { (unsafe { &*handlers }.window_key_down_special)(state, id, key, repeat) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_down_char    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, chr: u32, dead: bool, repeat: bool)   { (unsafe { &*handlers }.window_key_down_char)(state, id, chr, dead, repeat) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_down_unknown (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, key: u32, repeat: bool)               { (unsafe { &*handlers }.window_key_down_unknown)(state, id, key, repeat) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_up_special   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, key: crate::SpecialKey) { (unsafe { &*handlers }.window_key_up_special)(state, id, key) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_up_char      (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, chr: u32, dead: bool)   { (unsafe { &*handlers }.window_key_up_char)(state, id, chr, dead) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_key_up_unknown   (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, key: u32)               { (unsafe { &*handlers }.window_key_up_unknown)(state, id, key) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_text_input          (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, chr: u32) { (unsafe { &*handlers }.window_text_input)(state, id, chr) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_text_compose        (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, chr: u32) { (unsafe { &*handlers }.window_text_compose)(state, id, chr) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_text_compose_cancel (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)           { (unsafe { &*handlers }.window_text_compose_cancel)(state, id) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_window_dnd_motion    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, sameapp: bool, x: f64, y: f64, item: *mut HoveredItem)      { (unsafe { &*handlers }.window_dnd_motion)(state, id, sameapp, x, y, item) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_dnd_drop      (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, sameapp: bool, x: f64, y: f64, readable: *mut DataReadable) { (unsafe { &*handlers }.window_dnd_drop)(state, id, sameapp, x, y, readable) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_window_dnd_cancel    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, sameapp: bool)                                              { (unsafe { &*handlers }.window_dnd_cancel)(state, id, sameapp) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_data_source_send     (handlers: *const EvlHandlers, state: *mut void, id: crate::Id, kind: crate::DataKind, writer: *mut DataWriter) { (unsafe { &*handlers }.data_source_send)(state, id, kind, writer) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_data_source_success  (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                                 { (unsafe { &*handlers }.data_source_success)(state, id) }
#[unsafe(no_mangle)] unsafe extern "C" fn call_data_source_close    (handlers: *const EvlHandlers, state: *mut void, id: crate::Id)                                                 { (unsafe { &*handlers }.data_source_close)(state, id) }

#[unsafe(no_mangle)] unsafe extern "C" fn call_selection_update     (handlers: *const EvlHandlers, state: *mut void, readable: *mut DataReadable) { (unsafe { &*handlers }.selection_update)(state, readable) }
