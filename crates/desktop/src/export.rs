
//! This module contains a C-ABI compatible API which
//! makes this crate usable as a library from other languages.

use core::task;
use std::{ffi::{CStr, CString, c_void as void}, mem, ptr::null, sync::{Arc, Mutex}};

use common::{IsDisplay, SmartMutex};

#[repr(C)]
pub struct EventLoopConfig {
    pub appid: *const i8,
    pub intercept: bool,
}

pub type EventLoopHandler = extern "C" fn(*const SharedEventLoop, *mut void);

#[repr(C)]
pub struct SharedEventLoop;

pub type EventLoopErrorCode = i32;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_run(config0: EventLoopConfig, handler0: EventLoopHandler, state: *mut void) -> EventLoopErrorCode {

    let appid = unsafe { CStr::from_ptr(config0.appid) }
        .to_str().expect("`appid` must be valid utf8").to_string();

    let config = crate::EvlConfig {
        appid,
        intercept: config0.intercept,
    };

    let result = crate::EventLoop::run(config, |evl| {
        let ptr: *const Arc<crate::EventLoop> = &evl;
        handler0(ptr.cast(), state);
    });

    match result {
        Ok(()) => 0i32,
        Err(..) => 1i32,
    }

}

/// A convenience method when using the lib from rust, which
/// passes through state and vtable pointer directly.
/// # Safety
/// This is "unsafe" if the library is compiled with a different
/// rust compiler then the program that uses it. However in practice
/// the async interface probably won't really change, and if so,
/// I will notice immediatly when building these crates.
#[repr(C)]
pub struct EvlPollContextRust {
    pub waker: EvlPollWakerRust,
}

// #[repr(C)]
// pub struct EvlPollContext {
//     /// Ownership: &'shared
//     pub waker: *const EvlPollWaker,
// }

#[repr(C)]
pub struct EvlPollWakerRust {
    pub state: *const void,
    pub vtable: *const void
}

// #[repr(C)]
// pub struct EvlPollWaker {
//     /// Ownership: &'shared
//     pub state: *const void,
//     /// Ownership: &'static
//     pub vtable: *const EvlPollWakerVTable
// }

// unsafe impl Send for EvlPollWaker {}

// #[repr(C)]
// pub struct EvlPollWakerVTable {
//     pub clone: extern "C" fn(*const void) -> *mut EvlPollWaker,
//     pub wake: extern "C" fn(*const void),
//     pub wake_by_ref: extern "C" fn(*const void),
//     pub drop: extern "C" fn(*const void)
// }

// const VTABLE: task::RawWakerVTable = task::RawWakerVTable::new(
//     |ptr| {
//         let (state, vtable) = unsafe { as_evl_poll_waker_components(ptr) };
//         let new = (vtable.clone)(state);
//         task::RawWaker::new(new.cast(), &VTABLE)
//     },
//     |ptr| { let (state, vtable) = unsafe { as_evl_poll_waker_components(ptr) }; (vtable.wake)(state); },
//     |ptr| { let (state, vtable) = unsafe { as_evl_poll_waker_components(ptr) }; (vtable.wake_by_ref)(state); },
//     |ptr| { let (state, vtable) = unsafe { as_evl_poll_waker_components(ptr) }; (vtable.drop)(state); },
// );

// unsafe fn as_evl_poll_waker_components<'x>(ptr: *const ()) -> (*const void, &'x EvlPollWakerVTable) {
//     let waker = unsafe { &* (ptr as *const EvlPollWaker) };
//     let vtable = unsafe { as_evl_poll_waker_vtable(waker.vtable) };
//     (waker.state, vtable)
// }

// unsafe fn as_evl_poll_waker_vtable(ptr: *const EvlPollWakerVTable) -> &'static EvlPollWakerVTable {
//     unsafe { &* (ptr as *const EvlPollWakerVTable) }
// }

// pub unsafe extern "C" fn convert_vtable(vtable: EvlPollWakerVTable) -> *const EvlPollWakerVTable {

//     let vtable = task::RawWakerVTable::new(
//         |ptr| {
//             let new = (vtable.clone)(ptr.cast());
//             task::RawWaker::new(new, vtable)
//         }, wake, wake_by_ref, drop)

// }

#[repr(C)]
pub struct MonitorInfo {
    pub name: *mut i8,
    pub description: *mut i8,
    pub size: common::PhysicalSize,
    pub refresh: u32,
}

#[repr(C)]
pub struct Monitor;

#[repr(C)]
pub struct HoveredItem;

#[repr(C)]
pub struct Receiver;

#[repr(C)]
pub struct DataWriter;

#[repr(C)]
pub struct DataReadable;

#[repr(C)]
pub struct EvlHandlers {

    resume:  extern "C" fn(*mut void),
    suspend: extern "C" fn(*mut void),
    quit:    extern "C" fn(*mut void, crate::QuitReason),

    monitor_update: extern "C" fn(*mut void, crate::Id, info: MonitorInfo, monitor: *mut Monitor),
    monitor_remove: extern "C" fn(*mut void, crate::Id),

    window_should_close: extern "C" fn(*mut void, crate::Id),
    window_redraw:       extern "C" fn(*mut void, crate::Id),
    window_resize:       extern "C" fn(*mut void, crate::Id, size: common::PhysicalSize, fullscreen: bool),
    window_rescale:      extern "C" fn(*mut void, crate::Id, scale: f64),
    window_decorations:  extern "C" fn(*mut void, crate::Id, active: bool),
    window_enter:        extern "C" fn(*mut void, crate::Id),
    window_leave:        extern "C" fn(*mut void, crate::Id),

    window_mouse_enter:  extern "C" fn(*mut void, crate::Id),
    window_mouse_leave:  extern "C" fn(*mut void, crate::Id),
    window_mouse_motion: extern "C" fn(*mut void, crate::Id, point: common::LogicalPoint),
    window_mouse_down:   extern "C" fn(*mut void, crate::Id, point: common::LogicalPoint, button: crate::MouseButton),
    window_mouse_up:     extern "C" fn(*mut void, crate::Id, point: common::LogicalPoint, button: crate::MouseButton),
    window_mouse_scroll: extern "C" fn(*mut void, crate::Id, axis: crate::ScrollAxis, value: i16),

    window_key_down_special: extern "C" fn(*mut void, crate::Id, key: crate::SpecialKey, repeat: bool),
    window_key_down_char:    extern "C" fn(*mut void, crate::Id, chr: u32, dead: bool, repeat: bool),
    window_key_down_unknown: extern "C" fn(*mut void, crate::Id, key: u32, repeat: bool),

    window_key_up_special:   extern "C" fn(*mut void, crate::Id, key: crate::SpecialKey),
    window_key_up_char:      extern "C" fn(*mut void, crate::Id, chr: u32, dead: bool),
    window_key_up_unknown:   extern "C" fn(*mut void, crate::Id, key: u32),

    window_text_input:          extern "C" fn(*mut void, crate::Id, chr: u32),
    window_text_compose:        extern "C" fn(*mut void, crate::Id, chr: u32),
    window_text_compose_cancel: extern "C" fn(*mut void, crate::Id),

    window_dnd_motion: extern "C" fn(*mut void, crate::Id, sameapp: bool, x: f64, y: f64, *mut HoveredItem),
    window_dnd_drop:   extern "C" fn(*mut void, crate::Id, sameapp: bool, x: f64, y: f64, *mut Receiver),
    window_dnd_cancel: extern "C" fn(*mut void, crate::Id, sameapp: bool),

    data_source_send: extern "C" fn(*mut void, crate::Id, kind: crate::DataKind, writer: *mut DataWriter),
    data_source_success: extern "C" fn(*mut void, crate::Id),
    data_source_close: extern "C" fn(*mut void, crate::Id),

    selection_update: extern "C" fn(*mut void, *mut DataReadable),

}

#[repr(C)]
pub enum Poll {
    Ready,
    Pending,
    Err,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_poll_rust(this0: *const SharedEventLoop, rawcx: EvlPollContextRust, handlers0: *const EvlHandlers, state: *mut void) -> Poll {

    let this: &Arc<crate::EventLoop> = unsafe { &*this0.cast() };
    let handlers: &EvlHandlers = unsafe { &*handlers0 };

    let waker = unsafe { task::Waker::new(
        rawcx.waker.state.cast(),
        &*rawcx.waker.vtable.cast()
    ) };

    let cx = task::Context::from_waker(&waker);

    event_loop_poll_inner(this, cx, handlers, state)

}

// pub unsafe extern "C" fn event_loop_poll(this0: *const SharedEventLoop, rawcx: EvlPollContext, state: *mut void) {

//     let this: &Arc<crate::EventLoop> = unsafe { &*this0.cast() };

//     let waker = unsafe { task::Waker::new(rawcx.waker.cast(), &VTABLE) };
//     let cx = task::Context::from_waker(&waker);

//     event_loop_poll_inner(this, cx);

// }

fn event_loop_poll_inner(this: &Arc<crate::EventLoop>, mut cx: task::Context, handlers: &EvlHandlers, state: *mut void) -> Poll {

    use crate::{Key, Event, MonitorEvent, WindowEvent, DndEvent, DataSourceEvent};

    match this.poll(&mut cx) {

        task::Poll::Pending        => Poll::Pending,
        task::Poll::Ready(Err(..)) => Poll::Err,

        task::Poll::Ready(Ok(event)) => {

            match event {

                Event::Resume          => (handlers.resume)(state),
                Event::Suspend         => (handlers.suspend)(state),
                Event::Quit { reason } => (handlers.quit)(state, reason),

                Event::Monitor { id, event: MonitorEvent::Update { info, monitor } } => {

                    let info0 = MonitorInfo {
                        name:        CString::into_raw(CString::new(info.name)       .expect("contained nul")),
                        description: CString::into_raw(CString::new(info.description).expect("contained nul")),
                        refresh: info.refresh,
                        size: info.size,
                    };

                    let monitor0 = Box::into_raw(Box::new(monitor)).cast();

                    (handlers.monitor_update)(state, id, info0, monitor0)

                },

                Event::Monitor { id, event: MonitorEvent::Remove } => {
                    (handlers.monitor_remove)(state, id)
                },

                Event::Window { id, event } => match event {

                    WindowEvent::ShouldClose                 => (handlers.window_should_close) (state, id),
                    WindowEvent::Redraw                      => (handlers.window_redraw)       (state, id),
                    WindowEvent::Resize { size, fullscreen } => (handlers.window_resize)       (state, id, size, fullscreen),
                    WindowEvent::Rescale { scale }           => (handlers.window_rescale)      (state, id, scale),
                    WindowEvent::Decorations { active }      => (handlers.window_decorations)  (state, id, active),
                    WindowEvent::Enter                       => (handlers.window_enter)        (state, id),
                    WindowEvent::Leave                       => (handlers.window_leave)        (state, id),

                    WindowEvent::MouseEnter                  => (handlers.window_mouse_enter)  (state, id),
                    WindowEvent::MouseLeave                  => (handlers.window_mouse_enter)  (state, id),
                    WindowEvent::MouseMotion { point }       => (handlers.window_mouse_motion) (state, id, point),
                    WindowEvent::MouseDown { point, button } => (handlers.window_mouse_down)   (state, id, point, button),
                    WindowEvent::MouseUp { point, button }   => (handlers.window_mouse_up)     (state, id, point, button),
                    WindowEvent::MouseScroll { axis, value } => (handlers.window_mouse_scroll) (state, id, axis, value),

                    WindowEvent::KeyDown { key, repeat } => match key {
                        Key::Special(it)  => (handlers.window_key_down_special) (state, id, it, repeat),
                        Key::Char(it)     => (handlers.window_key_down_char)    (state, id, it as u32, false, repeat),
                        Key::DeadChar(it) => (handlers.window_key_down_char)    (state, id, it as u32, true, repeat),
                        Key::Unknown(it)  => (handlers.window_key_down_unknown) (state, id, it, repeat),
                    },

                    WindowEvent::KeyUp { key } => match key {
                        Key::Special(it)  => (handlers.window_key_up_special) (state, id, it),
                        Key::Char(it)     => (handlers.window_key_up_char)    (state, id, it as u32, false),
                        Key::DeadChar(it) => (handlers.window_key_up_char)    (state, id, it as u32, true),
                        Key::Unknown(it)  => (handlers.window_key_up_unknown) (state, id, it),
                    },

                    WindowEvent::TextInput { chr }   => (handlers.window_text_input)          (state, id, chr as u32),
                    WindowEvent::TextCompose { chr } => (handlers.window_text_compose)        (state, id, chr as u32),
                    WindowEvent::TextComposeCancel   => (handlers.window_text_compose_cancel) (state, id),

                    WindowEvent::Dnd { event, sameapp } => match event {
                        DndEvent::Motion { x, y, item } => {
                            let item0 = Box::into_raw(Box::new(item)).cast();
                            (handlers.window_dnd_motion)(state, id, sameapp, x, y, item0)
                        },
                        DndEvent::Drop { x, y, source } => {
                            let source0 = Box::into_raw(Box::new(source)).cast();
                            (handlers.window_dnd_drop)(state, id, sameapp, x, y, source0)
                        },
                        DndEvent::Cancel => {
                            (handlers.window_dnd_cancel)(state, id, sameapp)

                        }
                    }

                },

                Event::DataSource { id, event } => match event {
                    DataSourceEvent::Send { kind, writer } => {
                        let writer0 = Box::into_raw(Box::new(writer)).cast();
                        (handlers.data_source_send)(state, id, kind, writer0)
                    },
                    DataSourceEvent::Success => (handlers.data_source_success) (state, id),
                    DataSourceEvent::Close   => (handlers.data_source_close)   (state, id),
                },

                Event::SelectionUpdate { sink } => {
                    let sink0 = Box::into_raw(Box::new(sink)).cast();
                    (handlers.selection_update)(state, sink0)
                }

            }

            Poll::Ready

        }

    }

}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_suspend(this0: *const SharedEventLoop) {
    let this: &Arc<crate::EventLoop> = unsafe { &*this0.cast() };
    this.suspend();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_resume(this0: *const SharedEventLoop) {
    let this: &Arc<crate::EventLoop> = unsafe { &*this0.cast() };
    this.resume();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_quit(this0: *const SharedEventLoop) {
    let this: &Arc<crate::EventLoop> = unsafe { &*this0.cast() };
    this.quit();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_display_ptr(this0: *const SharedEventLoop) -> *const void {
    let this: &Arc<crate::EventLoop> = unsafe { &*this0.cast() };
    this.ptr()
}
