
//! This module contains a C-ABI compatible API which
//! makes this crate usable as a library from other languages.

use core::{slice, task};
use std::{ffi::{CStr, CString, c_void as void}, io::{Read, Write}, mem::{self, ManuallyDrop}, os::fd::{AsFd, AsRawFd}, ptr::{NonNull, null, null_mut}, sync::{Arc, Mutex}};

use common::{IsDisplay, IsSurface};

#[repr(C)]
pub struct EventLoopConfig {
    pub appid: *const i8,
    pub intercept: bool,
}

pub type EventLoopHandler = extern "C" fn(*const EventLoop, *mut void);

#[repr(C)]
pub struct EventLoop;

#[repr(C)]
pub enum EvlResult {
    Ok,
    Err
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_run(config0: EventLoopConfig, handler0: EventLoopHandler, state: *mut void) -> EvlResult {

    let appid = unsafe { CStr::from_ptr(config0.appid) }
        .to_str().expect("`appid` must be valid utf8").to_string();

    let config = crate::EvlConfig {
        appid,
        intercept: config0.intercept,
    };

    let result = crate::EventLoop::run(config, |evl| {

        let ptr = Arc::into_raw(evl).cast();

        handler0(ptr, state);

        // Make sure to actually drop the Arc.
        unsafe { drop(Arc::from_raw(ptr)) };

    });

    match result {
        Ok(()) => EvlResult::Ok,
        Err(..) => EvlResult::Err,
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

#[unsafe(no_mangle)]
pub unsafe extern "C" fn monitor_info_drop(this: MonitorInfo) {
    drop(unsafe { CString::from_raw(this.name) });
    drop(unsafe { CString::from_raw(this.description) });
}

#[repr(C)]
pub struct Monitor;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn monitor_drop(this0: *mut Monitor) {
    let mut this = unsafe { get_monitor(this0) };
    unsafe { ManuallyDrop::drop(&mut this) };
}

#[repr(C)]
pub struct CustomIcon;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn custom_icon_drop(this0: *mut CustomIcon) {
    let mut this = unsafe { get_custom_icon(this0) };
    unsafe { ManuallyDrop::drop(&mut this) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn custom_icon_new(evl0: *const EventLoop, size: common::LogicalSize, format: crate::IconFormat, data0: WriteSlice) -> *mut CustomIcon {
    let evl = unsafe { get_event_loop(evl0) };
    let data = unsafe { slice::from_raw_parts(data0.ptr, data0.len) };
    let icon = crate::CustomIcon::new(&evl, size, format, data);
    Box::into_raw(Box::new(icon)).cast()
}

#[repr(C)]
pub struct HoveredItem;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hovered_item_drop(this0: *mut HoveredItem) {
    let mut this = unsafe { get_hovered_item(this0) };
    unsafe { ManuallyDrop::drop(&mut this) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hovered_item_advertise(this0: *mut HoveredItem, kinds0: DataKindsSlice) {
    let this = unsafe { get_hovered_item(this0) };
    let kinds = unsafe { slice::from_raw_parts(kinds0.ptr, kinds0.len) };
    this.advertise(kinds);
}

#[repr(C)]
pub struct DataReadable;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_readable_drop(this0: *mut DataReadable) {
    let mut this = unsafe { get_data_readable(this0) };
    unsafe { ManuallyDrop::drop(&mut this) };
}

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

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_readable_kinds(this0: *mut DataReadable) -> DataKindsSlice {
    let this = unsafe { get_data_readable(this0) };
    let slice = this.kinds();
    DataKindsSlice {
        ptr: slice.as_ptr(),
        len: slice.len(),
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_readable_receive(this0: *mut DataReadable, evl0: *const EventLoop, kind: crate::DataKind) -> *mut DataReader {
    let this = unsafe { get_data_readable(this0) };
    let evl = unsafe { get_event_loop(evl0) };
    let reader = this.receive(&evl, kind);
    Box::into_raw(Box::new(reader)).cast()
}

#[repr(C)]
pub struct DataReader;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_reader_drop(this0: *mut DataReader) {
    let mut this = unsafe { get_data_reader(this0) };
    unsafe { ManuallyDrop::drop(&mut this) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_reader_as_fd(this0: *mut DataReader) -> i32 {
    let this = unsafe { get_data_reader(this0) };
    this.as_fd().as_raw_fd()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_reader_read(this0: *mut DataReader, out0: ReadSlice) -> usize {
    let mut this = unsafe { get_data_reader(this0) };
    let out = unsafe { slice::from_raw_parts_mut(out0.ptr, out0.len) };
    this.read(out).expect("cannot read") // TODO: foreward I/O error, otherwise a bad client could crash our program, also: do we even want this to be a IO read impl, what about windows/web where this isnt a file read (or is it?)?
}

#[repr(C)]
pub struct DataWritable;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writable_drop(this0: *mut DataWritable) {
    let mut this = unsafe { get_data_writable(this0) };
    unsafe { ManuallyDrop::drop(&mut this) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writable_id(this0: *mut DataWritable) -> crate::Id {
    let this = unsafe { get_data_writable(this0) };
    this.id()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writable_selection(evl0: *const EventLoop, offers0: DataKindsSlice) -> *mut DataWritable {
    let evl = unsafe { get_event_loop(evl0) };
    let offers = unsafe { slice::from_raw_parts(offers0.ptr, offers0.len) };
    Box::into_raw(Box::new(crate::DataWritable::selection(&evl, offers))).cast()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writable_dnd(wnd0: *mut Window, offers0: DataKindsSlice, icon0: *mut CustomIcon /* consumed */) -> *mut DataWritable {
    let wnd = unsafe { get_window(wnd0) };
    let icon1 = unsafe { get_custom_icon(icon0) };
    let icon = *ManuallyDrop::into_inner(icon1); // move out, since we want to take ownership
    let offers = unsafe { slice::from_raw_parts(offers0.ptr, offers0.len) };
    Box::into_raw(Box::new(crate::DataWritable::dnd(&wnd, offers, icon))).cast()
}

#[repr(C)]
pub struct DataWriter;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writer_drop(this0: *mut DataWriter) {
    let mut this = unsafe { get_data_writer(this0) };
    unsafe { ManuallyDrop::drop(&mut this) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writer_as_fd(this0: *mut DataWriter) -> i32 {
    let this = unsafe { get_data_writer(this0) };
    this.as_fd().as_raw_fd()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writer_write(this0: *mut DataWriter, src0: WriteSlice) -> usize {
    let mut this = unsafe { get_data_writer(this0) };
    let src = unsafe { slice::from_raw_parts(src0.ptr, src0.len) };
    this.write(src).expect("cannot write") // TODO: see data_reader_read
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writer_flush(this0: *mut DataWriter) {
    let mut this = unsafe { get_data_writer(this0) };
    this.flush().expect("cannot flush") // TODO: see data_reader_read
}

#[repr(C)]
pub struct EvlHandlers {

    pub resume:  extern "C" fn(*mut void),
    pub suspend: extern "C" fn(*mut void),
    pub quit:    extern "C" fn(*mut void, crate::QuitReason),

    pub monitor_update: extern "C" fn(*mut void, crate::Id, info: MonitorInfo, monitor: *mut Monitor),
    pub monitor_remove: extern "C" fn(*mut void, crate::Id),

    pub window_should_close: extern "C" fn(*mut void, crate::Id),
    pub window_redraw:       extern "C" fn(*mut void, crate::Id),
    pub window_resize:       extern "C" fn(*mut void, crate::Id, size: common::PhysicalSize, fullscreen: bool),
    pub window_rescale:      extern "C" fn(*mut void, crate::Id, scale: f64),
    pub window_decorations:  extern "C" fn(*mut void, crate::Id, active: bool),
    pub window_enter:        extern "C" fn(*mut void, crate::Id),
    pub window_leave:        extern "C" fn(*mut void, crate::Id),

    pub window_mouse_enter:  extern "C" fn(*mut void, crate::Id),
    pub window_mouse_leave:  extern "C" fn(*mut void, crate::Id),
    pub window_mouse_motion: extern "C" fn(*mut void, crate::Id, point: common::LogicalPoint),
    pub window_mouse_down:   extern "C" fn(*mut void, crate::Id, point: common::LogicalPoint, button: crate::MouseButton),
    pub window_mouse_up:     extern "C" fn(*mut void, crate::Id, point: common::LogicalPoint, button: crate::MouseButton),
    pub window_mouse_scroll: extern "C" fn(*mut void, crate::Id, axis: crate::ScrollAxis, value: i16),

    pub window_key_down_special: extern "C" fn(*mut void, crate::Id, key: crate::SpecialKey, repeat: bool),
    pub window_key_down_char:    extern "C" fn(*mut void, crate::Id, chr: u32, dead: bool, repeat: bool),
    pub window_key_down_unknown: extern "C" fn(*mut void, crate::Id, key: u32, repeat: bool),

    pub window_key_up_special:   extern "C" fn(*mut void, crate::Id, key: crate::SpecialKey),
    pub window_key_up_char:      extern "C" fn(*mut void, crate::Id, chr: u32, dead: bool),
    pub window_key_up_unknown:   extern "C" fn(*mut void, crate::Id, key: u32),

    pub window_text_input:          extern "C" fn(*mut void, crate::Id, chr: u32),
    pub window_text_compose:        extern "C" fn(*mut void, crate::Id, chr: u32),
    pub window_text_compose_cancel: extern "C" fn(*mut void, crate::Id),

    pub window_dnd_motion: extern "C" fn(*mut void, crate::Id, sameapp: bool, x: f64, y: f64, *mut HoveredItem),
    pub window_dnd_drop:   extern "C" fn(*mut void, crate::Id, sameapp: bool, x: f64, y: f64, *mut DataReadable),
    pub window_dnd_cancel: extern "C" fn(*mut void, crate::Id, sameapp: bool),

    pub data_source_send:    extern "C" fn(*mut void, crate::Id, kind: crate::DataKind, writer: *mut DataWriter),
    pub data_source_success: extern "C" fn(*mut void, crate::Id),
    pub data_source_close:   extern "C" fn(*mut void, crate::Id),

    pub selection_update: extern "C" fn(*mut void, *mut DataReadable),

}

#[unsafe(no_mangle)]
pub const extern "C" fn evl_handlers_default() -> EvlHandlers {

    macro_rules! noop {
        ($($arg:ty),*) => {{
            extern "C" fn f($(_: $arg),*) {} f
        }};
    }

    EvlHandlers {

        resume:  noop!(*mut void),
        suspend: noop!(*mut void),
        quit:    noop!(*mut void, crate::QuitReason),

        monitor_update: noop!(*mut void, crate::Id, MonitorInfo, *mut Monitor),
        monitor_remove: noop!(*mut void, crate::Id),

        window_should_close: noop!(*mut void, crate::Id),
        window_redraw:       noop!(*mut void, crate::Id),
        window_resize:       noop!(*mut void, crate::Id, common::PhysicalSize, bool),
        window_rescale:      noop!(*mut void, crate::Id, f64),
        window_decorations:  noop!(*mut void, crate::Id, bool),
        window_enter:        noop!(*mut void, crate::Id),
        window_leave:        noop!(*mut void, crate::Id),

        window_mouse_enter:  noop!(*mut void, crate::Id),
        window_mouse_leave:  noop!(*mut void, crate::Id),
        window_mouse_motion: noop!(*mut void, crate::Id, common::LogicalPoint),
        window_mouse_down:   noop!(*mut void, crate::Id, common::LogicalPoint, crate::MouseButton),
        window_mouse_up:     noop!(*mut void, crate::Id, common::LogicalPoint, crate::MouseButton),
        window_mouse_scroll: noop!(*mut void, crate::Id, crate::ScrollAxis, i16),

        window_key_down_special: noop!(*mut void, crate::Id, crate::SpecialKey, bool),
        window_key_down_char:    noop!(*mut void, crate::Id, u32, bool, bool),
        window_key_down_unknown: noop!(*mut void, crate::Id, u32, bool),

        window_key_up_special:   noop!(*mut void, crate::Id, crate::SpecialKey),
        window_key_up_char:      noop!(*mut void, crate::Id, u32, bool),
        window_key_up_unknown:   noop!(*mut void, crate::Id, u32),

        window_text_input:          noop!(*mut void, crate::Id, u32),
        window_text_compose:        noop!(*mut void, crate::Id, u32),
        window_text_compose_cancel: noop!(*mut void, crate::Id),

        window_dnd_motion: noop!(*mut void, crate::Id, bool, f64, f64, *mut HoveredItem),
        window_dnd_drop:   noop!(*mut void, crate::Id, bool, f64, f64, *mut DataReadable),
        window_dnd_cancel: noop!(*mut void, crate::Id, bool),

        data_source_send:    noop!(*mut void, crate::Id, crate::DataKind, *mut DataWriter),
        data_source_success: noop!(*mut void, crate::Id),
        data_source_close:   noop!(*mut void, crate::Id),

        selection_update: noop!(*mut void, *mut DataReadable)

    }

}

#[repr(C)]
pub enum Poll {
    Ready,
    Pending,
    Err,
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_poll_rust(this0: *const EventLoop, rawcx: EvlPollContextRust, handlers0: *const EvlHandlers, state: *mut void) -> Poll {

    let this = unsafe { get_event_loop(this0) };
    let handlers: &EvlHandlers = unsafe { &*handlers0 };

    let waker = unsafe { task::Waker::new(
        rawcx.waker.state.cast(),
        &*rawcx.waker.vtable.cast()
    ) };

    let cx = task::Context::from_waker(&waker);

    event_loop_poll_inner(&this, cx, handlers, state)

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
                        DndEvent::Drop { x, y, readable } => {
                            let readable0 = Box::into_raw(Box::new(readable)).cast();
                            (handlers.window_dnd_drop)(state, id, sameapp, x, y, readable0)
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

                Event::SelectionUpdate { readable } => {
                    let readable0 = readable.map(|it| Box::into_raw(Box::new(it)).cast())
                        .unwrap_or(null_mut::<DataReadable>());
                    (handlers.selection_update)(state, readable0)
                }

            }

            Poll::Ready

        }

    }

}

unsafe fn get_event_loop(ptr: *const EventLoop) -> ManuallyDrop<Arc<crate::EventLoop>> {
    unsafe { ManuallyDrop::new(Arc::from_raw(ptr.cast())) }
}

unsafe fn get_window(ptr: *mut Window) -> ManuallyDrop<Box<crate::Window>> {
    ManuallyDrop::new(unsafe { Box::from_raw(ptr.cast()) })
}

unsafe fn get_custom_icon(ptr: *mut CustomIcon) -> ManuallyDrop<Box<crate::CustomIcon>> {
    ManuallyDrop::new(unsafe { Box::from_raw(ptr.cast()) })
}

unsafe fn get_monitor(ptr: *mut Monitor) -> ManuallyDrop<Box<crate::Monitor>> {
    ManuallyDrop::new(unsafe { Box::from_raw(ptr.cast()) })
}

unsafe fn get_hovered_item(ptr: *mut HoveredItem) -> ManuallyDrop<Box<crate::HoveredItem>> {
    ManuallyDrop::new(unsafe { Box::from_raw(ptr.cast()) })
}

unsafe fn get_data_readable(ptr: *mut DataReadable) -> ManuallyDrop<Box<crate::DataReadable>> {
    ManuallyDrop::new(unsafe { Box::from_raw(ptr.cast()) })
}

unsafe fn get_data_reader(ptr: *mut DataReader) -> ManuallyDrop<Box<crate::DataReader>> {
    ManuallyDrop::new(unsafe { Box::from_raw(ptr.cast()) })
}

unsafe fn get_data_writable(ptr: *mut DataWritable) -> ManuallyDrop<Box<crate::DataWritable>> {
    ManuallyDrop::new(unsafe { Box::from_raw(ptr.cast()) })
}

unsafe fn get_data_writer(ptr: *mut DataWriter) -> ManuallyDrop<Box<crate::DataWriter>> {
    ManuallyDrop::new(unsafe { Box::from_raw(ptr.cast()) })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_suspend(this0: *const EventLoop) {
    let this = unsafe { get_event_loop(this0) };
    this.suspend();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_resume(this0: *const EventLoop) {
    let this = unsafe { get_event_loop(this0) };
    this.resume();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_quit(this0: *const EventLoop) {
    let this = unsafe { get_event_loop(this0) };
    this.quit();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_display_ptr(this0: *const EventLoop) -> *const void {
    let this = unsafe { get_event_loop(this0) };
    this.ptr()
}

#[repr(C)]
pub struct Window;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_drop(this0: *mut Window) {
    let mut this = unsafe { get_window(this0) };
    unsafe { ManuallyDrop::drop(&mut this) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_new(this0: *const EventLoop) -> *mut Window {
    let this = unsafe { get_event_loop(this0) };
    let window = crate::Window::new(&this);
    Box::into_raw(Box::new(window)).cast()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_id(this0: *mut Window) -> crate::Id {
    let this = unsafe { get_window(this0) };
    this.id()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_present(this0: *mut Window) {
    let this = unsafe { get_window(this0) };
    this.present()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_redraw(this0: *mut Window) {
    let this = unsafe { get_window(this0) };
    this.redraw()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_transparency(this0: *mut Window, value: bool) {
    let this = unsafe { get_window(this0) };
    this.transparency(value);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_decorations(this0: *mut Window, value: bool) {
    let this = unsafe { get_window(this0) };
    this.decorations(value);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_title(this0: *mut Window, text0: *const i8) {
    let this = unsafe { get_window(this0) };
    let text = unsafe { CStr::from_ptr(text0).to_str()
        .expect("`window title` must be valid utf8").to_string() };
    this.title(&text);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_maximize(this0: *mut Window, value: bool) {
    let this = unsafe { get_window(this0) };
    this.maximize(value);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_fullscreen(this0: *mut Window, value: bool, monitor0: *mut Monitor) {
    let this = unsafe { get_window(this0) };
    match NonNull::new(monitor0) {
        Some(it) => {
            let monitor = unsafe { get_monitor(it.as_ptr()) };
            this.fullscreen(value, Some(&monitor));
        },
        None => this.fullscreen(value, None)
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_sizehint(this0: *mut Window, size: common::PhysicalSize) {
    let this = unsafe { get_window(this0) };
    this.sizehint(size);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_minsize(this0: *mut Window, size: common::LogicalSize) {
    let this = unsafe { get_window(this0) };
    this.minsize(Some(size));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_minsize_unset(this0: *mut Window) {
    let this = unsafe { get_window(this0) };
    this.minsize(None);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_maxsize(this0: *mut Window, size: common::LogicalSize) {
    let this = unsafe { get_window(this0) };
    this.minsize(Some(size));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_maxsize_unset(this0: *mut Window) {
    let this = unsafe { get_window(this0) };
    this.minsize(None);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_alert(this0: *mut Window, urgency: crate::Urgency) {
    let this = unsafe { get_window(this0) };
    this.alert(urgency);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_ptr(this0: *mut Window) -> *mut void {
    let this = unsafe { get_window(this0) };
    this.ptr()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_size(this0: *mut Window) -> common::PhysicalSize {
    let this = unsafe { get_window(this0) };
    this.size()
}
