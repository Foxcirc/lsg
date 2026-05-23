
//! This module contains a C-ABI compatible API which
//! makes this crate usable as a library from other languages.

use core::{
    slice, task,
    ffi::{CStr, c_void as void},
    ptr::{NonNull, drop_in_place, null_mut},
};

use std::{sync::Arc, ffi::CString};

use common::{IsDisplay, IsSurface};

use crate::ffi::types::*;
use futures::ffi::{types::InternalWaker, waker::waker_clone};

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_run(config0: EventLoopConfig, handler0: EventLoopHandler, state: *mut void) -> EvlResult {

    let appid = unsafe { CStr::from_ptr(config0.appid) }
        .to_str().expect("`appid` must be valid utf8").to_string();

    let config = crate::EvlConfig {
        appid,
        intercept: config0.intercept,
    };

    let result = crate::EventLoop::run(config, |evl| {
        let ptr = &evl as *const Arc<crate::EventLoop>;
        unsafe { handler0(ptr.cast(), state) };
    });

    match result {
        Ok(()) => EvlResult::Ok,
        Err(..) => EvlResult::Err,
    }

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

#[unsafe(no_mangle)]
pub unsafe extern "C" fn event_loop_poll(this0: *const EventLoop, waker0: *const InternalWaker, handlers0: *const EvlHandlers, state: *mut void) -> Poll {

    let this = unsafe { get_event_loop(this0) };
    let handlers = unsafe { &*handlers0 };

    let cloned0 = unsafe { waker_clone(waker0) };
    let waker = unsafe { Arc::from_raw(cloned0 as *const task::Waker) }; // SAFETY: Unstable-Waker-FFI
    let cx = task::Context::from_waker(&waker);

    event_loop_poll_inner(&this, cx, handlers, state)

}

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
                    WindowEvent::MouseLeave                  => (handlers.window_mouse_leave)  (state, id),
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
                    let readable0: *mut DataReadable = readable
                        .map(|it| Box::into_raw(Box::new(it)).cast())
                        .unwrap_or(null_mut());
                    (handlers.selection_update)(state, readable0)
                }

            }

            Poll::Ready

        }

    }

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

#[unsafe(no_mangle)]
pub unsafe extern "C" fn monitor_info_drop(this: MonitorInfo) {
    drop(unsafe { CString::from_raw(this.name) });
    drop(unsafe { CString::from_raw(this.description) });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn monitor_drop(this0: *mut Monitor) {
    let this = unsafe { get_monitor(this0) };
    unsafe { drop_in_place(this ) };
}

// CustomIcon is only ever consumed, so this shall rather be left out:
//
// #[unsafe(no_mangle)]
// pub unsafe extern "C" fn custom_icon_drop(this0: *mut CustomIcon) {
//     let this = unsafe { get_custom_icon(this0) };
//     unsafe { drop_in_place(this ) };
// }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn custom_icon_new(
    evl0: *const EventLoop,
    size: common::LogicalSize,
    format: crate::IconFormat,
    data0: WriteSlice
) -> *mut CustomIcon {
    let evl = unsafe { get_event_loop(evl0) };
    let data = unsafe { slice::from_raw_parts(data0.ptr, data0.len) };
    let icon = crate::CustomIcon::new(&evl, size, format, data);
    Box::into_raw(Box::new(icon)).cast()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hovered_item_drop(this0: *mut HoveredItem) {
    let this = unsafe { get_hovered_item(this0) };
    unsafe { drop_in_place(this ) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn hovered_item_advertise(this0: *mut HoveredItem, kinds0: DataKindsSlice) {
    let this = unsafe { get_hovered_item(this0) };
    let kinds = unsafe { slice::from_raw_parts(kinds0.ptr, kinds0.len) };
    this.advertise(kinds);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_readable_drop(this0: *mut DataReadable) {
    let this = unsafe { get_data_readable(this0) };
    unsafe { drop_in_place(this ) };
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

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_reader_drop(this0: *mut DataReader) {
    let this = unsafe { get_data_reader(this0) };
    unsafe { drop_in_place(this) };
}

// #[unsafe(no_mangle)]
// pub unsafe extern "C" fn data_reader_as_fd(this0: *mut DataReader) -> i32 {
//     let this = unsafe { get_data_reader(this0) };
//     this.as_fd().as_raw_fd()
// }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_reader_read(this0: *mut DataReader, out0: ReadSlice) -> usize {
    let this = unsafe { get_data_reader(this0) };
    let out = unsafe { slice::from_raw_parts_mut(out0.ptr, out0.len) };
    this.read(out).expect("cannot read") // TODO: foreward I/O error, otherwise a bad client could crash our program, also: do we even want this to be a IO read impl, what about windows/web where this isnt a file read (or is it?)?
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writable_drop(this0: *mut DataWritable) {
    let this = unsafe { get_data_writable(this0) };
    unsafe { drop_in_place(this) };
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
    let icon = unsafe { own_custom_icon(icon0) };
    let offers = unsafe { slice::from_raw_parts(offers0.ptr, offers0.len) };
    Box::into_raw(Box::new(crate::DataWritable::dnd(&wnd, offers, *icon))).cast()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writer_drop(this0: *mut DataWriter) {
    let this = unsafe { get_data_writer(this0) };
    unsafe { drop_in_place(this) };
}

// #[unsafe(no_mangle)]
// pub unsafe extern "C" fn data_writer_as_fd(this0: *mut DataWriter) -> i32 {
//     let this = unsafe { get_data_writer(this0) };
//     this.as_fd().as_raw_fd()
// }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn data_writer_write(this0: *mut DataWriter, src0: WriteSlice) -> usize {
    let this = unsafe { get_data_writer(this0) };
    let src = unsafe { slice::from_raw_parts(src0.ptr, src0.len) };
    this.write(src).expect("cannot write") // TODO: see data_reader_read
}

// #[unsafe(no_mangle)]
// pub unsafe extern "C" fn data_writer_flush(this0: *mut DataWriter) {
//     let this = unsafe { get_data_writer(this0) };
//     this.flush().expect("cannot flush") // TODO: see data_reader_read
// }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_drop(this0: *mut Window) {
    let this = unsafe { get_window_mut(this0) };
    unsafe { drop_in_place(this) };
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
    this.maxsize(Some(size));
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn window_maxsize_unset(this0: *mut Window) {
    let this = unsafe { get_window(this0) };
    this.maxsize(None);
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

unsafe fn get_event_loop<'x>(ptr: *const EventLoop) -> &'x Arc<crate::EventLoop> {
    unsafe { &*ptr.cast() }
}

unsafe fn get_window<'x>(ptr: *const Window) -> &'x crate::Window {
    unsafe { & *ptr.cast() }
}

unsafe fn get_window_mut<'x>(ptr: *mut Window) -> &'x mut crate::Window {
    unsafe { &mut *ptr.cast() }
}

unsafe fn get_custom_icon<'x>(ptr: *mut CustomIcon) -> &'x mut crate::CustomIcon {
    unsafe { &mut *ptr.cast() }
}

unsafe fn own_custom_icon<'x>(ptr: *mut CustomIcon) -> Box<crate::CustomIcon> {
    unsafe { Box::from_raw(ptr.cast()) }
}

unsafe fn get_monitor<'x>(ptr: *mut Monitor) -> &'x mut crate::Monitor {
    unsafe { &mut *ptr.cast() }
}

unsafe fn get_hovered_item<'x>(ptr: *mut HoveredItem) -> &'x mut crate::HoveredItem {
    unsafe { &mut *ptr.cast() }
}

unsafe fn get_data_readable<'x>(ptr: *mut DataReadable) -> &'x mut crate::DataReadable {
    unsafe { &mut *ptr.cast() }
}

unsafe fn get_data_reader<'x>(ptr: *mut DataReader) -> &'x mut crate::DataReader {
    unsafe { &mut *ptr.cast() }
}

unsafe fn get_data_writable<'x>(ptr: *mut DataWritable) -> &'x mut crate::DataWritable {
    unsafe { &mut *ptr.cast() }
}

unsafe fn get_data_writer<'x>(ptr: *mut DataWriter) -> &'x mut crate::DataWriter {
    unsafe { &mut *ptr.cast() }
}
