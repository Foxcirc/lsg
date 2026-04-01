
//! This crate contains a C-ABI compatible API which
//! makes this crate usable as a library from other languages.

use core::task;
use std::{ffi::{CStr, c_void as void}, mem, ptr::null, sync::{Arc, Mutex}};

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

    let config = desktop::EventLoopConfig {
        appid,
        intercept: config0.intercept,
    };

    let result = desktop::EventLoop::run(config, |evl| {
        let ptr: *const Arc<desktop::EventLoop> = &evl;
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

#[repr(C)]
pub struct EvlPollContext {
    /// Ownership: &'shared
    pub waker: *const EvlPollWaker,
}

#[repr(C)]
pub struct EvlPollWakerRust {
    pub state: *const void,
    pub vtable: *const void
}

#[repr(C)]
pub struct EvlPollWaker {
    /// Ownership: &'shared
    pub state: *const void,
    /// Ownership: &'static
    pub vtable: *const EvlPollWakerVTable
}

unsafe impl Send for EvlPollWaker {}

#[repr(C)]
pub struct EvlPollWakerVTable {
    pub clone: extern "C" fn(*const void) -> *mut EvlPollWaker,
    pub wake: extern "C" fn(*const void),
    pub wake_by_ref: extern "C" fn(*const void),
    pub drop: extern "C" fn(*const void)
}

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
pub enum Poll {
    Ready,
    Pending,
}

pub unsafe extern "C" fn event_loop_poll_rust(this0: *const SharedEventLoop, rawcx: EvlPollContextRust, state: *mut void) -> Poll {

    let this: &Arc<desktop::EventLoop> = unsafe { &*this0.cast() };

    let waker = unsafe { task::Waker::new(
        rawcx.waker.state.cast(),
        &*rawcx.waker.vtable.cast()
    ) };

    let cx = task::Context::from_waker(&waker);

    event_loop_poll_inner(this, cx)

}

// pub unsafe extern "C" fn event_loop_poll(this0: *const SharedEventLoop, rawcx: EvlPollContext, state: *mut void) {

//     let this: &Arc<desktop::EventLoop> = unsafe { &*this0.cast() };

//     let waker = unsafe { task::Waker::new(rawcx.waker.cast(), &VTABLE) };
//     let cx = task::Context::from_waker(&waker);

//     event_loop_poll_inner(this, cx);

// }

fn event_loop_poll_inner(this: &Arc<desktop::EventLoop>, mut cx: task::Context) -> Poll {

    let poll = this.poll(&mut cx);

    if let task::Poll::Ready(result) = poll {
        eprintln!("{:?}", result);
        Poll::Ready
    } else {
        Poll::Pending
    }

}

pub unsafe extern "C" fn event_loop_suspend(this0: *const SharedEventLoop) {
    let this: &Arc<desktop::EventLoop> = unsafe { &*this0.cast() };
    this.suspend();
}

pub unsafe extern "C" fn event_loop_resume(this0: *const SharedEventLoop) {
    let this: &Arc<desktop::EventLoop> = unsafe { &*this0.cast() };
    this.resume();
}

pub unsafe extern "C" fn event_loop_quit(this0: *const SharedEventLoop) {
    let this: &Arc<desktop::EventLoop> = unsafe { &*this0.cast() };
    this.quit();
}

pub unsafe extern "C" fn event_loop_display_ptr(this0: *const SharedEventLoop) -> *const void {
    let this: &Arc<desktop::EventLoop> = unsafe { &*this0.cast() };
    this.ptr()
}
