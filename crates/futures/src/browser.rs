
//! When running in the browser, we need some extra
//! helper functions to make the glue-layer work.

use std::{ffi::c_void as void, ptr};
use crate::ffi::{types, waker::{self, waker_drop}};

unsafe extern "C" {
    pub fn waker_wake_browser_handler(state: *const void);
    pub fn waker_drop_browser_handler(state: *const void);
}

unsafe extern "C" fn waker_wake_browser_handler_storable(state: *const void) { unsafe { waker_wake_browser_handler(state) } }
unsafe extern "C" fn waker_drop_browser_handler_storable(state: *const void) { unsafe { waker_drop_browser_handler(state) } }

const VTABLE: types::WakerVTable = types::WakerVTable {
    wake: waker_wake_browser_handler_storable,
    drop: waker_drop_browser_handler_storable
};

#[unsafe(no_mangle)]
pub extern "C" fn waker_new_browser(state: *const void) -> *const types::InternalWaker {
    let waker = unsafe { waker::waker_build(types::ExternWaker { state, vtable: &VTABLE }) };
    Box::into_raw(Box::new(waker))
}

// This is included just for consistency reasons. JS could also use `waker_wake` directly.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_wake_browser(waker0: *const types::InternalWaker) {
    unsafe { waker::waker_wake(waker0) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_drop_browser(owned: *const types::InternalWaker) {
    unsafe { waker_drop(ptr::read(owned)); }
    unsafe { drop(Box::from_raw(owned.cast_mut())) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn call_future_vtable_poll(
    ptr: types::FutureVTablePollHandler,
    fut: *mut void,
    waker: *const types::InternalWaker
) -> types::PollResult {
    unsafe { (ptr)(fut, waker) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn call_future_vtable_drop(
    ptr: types::FutureVTableDropHandler,
    fut: *mut void,
) {
    unsafe { (ptr)(fut) }
}
