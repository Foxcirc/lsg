
//! When running in the browser, we need some extra
//! helper functions to make the glue-layer work.

use std::{ffi::c_void as void, ptr};
use crate::ffi::{types, waker::{self, waker_drop}};

unsafe extern "C" {
    fn waker_wake_browser(state: *const void);
    fn waker_drop_browser(state: *const void);
}

const VTABLE: types::WakerVTable = types::WakerVTable {
    wake: waker_wake_browser,
    drop: waker_drop_browser
};

#[unsafe(no_mangle)]
pub extern "C" fn waker_new_browser(state: *const void) -> types::InternalWaker {
    unsafe { waker::waker_build(types::ExternWaker { state, vtable: &VTABLE }) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_clone_heap(brwd: *const types::InternalWaker) -> *const types::InternalWaker {
    let cloned = unsafe { waker::waker_clone(brwd) };
    Box::into_raw(Box::new(cloned))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_drop_heap(owned: *const types::InternalWaker) {
    unsafe { waker_drop(ptr::read(owned)); }
    unsafe { drop(Box::from_raw(owned.cast_mut())) }
}
