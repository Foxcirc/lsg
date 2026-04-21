
//! When running in the browser, we need some extra
//! helper functions to make the glue-layer work.

use std::ffi::c_void as void;
use crate::ffi::types::WakerVTable;

unsafe extern "C" {
    fn waker_wake(state: *const void);
    fn waker_drop(state: *const void);
}

const VTABLE: WakerVTable = WakerVTable {
    wake: waker_wake, drop: waker_drop
};

#[unsafe(no_mangle)]
pub extern "C" fn get_browser_vtable() -> *const WakerVTable {
    &VTABLE
}
