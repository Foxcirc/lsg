
use std::ffi::c_void as void;

#[repr(C)]
pub struct Waker {
    pub state:  *const void,
    pub vtable: *const WakerVTable
}

#[repr(C)]
pub struct WakerVTable {
    pub wake: unsafe extern "C" fn(state: *const void),
    pub drop: unsafe extern "C" fn(state: *const void)
}
