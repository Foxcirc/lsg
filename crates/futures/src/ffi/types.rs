
use std::{task, ffi::c_void as void};

/// A type-erased waker which contains
/// pointer to valid `task::Waker` fields.
#[repr(C)]
pub struct InternalWaker {
    pub state: *const void,
    pub vtable: *const void
}

impl From<InternalWaker> for task::Waker {
    fn from(it: InternalWaker) -> Self {
        unsafe { Self::new(
            it.state.cast(),
            &*it.vtable.cast()
        ) }
    }
}

#[repr(C)]
pub struct ExternWaker {
    pub state:  *const void,
    pub vtable: *const WakerVTable
}

#[repr(C)]
pub struct WakerVTable {
    pub wake: unsafe extern "C" fn(state: *const void),
    pub drop: unsafe extern "C" fn(state: *const void)
}


#[repr(C)]
pub enum PollResult {
    Pending,
    Ready
}

#[repr(C)]
pub struct FutureVtable {
    pub poll: unsafe extern "C" fn(fut: *mut void, waker: InternalWaker) -> PollResult,
    pub drop: unsafe extern "C" fn(fut: *mut void)
}
