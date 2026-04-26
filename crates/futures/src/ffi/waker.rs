
//! We export functionality to create a Rust `Waker` from C,
//! which is mainly used to call the `event_loop_poll` function.

use std::{ptr, sync::Arc, task};

use super::types;

const VTABLE: task::RawWakerVTable = task::RawWakerVTable::new(
    |ptr| {
        let new = unsafe { waker_clone_inner(ptr.cast()) };
        task::RawWaker::new(new.cast(), &VTABLE)
    },
    |ptr| {
        unsafe { waker_wake_inner(ptr.cast()); }
        unsafe { waker_drop_inner(ptr.cast()); }
        // TODO: test if this behaviour is OK or if RawWaker.drop is called always (double-free?)
    },
    |ptr| unsafe { waker_wake_inner(ptr.cast()) },
    |ptr| unsafe { waker_drop_inner(ptr.cast()) },
);

unsafe fn waker_clone_inner(waker0: *const types::ExternWaker) -> *const types::ExternWaker {
    unsafe { Arc::increment_strong_count(waker0) };
    waker0
}

unsafe fn waker_drop_inner(waker0: *const types::ExternWaker) {
    let inner = unsafe { Arc::from_raw(waker0) };
    let vtable = unsafe { &*inner.vtable };
    if Arc::strong_count(&inner) == 1 {
        // SAFETY:
        // This is safe because we know we have
        // the **only** reference that is alive.
        unsafe { (vtable.drop)(inner.state) }
    }
}

unsafe fn waker_wake_inner(waker0: *const types::ExternWaker) {
    let inner = unsafe { &*waker0 };
    let vtable = unsafe { &*inner.vtable };
    unsafe { (vtable.wake)(inner.state) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_build(waker: types::ExternWaker) -> types::InternalWaker {
    let ptr = Arc::into_raw(Arc::new(waker));
    types::InternalWaker {
        state: ptr.cast(),
        vtable: ptr::from_ref(&VTABLE).cast(),
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_clone(waker0: *const types::InternalWaker) -> types::InternalWaker {
    let waker  = unsafe { &*waker0 };
    let new = unsafe { waker_clone_inner(waker.state.cast()) };
    types::InternalWaker {
        state: new.cast(),
        vtable: ptr::from_ref(&VTABLE).cast()
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_wake(waker0: *const types::InternalWaker) {
    let waker = unsafe { &*waker0 };
    unsafe { waker_wake_inner(waker.state.cast()) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_drop(waker: types::InternalWaker) {
    unsafe { waker_drop_inner(waker.state.cast()) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_equal(
    lhs0: *const types::InternalWaker,
    rhs0: *const types::InternalWaker
) -> bool {
    let lhs = unsafe { &*lhs0 };
    let rhs = unsafe { &*rhs0 };
    lhs.state == rhs.state &&
    lhs.vtable == rhs.vtable
}
