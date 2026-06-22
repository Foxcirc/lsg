
//! Waker-Interop between FFI and Rust.
//!
//! # Filename
//! In the terminology used in the lsg project, this file is
//! essentially an `export.rs`, since it exports functions to be
//! called by extern code. However, this file is special, since it
//! is also needed by `import` backends, specifically to create or use
//! InternalWaker`. So it has a special name, even though it exports stuff.

use std::{sync::Arc, task};

use super::types;

const VTABLE: task::RawWakerVTable = task::RawWakerVTable::new(
    |ptr| {
        let new = unsafe { extern_waker_clone_inner(ptr.cast()) };
        task::RawWaker::new(new.cast(), &VTABLE)
    },
    |ptr| { unsafe { extern_waker_wake_inner(ptr.cast()); }
            unsafe { extern_waker_drop_inner(ptr.cast()); } },
    |ptr| unsafe { extern_waker_wake_inner(ptr.cast()) },
    |ptr| unsafe { extern_waker_drop_inner(ptr.cast()) },
);

unsafe fn extern_waker_clone_inner(waker0: *const types::ExternWaker) -> *const types::ExternWaker {
    unsafe { Arc::increment_strong_count(waker0) };
    waker0
}

unsafe fn extern_waker_drop_inner(waker0: *const types::ExternWaker) {
    let inner = unsafe { Arc::from_raw(waker0) };
    let vtable = unsafe { &*inner.vtable };
    if Arc::strong_count(&inner) == 1 {
        // SAFETY:
        // This is safe because we know we have
        // the **only** reference that is alive.
        unsafe { (vtable.drop)(inner.state) }
    }
}

unsafe fn extern_waker_wake_inner(waker0: *const types::ExternWaker) {
    let inner = unsafe { &*waker0 };
    let vtable = unsafe { &*inner.vtable };
    unsafe { (vtable.wake)(inner.state) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_build(waker0: types::ExternWaker) -> *const types::InternalWaker {
    let state = Arc::into_raw(Arc::new(waker0));
    let rwaker = unsafe { task::Waker::new(state.cast(), &VTABLE) };
    Arc::into_raw(Arc::new(rwaker)).cast()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_clone(waker0: *const types::InternalWaker) -> *const types::InternalWaker {
    unsafe { Arc::increment_strong_count(waker0 as *const task::Waker) };
    waker0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_wake(waker0: *const types::InternalWaker) {
    let waker = unsafe { &*(waker0 as *const task::Waker) };
    waker.wake_by_ref();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_drop(waker0: *const types::InternalWaker) {
    unsafe { Arc::decrement_strong_count(waker0 as *const task::Waker) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn waker_wake_same(
    lhs0: *const types::InternalWaker,
    rhs0: *const types::InternalWaker
) -> bool {
    lhs0 == rhs0
    // Not needed right now, but maybe for future use when deep-checking is needed:
    // ((lhs0.is_null() || rhs0.is_null()) && lhs0 != rhs0) || {
    //     let lhs = unsafe { &*lhs0 };
    //     let rhs = unsafe { &*rhs0 };
    //     lhs.state == rhs.state &&
    //     lhs.vtable == rhs.vtable
    // }
}
