
//! We export functionality to create a Rust `Waker` from C,
//! which is mainly used to call the `event_loop_poll` function.

use std::{ptr, sync::Arc, task};

use super::types;

const VTABLE: task::RawWakerVTable = task::RawWakerVTable::new(
    |ptr| {
        let orig = unsafe { get_state_clone_arc(ptr) };
        let cloned = Arc::into_raw(Arc::clone(&orig));
        task::RawWaker::new(cloned.cast(), &VTABLE)
    },
    |ptr| {
        let orig = unsafe { get_state(ptr) };
        let vtable = unsafe { get_vtable(orig.vtable) };
        unsafe { (vtable.wake)(orig.state) };
    },
    |ptr| {
        let orig = unsafe { get_state(ptr) };
        let vtable = unsafe { get_vtable(orig.vtable) };
        unsafe { (vtable.wake)(orig.state) };
    },
    |ptr| {
        let orig = unsafe { get_state_consume_arc(ptr) };
        let vtable = unsafe { get_vtable(orig.vtable) };
        unsafe { (vtable.drop)(orig.state) };
    }
);

unsafe fn get_vtable<'s>(ptr: *const types::WakerVTable) -> &'s types::WakerVTable {
    unsafe { &*ptr }
}

unsafe fn get_state<'s>(ptr: *const ()) -> &'s types::ExternWaker {
    unsafe { &*ptr.cast() }
}

unsafe fn get_state_clone_arc(ptr0: *const ()) -> Arc<types::ExternWaker>{
    let typed0 = ptr0 as *const types::ExternWaker;
    unsafe { Arc::increment_strong_count(typed0) };
    unsafe { Arc::from_raw(typed0) }
}

unsafe fn get_state_consume_arc(ptr0: *const ()) -> Arc<types::ExternWaker>{
    let typed0 = ptr0 as *const types::ExternWaker;
    unsafe { Arc::from_raw(typed0) }
}

#[unsafe(no_mangle)]
pub extern "C" fn waker_build(waker: types::ExternWaker) -> types::InternalWaker {
    let ptr = Arc::into_raw(Arc::new(waker));
    types::InternalWaker {
        state: ptr.cast(),
        vtable: ptr::from_ref(&VTABLE).cast(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn waker_clone(waker0: *const types::InternalWaker) -> types::InternalWaker {
    let waker  = unsafe { &*waker0 };
    let cloned   = unsafe { get_state_clone_arc(waker.state.cast()) };
    let ptr = Arc::into_raw(cloned);
    types::InternalWaker {
        state: ptr.cast(),
        vtable: waker.vtable,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn waker_wake(waker0: *const types::InternalWaker) {
    let waker = unsafe { &*waker0 };
    let inner  = unsafe { get_state  (waker.state.cast()) };
    let vtable = unsafe { get_vtable (inner.vtable) };
    unsafe { (vtable.wake)(inner.state) }
}

#[unsafe(no_mangle)]
pub extern "C" fn waker_drop(waker0: *const types::InternalWaker) {
    let waker = unsafe { &*waker0 };
    let inner  = unsafe { get_state  (waker.state.cast()) };
    let vtable = unsafe { get_vtable (inner.vtable) };
    unsafe { (vtable.drop)(inner.state) }
}
