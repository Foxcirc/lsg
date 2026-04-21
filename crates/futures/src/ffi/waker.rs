
//! We export functionality to create a Rust `Waker` from C,
//! which is mainly used to call the `event_loop_poll` function.

use std::{mem::ManuallyDrop, sync::Arc, task};

use super::types;

const VTABLE: task::RawWakerVTable = task::RawWakerVTable::new(
    |ptr| {
        let orig = unsafe { get_state_arc(ptr) };
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
        let mut orig = unsafe { get_state_arc(ptr) };
        let vtable = unsafe { get_vtable(orig.vtable) };
        unsafe { (vtable.drop)(orig.state) };
        unsafe { ManuallyDrop::drop(&mut orig) };
    }
);

unsafe fn get_vtable<'s>(ptr: *const types::WakerVTable) -> &'s types::WakerVTable {
    unsafe { &*ptr }
}

unsafe fn get_state<'s>(ptr: *const ()) -> &'s types::Waker {
    unsafe { &*ptr.cast() }
}

unsafe fn get_state_arc(ptr: *const ()) -> ManuallyDrop<Arc<types::Waker>>{
    unsafe { ManuallyDrop::new(Arc::from_raw(ptr.cast())) }
}

pub fn into_rust_waker(waker: types::Waker) -> task::Waker {
    let ptr = Arc::into_raw(Arc::new(waker)).cast();
    unsafe { task::Waker::new(ptr, &VTABLE) }
}

#[unsafe(no_mangle)]
pub extern "C" fn waker_wake(borrowed: *const types::Waker) {
    let waker  = unsafe { get_state  (borrowed.cast()) };
    let vtable = unsafe { get_vtable (waker.vtable) };
    unsafe { (vtable.wake)(waker.state) }
}

#[unsafe(no_mangle)]
pub extern "C" fn waker_drop(borrowed: *const types::Waker) {
    let waker  = unsafe { get_state  (borrowed.cast()) };
    let vtable = unsafe { get_vtable (waker.vtable) };
    unsafe { (vtable.drop)(waker.state) }
}
