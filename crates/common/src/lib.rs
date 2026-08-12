
pub mod math;
pub mod desktop;
pub mod render;

pub use math::*;
pub use desktop::*;
pub use render::*;

use std::{ffi::c_void as void, fmt, iter, mem::ManuallyDrop, ops::{self, Range}, sync::{Mutex, MutexGuard}};

#[derive(Debug, Default)]
pub struct SmartMutex<T> {
    inner: Mutex<T>,
}

impl<T> SmartMutex<T> {

    pub const fn new(inner: T) -> Self {
        Self { inner: Mutex::new(inner) }
    }

    #[track_caller]
    pub fn lock<'s>(&'s self) -> MutexGuard<'s, T> {
        self.inner.lock().expect("Mutex was poisoned.")
    }

    #[track_caller]
    pub fn with<F, R>(&self, f: F) -> R
        where F: FnOnce(&mut T) -> R {

        f(&mut *self.lock())

    }

    #[track_caller]
    pub fn set(&self, val: T) {
        *self.lock() = val;
    }

}
