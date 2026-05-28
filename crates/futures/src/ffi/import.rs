
pub mod definitions {

    use std::ffi::c_void as void;
    use crate::ffi::types::*;

    unsafe extern "C" {
        pub fn spawn(fut: *mut void, vtable: FutureVTable);
    }

}

pub mod implementation {

    use std::{ffi::c_void as void, marker::PhantomData, pin::Pin, sync::Arc, task};

    use crate::ffi::{import::definitions, types, waker};

    pub fn spawn<F: Future<Output = ()> + 'static>(fut: F) {

        let ptr = Box::into_raw(Box::new(fut));

        let vtable = types::FutureVTable {
            poll: SpawnState::<F>::poll,
            drop: SpawnState::<F>::drop,
        };

        unsafe { definitions::spawn(ptr.cast(), vtable) };

    }

    struct SpawnState<F: Future<Output = ()> + 'static>(PhantomData<F>);

    impl<F: Future<Output = ()> + 'static> SpawnState<F> {

        pub unsafe extern "C" fn poll(fut0: *mut void, waker0: *const types::InternalWaker) -> types::PollResult {

            let cloned0 = unsafe { waker::waker_clone(waker0) };
            let waker = unsafe { Arc::from_raw(cloned0 as *const task::Waker) }; // SAFETY: Unstable-Waker-FFI

            let result = Future::poll(
                unsafe { Pin::new_unchecked(&mut *(fut0 as *mut F)) },
                &mut task::Context::from_waker(&waker)
            );

            match result {
                task::Poll::Ready(()) => types::PollResult::Ready,
                task::Poll::Pending   => types::PollResult::Pending
            }

        }

        pub unsafe extern "C" fn drop(fut0: *mut void) {
            unsafe { drop(Box::from_raw(fut0 as *mut F)) };
        }

    }

}
