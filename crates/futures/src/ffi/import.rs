
pub mod definitions {

    use std::ffi::c_void as void;
    use crate::ffi::types::*;

    unsafe extern "C" {
        pub fn spawn(fut: *mut void, vtable: FutureVtable);
    }

}

pub mod implementation {

    use std::{ffi::c_void as void, marker::PhantomData, pin::Pin, task};

    use crate::ffi::{import::definitions, types};

    pub fn spawn<F: Future<Output = ()> + 'static>(fut: F) {

        let ptr = Box::into_raw(Box::new(fut));

        let vtable = types::FutureVtable {
            poll: SpawnState::<F>::poll,
            drop: SpawnState::<F>::drop,
        };

        unsafe { definitions::spawn(ptr.cast(), vtable) };

    }

    struct SpawnState<F: Future<Output = ()> + 'static>(PhantomData<F>);

    impl<F: Future<Output = ()> + 'static> SpawnState<F> {

        pub unsafe extern "C" fn poll(fut0: *mut void, waker0: types::InternalWaker) -> types::PollResult {

            let result = Future::poll(
                unsafe { Pin::new_unchecked(&mut *(fut0 as *mut F)) },
                &mut task::Context::from_waker(&task::Waker::from(waker0))
            );

            match result {
                task::Poll::Ready(()) => types::PollResult::Ready,
                task::Poll::Pending   => types::PollResult::Pending
            }

        }

        pub unsafe extern "C" fn drop(fut0: *mut void) {
            let fut = unsafe { Box::from_raw(fut0 as *mut F) };
            drop(fut);
        }

    }

}
