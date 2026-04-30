
pub mod definitions {

    use std::ffi::c_void as void;
    use crate::ffi::types::*;

    unsafe extern "C" {
        pub fn spawn(fut: *mut void, vtable: FutureVTable);
    }

}

pub mod implementation {

    unsafe extern "C" {
        fn logs(it: *const i8);
    }

    use std::{ffi::c_void as void, marker::PhantomData, pin::Pin, task};

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

            unsafe { logs(c"vale".as_ptr()); }

            let cloned0 = unsafe { waker::waker_clone(waker0) };

            unsafe { logs(c"cloned".as_ptr()); }

            let result = Future::poll(
                unsafe { Pin::new_unchecked(&mut *(fut0 as *mut F)) },
                &mut task::Context::from_waker(&task::Waker::from(cloned0))
                //           `drop` will be called by the `Waker` ^^^^^^^
            );

            match result {
                task::Poll::Ready(()) => types::PollResult::Ready,
                task::Poll::Pending   => types::PollResult::Pending
            }

        }

        pub unsafe extern "C" fn drop(fut0: *mut void) {
            unsafe { logs(c"DROP CALLED".as_ptr()); }
            unsafe { drop(Box::from_raw(fut0 as *mut F)) };
        }

    }

}
