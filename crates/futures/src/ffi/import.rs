
pub mod definitions {

    use std::ffi::c_void as void;
    use crate::ffi::types::Waker;

    #[repr(C)]
    pub enum PollResult {
        Pending,
        Ready
    }

    pub type PollHandler = unsafe extern "C" fn(fut: *mut void, waker: Waker) -> PollResult;

    unsafe extern "C" {
        fn spawn(fut: *mut void, handler: PollHandler);
    }

}

pub mod implementation {

    use std::{ffi::c_void as void, pin::Pin};
    use super::definitions::*;

    pub fn block<F: Future>(fut: F) -> F::Output {

        let pinned = Box::pin(fut);
        let handler = HandlerForFuture::new(pinned);

        todo!()

    }

    struct HandlerForFuture<F> {
        pinned: Pin<Box<F>>,
    }

    impl<F> HandlerForFuture<F> {

        pub fn new(pinned: Pin<Box<F>>) -> Self {
            Self { pinned }
        }

        pub unsafe extern "C" fn handler(fut0: *mut void, state0: *mut void) -> PollResult {
            let fut = unsafe { &mut *(fut0 as *mut F) };
            // let state =
            todo!()
        }

    }

}
