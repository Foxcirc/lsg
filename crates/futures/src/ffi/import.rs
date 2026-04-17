
pub mod definitions {

    use std::ffi::c_void as void;

    #[repr(C)]
    pub enum PollResult2 {
        Pending,
        Ready
    }

    pub type PollHandler = unsafe extern "C" fn(fut: *mut void, state: *mut void) -> PollResult2;

    unsafe extern "C" {
        fn block(fut: *mut void, state: *mut void, handler: PollHandler);
    }

}

pub mod implementation {

    use std::{ffi::c_void as void, pin::Pin};
    use super::definitions::*;

    pub fn block<F: Future>(fut: F) -> F::Output {

        let state = HandlerState::new(fut);


    }

    struct HandlerState<F> {
        pinned: Pin<Box<F>>
    }

    impl<F> HandlerState<F> {

        pub fn new(fut: F) -> Self {
            Self { pinned: Box::pin(fut) }
        }

        pub unsafe extern "C" fn handler(fut0: *mut void, state0: *mut void) -> PollResult2 {
            let fut = unsafe { &mut *(fut0 as *mut F) };
        }

    }

}
