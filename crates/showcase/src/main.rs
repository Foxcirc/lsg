
use std::ffi::c_void as void;
use std::ptr::null_mut;
use std::task::Poll;

// use desktop::ffi::import::definitions as defs;
// use desktop::ffi::types as types;

unsafe extern "C" {
    fn logs(it: *const i8);
}

fn main() {
    run()
}

#[unsafe(no_mangle)]
extern "C" fn run() {

    unsafe { logs(c"runn called".as_ptr())};

    // let cfg = types::EventLoopConfig { appid: c"showcase-of-my-life".as_ptr(), intercept: false };

    // unsafe { defs::event_loop_run(cfg, handler, null_mut()) };

    use futures::ffi::import::implementation as fexeci;

    let mut count = 0;

    fexeci::spawn(std::future::poll_fn(move |_cx| {
        count += 1;
        let msg = std::ffi::CString::new(format!("from rust, count is {}", count)).unwrap();
        unsafe { logs(msg.as_ptr()) };
        _cx.waker().wake_by_ref();
        unsafe { logs(c"poll called wake".as_ptr()); }
        if count == 10 {
            Poll::Ready(())
        } else {
            Poll::Pending
        }
    }));

}

/*

extern "C" fn handler(evl0: *const types::EventLoop, state0: *mut void) {

    use futures::ffi::waker as fwaker;
    use futures::ffi::types as ftypes;

    extern "C" fn wake_handler(state: *const void) {
        unsafe { logs(c"wake handler called".as_ptr()) };
    }

    extern "C" fn drop_handler(state: *const void) {
        unsafe { logs(c"drop handler called".as_ptr()) };
    }

    let waker = ftypes::ExternWaker {
        state: null_mut(),
        vtable: &ftypes::WakerVTable {
            wake: wake_handler,
            drop: drop_handler,
        },
    };

    let iwaker = unsafe { fwaker::waker_build(waker) };

    unsafe { defs::event_loop_poll(evl0, &iwaker, null_mut(), null_mut()) };

    unsafe { fwaker::waker_drop(iwaker) }; //

}
*/
