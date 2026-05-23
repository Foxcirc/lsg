
use std::ffi::c_void as void;
use std::ptr::null_mut;
use std::task::Poll;

use desktop::ffi::import::definitions as defs;
use desktop::ffi::types as types;
use futures::ffi::types::InternalWaker;

unsafe extern "C" {
    fn logs(it: *const i8);
}

fn main() {
    run()
}

#[unsafe(no_mangle)]
extern "C" fn run() {

    unsafe { logs(c"runn called".as_ptr())};

    let cfg0 = types::EventLoopConfig { appid: c"blablabla".as_ptr(), intercept: false };

    unsafe { defs::event_loop_run(cfg0, handler0, null_mut()) };

    // desktop::EventLoop::run(desktop::EvlConfig::default(), move |evl| {


    // }).unwrap();

    // let cfg = types::EventLoopConfig { appid: c"showcase-of-my-life".as_ptr(), intercept: false };

    // unsafe { defs::event_loop_run(cfg, handler, null_mut()) };

}

extern "C" fn handler0(evl0: *const types::EventLoop, state0: *mut void) {


    use futures::ffi::import::implementation as fexeci;

    fexeci::spawn(std::future::poll_fn(move |_cx| {
        println!("should run every sec!");
        let iwaker0 = InternalWaker::from(_cx.waker().clone());
        unsafe { defs::event_loop_poll(evl0, &iwaker0, null_mut(), null_mut()) };
        Poll::Pending
    }));

}

//     use futures::ffi::waker as fwaker;
//     use futures::ffi::types as ftypes;

//     extern "C" fn wake_handler(state: *const void) {
//         unsafe { logs(c"wake handler called".as_ptr()) };
//     }

//     extern "C" fn drop_handler(state: *const void) {
//         unsafe { logs(c"drop handler called".as_ptr()) };
//     }

//     let waker = ftypes::ExternWaker {
//         state: null_mut(),
//         vtable: &ftypes::WakerVTable {
//             wake: wake_handler,
//             drop: drop_handler,
//         },
//     };

//     let iwaker = unsafe { fwaker::waker_build(waker) };

//     unsafe { defs::event_loop_poll(evl0, &iwaker, null_mut(), null_mut()) };

//     unsafe { fwaker::waker_drop(iwaker) }; //

// }
