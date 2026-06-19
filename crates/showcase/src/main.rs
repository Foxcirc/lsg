
// use std::ffi::c_void as void;
// use std::ptr::null_mut;
// use std::sync::Arc;
// use std::task::Poll;

// use desktop::ffi::import::definitions as defs;
// use desktop::ffi::types as types;

unsafe extern "C" {
    fn logs(it: *const i8);
}

fn log(it: &str) {
    let mut buf = [0u8; 2048];
    buf[..it.len()].copy_from_slice(it.as_bytes());
    assert!(it.len() != 2048);
    unsafe { logs(buf.as_ptr().cast()) }
}

fn main() {
    run()
}

#[unsafe(no_mangle)]
extern "C" fn run() {

    use futures::ffi::import::implementation as fexeci;

    log("Run called.");

    desktop::EventLoop::run(desktop::EvlConfig::default(), move |evl| {

        log("WASM handler called.");

        fexeci::spawn(async move {
            log("Future first polled. Creating window...");

            #[allow(unused)]
            let window = desktop::Window::new(&evl);

            loop {
                log("Waiting for next event...");
                let event = evl.next().await.expect("Next event.");
                log(&format!("{event:?}"));
            }
        });

    }).expect("Run event loop.");

}

// extern "C" fn handler0(evl0: *const types::EventLoop, state0: *mut void) {

//     fexeci::spawn(std::future::poll_fn(move |cx| {
//         println!("should run every sec!");
//         let waker0 = Arc::new(cx.waker().clone());
//         unsafe { defs::event_loop_poll(
//             evl0,
//             Arc::as_ptr(&waker0).cast(),
//             null_mut(),
//             null_mut()
//         ) };
//         Poll::Pending
//     }));

// }

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
