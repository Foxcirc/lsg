
use std::{ffi::c_void as void, sync::Arc};

use desktop::*;
use common::*;

const APPID: &str = file!();

fn main() {}

// #[unsafe(no_mangle)]
// fn run() -> Result<(), Box<dyn std::error::Error>> {
//     EventLoop::run(EvlConfig {
//         appid: APPID.into(),
//         intercept: false,
//     }, app)?
// }

// fn app(evl: Arc<EventLoop>) -> Result<(), Box<dyn std::error::Error>> {

//     todo!()

// }

use desktop::ffi::{types, import::definitions::*};

#[unsafe(no_mangle)]
fn run() {

    let config0 = types::EventLoopConfig {
        appid: b"what the rusty crusty dog doin\n\0".as_ptr().cast(),
        intercept: true,
    };

    unsafe { event_loop_run(config0, handler0, 69 as *mut void) };

}

unsafe extern "C" {
    unsafe fn logs(s: *const i8);
}

fn log(s: &str) {
    unsafe { logs(s.as_bytes().as_ptr().cast()) }
}

extern "C" fn handler0(evl: *const types::EventLoop, state: *mut void) {
    log(&format!("state: {:?}", state as usize));
}
