
#![allow(unexpected_cfgs)]

pub mod ffi;

// #[cfg(target_family = "wasm")] pub mod ffi;
// #[cfg(target_family = "wasm")] use ffi as backend;

pub mod browser;

// pub fn block<F: Future>(fut: F) -> F::Output {
//     backend::block(fut)
// }
