
#![allow(unexpected_cfgs)]

#[cfg(target_family = "wasm")] pub mod browser;
#[cfg(target_family = "wasm")] use browser as backend;

#[cfg(target_family = "wasm")] pub mod ffi;

#[cfg(lsp)] pub mod browser;
#[cfg(lsp)] use browser as backend;

#[cfg(lsp)] pub mod ffi;

pub fn block<F: Future>(fut: F) -> F::Output {
    backend::block(fut)
}
