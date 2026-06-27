
// #[cfg(not(target_family = "wasm"))] mod app;
// #[cfg(not(target_family = "wasm"))] mod interactive;
// #[cfg(not(target_family = "wasm"))] mod svgs;
// #[cfg(not(target_family = "wasm"))] mod atlas;

#[cfg(target_family = "wasm")]
mod wasm;
