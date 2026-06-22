
#[cfg(feature = "browser")] pub mod browser;

#[cfg(feature = "waker")]  pub mod ffi;
#[cfg(feature = "import")] pub mod ffi;
#[cfg(feature = "import")] pub use ffi::import as backend;

#[cfg(feature = "native")] pub mod native;
#[cfg(feature = "native")] pub use native as backend;

/// Spawns a future onto the browsers executor and returns immediatly.
///
/// In the browser there is no concept of blocking, rather the browser
/// owns the event loop and calls rust code when it receives new events
///
/// # Capturing/Dropping
/// Make sure to capture all necessary items in your future, since others will
/// eventually be dropped when your outer function returns control to the browser.
///
/// # Platform-Specific
/// This function is only available in the `browser`.
/// On `native` platforms you have to use the global `blockon` function.
#[cfg(feature = "browser")]
pub fn spawn<F: Future<Output = ()> + 'static>(fut: F) -> () {
    backend::spawn(fut)
}

/// Blocks and runs a future to completion.
///
/// # Platform-Specific
/// This function is only available on `native` platforms.
/// In the `browser` you have to use the global `spawn` function.
#[cfg(feature = "native")]
pub fn blockon<F: Future>(fut: F) -> F::Output {
    backend::blockon(fut)
}
