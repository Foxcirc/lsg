
//! This workspace crate contains code to render using OpenGL.

#[cfg(any(target_os = "windows", target_os = "linux", target_os = "android"))] pub mod egl;
#[cfg(any(target_os = "windows", target_os = "linux", target_os = "android"))] pub use egl::*;

pub mod shared;
pub use shared::*;

pub mod transform;
pub use transform::*; // TODO: make pub(crate)
