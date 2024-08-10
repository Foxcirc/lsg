
//! This workspace crate contains code to render using OpenGL.

#[cfg(any(target_os = "windows", target_os = "linux", target_os = "android"))] pub mod egl;
#[cfg(any(target_os = "windows", target_os = "linux", target_os = "android"))] pub use egl::*;

pub(crate) mod delauney;
pub(crate) use delauney::*;
