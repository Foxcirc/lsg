
//! This workspace crate contains code to render curved shapes on the CPU + GPU.

#[cfg(any(target_os = "windows", target_os = "linux", target_os = "android"))] pub mod egl;
#[cfg(any(target_os = "windows", target_os = "linux", target_os = "android"))] pub use egl::*;

pub(crate) mod shaper;
pub(crate) use shaper::*;


pub static mut SHAPE_TAKE_PART: usize = 0;
