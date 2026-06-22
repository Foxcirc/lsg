
//! This workspace crate contains code to render curved shapes on the CPU + GPU.

pub mod gl;
pub use gl::*;

pub mod shaper;
pub use shaper::*;
