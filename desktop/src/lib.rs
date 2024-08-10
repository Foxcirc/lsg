
//! # Overview
//! This crate is responsible for handling desktop environment specific stuff.
//! This includes:
//! 1. Windowing
//! 2. Notifications
//! 3. Status Icons

pub mod shared;
pub use shared::*;

#[cfg(target_os = "linux")] pub mod linux;
#[cfg(target_os = "linux")] pub use linux::*;
