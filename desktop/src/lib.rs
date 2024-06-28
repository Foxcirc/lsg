
pub mod shared;
pub use shared::*;

#[cfg(target_os = "linux")] pub mod wayland;
#[cfg(target_os = "linux")] pub use wayland::*;

// #[cfg(target_os = "windows")] pub mod wayland;
