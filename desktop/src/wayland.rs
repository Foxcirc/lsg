
// the main logic: windowing, egl, ...
pub mod window;
pub use window::*;

// needed for notifications, status icons, ...
pub mod dbus;
pub use dbus::*;
