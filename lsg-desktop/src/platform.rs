
#[cfg(target_os = "linux")] mod wayland; // windowing, clipboard
#[cfg(target_os = "linux")] mod dbus; // notifications, system tray
#[cfg(target_os = "linux")] pub(crate) use wayland::*;
#[cfg(target_os = "linux")] pub(crate) use dbus::*;

#[cfg(target_os = "windows")] mod windows;
#[cfg(target_os = "windows")] pub(crate) use windows::*;
