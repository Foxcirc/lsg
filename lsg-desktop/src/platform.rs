
#[cfg(target_os = "linux")] pub(super) mod wayland; // windowing, clipboard
#[cfg(target_os = "linux")] pub(super) mod dbus; // notifications, system tray

#[cfg(target_os = "windows")] pub(super) mod windows;
