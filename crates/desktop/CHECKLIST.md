
TODO: what happens if i try to run wayland app without having a compositor launched?

# Checklist for a new Backend
[x] Handle events
[x] User events / Event loop proxy
[x] Open and manage windows
[x] Window transparency
[x] Multiple windows
[x] Window levels (eg. always-on-top)
[x] Correct keyboard handling
[x] Key repeat
[ ] IME client (eg. wayland_input_v3)
[ ] IME server
[ ] Accessibility features (same as IME on wayland)
[x] Drag-and-Drop
[ ] In-app Drag-and-Drop
[x] Monitor handling
[x] I/O Device events (eg. when a mouse is plugged in)
[x] Fullscreen
[x] Fractional scaling
[ ] DPI handling (logical vs. physical sizes)
[x] Managing (server-side) decorations
[x] Damage areas
[x] Rendering to a texture or pixel buffer
[x] Continous redraw + Vsync
[x] Handling quit-signals (eg. SIGTERM on linux)
[x] Request user attention
[ ] Window Icons
[x] Changing/Hiding the cursor + custom cursors
[x] Fully async + Correct threading
[x] OpenGL context creation
[x] Clean code

# Other platform features
[x] Clipboard
[ ] Notifications (using libnotify DBUS interface OR xdp.Notification)
[ ] System tray (using kde StatusNotifier OR xdp.TrayIcon)
[ ] File manager (using ??? or xdp)
[ ] Screen saver (+ idle inhibition)
[ ] Global menus (dbus-menu kde menu protocol)

# Let's fucking go!
