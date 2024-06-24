
# Requirements for a new backend
[x] Open and configure windows
[x] Handling events
[x] User events
[x] Correct keyboard mapping
[x] Key repeat
[x] Special text input mode & Dead keys
[ ] IME client (eg. wayland text_intput_v3)
[ ] IME server
[ ] Accessibility features (same as IME on wayland)
[x] Window transparency
[x] Window levels (always-on-top, always-on-bottom)
[x] Multiple windows
[x] Pop-Up windows (over screen)
[x] Drag 'n drop
[ ] In-app Drag 'n drop
[x] Monitor handling
[ ] IO Device events (eg. when you plug in a mouse to a tablet)
[x] Fullscreen (no custom video modes for now)
[x] Fractional scaling
[ ] Dpi handling / Logical Scaling (this is fucking hard) (TODO: test + refine)
[x] enabling or disabling server side decorations (hyprland forces server-side decorations)
[x] OpenGL context creation (EGL)
[x] Damage areas (OH YEAH BABY)
[x] Rendering to a texture/pixmap
[x] Full Async (run() returns a future which should be run on the main thread)
[x] Continious Redraw + Vsync
[x] Handling things like sigterm as the Quit event
[x] Request user attention
[x] Changing/hiding the cursor (CursorIcon::None to hide)
[x] Cross platform structure
[ ] Clean up & TODO's

# Other platform features
[x] Clipboard
[ ] Notifications (using libnotify DBUS interface OR xdp.Notification)
[ ] System tray (using kde StatusNotifier OR xdp.TrayIcon)
[ ] File manager (using ??? or xdp)
[ ] Screen saver (idle inhibition)

# Egl todo's
[ ] "platform" window support on egl >= 1.5
[ ] better errors
[ ] pbuffer from clientbuffer?
[x] pixmaps (or pbuffers?)

# Platforms that will be supported
[x] Linux (Wayland only)
[ ] Windows
[ ] Android

# Let's fucking go!

```
button.set_title("...");
button.title("...");
button.lock().title = "..." // <= this is also kinda nice
button.title = "..."; button.updt();
button.updt(|it| it.title = "...");
button.title.set("xxx"); // <= this is it

--------------------

button.title.get();
button.title.set("...");

type Ws = WidgetState;

struct Button {
	title: Ws<String>,
}

impl Button {
	fn new() -> Arc<Self> { ... }
}

--------------------

SimpleWidget -> GroupWidget -> FullWidget
```

