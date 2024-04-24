
# Requirements for a new backend
[x] Open and configure windows
[x] Handling events
[x] User events
[x] Correct keyboard mapping
[x] Key repeat
[x] Special text input mode & Dead keys
[ ] IME (eg. wayland text_intput_v3)
[ ] Accessibility features
[x] Window transparency
[x] Window levels (always-on-top, always-on-bottom)
[x] Multiple windows
[x] Pop-Up windows (over screen)
[x] Drag 'n drop (TODO: still needs some testing and refinement)
[x] Monitor handling
[ ] IO Device events (eg. when you plug in a mouse to a tablet)
[x] Fullscreen (no custom video modes for now)
[x] Fractional scaling
[ ] Buffer transforms (eg. screen flip)
[ ] Dpi handling / Logical Scaling (this is fucking hard) (TODO: test + refine)
[x] enabling or disabling server side decorations (hyprland forces server-side decorations)
[x] OpenGL context creation (EGL)
[x] Damage areas (OH YEAH BABY)
[ ] Rendering to a texture/pixmap
[ ] Full Async
[x] Continious Redraw + Vsync
[x] Handling things like sigterm as the Quit event
[x] Request user attention
[ ] Changing/hiding the cursor (CursorIcon::None to hide)
[ ] Clean up & TODO's

# Other platform features
[ ] Clipboard
[ ] Notifications (using libnotify DBUS interface OR portal)
[ ] System tray (using kde StatusNotifier OR portal)
[ ] File manager (using ??? or portal)
[ ] Screen saver (idle inhibition)

# Platforms that will be supported
- Linux (Wayland only)
- Windows
- Android

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

