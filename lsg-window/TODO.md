
# Requirements for a new backend
[x] Open and configure windows
[x] Handling events
[x] User events
[x] Correct keyboard mapping
[x] Key repeat
[x] Special text input mode & Dead keys
[ ] IME
[x] Window transparency
[ ] Drag 'n drop
[x] Fullscreen (no custom video modes for now)
[ ] Dpi handling / Logical Scaling (this is fucking hard)
[x] OpenGL context creation (EGL)
[x] Damage areas (OH YEAH BABY)
[ ] Full Async
[x] Continious Redraw + Vsync
[ ] Handling things like sigterm as the Quit event
[ ] Request user attention
[ ] Clean up & TODO's

# Other platform features
[ ] Clipboard
[ ] Notifications
[ ] System tray
[ ] File manager
[ ] Screen saver (idle inhibition)

# Platforms that will be supported
- Linux (Wayland is the main focus)
- Windows
- Android

# Let's fucking go!

```
button.set_title("...");
button.title("...");
button.lock().title = "..."
button.title = "..."; button.updt();
button.updt(|it| it.title = "...");

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

-------------------

IN BETWEEN FRAMES EVENT

--------------------

SimpleWidget -> GroupWidget -> FullWidget
```

