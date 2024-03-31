
# Requirements for a new backend
[x] Open and configure windows
[ ] Handling events
[x] User events
[ ] Correct keyboard mapping
[ ] Window transparency
[ ] Drag 'n drop
[ ] Monitor handling
[ ] Dpi handling / Logical Scaling
[x] OpenGL context creation (EGL)
[ ] Full Async
[ ] IME
[x] Continious Redraw + Vsync
[ ] Handling things like sigterm as the Quit event

# Outsources platform features
[ ] Clipboard
[ ] Notifications
[ ] System tray

# Platforms that will be supported
- Linux (Wayland only)
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

