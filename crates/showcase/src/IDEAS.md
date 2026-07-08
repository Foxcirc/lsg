
```rust

use lsg::widget::{layout, shapes};
use lsg::common::rel;

async fn handle(app: Arc<lsg::App>) {

    let window = lsg::Window::new(&app)?;

    let widget = layout::Cols::new([
        (rel(5000), shapes::Rect::colored((130, 0, 0, 255))),
        (rel(5000), shapes::Rect::colored((50, 0, 100, 255)))
    ]);

    window.content(Arc::new(widget));

    app.connect(&window, Window::closed, async move |(app, ..)| {
        app.quit();
    });
    
    let (app2, window2) = (clone(app), clone(window));
    app.connect(window.closed(), async move |ev| {
        // use ev, app2, window2
    });
    
    connect!({app, window}, window.closed(), async move |ev| {
        // use app, window
    });
    
    let (app2, window2) = (clone(app), clone(window));
    app.spawn(async move {
        let listener = window2.closed();
        loop {
            let ev = listener.next().await;
            // use app2, window2, ev
        }
    });
    
    let (app2, window2) = (clone(app), clone(window));
    app.spawn(async move {
        window2.closed().handle(async move |ev| {
            // use app2, window2, ev
        });
    });
        
    app.quitted().next().await;

}

```
