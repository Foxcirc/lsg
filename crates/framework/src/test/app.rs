
use std::sync::Arc;
use crate::*;

#[test]
fn app() -> Result<(), desktop::EvlError> {
    App::run(handler, Config { appid: file!().into() })
}

async fn handler(app: Arc<App>) {

    let window = Window::new(&app);
    window.resize(LogicalSize::new(500, 500));

    // app.spawn(async move {
    //     window.closed().next().await;
    //     app2.quit();
    // });

    app.connect(&window, Window::closed, async move |app, ()| {
        app.quit();
    });

    // window.content(widget);

    app.quitted().next().await;

}
