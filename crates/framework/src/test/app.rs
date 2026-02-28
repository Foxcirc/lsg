
use std::sync::Arc;
use crate::*;

#[test]
fn app() -> Result<(), desktop::EvlError> {
    App::run(handler, Config { appid: file!().into() })
}

async fn handler(app: Arc<App>) {

    let window = Window::new(&app);
    window.resize(LogicalSize::new(500, 500));

    let app2 = Arc::clone(&app);
    app.spawn(async move {
        window.closed().next().await;
        app2.quit();
    });

    app.quitted().next().await;

}
