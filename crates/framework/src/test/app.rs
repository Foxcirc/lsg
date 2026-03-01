
use std::sync::Arc;
use crate::*;

#[test]
fn app() -> Result<(), desktop::EvlError> {
    App::run(handler, Config { appid: file!().into() })
}

async fn handler(app: Arc<App>) {

    let window = Window::new(&app);

    app.connect(&window, Window::closed, async move |(app, ..)| {
        app.quit();
    });

    app.quitted().next().await;

}
