
use std::sync::Arc;
use crate::*;

#[test]
fn app() -> Result<(), desktop::EvlError> {
    App::run(handler, Config { appid: file!().into() })
}

async fn handler(app: Arc<App>) {

    let window = Window::new(&app);
    // window.show();

    let app2 = Arc::clone(&app);
    app.spawn(async move {
        window.closed().await;
        app2.quit();
    });

    // let app2 = Arc::clone(&app);

    // app.connect(&window, Window::closed, async move |()| {
    //     app2.quit();
    // });

    app.quitted().await;

}
