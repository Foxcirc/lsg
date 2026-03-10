
use std::sync::Arc;
use common::{CurvePoint, PointKind};

use crate::*;

#[test]
fn app() -> Result<(), desktop::EvlError> {
    App::run(handler, Config { appid: file!().into() })?
}

async fn handler(app: Arc<App>) -> Result<(), desktop::EvlError> {

    let window = Window::new(&app)?;

    // let handler = window.closed(&app);
    // handler.next().await; // ...

    // window.closed(&app).connect(async move |app| {

    // });

    window.content(Arc::new(RedPill));

    app.connect(&window, Window::closed, async move |(app, ..)| {
        app.quit();
    });

    app.quitted().next().await;

    Ok(())

}

struct RedPill;

impl Widget for RedPill {
    fn action(&self, action: Action) {
        if let Action::Render { mut space } = action {
            let key = space.curves(&[
                CurvePoint::new(1,    1,    PointKind::Base),
                CurvePoint::new(499, 1,    PointKind::Base),
                CurvePoint::new(499, 499, PointKind::Base),
                CurvePoint::new(1,    499, PointKind::Base),
            ]);
            space.instance(key, Instance {
                pos: Position::rel(0, 0),
                size: Size::rel(5000, 5000),
            });
            println!("render called.");
        }
    }
}
