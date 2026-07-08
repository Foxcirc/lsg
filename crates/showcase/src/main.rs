
/*

// use std::ffi::c_void as void;
// use std::ptr::null_mut;
// use std::sync::Arc;
// use std::task::Poll;

// use desktop::ffi::import::definitions as defs;
// use desktop::ffi::types as types;

#[cfg(target_family = "wasm")]
unsafe extern "C" {
    fn logs(it: *const i8);
}

#[cfg(target_family = "wasm")]
fn log(it: &str) {
    let mut buf = [0u8; 2048];
    buf[..it.len()].copy_from_slice(it.as_bytes());
    assert!(it.len() != 2048);
    unsafe { logs(buf.as_ptr().cast()) }
}

#[cfg(not(target_family = "wasm"))]
fn log(it: &str) {
    println!("{}", it);
}

fn main() {
    run();
}

use std::sync::Arc;

use desktop::{InputMode::Text, *};
use common::*;
use render::{DrawableGeometry, shaper};

const APPID: &str = file!();

#[unsafe(no_mangle)]
extern "C" fn run() {
    log("Entering `run` function...");
    desktop::EventLoop::run(desktop::EvlConfig {
        appid: APPID.into(),
        intercept: false,
    }, app).unwrap().unwrap();
}

fn app(evl: Arc<EventLoop>) -> Result<(), Box<dyn std::error::Error>> {

    log("Entering `app` function...");

    let window = Window::new(&evl);

    window.title(APPID);
    window.transparency(true);

    log(&format!("window.size: {:?}", window.size()));

    let mut renderer = render::Renderer::new(&*evl)?;
    let mut storage = graphics::Texture::new(&renderer.gp, window.size(), None);
    let mut surface = graphics::Surface::new(&renderer.gp, &window);
    let atlas = render::TextureAtlas::new(&renderer);

    let mut geometry = common::CurveGeometry::new();
    let mut shaper = shaper::GeometryShaper::new();

    let mut instances: Vec<common::Instance> = Vec::new();

    geometry.points.push(CurvePoint::new(1250, 1250, PointKind::Base));
    geometry.points.push(CurvePoint::new(3750, 1250, PointKind::Ctrl));
    geometry.points.push(CurvePoint::new(3750, 3750, PointKind::Base));

    geometry.shapes.push(Shape::new(0..3));

    instances.push(common::Instance {
        target: common::GeometryTarget { geometry: 0, shape: 0 },
        pos: PhysicalPair::new(0, 0),
        size: PhysicalPair::new(500, 500),
        texture: common::TextureKind::Color(0, 255, 100, 255),
    });

    log("Spawning event handler now...");

    // run the event loop
    let fut = async move {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {

                        let vertices = shaper.process(&geometry);

                        let drawable = DrawableGeometry {
                            source: &[vertices],
                            instances: &instances,
                        };

                        storage.clear([0.0, 0.0, 0.0, 0.5]);

                        window.present();
                        renderer.draw(&drawable, &atlas, &mut storage);

                        surface.blit(&mut storage);
                        surface.swap();

                    },

                    WindowEvent::Resize { size, .. } => {
                        log(&format!("got resize event: new size = {size:?}"));
                        storage.resize(size, None);
                        surface.resize(size);
                    },

                    WindowEvent::MouseMotion { point } => {
                        log(&format!("mouse motion: {:?}", point));
                        if let Some(gpoint) = geometry.points.last_mut() {
                            *gpoint = CurvePoint::new(point.x * 10, point.y * 10, gpoint.kind());
                            window.redraw();
                        }
                    },

                    WindowEvent::MouseDown { point, button } => {

                        let kind = match button {
                            MouseButton::Left => PointKind::Base,
                            MouseButton::Right => PointKind::Ctrl,
                            _ => continue,
                        };

                        println!("add point {:?}", point);

                        geometry.points.push(
                            CurvePoint::new(point.x * 10, point.y * 10, kind)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.end += 1;
                            window.redraw();
                        }

                    },

                    WindowEvent::MouseScroll { dx, dy } => {
                        instances.last_mut().map(|it| {
                            it.pos.x -= dx / 10;
                            it.pos.y += dy / 10;
                        });
                        window.redraw();
                    },

                    WindowEvent::ShouldClose => evl.quit(),

                    other => println!("unhandeled window event '{:?}'", other),

                },

                Event::Quit { reason } => {
                    println!("quitting: {:?}", reason);
                    break
                },

                other => println!("unhandeled event '{:?}'", other),

            }

        }

    };

    #[cfg(feature = "browser")]
    futures::spawn(fut);

    #[cfg(feature = "linux")]
    futures::blockon(fut);

    Ok(())

}

/*

#[unsafe(no_mangle)]
extern "C" fn run() {

    use futures::ffi::import::implementation as fexeci;

    log("Run called.");

    desktop::EventLoop::run(desktop::EvlConfig::default(), move |evl| {

        log("WASM handler called.");

        fexeci::spawn(async move {
            log("Future first polled. Creating window...");

            #[allow(unused)]
            let window = desktop::Window::new(&evl);

            loop {
                log("Waiting for next event...");
                let event = evl.next().await.expect("Next event.");
                log(&format!("{event:?}"));
            }
        });

    }).expect("Run event loop.");

}

// extern "C" fn handler0(evl0: *const types::EventLoop, state0: *mut void) {

//     fexeci::spawn(std::future::poll_fn(move |cx| {
//         println!("should run every sec!");
//         let waker0 = Arc::new(cx.waker().clone());
//         unsafe { defs::event_loop_poll(
//             evl0,
//             Arc::as_ptr(&waker0).cast(),
//             null_mut(),
//             null_mut()
//         ) };
//         Poll::Pending
//     }));

// }

//     use futures::ffi::waker as fwaker;
//     use futures::ffi::types as ftypes;

//     extern "C" fn wake_handler(state: *const void) {
//         unsafe { logs(c"wake handler called".as_ptr()) };
//     }

//     extern "C" fn drop_handler(state: *const void) {
//         unsafe { logs(c"drop handler called".as_ptr()) };
//     }

//     let waker = ftypes::ExternWaker {
//         state: null_mut(),
//         vtable: &ftypes::WakerVTable {
//             wake: wake_handler,
//             drop: drop_handler,
//         },
//     };

//     let iwaker = unsafe { fwaker::waker_build(waker) };

//     unsafe { defs::event_loop_poll(evl0, &iwaker, null_mut(), null_mut()) };

//     unsafe { fwaker::waker_drop(iwaker) }; //

// }

*/

*/

use std::sync::Arc;

use lsg::Window;

fn main() {
    lsg::App::run(handle, lsg::Config::default())
        .expect("run app");
}

async fn handle(app: Arc<lsg::App>) {

    use lsg::widget::{layout, shapes};
    use lsg::common::rel;

    let window = lsg::Window::new(&app).expect("open window");

    let widget = layout::Cols::new(vec![
        (rel(5000), shapes::Rect::colored((130, 0, 0, 255))),
        (rel(5000), shapes::Rect::colored((50, 0, 100, 255)))
    ]);

    window.content(Arc::new(widget));

    app.connect(&window, Window::closed, async move |(app, ..)| {
        app.quit();
    });

    app.quitted().next().await;

}
