
//! Interactive test to try out features that are currently being worked on.

use futures_lite::future::block_on;
use render::{CurvePoint, Instance, Shape, PerWindow};
use tracing::debug;

use desktop::*;
use common::*;

#[test]
fn interactive() -> Result<(), Box<dyn std::error::Error>> {
    EventLoop::run(app, "interactive-test")?
}

fn app(mut evl: EventLoop) -> Result<(), Box<dyn std::error::Error>> {

    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_ansi(true)
        .with_max_level(tracing::Level::DEBUG)
        .with_test_writer()
        .finish();

    tracing::subscriber::set_global_default(subscriber).unwrap();

    let mut window = Window::new(&mut evl, Size::new(500, 500));

    window.set_title(evl.app_name());
    window.set_transparency(true);

    let mut renderer = render::GlRenderer::new(&evl).unwrap();
    let mut perwindow = PerWindow::new(&renderer, &*window, Size::new(500, 500)).unwrap();

    renderer.shape.geometry.points.extend([
        CurvePoint::base(0, 0),
        CurvePoint::base(0, 500),
        CurvePoint::base(500, 500),
        CurvePoint::base(500, 0),
    ]);
    renderer.shape.geometry.shapes.push(Shape::new_singular(0..4, 0));
    renderer.shape.geometry.instances.push(Instance {
        pos: [0.0, 0.0, 0.5],
        texture: [0.1, 0.1, 0.14],
    });

    // initial render

    window.pre_present_notify();
    renderer.draw(&perwindow).expect("first render should be error-free");
    window.redraw_with_vsync();

    // add movable point

    renderer.shape.geometry.points.push(CurvePoint::base(40, 400));
    renderer.shape.geometry.points.push(CurvePoint::ctrl(240, 400));
    renderer.shape.geometry.points.push(CurvePoint::base(240, 200));

    renderer.shape.geometry.shapes.push(Shape::new_instanced(4..7, 1..3));
    renderer.shape.geometry.instances.extend([
        Instance { pos: [0.0, 0.0, 0.1], texture: [0.85, 0.75, 0.35] },
        Instance { pos: [1.0, 0.0, 0.4], texture: [0.7, 0.0, 0.15] },
    ]);

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Resume => {
                    debug!("resuming, initial render");
                    // render on resume, to make wayland accept our
                    // window size
                    // window.pre_present_notify();
                    // renderer.draw(&perwindow).expect("first render should be error-free");
                    // TODO: why does rendering here cause a complete freeze?? i hate wayland
                    // window.redraw();
                }

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {
                        window.pre_present_notify();
                        renderer.draw(&perwindow).ok();
                    },

                    WindowEvent::Resize { size, .. } => {
                        perwindow.resize(size);
                        window.pre_present_notify();
                        renderer.draw(&perwindow).ok();
                    },

                    /* WindowEvent::MouseMotion { x, y } => {

                        if x < 0.0 || y < 0.0 { continue }; // TODO: sometimes -1.0, handle this by default

                        // TODO: i think on wayland x, y can be negative .-. try clicking and then moving the mouse out the upper window boundry
                        if let Some(point) = renderer.shape.geometry.points.last_mut() {
                            if point.kind() {
                                *point = CurvePoint::base(x as i16, y as i16);
                            } else {
                                *point = CurvePoint::ctrl(x as i16, y as i16);
                            }
                        }

                        window.pre_present_notify();
                        renderer.draw(&perwindow).ok();

                    }, */

                    WindowEvent::MouseDown { button: MouseButton::Left, x, y } => {

                        renderer.shape.geometry.points.push(
                            CurvePoint::base(x as i16, y as i16)
                        );

                        if let Some(shape) = renderer.shape.geometry.shapes.last_mut() {
                            match shape.is_singular() {
                                true => shape.polygon.end += 1,
                                false => shape.polygon.end -= 1,
                            }
                        }

                        debug!("add point {:?}", renderer.shape.geometry.points.last().unwrap());
                        window.pre_present_notify();
                        renderer.draw(&perwindow).ok();

                    },


                    WindowEvent::MouseDown { button: MouseButton::Right, x, y } => {

                        renderer.shape.geometry.points.push(
                            CurvePoint::ctrl(x as i16, y as i16)
                        );

                        if let Some(shape) = renderer.shape.geometry.shapes.last_mut() {
                            match shape.is_singular() {
                                true => shape.polygon.end += 1,
                                false => shape.polygon.end -= 1,
                            }
                        }

                        debug!("add control point {:?}", renderer.shape.geometry.points.last().unwrap());
                        window.pre_present_notify();
                        renderer.draw(&perwindow).ok();

                    },

                    // WindowEvent::MouseUp { button, .. } => match button {
                    //     MouseButton::Left => left_down = false,
                    //     MouseButton::Middle => middle_down = false,
                    //     MouseButton::Right => right_down = false,
                    //     _ => (),
                    // },

                    // WindowEvent::KeyDown { key, .. } => {

                    //     match key {
                    //         Key::ArrowUp   if taketrigs < 10 => { taketrigs += 1 },
                    //         Key::ArrowDown if taketrigs > 0  => { taketrigs -= 1 },
                    //         _ => (),
                    //     }

                    //     dbg!(taketrigs);

                    //     let temp = state.triangulate(&points);

                    //     trigs = temp.into_iter().enumerate().map(|(idx, [x, y])| {
                    //         let x = (x as f64 / 50.0) * 2.0 - 1.0;
                    //         let y = 50.0 as f64 - y as f64; // flip y
                    //         let y = (y / 50.0) * 2.0 - 1.0;
                    //         [x as f32 + 0.5, y as f32 - 0.5] // +100.0 to offset it a bit
                    //     }).flatten().collect();

                    //     draw(&linked, &vertex_array, &buffer, &trigs);
                    //     window.pre_present_notify();
                    //     ctx.swap_buffers(None).unwrap();
                    //     // window.redraw();

                    // },

                    WindowEvent::Close => {
                        debug!("quitting...");
                        evl.quit();
                    },
                    other => tracing::trace!("unhandeled window event '{:?}'", other),

                },

                Event::Quit { reason } => {
                    debug!("quitting: {:?}", reason);
                    break
                },

                other => debug!("unhandeled event '{:?}'", other),

            }

        }

    });

    Ok(())

}
