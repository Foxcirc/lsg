
//! Interactive test to try out features that are currently being worked on.

use futures_lite::future::block_on;
use tracing::debug;

use desktop::*;
use common::*;

#[test]
fn interactive() -> Result<(), Box<dyn std::error::Error>> {
    EventLoop::run(app, "interactive test")?
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
    let mut surface = render::GlSurface::new(&renderer, &*window, Size::new(500, 500)).unwrap();

    let mut geometry = CurveGeometry::new();

    /* add basic triangle to extend by clicking

    geometry.points.push(CurvePoint::base(40, 400));
    geometry.points.push(CurvePoint::base(240, 400));
    geometry.points.push(CurvePoint::base(240, 200));

    geometry.shapes.push(Shape::instanced(0..3, 0..2));
    // geometry.shapes.push(Shape::singular(4..7, 1));

    geometry.instances.extend([
        Instance { pos: [0.0, 0.0, 0.1], texture: [0.85, 0.75, 0.35] },
        Instance { pos: [1.0, 0.0, 0.4], texture: [0.7, 0.0, 0.15] },
    ]);

    */

    /* shape with two disconnected areas

    let shape = [
        // outer sqare
        CurvePoint::base(50, 250),
        CurvePoint::base(50, 400),
        CurvePoint::base(250, 400),
        CurvePoint::base(250, 250),
        // magic trick
        CurvePoint::base(250, 200),
        CurvePoint::base(250, 50),
        CurvePoint::base(50, 50),
        CurvePoint::base(50, 200),
        CurvePoint::base(250, 200),
        // return
        CurvePoint::base(250, 250),
    ];

    geometry.points.extend(shape);

    */

    /* shape with allowed self intersection

    let shape = [
        CurvePoint::base(100, 100),
        CurvePoint::base(100, 50),
        CurvePoint::base(120, 50),
        CurvePoint::base(40, 70),
        CurvePoint::base(40, 80),
        CurvePoint::base(130, 80),
        CurvePoint::base(130, 40),
        CurvePoint::base(120, 40),
    ];

    geometry.points.extend(shape);

    */

    let p = CurvePoint::new(100, 1203, PointKind::Ctrl);

    // let len = geometry.points.len() as i16;
    // geometry.shapes.push(Shape::instanced(4..len as u16, 1..3));

    // geometry.instances.extend([
    //     Instance { pos: [0.0, 0.0, 0.1], texture: [0.85, 0.75, 0.35] },
    //     Instance { pos: [1.0, 0.0, 0.4], texture: [0.7, 0.0, 0.15] },
    // ]);

    // const TEST2: &str = "M3 18H21V6H3V18ZM1 5C1 4.44772 1.44772 4 2 4H22C22.5523 4 23 4.44772 23 5V19C23 19.5523 22.5523 20 22 20H2C1.44772 20 1 19.5523 1 19V5ZM9 10C9 9.44772 8.55228 9 8 9C7.44772 9 7 9.44772 7 10C7 10.5523 7.44772 11 8 11C8.55228 11 9 10.5523 9 10ZM11 10C11 11.6569 9.65685 13 8 13C6.34315 13 5 11.6569 5 10C5 8.34315 6.34315 7 8 7C9.65685 7 11 8.34315 11 10ZM8.0018 16C7.03503 16 6.1614 16.3907 5.52693 17.0251L4.11272 15.6109C5.10693 14.6167 6.4833 14 8.0018 14C9.52031 14 10.8967 14.6167 11.8909 15.6109L10.4767 17.0251C9.84221 16.3907 8.96858 16 8.0018 16ZM16.2071 14.7071L20.2071 10.7071L18.7929 9.29289L15.5 12.5858L13.7071 10.7929L12.2929 12.2071L14.7929 14.7071L15.5 15.4142L16.2071 14.7071Z";

    // const TEST_SVG_PATH: &str = "M12 2C17.52 2 22 6.48 22 12C22 17.52 17.52 22 12 22C6.48 22 2 17.52 2 12C2 6.48 6.48 2 12 2ZM12 20C16.42 20 20 16.42 20 12C20 7.58 16.42 4 12 4C7.58 4 4 7.58 4 12C4 16.42 7.58 20 12 20ZM13 12V16H11V12H8L12 8L16 12H13Z";

    let mut shape_scale_factor = 0.1;
    let mut shape_take_part = 0;

    const SVG1: &str = "M12 2C17.52 2 22 6.48 22 12C22 17.52 17.52 22 12 22C6.48 22 2 17.52 2 12C2 6.48 6.48 2 12 2ZM13 12H16L12 8L8 12H11V16H13V12Z";
    // const SVG2: &str = "M12 8H8.001L8 20H6V8H2L7 3L12 8ZM22 16L17 21L12 16H16V4H18V16H22Z"; // "M4 3C3.44772 3 3 3.44772 3 4V20C3 20.5523 3.44772 21 4 21H20C20.5523 21 21 20.5523 21 20V4C21 3.44772 20.5523 3 20 3H4ZM16.0001 8V16.4142L12.5001 12.9142L8.70718 16.7071L7.29297 15.2929L11.0859 11.5L7.58586 8H16.0001Z";

    let (_, parsed_path) = widget::svg::parser::path(SVG1).expect("valid svg path");
    let mut points = widget::svg::path_to_shape(parsed_path);

    widget::svg::scale_all_points(&mut points, shape_scale_factor);
    points.dedup();
    unsafe { render::SHAPE_TAKE_PART = 1 };
    dbg!(&points);

    geometry.shapes.push(Shape::singular(0..points.len() as u16, 0));
    geometry.instances.push(Instance { pos: [0.1, 0.0, 1.0], texture: [0.7; 3] });
    geometry.points.extend(points);

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Resume => {
                    debug!("resuming");
                },

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {
                        // TODO: it seems there is a wayland bug where Ctrl+C doesnt work sometimes if the window is minimized? is it blocking somewhere unexpected? (I think it was blocking in eglSwapBuffers but it should be fixed I think, still need to check tho)
                        window.pre_present_notify();
                        // geometry.shapes.last_mut().map(|it| {
                        //     it.polygon.end = 4 + shape_take_part;
                        //     println!("points to render: {:?}", geometry.points.get(it.polygon.start as usize .. it.polygon.end as usize));
                        // });
                        // dbg!(&geometry.points);
                        let result = renderer.draw(&geometry, &surface);
                        if let Err(err) = result {
                            tracing::error!("draw failed: {err:?}");
                        }
                        // window.redraw_with_vsync(&mut evl);
                        // evl.push_redraw_test(&window);
                    },

                    WindowEvent::Resize { size, .. } => {
                        surface.resize(size);
                    //     window.pre_present_notify();
                    //     renderer.draw(&perwindow).ok();
                    },

                    WindowEvent::KeyDown { key, .. } => {
                        if key == Key::ArrowUp {
                            shape_scale_factor += 0.01;
                        } else if key == Key::ArrowDown {
                            shape_scale_factor -= 0.01;
                        } else if key == Key::ArrowRight {
                            unsafe { render::SHAPE_TAKE_PART += 1 };
                            println!("{:?}", unsafe { render::SHAPE_TAKE_PART });
                            shape_take_part += 1;
                        } else if key == Key::ArrowLeft {
                            unsafe { render::SHAPE_TAKE_PART -= 1 };
                            println!("{:?}", unsafe { render::SHAPE_TAKE_PART });
                            shape_take_part -= 1;
                        };
                        window.redraw_with_vsync(&mut evl);
                    },

                    WindowEvent::MouseMotion { x, y } => {

                        /*
                        if let Some(point) = geometry.points.last_mut() {
                            if point.is_base() {
                                *point = CurvePoint::base(x as i16, y as i16);
                            } else {
                                *point = CurvePoint::ctrl(x as i16, y as i16);
                            }
                        }

                        window.redraw_with_vsync(&mut evl);

                        */

                    },

                    WindowEvent::MouseDown { button: MouseButton::Left, x, y } => {

                        geometry.points.push(
                            CurvePoint::new(x as u16, y as u16, PointKind::Base)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.polygon.end += 1;
                        }

                        debug!("add point {:?}", geometry.points.last().unwrap());

                        window.redraw_with_vsync(&mut evl);

                    },


                    WindowEvent::MouseDown { button: MouseButton::Right, x, y } => {

                        geometry.points.push(
                            CurvePoint::new(x as u16, y as u16, PointKind::Ctrl)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.polygon.end += 1;
                        }

                        debug!("add control point {:?}", geometry.points.last().unwrap());

                        window.redraw_with_vsync(&mut evl);

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
