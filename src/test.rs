
//! Interactive test to try out features that are currently being worked on.

use futures_lite::future::block_on;
use tracing::debug;

use desktop::*;
use common::*;
use render::{DrawableGeometry, shaper};

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

    let mut geometry = shaper::CurveGeometry::new();
    let mut instances: Vec<Instance> = Vec::new();
    let mut shaper = shaper::GeometryShaper::new();

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

    const SVGS: &[&str] = &[
        // "M12 2C17.52 2 22 6.48 22 12C22 17.52 17.52 22 12 22C6.48 22 2 17.52 2 12C2 6.48 6.48 2 12 2ZM13 12H16L12 8L8 12H11V16H13V12Z",
        // "M4 3C3.44772 3 3 3.44772 3 4V20C3 20.5523 3.44772 21 4 21H20C20.5523 21 21 20.5523 21 20V4C21 3.44772 20.5523 3 20 3H4ZM16.0001 8V16.4142L12.5001 12.9142L8.70718 16.7071L7.29297 15.2929L11.0859 11.5L7.58586 8H16.0001Z",
        // "M16.0503 12.0498L21 16.9996L16.0503 21.9493L14.636 20.5351L17.172 17.9988L4 17.9996V15.9996L17.172 15.9988L14.636 13.464L16.0503 12.0498ZM7.94975 2.0498L9.36396 3.46402L6.828 5.9988L20 5.99955V7.99955L6.828 7.9988L9.36396 10.5351L7.94975 11.9493L3 6.99955L7.94975 2.0498Z",
        // "M5.82843 6.99955L8.36396 9.53509L6.94975 10.9493L2 5.99955L6.94975 1.0498L8.36396 2.46402L5.82843 4.99955H13C17.4183 4.99955 21 8.58127 21 12.9996C21 17.4178 17.4183 20.9996 13 20.9996H4V18.9996H13C16.3137 18.9996 19 16.3133 19 12.9996C19 9.68584 16.3137 6.99955 13 6.99955H5.82843Z",
        // "M21 13V20C21 20.5523 20.5523 21 20 21H4C3.44772 21 3 20.5523 3 20V13H2V11L3 6H21L22 11V13H21ZM5 13V19H19V13H5ZM4.03961 11H19.9604L19.3604 8H4.63961L4.03961 11ZM6 14H14V17H6V14ZM3 3H21V5H3V3Z",
        // "M21 3C21.5523 3 22 3.44772 22 4V20.0066C22 20.5552 21.5447 21 21.0082 21H2.9918C2.44405 21 2 20.5551 2 20.0066V19H20V7.3L12 14.5L2 5.5V4C2 3.44772 2.44772 3 3 3H21ZM8 15V17H0V15H8ZM5 10V12H0V10H5ZM19.5659 5H4.43414L12 11.8093L19.5659 5Z",
    ];

    const SVG1: &str = "M12 2C17.52 2 22 6.48 22 12C22 17.52 17.52 22 12 22C6.48 22 2 17.52 2 12C2 6.48 6.48 2 12 2ZM13 12H16L12 8L8 12H11V16H13V12Z";
    // const SVG2: &str = ""; // "";

    let mut shape_idx = 0;
    let mut item_idx = 0;

    for svg in SVGS {

        let (_, parsed_path) = widget::svg::parser::path(svg).expect("valid svg path");
        let shapes = widget::svg::path_to_shape(parsed_path, 416.666);

        for points in shapes {

            let len = geometry.points.len();
            geometry.shapes.push(Shape::new(len as u16 .. (len + points.len()) as u16));
            // geometry.instances.push(Instance { pos: [(item_idx as f32 + 1.0) / 8.0, 0.0, 1.0], texture: [0.7; 3] });
            geometry.points.extend(points);

            instances.push(Instance {
                pos: Point::new(item_idx as f32 * 30.0, 10.0),
                target: [0, shape_idx],
                scale: 100,
            });

            shape_idx += 1;

        }

        item_idx += 1;

    }

    geometry.points.push(CurvePoint::new(40, 400, PointKind::Base));
    geometry.points.push(CurvePoint::new(240, 400, PointKind::Ctrl));
    geometry.points.push(CurvePoint::new(240, 200, PointKind::Base));

    geometry.shapes.push(Shape::new(0..3));

    instances.push(Instance {
        pos: Point::new(0.0, 0.0),
        scale: 10_000,
        target: [0, 0],
    });

    shape_scale_factor = 1.0;

    unsafe { render::SHAPE_TAKE_PART = 100 };

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Resume => {
                    debug!("resuming");
                },

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {
                        window.pre_present_notify();
                        // geometry.shapes.last_mut().map(|it| {
                        //     it.polygon.end = 4 + shape_take_part;
                        //     println!("points to render: {:?}", geometry.points.get(it.polygon.start as usize .. it.polygon.end as usize));
                        // });
                        // dbg!(&geometry.points);
                        // let mut cloned = geometry.points.clone();
                        // widget::svg::scale_all_points(&mut cloned, shape_scale_factor);
                        // std::mem::swap(&mut cloned, &mut geometry.points);

                        let vertices = shaper.process(&geometry);

                        let drawable = DrawableGeometry {
                            source: &[vertices],
                            instances: &instances,
                        };

                        let result = renderer.draw(&drawable, &surface);
                        // std::mem::swap(&mut cloned, &mut geometry.points);
                        if let Err(err) = result {
                            tracing::error!("draw failed: {err:?}");
                        }
                        // window.redraw_with_vsync(&mut evl);
                        // evl.push_redraw_test(&window);

                    },

                    WindowEvent::Resize { size, .. } => {
                        surface.resize(&renderer, size).expect("resizing failed");
                    },

                    WindowEvent::KeyDown { key, .. } => {
                        if key == Key::ArrowUp {
                            shape_scale_factor += 0.001;
                        } else if key == Key::ArrowDown {
                            shape_scale_factor -= 0.001;
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

                        if let Some(point) = geometry.points.last_mut() {
                            *point = CurvePoint::new(x, y, point.kind())
                        }

                        window.redraw_with_vsync(&mut evl);

                    },

                    WindowEvent::MouseDown { button: MouseButton::Left, x, y } => {

                        geometry.points.push(
                            CurvePoint::new(x as u16, y as u16, PointKind::Base)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.target.end += 1;
                        }

                        debug!("add point {:?}", geometry.points.last().unwrap());

                        dbg!(&geometry.shapes, &geometry.points);

                        window.redraw_with_vsync(&mut evl);

                    },


                    WindowEvent::MouseDown { button: MouseButton::Right, x, y } => {

                        geometry.points.push(
                            CurvePoint::new(x as u16, y as u16, PointKind::Ctrl)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.target.end += 1;
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
