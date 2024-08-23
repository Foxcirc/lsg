
use futures_lite::future::block_on;
use render::{CurvePoint, CurveShape};
use tracing::{debug, trace, warn};

use desktop::*;
use egl::*;
use common::*;

// TODO: this test should only test the desktop event handling etc.
// TODO: rendering, triangulating and more should be a different test

fn main() -> Result<(), Box<dyn std::error::Error>> {
    EventLoop::run(app, "lsg/test")?
}

fn app(mut evl: EventLoop) -> Result<(), Box<dyn std::error::Error>> {

    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_ansi(true)
        .with_max_level(tracing::Level::DEBUG)
        .with_test_writer()
        .finish();

    tracing::subscriber::set_global_default(subscriber).unwrap();

    let egl = Instance::new(&evl)?;

    gl::load_with(|name|
        egl.get_proc_address(name).unwrap() as *const _
    );

    gl::debug_message_callback(gl::debug_message_tracing_handler);
    
    let size = Size { width: 500 , height: 500 };
    let mut window = Window::new(&mut evl, size);

    let mut ctx = Context::new(&egl, &*window, size, None)?; // create an egl context for our window
    ctx.bind()?; // make the context current

    window.set_title("lsg/test");
    window.set_transparency(true);
    window.set_input_mode(&mut evl, InputMode::SingleKey);

    // let points: &mut [[i32; 2]] = &mut [
    //     [0, 0],
    //     [10, 0],
    //     [15, 5],
    //     [20, 10],
    //     [15, 15],
    //     [10, 20],
    //     [0, 20],
    // ];

    // let points: &mut [Vertex] = &mut [
    //     Vertex { x: 0, y:  0 },
    //     Vertex { x: 20, y:  0 },
    //     Vertex { x: 20, y:  20 },
    //     Vertex { x: 0, y:  20 },
    //     // inner
    //     Vertex { x: 5, y:  15 },
    //     Vertex { x: 15, y:  15 },
    //     Vertex { x: 15, y:  5 },
    //     Vertex { x: 5, y:  5 },
    //     // path back
    //     Vertex { x: 5, y:  15 },
    //     Vertex { x: 0, y:  20 },
    // ];

    let shared = render::CurveShared::new(&evl).unwrap();
    let mut renderer = render::CurveRenderer::new(&shared, &*window, Size { width: 500, height: 500 }).unwrap();

    let mut geometry = render::CurveGeometry {
        points: Vec::new(),
        shapes: Vec::new(),
    };

    geometry.points.push(CurvePoint::base(0, 0));
    geometry.shapes.push(CurveShape::new(0, 0));
    
    // let points: &mut [CurvePoint] = &mut [
    //     CurvePoint::base(0, 0),
    //     CurvePoint::base(10, 0),
    //     CurvePoint::base(15, 5),
    //     CurvePoint::base(20, 10),
    //     CurvePoint::base(15, 15),
    //     CurvePoint::base(10, 20),
    //     CurvePoint::base(0, 20),
    // ];

    // points.reverse();

    // geometry.points.extend_from_slice(points);

    geometry.shapes.last_mut().unwrap().len += 1; // TODO: this is counter-intuitive: we use exclusive range so 0..1 means &[]
    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {

                        gl::clear(0.0, 0.0, 0.0, 1.0);
                        // println!("geometry: {:?}", &geometry.verticies);
                        renderer.render(&geometry).ok();
                        window.pre_present_notify();
                        ctx.swap_buffers(None).unwrap();

                        window.redraw();

                    },

                    WindowEvent::Resize { size, .. } => {
                        // ctx.resize(size);
                        renderer.resize(size);
                        ctx.resize(size);
                        gl::resize_viewport(size);
                        // current_size = size;
                    },

                    WindowEvent::MouseMotion { x, y } => {

                        // TODO: i think on wayland x, y can be negative .-. try clicking and then moving the mouse out the upper window boundry
                        if let Some(point) = geometry.points.last_mut() {
                            if point.basic() {
                                *point = CurvePoint::base(x as i16, y as i16);
                            } else {
                                *point = CurvePoint::control(x as i16, y as i16);
                            }
                        }

                    },

                    WindowEvent::MouseDown { button: MouseButton::Left, x, y } => {

                        geometry.points.push(
                            CurvePoint::base(x as i16, y as i16)
                        );

                        geometry.shapes.last_mut().unwrap().len += 1;

                        debug!("add point {:?}", geometry.points.last().unwrap());

                        println!("{:?}", geometry.points);
                    },


                    WindowEvent::MouseDown { button: MouseButton::Right, x, y } => {

                        geometry.points.push(
                            CurvePoint::control(x as i16, y as i16)
                        );

                        geometry.shapes.last_mut().unwrap().len += 1;

                        debug!("add control point {:?}", geometry.points.last().unwrap());

                        println!("{:?}", geometry.points);

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
                    
                    WindowEvent::Close => evl.quit(),
                    other => debug!("unhandeled window event '{:?}'", other),
                    
                },

                Event::Resume => debug!("resuming"),
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
