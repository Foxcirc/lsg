
use futures_lite::future::block_on;
use render::{CurvePoint, Instance, Shape, PerWindow};
use tracing::debug;

use desktop::*;
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
    
    let size = Size { w: 500 , h: 500 };
    let mut window = Window::new(&mut evl, size);

    // let mut ctx = Context::new(&egl, &*window, size, None)?; // create an egl context for our window
    // ctx.bind()?; // make the context current

    window.set_title("lsg/test");
    window.set_transparency(true);
    window.set_input_mode(&mut evl, InputMode::SingleKey); // TODO: Always emit both events!!! Remove set_input_mode

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

    let mut renderer = render::GlRenderer::new(&evl).unwrap();
    let mut perwindow = PerWindow::new(&renderer, &*window, Size::new(500, 500)).unwrap();

    renderer.shape.geometry.points.extend([
        CurvePoint::base(0, 0),
        CurvePoint::base(0, 500),
        CurvePoint::base(500, 500),
        CurvePoint::base(500, 0),
    ]);
    renderer.shape.geometry.shapes.push(Shape::singular(0..4, 0));
    renderer.shape.geometry.instances.push(Instance {
        pos: [0.0, 0.0, 0.5],
        texture: [0.1, 0.1, 0.14],
    });

    // initial render
    
    window.pre_present_notify();
    renderer.draw(&perwindow).expect("first render should be error-free");
    window.redraw();

    // add movable point

    renderer.shape.geometry.points.push(CurvePoint::base(40, 400));
    renderer.shape.geometry.points.push(CurvePoint::ctrl(240, 400));
    renderer.shape.geometry.points.push(CurvePoint::base(240, 200));

    renderer.shape.geometry.shapes.push(Shape::instanced(4..7, 1..3));
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
                        window.redraw();
                    },

                    WindowEvent::Resize { size, .. } => {
                        perwindow.resize(size);
                    },

                    WindowEvent::MouseMotion { x, y } => {

                        continue;

                        if x < 0.0 || y < 0.0 { continue }; // TODO: sometimes -1.0, handle this by default

                        // TODO: i think on wayland x, y can be negative .-. try clicking and then moving the mouse out the upper window boundry
                        if let Some(point) = renderer.shape.geometry.points.last_mut() {
                            if point.kind() {
                                *point = CurvePoint::base(x as i16, y as i16);
                            } else {
                                *point = CurvePoint::ctrl(x as i16, y as i16);
                            }
                        }

                    },

                    WindowEvent::MouseDown { button: MouseButton::Left, x, y } => {

                        renderer.shape.geometry.points.push(
                            CurvePoint::base(x as i16, y as i16)
                        );

                        if let Some(shape) = renderer.shape.geometry.shapes.last_mut() {
                            match shape.kind() {
                                true => shape.polygon.end += 1,
                                false => shape.polygon.end -= 1,
                            }
                        }

                        debug!("add point {:?}", renderer.shape.geometry.points.last().unwrap());
                    },


                    WindowEvent::MouseDown { button: MouseButton::Right, x, y } => {

                        renderer.shape.geometry.points.push(
                            CurvePoint::ctrl(x as i16, y as i16)
                        );

                        if let Some(shape) = renderer.shape.geometry.shapes.last_mut() {
                            match shape.kind() {
                                true => shape.polygon.end += 1,
                                false => shape.polygon.end -= 1,
                            }
                        }

                        debug!("add control point {:?}", renderer.shape.geometry.points.last().unwrap());

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
                    other => debug!("unhandeled window event '{:?}'", other),
                    
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
