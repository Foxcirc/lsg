
use futures_lite::future::block_on;
use render::Vertex;
use tracing::{debug, trace};

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
    
    let size = Size { width: 500 , height: 500 };
    let mut window = Window::new(&mut evl, size);

    let mut ctx = Context::new(&egl, &*window, size, None)?; // create an egl context for our window
    ctx.bind()?; // make the context current

    window.set_title("lsg/test");
    window.set_transparency(true);
    window.set_input_mode(&mut evl, InputMode::SingleKey);

    gl::debug_message_callback(gl::debug_message_tracing_handler);

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

    let mut geometry = render::Geometry {
        verticies: Vec::new(),
        polygons: Vec::new(),
    };

    geometry.verticies.push(Vertex { x: 0, y: 0 });

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {

                        gl::clear(0.0, 0.0, 0.0, 1.0);
                        // println!("geometry: {:?}", &geometry.verticies);
                        renderer.render(&geometry);
                        window.pre_present_notify();
                        ctx.swap_buffers(None).unwrap();

                        // TODO: reimplement window.redraw() sending a redraw event if no frame cb is registered
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

                        if let Some(point) = geometry.verticies.last_mut() {
                            *point = Vertex { x: x as u16, y: y as u16 };
                        }

                    },

                    WindowEvent::MouseDown { button: MouseButton::Left, x, y } => {

                        geometry.verticies.push(
                            Vertex { x: x as u16, y: y as u16 }
                        );

                        debug!("add point {:?}", geometry.verticies.last().unwrap());

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
