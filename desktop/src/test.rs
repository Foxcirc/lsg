
use futures_lite::future::block_on;
use tracing::{debug, trace};

use desktop::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run(app, "lsg/test")?
}

fn app(mut evl: EventLoop<&str>) -> Result<(), Box<dyn std::error::Error>> {

    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_ansi(true)
        .with_max_level(tracing::Level::DEBUG)
        .with_test_writer()
        .finish();

    tracing::subscriber::set_global_default(subscriber).unwrap();

    // we will be using the built-in gl functionality
    let egl = EglInstance::new(&mut evl)?;

    gl::load_with(|name|
        egl.get_proc_address(name).unwrap() as *const _
    );
    
    let size = Size { width: 500 , height: 500 };
    let mut window = Window::new(&mut evl, size);

    let mut ctx = EglContext::new(&egl, &window, size)?; // create an egl context for our window
    ctx.bind()?; // make the context current

    window.set_title("lsg/test");
    window.set_transparency(true);
    window.set_input_mode(&mut evl, InputMode::SingleKey);

    // init opengl stuffff

    gl::debug_message_callback(gl::debug_message_tracing_handler);

    fn linear_interpolate_points(a: [f32; 2], b: [f32; 2], t: f32) -> [f32; 2] {
        [a[0] + (b[0] - a[0]) * t,
         a[1] + (b[1] - a[1]) * t]
    }

    fn bezier_interpolate_points(a: [f32; 2], b: [f32; 2], c: [f32; 2], t: f32) -> [f32; 2] {
        let i1 = linear_interpolate_points(a, b, t);
        let i2 = linear_interpolate_points(b, c, t);
        linear_interpolate_points(i1, i2, t)
    }

    fn bezier_curve(resolution: usize, p1: [f32; 2], p2: [f32; 2], p3: [f32; 2], buf: &mut Vec<f32>) {
        buf.clear();
        for idx in 0..=resolution {
            let t = idx as f32 / resolution as f32;
            let p = bezier_interpolate_points(p1, p2, p3, t);
            buf.extend([p[0], p[1], 0.0]);
        }
    }

    let mut resolution = 20;
    let mut points = Vec::with_capacity(resolution);

    let vao = gl::gen_vertex_array();
    let vbo = gl::gen_buffer(gl::BufferType::ArrayBuffer);

    gl::vertex_attribs(&vao, &vbo, 0, 3, gl::DataType::Float, false, 4 * 3, 0);

    const VERT: &str = "#version 320 es
        precision mediump float;
        layout (location = 0) in vec3 pos;
        void main() {
            gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
        }
    ";

    const FRAG: &str = "#version 320 es
        precision mediump float;
        out vec4 color;
        void main() {
            color = vec4(1.0, 0.5, 0.2, 1.0);
        }
    ";

    let vert = gl::create_shader(gl::ShaderType::Vertex, VERT).unwrap();
    let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG).unwrap();

    let mut program = gl::create_program();
    gl::attach_shader(&mut program, vert);
    gl::attach_shader(&mut program, frag);
    let linked = gl::link_program(program).unwrap();

    let mut p0 = [-0.7f32, -0.2];
    let mut p1 = [ 0.0f32,  0.2];
    let mut p2 = [ 0.7f32, -0.2];

    let mut left_down = false;
    let mut middle_down = false;
    let mut right_down = false;

    let mut current_size = Size { width: 500, height: 500 };

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {

                        bezier_curve(resolution, p0, p1, p2, &mut points);
                        gl::buffer_data(&vbo, &points, gl::DrawHint::Dynamic);

                        gl::clear(0.0, 0.0, 0.0, 1.0);
                        // tracing::info!("{points:?}");
                        gl::draw_arrays(&linked, &vao, gl::Primitive::LineStrip, 0, points.len() / 3);
                        
                        let token = window.pre_present_notify();
                        ctx.swap_buffers(None, token).unwrap();

                        window.redraw(token);

                    },

                    WindowEvent::Resize { size, .. } => {
                        // ctx.resize(size);
                        ctx.resize(size);
                        gl::resize_viewport(size.width, size.height);
                        current_size = size;
                    },

                    WindowEvent::MouseMotion { x, y } => {


                        // convert to opengl coords
                        let x = (x / current_size.width as f64) * 2.0 - 1.0;
                        let y = current_size.height as f64 - y; // flip y
                        let y = (y / current_size.height as f64) * 2.0 - 1.0;

                        if left_down {
                            p0 = [x as f32, y as f32];
                        } else if middle_down {
                            p1 = [x as f32, y as f32];
                        } else if right_down {
                            p2 = [x as f32, y as f32];
                        }

                    },

                    WindowEvent::MouseDown { button, .. } => match button {
                        MouseButton::Left => left_down = true,
                        MouseButton::Middle => middle_down = true,
                        MouseButton::Right => right_down = true,
                        _ => (),
                    },

                    WindowEvent::MouseUp { button, .. } => match button {
                        MouseButton::Left => left_down = false,
                        MouseButton::Middle => middle_down = false,
                        MouseButton::Right => right_down = false,
                        _ => (),
                    },

                    WindowEvent::MouseScroll { value, .. } => {
                        if value.is_sign_negative() { resolution += 1 }
                        else { resolution -= 1 }
                        debug!(resolution);
                    }

                    WindowEvent::Close => evl.quit(),
                    other => trace!("unhandeled window event '{:?}'", other),
                    
                },

                Event::Resume => debug!("resuming"),
                Event::Quit { reason } => {
                    debug!("quitting: {:?}", reason);
                    break
                },

                other => trace!("unhandeled event '{:?}'", other),
                
            }
            
        }

    });

    Ok(())
    
}
