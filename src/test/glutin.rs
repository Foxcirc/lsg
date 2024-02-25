
use std::{ffi::{c_void as void, CStr}, num::NonZeroU32};

use async_executor::LocalExecutor;
use futures_lite::future::block_on;
use glutin::{display::Display, context::PossiblyCurrentContext, surface::{Surface, WindowSurface}};
use lsg_gl::{GlBuffer, VertexArrayObject, LinkedProgram, DebugSeverity};
use lsg_winit::winit::{dpi::PhysicalSize, window::Window, event::{WindowEvent, Event}};

fn main() {

    env_logger::init();

    let (evl, mut runner) = lsg_winit::AsyncEventLoop::new().unwrap();

    runner.spawn(move || {
        let exec = LocalExecutor::new();
        let task = exec.spawn(run(evl));
        block_on(exec.run(task))
    });

    runner.run().unwrap()
    
}

async fn run(evl: lsg_winit::AsyncEventLoop) {

    // create a simple window
    let window = lsg_winit::WindowBuilder::new()
        .with_title("lsg-test")
        .build(&evl).await.unwrap();

    let mut state = GlutinState::new(&window);

    loop {

        let event = evl.next().await;

        match event {

            Event::WindowEvent { window_id: _, event } => match event {
                WindowEvent::CloseRequested  => break,
                WindowEvent::Resized(size)   => state.resize(size),
                WindowEvent::RedrawRequested => state.render(),
                _ => (),
            },

            _ => ()
            
        }

    }
    
}

struct GlutinState {
    _display: Display,
    context: PossiblyCurrentContext,
    surface: Surface<WindowSurface>,
    vao: VertexArrayObject,
    program: LinkedProgram,
}

impl GlutinState {

    pub fn new(window: &Window) -> Self {

        use raw_window_handle::{HasRawDisplayHandle, HasRawWindowHandle};
        use glutin::{
            display::{DisplayApiPreference, GlDisplay},
            context::{ContextAttributesBuilder, ContextApi, GlProfile, NotCurrentGlContext},
            config::ConfigTemplateBuilder,
            surface::SurfaceAttributesBuilder
        };
        
        // create display
        let display = unsafe { Display::new(window.raw_display_handle(), DisplayApiPreference::Egl) }.unwrap();

        // create config & context
        let config_attrs = ContextAttributesBuilder::new()
            .with_context_api(ContextApi::OpenGl(None))
            // .with_context_api(ContextApi::Gles(None))
            .with_debug(true)
            .with_profile(GlProfile::Core)
            .build(Some(window.raw_window_handle()));

        let config_template = ConfigTemplateBuilder::new()
            .build();
        let config = unsafe { display.find_configs(config_template) }.unwrap().next().unwrap();

        let not_current_context = unsafe { display.create_context(&config, &config_attrs) }.unwrap();

        // create surface
        let size = window.inner_size();
        let surface_attrs = SurfaceAttributesBuilder::<WindowSurface>::new().build(
            window.raw_window_handle(),
            NonZeroU32::new(size.width).unwrap(),
            NonZeroU32::new(size.height).unwrap()
        );

        let surface = unsafe { display.create_window_surface(&config, &surface_attrs).unwrap() };

        // make context current (we can only load all functions with the current context)
        let context = not_current_context.make_current(&surface).unwrap();

        // load opengl functions
        gl_load_with(|name| display.get_proc_address(name));

        lsg_gl::debug_message_callback(|source, _kind, severity, _id, message| {
            let color = if severity == DebugSeverity::High { "31" } else { "34" };
            println!(
                "gl/{:?} (Severity: {:?}): \x1b[{}m{}\x1b[39m",
                source, severity, color, message.trim_end_matches("\n")
            );
        });

        // vertex shader
        let vertex_shader_code = include_str!("vert.glsl");
        let vertex_shader = lsg_gl::Shader::new(
            lsg_gl::ShaderType::Vertex,
            vertex_shader_code
        ).unwrap();

        // vertex shader
        let fragment_shader_code = include_str!("frag.glsl");
        let fragment_shader = lsg_gl::Shader::new(
            lsg_gl::ShaderType::Fragment,
            fragment_shader_code
        ).unwrap();

        // create the program
        let mut builder = lsg_gl::Program::new();
        builder.attach(vertex_shader);
        builder.attach(fragment_shader);
        let program = builder.link();

        // setup for the triangle

        let vao = lsg_gl::VertexArrayObject::new();
        vao.with(|| {

            let vertices = &[
                -0.5, -0.5, 0.0,
                 0.5, -0.5, 0.0,
                 0.0,  0.5, 0.0f32,
            ];

            let vbo = lsg_gl::ArrayBuffer::new();
            vbo.data(vertices, lsg_gl::DrawHint::Static);
            vbo.attribs(0, 3, lsg_gl::DataType::Float, false, 3);
            
        });
       
        Self {
            _display: display,
            context,
            surface,
            vao,
            program
        }
        
    }

    pub fn resize(&mut self, size: PhysicalSize<u32>) {

        use glutin::surface::GlSurface;

        self.surface.resize(
            &self.context,
            NonZeroU32::new(size.width).unwrap(),
            NonZeroU32::new(size.height).unwrap()
        );

        lsg_gl::resize_viewport(size.width, size.height);

    }

    pub fn render(&self) {

        use glutin::surface::GlSurface;

        lsg_gl::clear(0.0, 0.0, 0.0, 1.0);
        lsg_gl::draw_array(&self.program, &self.vao, lsg_gl::Primitive::Triangles, 0, 3);

        self.surface.swap_buffers(&self.context).unwrap();
        
    }
    
}

fn gl_load_with<F: Fn(&CStr) -> *const void>(f: F) {

    // load opengl functions (without allocating)
    let mut buf = [0u8; 2048];
    lsg_gl::load_with(move |name| {

        let len = name.len();

        buf[..len].copy_from_slice(name.as_bytes()); // copy the name
        buf[len] = 0u8; // add the null byte

        let cstr = CStr::from_bytes_with_nul(&buf[..=len]).unwrap();

        f(cstr)

    });

}

