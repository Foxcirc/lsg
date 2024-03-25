
use std::{ffi::{c_void as void, CStr}, num::NonZeroU32};

use async_executor::LocalExecutor;
use futures_lite::future::block_on;
use glutin::{display::Display, context::PossiblyCurrentContext, surface::{Surface, WindowSurface}};
use lsg_gl::{VertexArrayObject, LinkedProgram, to_small_cstr};
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
                WindowEvent::RedrawRequested => {
                    state.render(&window);
                    // window.request_redraw();
                },
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

        lsg_gl::debug_message_callback(lsg_gl::colored_print);
        // lsg_gl::polygon_mode(lsg_gl::PolygonMode::Line);

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
        let program = builder.link().unwrap();

        // setup for the triangle
        let vao = lsg_gl::gen_vertex_array();
        lsg_gl::bind_vertex_array(&vao);

        let vertices: &[f32] = &[
            // positions         // colors
             0.5, -0.5, 0.0,  1.0, 0.0, 0.0,   // bottom right
            -0.5, -0.5, 0.0,  0.0, 1.0, 0.0,   // bottom left
             0.0,  0.5, 0.0,  0.0, 0.0, 1.0    // top
           //  0.5,  0.5, 0.0, /* top right */      1.0, 1.0, 1.0,
           //  0.5, -0.5, 0.0, /* bottom right */   1.0, 1.0, 1.0,
           // -0.5, -0.5, 0.0, /* bottom left */    1.0, 1.0, 1.0,
           // -0.5,  0.5, 0.0, /* top left */       1.0, 1.0, 1.0,
        ];

        let indices: &[u32] = &[
            0, 1, 3,
            // 1, 2, 3,
        ];

        let vbo = lsg_gl::gen_buffer(lsg_gl::BufferType::ArrayBuffer);
        lsg_gl::buffer_data(&vbo, vertices, lsg_gl::DrawHint::Static);
        
        let ebo = lsg_gl::gen_buffer(lsg_gl::BufferType::ElementBuffer);
        lsg_gl::buffer_data(&ebo, indices, lsg_gl::DrawHint::Static);
        
        let vertex_pos = lsg_gl::attrib_location(&program, "vertexPos").unwrap(); // TODO: add Attrib type
        let vertex_color = lsg_gl::attrib_location(&program, "vertexColor").unwrap(); // TODO: add Attrib type
        lsg_gl::vertex_attribs(&vbo, vertex_pos, 3, lsg_gl::DataType::Float, false, 6, 0);
        lsg_gl::vertex_attribs(&vbo, vertex_color, 3, lsg_gl::DataType::Float, false, 6, 3);

        // let input_color = lsg_gl::uniform_location(&program, "inputColor").unwrap(); // TODO: add Uniform type
        // lsg_gl::uniform_3f(&program, input_color, 1.0, 1.0, 1.0);
       
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
        println!("resized: {:?}", size);

        self.surface.resize(
            &self.context,
            NonZeroU32::new(size.width).unwrap(),
            NonZeroU32::new(size.height).unwrap()
        );

        lsg_gl::resize_viewport(size.width, size.height);

    }

    pub fn render(&self, window: &Window) {

        use glutin::surface::GlSurface;

        lsg_gl::clear(1.0, 0.0, 0.0, 1.0);
        // lsg_gl::draw_arrays(&self.program, &self.vao, lsg_gl::Primitive::Triangles, 0, 6);
        lsg_gl::draw_elements(&self.program, &self.vao, lsg_gl::Primitive::Triangles, 3);

        window.pre_present_notify();
        self.surface.swap_buffers(&self.context).unwrap();
        
    }
    
}

fn gl_load_with<F: Fn(&CStr) -> *const void>(f: F) {

    // load opengl functions (without allocating)
    let mut buf = [0u8; 1024];
    lsg_gl::load_with(move |name| {
        let cname = to_small_cstr(&mut buf, name);
        f(cname)
    });

}

