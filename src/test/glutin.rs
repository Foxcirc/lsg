
use std::{ffi::CStr, num::NonZeroU32};

use async_executor::LocalExecutor;
use futures_lite::future::block_on;
use glutin::{display::Display, context::PossiblyCurrentContext, surface::{Surface, WindowSurface}};
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
        .with_title("lsg test")
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
    display: Display,
    context: PossiblyCurrentContext,
    surface: Surface<WindowSurface>
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
            .with_context_api(ContextApi::Gles(None))
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

        // load opengl functions (without allocating)
        let mut buf = [0u8; 2048];
        gl::load_with(|name| {
            debug_assert!(name.is_ascii());
            let len = name.len();
            buf[..len].copy_from_slice(name.as_bytes()); // copy the name
            buf[len] = 0u8; // add the null byte
            let cstr = CStr::from_bytes_with_nul(&buf[..=len]).unwrap();
            display.get_proc_address(cstr)
        });

        Self {
            display,
            context,
            surface
        }
        
    }

    pub fn resize(&mut self, size: PhysicalSize<u32>) {

        use glutin::surface::GlSurface;

        self.surface.resize(
            &self.context,
            NonZeroU32::new(size.width).unwrap(),
            NonZeroU32::new(size.height).unwrap()
        );

    }

    pub fn render(&self) {

        use glutin::surface::GlSurface;

        unsafe { gl::ClearColor(1.0, 0.0, 0.0, 1.0) };
        self.surface.swap_buffers(&self.context).unwrap();
        
    }
    
}

