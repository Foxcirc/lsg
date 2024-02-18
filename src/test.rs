
use async_executor::LocalExecutor;
use futures_lite::future::block_on;
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
        .with_title("lsg-winit + opengl test")
        .build(&evl).await.unwrap();

    let mut state = WgpuState::new(&window).await;

    loop {

        let event = evl.next().await;

        match event {

            Event::WindowEvent { window_id: _, event } => match event {
                WindowEvent::CloseRequested => break,
                WindowEvent::Resized(size) => state.resize(size),
                WindowEvent::RedrawRequested => {
                    match state.render() {
                        Ok(()) => (),
                        Err(wgpu::SurfaceError::Lost) => state.resize(state.size),
                        Err(wgpu::SurfaceError::OutOfMemory) => panic!(),
                        other => println!("surface error: {:?}", other),
                    }
                },
                _ => (),
            },

            _ => ()
            
        }

    }
    
}

struct WgpuState<'w> {
    surface: wgpu::Surface<'w>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    size: PhysicalSize<u32>,
}

impl<'w> WgpuState<'w> {

    async fn new(window: &'w Window) -> Self {

        let size = window.inner_size();
        assert!(size.width + size.height > 0);

        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backend::Gl.into(),
            ..Default::default()
        });

        let surface = instance.create_surface(window).unwrap();

        let adapter = instance.request_adapter(&wgpu::RequestAdapterOptionsBase {
            power_preference: wgpu::PowerPreference::LowPower, // NOTE to myself: i have the wrong nvidia drivers or smth bc prime-run doesn't work for some reason
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }).await.unwrap();

        let (device, queue) = adapter.request_device(
            &Default::default(), // basically no required features etc.
            None // trace path
        ).await.unwrap();

        let caps = surface.get_capabilities(&adapter);
        let format = caps.formats.iter().filter(|val| val.is_srgb()).next().copied().unwrap();
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format,
            width: size.width,
            height: size.height,
            present_mode: caps.present_modes[0], // PresentMode::Fifo would be VSync
            alpha_mode: caps.alpha_modes[0],
            view_formats: Vec::new(),
            desired_maximum_frame_latency: 2, 
        };

        surface.configure(&device, &config);

        Self {
            surface,
            device,
            queue,
            config,
            size
        }
        
    }

    fn resize(&mut self, size: PhysicalSize<u32>) {

        assert!(size.width + size.height > 0);
        self.size = size;
        self.config.width = size.width;
        self.config.height = size.height;
        self.surface.configure(&self.device, &self.config);
        
    }

    // fn input(&mut self, event: &WindowEvent) -> bool {
    //     false
    // }

    // fn update(&mut self) {
    //     todo!()
    // }

    fn render(&mut self) -> Result<(), wgpu::SurfaceError> {

        let output = self.surface.get_current_texture()?;
        let view = output.texture.create_view(&Default::default());

        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: None,
            ..Default::default()
        });

        let render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: None,
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                    store: wgpu::StoreOp::Store,
                }
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None
        });

        drop(render_pass); // don't need it for now

        self.queue.submit([encoder.finish()]);

        output.present();

        Ok(())
    }

}
