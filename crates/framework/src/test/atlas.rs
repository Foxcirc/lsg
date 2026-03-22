
use common::{IsSurface, LogicalPoint, LogicalSize, PhysicalPoint, PhysicalSize, Shape};
use desktop::{Event, WindowEvent};
use futures_lite::future::block_on;
use render::PartialVertex;

use std::sync::Arc;

#[test]
pub fn atlas() -> Result<(), Box<dyn std::error::Error>> {
    desktop::EventLoop::run(desktop::EventLoopConfig {
        appid: file!().into(),
        intercept: false,
    }, app)?
}

fn app(evl: Arc<desktop::EventLoop>) -> Result<(), Box<dyn std::error::Error>> {

    let mut renderer = render::GlRenderer::new(&*evl)?;
    let mut atlas = render::GlTextureAtlas::new(&renderer);

    let window = desktop::Window::new(&evl);
    window.sizehint(PhysicalSize::quad(12));

    let mut surface = render::GlSurface::new(&renderer, &window);
    let mut storage = render::GlRenderStorage::new(&renderer, window.size());

    println!("UPLOAD!");
    let index = atlas.upload(&renderer, &[255; 12*12*4], PhysicalSize::quad(12));

    loop {
        match block_on(evl.next())? {
            Event::Window { event: WindowEvent::Redraw, .. } => {

                let geometry = render::DrawableGeometry {
                    source: &[&render::VertexGeometry {
                        vertices: Vec::from([
                            // Triangle 1
                            PartialVertex::new([0,  0],    render::FillKind::Filled, 0),
                            PartialVertex::new([5000,  0], render::FillKind::Filled, 0),
                            PartialVertex::new([5000, 5000], render::FillKind::Filled, 0),
                            // Triangle 2
                            PartialVertex::new([0, 0],    render::FillKind::Filled, 0),
                            PartialVertex::new([0, 5000],    render::FillKind::Filled, 0),
                            PartialVertex::new([5000, 5000],    render::FillKind::Filled, 0),
                        ]),
                        shapes: Vec::from([Shape::new(0u16..6u16)]),
                    }],
                    instances: &[
                        render::Instance {
                            target: render::GeometryTarget { geometry: 0, shape: 0 },
                            pos: LogicalPoint::ZERO,
                            size: LogicalSize::from(window.size()),
                            texture: render::TextureKind::Atlas(index, PhysicalPoint::ZERO),
                        }
                    ]
                };

                println!("DRAW");
                window.present();
                renderer.draw(&geometry, &atlas, &storage);
                renderer.blit(&surface, &storage);
                renderer.swap(&surface);
            },
            Event::Window { event: WindowEvent::Resize { size, .. }, .. } => {
                println!("RESIZE: {size:?}");
                surface.resize(&renderer, size);
                storage.resize(&renderer, size);
            },
            Event::Window { event: WindowEvent::ShouldClose, .. } => {
                break
            },
            _ => (),
        }
    }

    println!("DOWNLOAD!");
    let data = atlas.inspect();

    println!("data = {:?}", data);
    println!("data.len = {:?}", data.len());

    Ok(())

}
