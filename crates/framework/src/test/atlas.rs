
use common::{LogicalPoint, LogicalSize, PhysicalSize, Shape};
use render::PartialVertex;

#[test]
pub fn atlas() -> Result<(), Box<dyn std::error::Error>> {
    EventLoop::run(EventLoopConfig { appid: file!().into() }, app)?
}

fn app(evl: Arc<EventLoop>) -> Result<(), Box<dyn std::error::Error>> {

    let renderer = render::GlRenderer::new(&*evl)?;
    let atlas = render::GlTextureAtlas::new(&renderer);

    let pixbuf = GlPixelBuffer::new();
    pixbuf.resize(&renderer, PhysicalSize::new(1000, 1000));

    let geometry = render::DrawableGeometry {
        source: &[&render::VertexGeometry {
            vertices: Vec::from([
                PartialVertex::new([0,    0],    render::FillKind::Filled, 0),
                PartialVertex::new([0,    1000], render::FillKind::Filled, 0),
                PartialVertex::new([1000, 1000], render::FillKind::Filled, 0),
                PartialVertex::new([1000, 0],    render::FillKind::Filled, 0),
            ]),
            shapes: Vec::from([Shape::new(0u16..4u16)]),
        }],
        instances: &[
            render::Instance {
                target: render::GeometryTarget { geometry: 0, shape: 0 },
                pos: LogicalPoint::ZERO,
                size: LogicalSize::FULL,
                texture: render::TextureKind::Color(255, 0, 0, 1),
            }
        ]
    };

    renderer.draw(&geometry, &pixbuf);

    // pixbuf.data;

    Ok(())

}
