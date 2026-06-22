
//! Interactive test to try out features that are currently being worked on.

use std::sync::Arc;

use futures_lite::future::block_on;

use desktop::*;
use common::*;
use render::{DrawableGeometry, shaper};

const APPID: &str = file!();

#[test]
fn interactive() -> Result<(), Box<dyn std::error::Error>> {
    desktop::EventLoop::run(desktop::EvlConfig {
        appid: APPID.into(),
        intercept: false,
    }, app)?
}

fn app(evl: Arc<EventLoop>) -> Result<(), Box<dyn std::error::Error>> {

    let window = Window::new(&evl);

    window.title(APPID);
    window.transparency(true);

    let mut renderer = render::Renderer::new(&*evl)?;
    let mut storage = graphics::Texture::new(&renderer.gp, window.size(), None);
    let mut surface = graphics::Surface::new(&renderer.gp, &window);
    let atlas = render::TextureAtlas::new(&renderer);

    let mut geometry = shaper::CurveGeometry::new();
    let mut shaper = shaper::GeometryShaper::new();

    let mut instances: Vec<render::Instance> = Vec::new();

    geometry.points.push(CurvePoint::new(1250, 1250, PointKind::Base));
    geometry.points.push(CurvePoint::new(3750, 1250, PointKind::Ctrl));
    geometry.points.push(CurvePoint::new(3750, 3750, PointKind::Base));

    geometry.shapes.push(Shape::new(0..3));

    instances.push(render::Instance {
        target: render::GeometryTarget { geometry: 0, shape: 0 },
        pos: LogicalPoint::new(0, 0),
        size: LogicalSize::new(500, 500),
        texture: render::TextureKind::Color(0, 255, 100, 255),
    });

    // run the event loop
    block_on(async move {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {

                        let vertices = shaper.process(&geometry);

                        let drawable = DrawableGeometry {
                            source: &[vertices],
                            instances: &instances,
                        };

                        storage.clear([0.0, 0.0, 0.0, 1.0]);

                        window.present();
                        renderer.draw(&drawable, &atlas, &mut storage);

                        surface.blit(&mut storage);
                        surface.swap();

                    },

                    WindowEvent::Resize { size, .. } => {
                        println!("got resize event: new size = {size:?}");
                        storage.resize(size, None);
                        surface.resize(size);
                    },

                    WindowEvent::MouseMotion { point } => {
                        if let Some(gpoint) = geometry.points.last_mut() {
                            *gpoint = CurvePoint::new(point.x * 10, point.y * 10, gpoint.kind());
                            window.redraw();
                        }
                    },

                    WindowEvent::MouseDown { point, button } => {

                        let kind = match button {
                            MouseButton::Left => PointKind::Base,
                            MouseButton::Right => PointKind::Ctrl,
                            _ => continue,
                        };

                        println!("add point {:?}", point);

                        geometry.points.push(
                            CurvePoint::new(point.x * 10, point.y * 10, kind)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.end += 1;
                            window.redraw();
                        }

                    },

                    WindowEvent::MouseScroll { dx, dy } => {
                        instances.last_mut().map(|it| {
                            it.pos.x -= dx / 10;
                            it.pos.y += dy / 10;
                        });
                        window.redraw();
                    },

                    WindowEvent::ShouldClose => evl.quit(),

                    other => println!("unhandeled window event '{:?}'", other),

                },

                Event::Quit { reason } => {
                    println!("quitting: {:?}", reason);
                    break
                },

                other => println!("unhandeled event '{:?}'", other),

            }

        }

        Ok(())

    })

}
