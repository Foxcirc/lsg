
//! Interactive test to try out features that are currently being worked on.

use std::{sync::Arc, time::Instant};

use futures_lite::future::block_on;

use desktop::*;
use common::*;
use render::{DrawableGeometry, shaper};

#[test]
fn interactive() -> Result<(), Box<dyn std::error::Error>> {
    EventLoop::run(EventLoopConfig { appid: file!().into() }, app)?
}

fn app(evl: Arc<EventLoop>) -> Result<(), Box<dyn std::error::Error>> {

    let window = Window::new(&evl);

    window.title(&evl.config().appid);
    window.transparency(true);

    let mut renderer = render::GlRenderer::new(&*evl)?;
    let mut surface = render::GlSurface::new(&renderer, &window)?;

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
    });

    unsafe { render::SHAPE_TAKE_PART = usize::MAX };

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

                        window.present();
                        renderer.draw(&drawable, &surface)?;

                    },

                    WindowEvent::Resize { size, .. } => {
                        println!("got resize event: new size = {size:?}");
                        surface.resize(&renderer, size)?
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
                            shape.target.end += 1;
                            window.redraw();
                        }

                    },

                    WindowEvent::MouseScroll { axis, value } => {
                        instances.last_mut().map(|it|
                            match axis {
                                ScrollAxis::Horizontal => it.pos.x -= value / 10,
                                ScrollAxis::Vertical => it.pos.y += value / 10
                            }
                        );
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
