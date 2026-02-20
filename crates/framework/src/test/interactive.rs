
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

fn app(mut evl: Arc<EventLoop>) -> Result<(), Box<dyn std::error::Error>> {

    let window = Window::new(&evl);

    window.title(&evl.config().appid);
    window.transparency(true);
    window.resize(LogicalSize::new(500, 500));

    let mut renderer = render::GlRenderer::new(&*evl).unwrap();
    let mut surface = render::GlSurface::new(&renderer, &window).unwrap();

    let mut geometry = shaper::CurveGeometry::new();
    let mut instances: Vec<Instance> = Vec::new();
    let mut shaper = shaper::GeometryShaper::new();

    geometry.points.push(CurvePoint::new(40, 400, PointKind::Base));
    geometry.points.push(CurvePoint::new(240, 400, PointKind::Ctrl));
    geometry.points.push(CurvePoint::new(240, 200, PointKind::Base));

    geometry.shapes.push(Shape::new(0..3));

    instances.push(Instance {
        pos: LogicalPoint::new(0, 0),
        size: LogicalSize::MAX,
        target: [0, 0],
    });

    unsafe { render::SHAPE_TAKE_PART = usize::MAX };

    let mut lt = Instant::now();

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {

                        println!("got redraw event...");

                        let vertices = shaper.process(&geometry);

                        let drawable = DrawableGeometry {
                            source: &[vertices],
                            instances: &instances,
                        };

                        window.pre_present_notify();
                        renderer.draw(&drawable, &surface).unwrap();

                        println!("dt: {:?}", Instant::now() - lt);
                        lt = Instant::now();

                    },

                    WindowEvent::Resize { size, .. } => {
                        println!("got resize event: new size = {size:?}");
                        surface.resize(&renderer, size).unwrap()
                    },

                    WindowEvent::MouseMotion { point } => {
                        if let Some(gpoint) = geometry.points.last_mut() {
                            *gpoint = CurvePoint::new(point.x, point.y, gpoint.kind());
                            window.redraw(&mut evl);
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
                            CurvePoint::new(point.x, point.y, kind)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.target.end += 1;
                            window.redraw(&mut evl);
                        }

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

    });

    Ok(())

}
