
//! Interactive test to try out features that are currently being worked on.

use futures_lite::future::block_on;

use desktop::*;
use common::*;
use render::{DrawableGeometry, shaper};

#[test]
fn interactive() -> Result<(), Box<dyn std::error::Error>> {
    EventLoop::run(app, "interactive test")?
}

fn app(mut evl: EventLoop) -> Result<(), Box<dyn std::error::Error>> {

    let mut window = Window::new(&mut evl, Size::new(500, 500));

    window.set_title(evl.app_name());
    window.set_transparency(true);

    let mut renderer = render::GlRenderer::new(&evl).unwrap();
    let mut surface = render::GlSurface::new(&renderer, &*window, Size::new(500, 500)).unwrap();

    let mut geometry = shaper::CurveGeometry::new();
    let mut instances: Vec<Instance> = Vec::new();
    let mut shaper = shaper::GeometryShaper::new();

    geometry.points.push(CurvePoint::new(40, 400, PointKind::Base));
    geometry.points.push(CurvePoint::new(240, 400, PointKind::Ctrl));
    geometry.points.push(CurvePoint::new(240, 200, PointKind::Base));

    geometry.shapes.push(Shape::new(0..3));

    instances.push(Instance {
        pos: Point::new(0.0, 0.0),
        scale: 10_000,
        target: [0, 0],
    });

    unsafe { render::SHAPE_TAKE_PART = usize::MAX };

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {

                        let vertices = shaper.process(&geometry);

                        let drawable = DrawableGeometry {
                            source: &[vertices],
                            instances: &instances,
                        };

                        window.pre_present_notify();
                        renderer.draw(&drawable, &surface).unwrap();

                    },

                    WindowEvent::Resize { size, .. } => surface.resize(&renderer, size).unwrap(),

                    WindowEvent::MouseMotion { x, y } => {
                        if let Some(point) = geometry.points.last_mut() {
                            *point = CurvePoint::new(x, y, point.kind());
                            window.redraw_with_vsync(&mut evl);
                        }
                    },

                    WindowEvent::MouseDown { button: MouseButton::Left, x, y } => {

                        println!("add point {:?}", geometry.points.last().unwrap());

                        geometry.points.push(
                            CurvePoint::new(x as u16, y as u16, PointKind::Base)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.target.end += 1;
                            window.redraw_with_vsync(&mut evl);
                        }

                    },


                    WindowEvent::MouseDown { button: MouseButton::Right, x, y } => {

                        println!("add point {:?}", geometry.points.last().unwrap());

                        geometry.points.push(
                            CurvePoint::new(x as u16, y as u16, PointKind::Ctrl)
                        );

                        if let Some(shape) = geometry.shapes.last_mut() {
                            shape.target.end += 1;
                            window.redraw_with_vsync(&mut evl);
                        }

                    },

                    WindowEvent::Close => evl.quit(),

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
