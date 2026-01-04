
use std::{fs, panic::catch_unwind};

use futures_lite::future::block_on;

use desktop::*;
use common::*;
use render::{DrawableGeometry, shaper};

#[test]
fn svgs() -> Result<(), Box<dyn std::error::Error>> {
    EventLoop::run(EventLoopConfig { appid: file!().into() }, app)?
}

fn app(mut evl: EventLoop) -> Result<(), Box<dyn std::error::Error>> {

    let mut window = Window::new(&mut evl, Size::new(1000, 1000));

    window.set_title(&evl.config().appid);
    window.set_transparency(true);

    let mut renderer = render::GlRenderer::new(&evl).unwrap();
    let mut surface = render::GlSurface::new(&renderer, &*window, Size::new(1000, 1000)).unwrap();

    let mut geometry = shaper::CurveGeometry::new();
    let mut instances: Vec<Instance> = Vec::new();
    let mut shaper = shaper::GeometryShaper::new();

    // Load all the svgs.

    let mut errors = 0;
    let mut total = 0;

    let mut parsed_svgs: Vec<(String, Vec<Vec<CurvePoint>>)> = Vec::new();

    let topdirs = fs::read_dir("assets/RemixIcons").unwrap();
    for topdir in topdirs {
        let topdir = topdir.unwrap(); // this is a category directory
        let subdir = fs::read_dir(topdir.path()).unwrap();
        println!("current subdir: {:?}", topdir.file_name());
        for file in subdir {
            let file = file.map_err(|it| it.to_string()); // make it unwind-safe
            total += 1;
            // test case:
            let result = catch_unwind(move || {

                let file = file.unwrap(); // this is a single svg file
                // println!("file: {:?}", file.file_name());
                let contents = fs::read_to_string(file.path()).unwrap();
                let (_, svg) = widget::svg::parser::svg_file(&contents).unwrap();
                let parsed = widget::svg::path_to_shape(svg.path, 416.66).unwrap();
                (file.path().to_str().unwrap().to_owned(), parsed)

            });
            match result {
                Ok(parsed) => parsed_svgs.push(parsed),
                Err(_) => errors += 1,
            }
        }
    }

    println!("\n--- there were {} errornous shapes (of {} total) ---\n", errors, total);

    // Now we layout the geometry.

    let mut page = 0;

    unsafe { render::SHAPE_TAKE_PART = usize::MAX };

    // Run the event loop.
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

                    WindowEvent::MouseDown { x, y, .. } => {

                        // let physical_x = 10 + (idx % 10) * 100; // idx from 0 to 9
                        // let physical_y = 10 + (idx / 10) * 100;

                        let idx_first_half = (x as f32 - 10.0) / 100.0;
                        let idx_second_half = ((y as f32 - 10.0) / 100.0).floor() * 10.0;

                        let idx = page * 50 + idx_first_half.floor() as usize + idx_second_half.floor() as usize;

                        println!("{}. FILE: {:?}", idx, parsed_svgs[idx]);

                    }

                    WindowEvent::KeyDown { key: Key::Space, .. } => {

                        // Switch to the next page.

                        geometry.points.clear();
                        geometry.shapes.clear();
                        instances.clear();

                        let start_idx = page * 50;

                        for (idx, (svg_path, svg_shapes)) in parsed_svgs[start_idx..start_idx+100].iter().enumerate() {

                            // 10px - 80x80px - 10px --- ETC --- ETC

                            let physical_x = 10 + (idx % 10) * 100; // idx from 0 to 9
                            let physical_y = 10 + (idx / 10) * 100;

                            for shape in svg_shapes {

                                println!("{idx}. adding {}", svg_path);

                                let shape_idx = geometry.shapes.len();
                                let start = geometry.points.len();
                                geometry.points.extend(shape);
                                let end = geometry.points.len();
                                geometry.shapes.push(Shape::new(start as u16..end as u16));
                                instances.push(Instance {
                                    pos: Point::new(physical_x as f32, physical_y as f32),
                                    scale: 80,
                                    target: [0, shape_idx],
                                });
                            }

                        }

                        page += 1;

                        window.redraw_with_vsync(&mut evl);

                    }

                    WindowEvent::MouseEnter => {
                        CursorStyle::Predefined { shape: CursorShape::Crosshair }.apply(&mut evl);
                    }

                    WindowEvent::MouseMotion { .. } => (),
                    WindowEvent::Resize { size, .. } => surface.resize(&renderer, size).unwrap(),
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
