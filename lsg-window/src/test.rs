
use std::io::Write;

#[test]
fn wayland() -> anyhow::Result<()> {

    use crate::wayland::*;

    // create a new event loop, this will initialize a connection to the wayland compositor
    let mut evl = EventLoop::new("lsg-test")?;

    let proxy = evl.new_proxy();
    proxy.send(Event::User("lsg-test"))?;

    // we will be using the built-in gl functionality
    let egl = EglInstance::new(&mut evl)?;

    lsg_gl::load_with(|name|
        egl.get_proc_address(name).unwrap() as *const _
    );
    
    let size = Size { width: 1920 , height: 1080 };
    let window = Window::new(&mut evl, size);
    // window.transparency(false);
    // window.force_size(Some(size));

    // window.margin(20);
    // window.anchor(WindowAnchor::Right);
    // window.interactivity(KbInteractivity::None);
    
    let mut ctx = EglContext::new(&egl, &window, size)?; // create an egl context for our window
    // ctx.bind(&egl)?; // make the context current

    window.transparency(true);

    evl.input(InputMode::SingleKey);

    let mut popup_window = None;
    let mut popup_ctx = None;

    let mut max = false;

    // run the event loop
    evl.run(move |evl, event| {
        match event {
            Event::User(message) => {
                println!("title: {}", &message);
                window.title(message);
            },
            Event::Resume => {
                // window.title("no-test");
            }, // TODO: implement the suspend event
            Event::Suspend => unimplemented!(),
            Event::Quit => evl.exit(),
            Event::MonitorUpdate { id, state } => {
                println!("new monitor with id {id}: {state:?}");
                println!("refresh as fps: {}", state.info.fps());
            },
            Event::MonitorRemove { .. } => todo!(),
            Event::Window { id, event } if id == window.id() => match event {
                WindowEvent::Close => {
                    // drop(window);
                    // std::thread::sleep_ms(1000);
                    // std::process::exit(0);
                    evl.request_quit();
                },
                WindowEvent::Redraw => {
                    ctx.bind(&egl).unwrap();
                    lsg_gl::clear(0.3, 0.1, 0.6, 0.0);
                    let token = window.pre_present_notify();
                    let damage = [Rect::new(0, 0, 100, 100)];
                    ctx.swap_buffers(&egl, &damage, token).unwrap();
                    // window.request_redraw(token); // fuck. you.
                },
                WindowEvent::Resize { size, .. } => {
                    ctx.bind(&egl).unwrap();
                    ctx.resize(size);
                    lsg_gl::resize_viewport(size.width, size.height);
                },
                WindowEvent::Rescale { scale } => {
                    println!("NEW SCALE: {scale}");
                },
                WindowEvent::Decorations { active } => {
                    println!("server side decorations are: {}", if active { "enabled" } else { "disabled" });
                }
                WindowEvent::MouseDown { x, y, button } => {
                    println!("mouse down at ({}, {}) ({:?} button)", x, y, button);
                },
                WindowEvent::MouseScroll { axis, value } => {
                    println!("scrolling with axis = {:?}, value = {}", axis, value);
                },
                WindowEvent::KeyDown { key, .. } => {
                    if let Key::Tab = key {
                        max = !max;
                        window.fullscreen(max, None);
                    }
                    else if let Key::ArrowUp = key {
                        let size = Size { width: 250, height: 100 };
                        let popup_window2 = PopupWindow::new(evl, size, &window);
                        let popup_ctx2 = EglContext::new(&egl, &popup_window2, size).unwrap();
                        popup_window = Some(popup_window2);
                        popup_ctx = Some(popup_ctx2);
                    }
                    if !key.modifier() {
                        if let Key::Char(chr) = key {
                            print!("{}", chr);
                            std::io::stdout().flush().unwrap();
                        } else {
                            println!("\n{:?}", key);
                        }
                    }
                },
                WindowEvent::TextCompose { chr } => {
                    print!("\x1b[31m{}\x1b[0m", chr);
                    std::io::stdout().flush().unwrap();
                },
                WindowEvent::TextInput { chr } => {
                    print!("{}", chr);
                    std::io::stdout().flush().unwrap();
                },
                _ => (),
            },
            Event::Window { id, event } if Some(id) == popup_window.as_ref().map(|val| val.id()) => {
                let popup_window2 = popup_window.as_mut().unwrap();
                let popup_ctx2 = popup_ctx.as_mut().unwrap();
                match event {
                    WindowEvent::Redraw => {
                        popup_ctx2.bind(&egl).unwrap();
                        println!("@popup redraw");
                        lsg_gl::clear(0.2, 0.7, 0.1, 1.0);
                        let token = window.pre_present_notify();
                        // let damage = [Rect::INFINITE];
                        let damage = [Rect::new(0, 0, 100, 100)];
                        popup_ctx2.swap_buffers(&egl, &damage, token).unwrap();
                        window.request_redraw(token); // fuck. you.
                    },
                    WindowEvent::Resize { size, .. } => {
                        println!("@popup resize");
                        popup_ctx2.bind(&egl).unwrap();
                        popup_ctx2.resize(size);
                        lsg_gl::resize_viewport(size.width, size.height);
                    },
                    WindowEvent::Close => {
                        drop(popup_ctx.take());
                        drop(popup_window.take());
                    },
                    WindowEvent::MouseDown { button, .. } => {
                        println!("@popup MOUSE DOWN button = {:?}", button);
                    },
                    WindowEvent::KeyDown { key, .. } => {
                        println!("@popuo KEY DOWN key = {:?}", key);
                    }
                    _ => (),
                }
            },
            _ => ()
        }
    })?;

    Ok(())
    
}
