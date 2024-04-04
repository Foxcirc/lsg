use std::io::Write;


#[test]
fn wayland() -> anyhow::Result<()> {

    use crate::wayland::*;

    // create a new event loop, this will initialize a connection to the wayland compositor
    let mut evl = EventLoop::new()?;

    // EVH SHOULD ONLY BE NEEDED FOR
    // - Creating an EGL Instance CHECK
    // - Opening a window CHECK
    // - Exiting CHECK
    // - Iterating monitors NAH
    // - CONTINIOUS REDRAW CHECK
    // - FOCUS events

    let proxy = evl.new_proxy();
    proxy.send(Event::User("Hello world!"))?;

    // we will be using the built-in gl functionality
    let egl = EglInstance::new(&mut evl)?;

    lsg_gl::load_with(|name|
        egl.get_proc_address(name).unwrap() as *const _
    );
    
    let size = Size { width: 500, height: 500 };
    let mut window = Window::new(&mut evl, size); // the actual window is stored inside the event loop, wh represents a handle to the window, it's just a usize
    // window.force_size(Some(size));
    // window.max_size(Some(size));
    let ctx = EglContext::new(&egl, &window, size)?; // create an egl context for our window
    ctx.bind(&egl)?; // make the context current

    window.request_redraw();
    window.transparency(false);

    evl.input_mode(InputMode::Text);

    // run the event loop
    evl.run(move |evl, event| {
        match event {
            Event::Resume => {
                window.title("lsg-test");
                window.class("lsg-test");
            }, // TODO: implement the suspend event
            Event::Suspend => unimplemented!(),
            Event::Quit => evl.exit(),
            Event::User(message) => println!("{}", message),
            Event::Window { id: _id, event } => match event {
                WindowEvent::Close => evl.quit(),
                WindowEvent::Redraw => {
                    lsg_gl::clear(1.0, 0.0, 0.0, 1.0);
                    let token = window.pre_present_notify();
                    ctx.swap_buffers(&egl, token).unwrap();
                    // window.request_redraw();
                },
                WindowEvent::Resize { size, flags } => {
                    // println!("resizing to: {:?}", size);
                    // eprintln!("FLAGS: {:?}", flags);
                    // let size = Size { width: 300, height: 300 };
                    // ctx.resize(Size { width: 300, height: 300 });
                    ctx.resize(size);
                    lsg_gl::resize_viewport(size.width, size.height);
                    // window.fullscreen();
                    // window.request_redraw();
                },
                WindowEvent::MouseDown { x, y, button } => {
                    println!("mouse down at ({}, {}) ({:?} button)", x, y, button);
                },
                WindowEvent::MouseScroll { axis, value } => {
                    println!("scrolling with axis = {:?}, value = {}", axis, value);
                },
                WindowEvent::KeyDown { key, .. } => {
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
                    print!("{:?}", chr);
                    std::io::stdout().flush().unwrap();
                },
                _ => (),
            },
        }
    })?;

    Ok(())
    
}
