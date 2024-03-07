
use std::error::Error as StdError;

#[test]
fn wayland() -> Result<(), Box<dyn StdError>> {

    use crate::wayland::*;

    // create a new event loop, this will initialize a connection to the wayland compositor
    let mut evl = EventLoop::new()?;

    // EVH SHOULD ONLY BE NEEDED FOR
    // - Creating an EGL Instance CHECK
    // - Opening a window CHECK
    // - Exiting CHECK
    // - Iterating monitors NAH
    // - CONTINIOUS REDRAW CHECK

    let proxy = evl.proxy();
    proxy.send("Hello world!")?;

    // we will be using the built-in gl functionality
    let egl = EglInstance::new(&mut evl)?;
    
    let mut window = Window::new(&mut evl)?; // the actual window is stored inside the event loop, wh represents a handle to the window, it's just a usize
    let ctx = EglContext::new(&egl, &window)?; // create an egl context for our window
    ctx.bind(&egl)?; // make the context current

    lsg_gl::load_with(|name|
        egl.get_proc_address(name).unwrap() as *const _
    );

    window.transparency(false);

    // run the event loop
    evl.run(move |evl, event| {
        match event {
            Event::Resume => {
                window.title("what the fuck");
            }, // TODO: implement the suspend event
            Event::User(message) => println!("{}", message),
            Event::Window { id: _id, event } => match event {
                WindowEvent::Close => evl.exit(),
                WindowEvent::Redraw => {
                    lsg_gl::clear(0.1, 0.0, 0.0, 1.0);
                    let token = window.pre_present_notify();
                    ctx.swap_buffers(&egl, token).unwrap();
                    // window.request_redraw();
                },
                WindowEvent::Resize { width, height } => {
                    println!("resizing to: {}x{}", width, height);
                    ctx.resize(width, height);
                    lsg_gl::resize_viewport(width, height);
                },
            },
        }
    })?;

    Ok(())
    
}
