
use std::error::Error as StdError;

#[test]
fn wayland() -> Result<(), Box<dyn StdError>> {

    use crate::wayland::*;

    // create a new event loop, this will initialize a connection to the wayland compositor
    let mut evl = EventLoop::new()?;

    // EVH SHOULD ONLY BE NEEDED FOR
    // - Creating an EGL Instance CHECK
    // - Opening a window CHECK
    // - Exiting
    // - Iterating monitors

    // we will be using the built-in gl functionality
    let egl = EglInstance::new(&mut evl)?;
    
    let window = Window::new(&mut evl)?; // the actual window is stored inside the event loop, wh represents a handle to the window, it's just a usize
    let ctx = EglContext::new(&egl, &window)?; // create an egl context for our window
    ctx.bind(&egl)?; // make the context current

    // run the event loop
    evl.run(move |evl, event| {
        match event {
            Event::Init => evl.exit(),
        }
    })?;

    Ok(())
    
}
