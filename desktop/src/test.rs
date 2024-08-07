
use futures_lite::future::block_on;
use tracing::{debug, trace};

use desktop::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run(app, "lsg/test")?
}

fn app(mut evl: EventLoop<&str>) -> Result<(), Box<dyn std::error::Error>> {

    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_ansi(true)
        .with_max_level(tracing::Level::DEBUG)
        .with_test_writer()
        .finish();

    tracing::subscriber::set_global_default(subscriber).unwrap();

    // we will be using the built-in gl functionality
    let egl = EglInstance::new(&mut evl)?;

    gl::load_with(|name|
        egl.get_proc_address(name).unwrap() as *const _
    );
    
    let size = Size { width: 1920 , height: 1080 };
    let mut window = Window::new(&mut evl, size);
    
    let mut ctx = EglContext::new(&egl, &window, size)?; // create an egl context for our window
    ctx.bind()?; // make the context current

    window.set_title("lsg/test");
    window.set_transparency(true);
    window.set_input_mode(&mut evl, InputMode::SingleKey);

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {

                Event::Resume => debug!("resuming"),

                Event::Window { event, .. } => match event {

                    WindowEvent::Redraw => {
                        gl::clear(0.8, 0.8, 0.8, 0.5);
                        let token = window.pre_present_notify();
                        ctx.swap_buffers(None, token).unwrap();
                    },

                    WindowEvent::Resize { size, .. } => {
                        gl::resize_viewport(size.width, size.height);
                        ctx.resize(size);
                    },

                    WindowEvent::Close => {
                        evl.quit();
                    },

                    other => trace!("unhandeled window event '{:?}'", other),
                    
                },

                Event::Quit { reason } => {
                    debug!("quitting: {:?}", reason);
                    break
                },

                other => trace!("unhandeled event '{:?}'", other),
                
            }
            
        }

    });

    Ok(())
    
}
