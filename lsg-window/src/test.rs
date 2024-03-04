
#[test]
fn wayland() {

    use crate::wayland::*;

    let evl = EventLoop::new();

    evl.run(|evh, event| {

        match event {
            Event::Init => {
                let window = WindowBuilder::new().build(evh);
                
            }
        }

    });
    
}
