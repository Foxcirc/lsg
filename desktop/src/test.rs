
use std::io::{Write, Read};

use futures_lite::future::block_on;
use desktop::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run(app, "lsg-test")?
}

fn app(mut evl: EventLoop<&str>) -> Result<(), Box<dyn std::error::Error>> {

    let proxy = EventProxy::new(&evl);
    proxy.send(Event::User("lsg-test"));

    // we will be using the built-in gl functionality
    let egl = EglInstance::new(&mut evl)?;

    gl::load_with(|name|
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

    window.set_transparency(true);
    window.set_input_mode(&mut evl, InputMode::SingleKey);

    let mut popup_window: Option<Window<&str>> = None;
    let mut popup_ctx: Option<EglContext> = None;
    let mut data_source = None;

    let mut max = false;
    let mut dnd_ready = false;

    let mut current_selection = None;

    // run the event loop
    block_on(async {

        while let Ok(event) = evl.next().await {

            match event {
                Event::User(message) => {
                    println!("title: {}", &message);
                    window.set_title(message);
                },
                Event::Resume => {
                    // window.title("no-test");
                },
                Event::Quit { reason } => {
                    println!("quit reason: {reason:?}");
                    return;
                },
                Event::MonitorUpdate { id, state } => {
                    println!("new monitor with id {id}: {state:?}");
                    println!("refresh as fps: {}", state.info().fps());
                },
                Event::MonitorRemove { .. } => todo!(),
                Event::SelectionUpdate { offer } => current_selection = offer,
                Event::Window { id, event } if id == window.id() => match event {
                    WindowEvent::Close => {
                        // drop(window);
                        // std::thread::sleep_ms(1000);
                        // std::process::exit(0);
                        evl.quit();
                    },
                    WindowEvent::Enter => {
                        // println!("focused");
                        // if let Some(offer) = evl.get_clip_board() {
                        //     println!("accepting... (available kinds: {:?})", offer.kinds());
                        //     let mut stream = offer.receive(DataKind::Text, false).unwrap();
                        //     let mut buf = String::new();
                        //     println!("reading... (blocking)");
                        //     let _res = stream.read_to_string(&mut buf);
                        //     println!("current clip board: {:?} (empty means an error)", buf);
                        // }
                    },
                    WindowEvent::Redraw => {
                        ctx.bind().unwrap();
                        gl::clear(0.3, 0.1, 0.6, 0.0);
                        let token = window.pre_present_notify();
                        ctx.swap_buffers(None, token).unwrap();
                        // window.request_redraw(token); // fuck. you.
                    },
                    WindowEvent::Resize { size, .. } => {
                        ctx.bind().unwrap();
                        // println!("resizing main surface!");
                        // TOOD: when popup is opened it messes up the whole resizing/configuring again :(
                        ctx.resize(size);
                        gl::resize_viewport(size.width, size.height);
                    },
                    WindowEvent::Rescale { scale } => {
                        println!("scale factor: {scale}");
                    },
                    WindowEvent::Decorations { active } => {
                        println!("server side decorations are: {}", if active { "enabled" } else { "disabled" });
                    }
                    WindowEvent::MouseDown { x, y, button } => {
                        println!("mouse down at ({}, {}) ({:?} button)", x, y, button);
                        if let MouseButton::Left = button {
                            dnd_ready = true;
                        }
                    },
                    WindowEvent::MouseUp { button, .. } => {
                        if let MouseButton::Left = button {
                            dnd_ready = false;
                        }
                    },
                    WindowEvent::MouseMotion { .. } => {
                        if dnd_ready {
                            let size = Size { width: 100, height: 100 };
                            let data = [255; 100 * 100 * 4];
                            let icon = CustomIcon::new(&mut evl, size, IconFormat::Argb8, &data).unwrap();
                            let ds = DataSource::new(&mut evl, DataKinds::TEXT, IoMode::Blocking);
                            // drag 'n drop
                            window.start_drag_and_drop(&mut evl, icon, &ds);
                            data_source = Some(ds);
                            dnd_ready = false;
                        }
                    }
                    WindowEvent::MouseScroll { axis, value } => {
                        println!("scrolling on axis = {:?}, value = {}", axis, value);
                    },
                    WindowEvent::KeyDown { key, .. } => {
                        if let Key::Tab = key {
                            max = !max;
                            window.set_fullscreen(max, None);
                        }
                        if let Key::Space = key {
                            window.set_cursor(&mut evl, CursorStyle::Predefined { shape: CursorShape::Help });
                        }
                        else if let Key::Return = key {
                            // let popup_window2 = PopupWindow::new(evl, size, &window);
                            let popup_window2 = Window::new(&mut evl, size);
                            let popup_ctx2 = EglContext::new(&egl, &popup_window2, size).unwrap();
                            popup_window = Some(popup_window2);
                            popup_ctx = Some(popup_ctx2);
                        }
                        else if let Key::Escape = key {
                            popup_window.take();
                            popup_ctx.take();
                        }
                        else if let Key::ArrowDown = key {
                            if let Some(ref offer) = current_selection {
                                let mut stream = offer.receive(DataKinds::TEXT, IoMode::Blocking).unwrap();
                                let mut buf = String::new();
                                let _res = stream.read_to_string(&mut buf);
                                println!("clipboard: {}", buf);
                            }
                        }
                        else if !key.modifier() {
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
                    WindowEvent::Dnd { event, sameapp } => match event {
                        DndEvent::Motion { handle: dnd, .. } => {
                            dnd.advertise(&[DataKinds::TEXT]);
                        },
                        DndEvent::Drop { x, y, offer } => {
                            println!("object dropped at {x}, {y}, sameapp: {sameapp}");
                            println!("available kinds: {:?}", offer.kinds());
                            if !sameapp {
                                let mut stream = offer.receive(DataKinds::TEXT, IoMode::Blocking).unwrap();
                                let mut buf = String::new();
                                let _res = stream.read_to_string(&mut buf);
                                println!("{}", buf);
                            } else {
                                println!("{{ignored because internal}}");
                            }
                        },
                        _ => (),
                    },
                    _ => (),
                },
                Event::Window { id, event } if Some(id) == popup_window.as_ref().map(|val| val.id()) => {
                    let popup_window2 = popup_window.as_mut().unwrap();
                    let popup_ctx2 = popup_ctx.as_mut().unwrap();
                    match event {
                        WindowEvent::Redraw => {
                            popup_ctx2.bind().unwrap();
                            gl::clear(0.2, 0.7, 0.1, 1.0);
                            let token = popup_window2.pre_present_notify();
                            popup_ctx2.swap_buffers(None, token).unwrap();
                            popup_window2.request_redraw(token); // fuck. you.
                        },
                        WindowEvent::Resize { size, .. } => {
                            println!("@popup resize");
                            popup_ctx2.bind().unwrap();
                            popup_ctx2.resize(size);
                            gl::resize_viewport(size.width, size.height);
                        },
                        // WindowEvent::Rescale { scale } => {
                        //     println!("@popup rescale to {scale}");
                        // }
                        WindowEvent::Close => {
                            drop(popup_ctx.take());
                            drop(popup_window.take());
                        },
                        WindowEvent::MouseDown { button, .. } => {
                            println!("@popup MOUSE DOWN button = {:?}", button);
                        },
                        WindowEvent::KeyDown { key, .. } => {
                            println!("@popuo KEY DOWN key = {:?}", key);
                            if let Key::Return = key {
                                println!("Requesting attention for the other window...");
                                window.request_user_attention(&mut evl, Urgency::Switch);
                            }
                        }
                        _ => (),
                    }
                },
                Event::DataSource { id: _id, event } => match event {
                    DataSourceEvent::Send { kind, mut writer } => {
                        println!("send to fd ({:?})", kind);
                        writer.write(b"Hello world! (from lsg-test)").unwrap();
                    },
                    DataSourceEvent::Success => println!("transferred."),
                    DataSourceEvent::Close => {
                        drop(data_source.take());
                        println!("offer closed");
                    },
                },
                _ => ()
            }
            
        }

    });

    Ok(())
    
}
