
#![doc(html_logo_url = "https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png")]

#[cfg(test)]
mod test;

pub struct EventLoop {
    inner: desktop::EventLoop<()>,
}

impl EventLoop {

    pub fn run<R, H>(handler: H) -> R
        where H: FnOnce(Self) -> R {

            let config = desktop::EventLoopConfig {
                appid: "unknown".into() // TODO
            };

            desktop::EventLoop::run(config, |ev| {

                let this = Self {
                    inner: ev,
                };

                handler(this)

            }).unwrap()

    }

}

/*

    fn main() {
        lsg::run(app)
    }

    #[test]
    fn test() {
        lsg::simulate(app, async |sim| {

            let window = sim.window("window-zero");
            window.click("button-counter").await;

            // let textbox = window.content.inner.items[1];
            let textbox = window.widget("textbox-counter");

            let text = textbox.inner.get();

            assert!(text == "1");

        });
    }

    async fn app(ev: EventLoop) {

        let window = lsg::Window::new(&ev);
        window.id("window-zero");

        window.content.set(Widget);

        ev.spawn(async {
            loop {
                button.leftclicked().await;
                counter.update(|it| *it += 1);
                text.inner.set(format!("{counter}"));
                ev.redraw(&text);
            }
        });

        button.leftclicked(&ev, async || {
            counter.update(|it| *it += 1);
            text.inner.set(format!("{counter}"));
            ev.redraw(&text);
        });

        window.closed(&ev, ()).await;

    }));

 */
