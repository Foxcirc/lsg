
fn main() {

    lsg::run(main2)

}

fn main2(app: App) {

    use lsg::layout as layout;
    use lsg::widget as widget;

    block_on(async {

        // set the theme, which makes some things like
        // widgets::Text have a style by default
        let theme = lsg::themes::Dark::new();
        app.theme.set(Some(theme));

        // create a window
        let window = lsg::Window::new();
        app.add(&window); // clones the Arc

        // set the title and hide the window to avoid flickering
        window.title.set("lsg-test");
        window.hidden.set(true);

        // create our content
        let loading = {
            
            let center = layout::Center::new();
            let text = widget::Text::new();
            center.show(&text);

            spawn(async {
                loop {
                    let date = Date::now();
                    text.value.set(format!("{:?}", date)); // set takes Into<T>
                    text.redraw();
                    lsg::sleep(Duration::Millisecond * 1);
                }
            });

            center

        };

        // change the color using a filter, could also just be text.color.set(...)
        let filter = lsg::widget::ColorFilter::new(lsg::Color::BLUE);
        filter.show(&loading);

        window.show(&filter);
        // when loading is done...
        drop(loading);

        
        // unhide the window and wait 'till it's closed
        window.hidden.set(false);
        window.closed().await

        // if this returns the app exits
        
    })
    
}
