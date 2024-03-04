
fn main() {

//     let (app, runner) = App::new();

//     runner.spawn(move || {

//         async {
            
//             // main window
//             let window = Window::new();
//             window.title("lsg-test");

//             // a layout widget
//             let vertical = VerticalLayout::new();

//             let label = Label::new();
//             label.text("You just clicked the button.");
//             label.hide();

//             // some content
//             let button = Button::new();
//             vertical.add(&button);            
//             button.text("Click me!"); // can also modify widget after adding it

//             let label2 = clone(label);
//             exec.spawn(async move { loop {
//                 button.clicked().await;
//                 label2.show();
//                 timeout(2000).await;
//                 label2.hide();
//             }});

//             vertical.add(label);
//             window.add(vertical);
                        
//         };

//         block_on(exec.run(app.run()));
        
//     });

//     runner.run();
    
}

