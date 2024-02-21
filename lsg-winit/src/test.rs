
// use winit::{event_loop::{EventLoop, ControlFlow}, window::Window};

use std::process::ExitCode;

use async_executor::LocalExecutor;
use futures_lite::future::block_on;
use lsg_winit::{AsyncEventLoop, WindowBuilder};

fn main() -> ExitCode {

    let (evl, mut runner) = AsyncEventLoop::new().expect("create async eventloop");

    runner.spawn(move || {
        let exec = LocalExecutor::new();
        let task = exec.spawn(async move {
            let monitors = evl.available_monitors().await.count();
            println!("you've got {} monitor(s)", monitors);
            let window = WindowBuilder::new().with_title("hello world!").build(&evl).await.unwrap();
            println!("is-decorated: {}", window.is_decorated());
            return ExitCode::SUCCESS; // don't actually do anything with the window, just quit
        });
        block_on(exec.run(task))
    });

    runner.run().expect("run event loop")
    
}

