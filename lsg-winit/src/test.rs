
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
            // let _window = Window::new(&evl).await.unwrap();
            // let window = WindowBuilder::new().with_title("hello world!");
            return ExitCode::SUCCESS; // don't actually do anything with the window, just quit
        });
        block_on(exec.run(task))
    });

    runner.run().expect("run event loop")
    
}

