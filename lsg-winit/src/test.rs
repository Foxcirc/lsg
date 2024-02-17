
// use winit::{event_loop::{EventLoop, ControlFlow}, window::Window};

use std::process::ExitCode;

use async_executor::LocalExecutor;
use futures_lite::future::block_on;
use lsg_winit::AsyncEventLoop;

fn main() -> ExitCode {

    let (_evl, mut runner) = AsyncEventLoop::new().expect("create async eventloop");

    runner.spawn(move || {
        let exec = LocalExecutor::new();
        let task = exec.spawn(async move {
            loop {
                return ExitCode::SUCCESS;
                // let _event = evl.next().await;
            }
        });
        block_on(exec.run(task))
    });

    runner.run().expect("run event loop")
    
}

