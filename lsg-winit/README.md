
`lsg-winit` is an async [`winit`](https://docs.rs/winit) wrapper written for the `lsg` library.
However this crate is also meant to be used as a standalone.

# Example
This crate can be used with any async runtime, or none at all.
```rust
let (evl, mut runner) = AsyncEventLoop::new()?;
runner.spawn(move || { // slightly altered thread::spawn
    block_on(async move {
        let monitor = evl.primary_monitor()?; // exposes normal winit functions
        let window = lsg_winit::WindowBuilder::new().build(&evl)?; // create a window
        while let event = evl.next().await { // get the next event
            // handle events here
            if should_close { return } // return to terminate the event loop
        }
    });
});

runner.run()?; // run the event loop on the main thread
println!("event loop terminated");
```
