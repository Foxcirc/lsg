
<div align="center">
    <img src="https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/lsg-colored.png" alt="colourful lsg icon" style="display:block; margin:auto; width: 150px">
</div>

# light and small gui apps

`lsg` is a library for making desktop/mobile gui applications in rust
It has a lot of other features that make it awesome:
- make it easy and pleasant to **style widgets** yourself
- fully controllable **semi-automatic layouting**
- ergonomic **async support** that can abstract away your typical event loop
- you can tightly **associate widgets with your application state**
- **gpu accelerated** drawing (using wgpu)

# async

This library requires the use of `async` rust to handle events. This provides a very
powerful architecture and means your `ui` and `logic` can run on the same thread.

We also don't force any `async runtime` upon you. You can use `smol`, `tokio` or even just `futures-lite`.
It is recommended to use a runtime that let's you spawn tasks though, because this let's you handle events
in a really ergonomic way.
In my opinion `smol`/`async-executor` is the best choice, since it fits into this crates lightweight philosophy.

# lightweight

Being "light" means following things:
- short compilation times (for rust standards)
- fast, even in debug builds
- reasonably small executable size

`lsg` keeps it's dependencies minimal. However this also means that the library itself is missing
some features you would expect in a traditional gui framework, notably we don't have builtin support for:
- a lot of image formats (only png is included)
- a lot of font formats (only ttf/otf is included)
- platform specific api's

# currently missing features

These features should be implemented in the future:
- support for accesskit
- drag and drop
- more platforms

# platform support

Currently this library supports:
- Linux (Wayland or X11)
- Windows

Will be implemented in the near future:
- MacOS
- Android
