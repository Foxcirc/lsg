
<div align="center">
    <img src="https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png" alt="colourful icon with the letters 'lsg'" style="display:block; margin:auto; width: 150px">
</div>

Light and Small Gui Apps
========================

`lsg` is a library for making desktop/mobile gui applications in rust
It has a lot of features that make it awesome:

- Many, very functional **builtin widgets**.
- Custom **styling and advanced animations** is easier then ever.
- Widgets are powerful and you can **create your own**.
- Integration of **async** that abstracts aways the typical event loop.
- Rendering on the **gpu** using OpenGL. Support for **custom rendering**.
- Transparent architecture. Implementation details are there to be seen!

Async
-----

This library requires the use of `async` rust to handle events. This provides a very
powerful architecture that doesn't get messy quickly.

We also don't force an async executor upon you and you can use any you want.
It is recommended to use a runtime that let's you spawn tasks though, since that really makes
the architecture shine.

In my opinion `smol` or `async-executor` is the best choice, since it fits into this crates
lightweight philosophy. You can also just use `futures_lite` if you want to be really barebones.

Lightweight
----------

I try to keep the number of dependencies to a reasonable minumum for rust standarts.
When compiling for the first time there are still >100 crates that need to be build but this
number should only get smaller with time.

Apps should also feel fast and responsive even in debug builds.

Correctness, Stability, Features
--------------------------------

This library is meant to provide all features you need to write native feeling applications
on all supported platforms. All supported platforms provide a complete feature-set with additional
access to important native apis.

As it matures the toolkit should be stable on every supported platform.

Currently Missing Features
--------------------------

Literally everything right now.

These features will be implemented in the future:
- support for accesskit
- more platforms

Platform Support
----------------

| Platform | Supported | Comment                 |
|----------|-----------|-------------------------|
| Windows  | ❌         | Coming soon!            |
| Linux    | ✅         | Only Wayland for now.   |
| Android  | ❌         | Coming soon!            |
| MacOS    | ❌         | Possibly in the future. |
| iOS      | ❌         | Possibly in the future. |
| WASM     | ❌         | Coming soon!            |

Build Dependencies
==================

| Platform | Dependencies                   |
|----------|--------------------------------|
| Windows  | ❌                             |
| Linux    | wayland-devel, xkbcommon-devel |
| Android  | ❌                             |
| MacOS    | ❌                             |
| iOS      | ❌                             |
