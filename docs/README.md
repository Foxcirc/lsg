
<div align="center">
    <img src="https://raw.githubusercontent.com/Foxcirc/lsg/main/docs/icon.png" alt="colourful icon with the letters 'lsg'" style="display:block; margin:auto; width: 150px">
</div>

Light and Small Gui Apps
========================

`lsg` is a cross-platform, cross-language library for making
desktop/mobile/web gui apps in Rust.

These features make it shine:

- Make custom widgets which have complete control over their children.
- Ability to do custom rendering using a cross-platform API similar to OpenGl.
- Event handling fully using `async`. Generally nonblocking, no threads.
- Minimal dependencies. Does almost not depend on the rest of the Rust graphics ecosystem.
- Ability to link as static library for better compile times.
- Simple, clean and modular architecture. Implementation details are there to be seen.

Platform Support
----------------

| Platform | Supported | Comment                 |
|----------|-----------|-------------------------|
| Windows  | ❌        | Coming soon!            |
| Linux    | ✅        | Only Wayland for now.   |
| Android  | ❌        | Coming soon!            |
| MacOS    | ❌        | Possibly in the future. |
| iOS      | ❌        | Possibly in the future. |
| WASM     | ❌        | Coming soon!            |

Build Dependencies
==================

| Platform | Dependencies                   |
|----------|--------------------------------|
| Windows  | ❌                             |
| Linux    | wayland-devel, xkbcommon-devel |
| Android  | ❌                             |
| MacOS    | ❌                             |
| iOS      | ❌                             |
