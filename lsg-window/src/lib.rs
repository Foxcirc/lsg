
/// Create windows on Wayland, Windows and Android.
/// Provides an OpenGL context using Egl.
/// There will be no support for Apple platforms as they deprecated OpenGl.

#[cfg(test)]
mod test;

pub mod wayland;
