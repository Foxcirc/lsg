
//! This crate provides a cross platform native rendering API,
//! similar to OpenGL but in a more functional style, without binding.

mod gl;
use gl as backend;

// #[cfg(all(target_os = "linux", not(feature = "import")))] mod gl;
// #[cfg(all(target_os = "linux", not(feature = "import")))] use gl as backend;

// #[cfg(any(feature = "import", feature = "export"))] pub mod ffi;
// #[cfg(feature = "import")] use ffi::import as backend;

use common::*;
use std::fmt;

pub struct Graphics {
    backend: backend::GraphicsBackend,
}

impl Graphics {
    pub fn new<D: IsDisplay>(display: &D) -> Result<Self, GraphicsError> {
        Ok(Self { backend: backend::GraphicsBackend::new(display)? })
    }
    // pub fn draw(&self, set: DrawSet, target: impl IsRenderTarget) {
    //     todo!()
    // }
}

pub struct Program {
    backend: backend::ProgramBackend
}

impl Program {
    pub fn new(gp: &Graphics, shaders: &[Source]) -> Result<Self, GraphicsError> {
        Ok(Self { backend: backend::ProgramBackend::new(&gp, shaders)? })
    }
    #[cfg(target_os = "linux")]
    #[track_caller]
    pub fn uniformloc(&self, gp: &crate::Graphics, name: &str) -> crate::Location {
        self.backend.uniformloc(gp, name)
    }
}

/// A "physcial" surface, backed by a region connected to the environment.
///
/// You can use a `Surface` as a render target or copy pixels from
/// a [`Texture`] directly into a surface to update it.
pub struct Surface {
    backend: backend::SurfaceBackend
}

impl Surface {

    pub fn new<S: IsSurface>(gp: &Graphics, window: &S) -> Self {
        Self { backend: backend::SurfaceBackend::new(&gp, window )}
    }

    pub fn resize(&mut self, gp: &Graphics, size: PhysicalSize) {
        self.backend.resize(&gp, size);
    }

    /// Blit all contents from the texture onto the surface.
    ///
    /// # Panics
    /// The sizes of the `Texture` and `self` have to be equal.
    #[track_caller]
    pub fn blit(&mut self, gp: &Graphics, texture: &Texture) {
        self.backend.blit(gp, texture);
    }

    /// Swap the buffers and make changes visible to the user.
    pub fn swap(&mut self, gp: &crate::Graphics) {
        self.backend.swap(gp);
    }

}

// /// An "offscreen" render storage backed by a texture, which can be rendered to.
// pub struct RenderStorage {
//     backend: backend::RenderStorageBackend
// }

// impl RenderStorage {
//     pub fn new(i: &Graphics, size: PhysicalSize) -> Self {
//         Self { backend: backend::RenderStorageBackend::new(&i, size) }
//     }
//     pub fn texture(&self) -> &Texture {
//         self.backend.texture()
//     }
//     pub fn resize(&mut self, gp: &Graphics, size: PhysicalSize) {
//         self.backend.resize(&gp, size);
//     }
//     pub fn clear(&mut self, gp: &Graphics, values: [f32; 4]) {
//         self.backend.clear(gp, values);
//     }
//     /// Copy the data from the GPU over to the CPU.
//     /// The color format is RGBA-8.
//     pub fn inspect(&mut self, gp: &Graphics) -> Vec<u8> {
//         self.backend.inspect(&gp)
//     }
// }

/// A 2D, RGBA-8 texture.
pub struct Texture {
    backend: backend::TextureBackend
}

impl Texture {
    pub fn maxsize(gp: &Graphics) -> usize {
        backend::TextureBackend::maxsize(&gp)
    }
    /// You can provide `None` for `data`, if you only want to define and allocate the texture.
    pub fn new(gp: &Graphics, size: PhysicalSize, data: Option<&[u8]>) -> Self {
        Self { backend: backend::TextureBackend::new(&gp, size, data) }
    }
    pub fn size(&self) -> PhysicalSize {
        self.backend.size()
    }
    pub fn resize(&mut self, gp: &Graphics, size: PhysicalSize, data: Option<&[u8]>) {
        self.backend.resize(&gp, size, data)
    }
    pub fn clear(&mut self, gp: &crate::Graphics, values: [f32; 4]) {
        self.backend.clear(gp, values)
    }
    pub fn inspect(&mut self, gp: &Graphics) -> Vec<u8> {
        self.backend.inspect(&gp)
    }
    #[track_caller]
    pub fn frombuf(&mut self, gp: &Graphics, src: &[u8], dstrect: PhysicalRect) {
        self.backend.frombuf(&gp, src, dstrect)
    }
    #[track_caller]
    pub fn fromtex(&mut self, gp: &Graphics, src: &Texture, srcrect: PhysicalRect, destrect: PhysicalRect) {
        self.backend.fromtex(&gp, &src.backend, srcrect, destrect)
    }
    pub fn draw(&mut self, gp: &Graphics, cmd: DrawCommand) {
        self.backend.draw(gp, cmd)
    }
}

pub struct VertexBuffer {
    backend: backend::VertexBufferBackend,
}

impl VertexBuffer {
    pub fn new(gp: &Graphics, layout: &[Attrib]) -> Self {
        Self { backend: backend::VertexBufferBackend::new(gp, layout) }
    }
    pub fn frombuf(&mut self, gp: &Graphics, src: &[u8]) {
        self.backend.frombuf(gp, src);
    }

    fn vertsize(&self) -> usize {
        self.backend.vertsize()
    }
}

pub struct Attrib {
    pub kind: AttribKind,
    pub count: usize,
    pub divisor: Divisor,
    pub loc: Location
}

/// The default attrib is a `F32, count(1), per-vertex, location(0)`.
impl Default for Attrib {
    fn default() -> Self {
        Self { kind: AttribKind::F32, count: 1, divisor: Divisor::PERVERTEX, loc: Location(0) }
    }
}

#[derive(Copy, Clone)]
pub struct Location(pub usize);

#[derive(Copy, Clone)]
pub struct Divisor(pub usize);

impl Divisor {
    pub const PERVERTEX: Self = Self(0);
    pub const PERINSTANCE: Self = Self(1);
}

pub enum AttribKind {
    F32,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8
}

pub struct Source<'a> {
    pub kind: SourceKind,
    pub data: &'a str,
}

pub enum SourceKind {
    Vertex,
    Fragment
}

#[derive(Clone)]
pub struct DrawOptions {
    pub primitive: Primitive,
    pub blend: BlendMode,
    pub polygon: PolygonMode
}

#[derive(Clone)]
pub enum Primitive {
    Triangles
}

#[derive(Clone)]
pub enum BlendMode {
    None,
    OrderedTransparency
}

#[derive(Clone)]
pub enum PolygonMode {
    Filled,
    Outline
}

pub struct TextureAttrib<'a> {
    pub src: &'a Texture,
    pub sampler: crate::Location,
}

pub struct DrawCommand<'a> {
    pub src: &'a VertexBuffer,
    pub program: &'a Program,
    pub textures: &'a [TextureAttrib<'a>],
    pub options: &'a DrawOptions
}

#[derive(Debug)]
pub struct GraphicsError {
    msg: String,
}

impl fmt::Display for GraphicsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "graphics error: {}", self.msg)
    }
}

impl std::error::Error for GraphicsError {}

impl From<egl::LoadError> for GraphicsError {
    fn from(value: egl::LoadError) -> Self {
        Self { msg: format!("egl call failed, {}", value) }
    }
}

impl From<::gl::ShaderError> for GraphicsError {
    fn from(value: ::gl::ShaderError) -> Self {
        Self { msg: format!("gl shader compilation failed, {}", value) }
    }
}

impl From<::gl::LinkError> for GraphicsError {
    fn from(value: ::gl::LinkError) -> Self {
        Self { msg: format!("gl program linking failed, {}", value) }
    }
}
