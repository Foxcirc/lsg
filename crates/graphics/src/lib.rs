
//! This crate provides a cross platform native rendering API,
//! similar to OpenGL but in a more functional style, without binding.

#[cfg(not(any(feature = "import", feature = "gl")))] mod dummy;
#[cfg(not(any(feature = "import", feature = "gl")))] use dummy as backend;

#[cfg(feature = "gl")] mod gl;
#[cfg(feature = "gl")] use gl as backend;

#[cfg(feature = "import")] pub mod ffi;
#[cfg(feature = "import")] use ffi::import as backend;

use common::*;
use std::{fmt, marker::PhantomData, rc::Rc};

pub struct Graphics {
    #[allow(unused)] // On dummy target it is unused.
    backend: backend::GraphicsBackend,
    _marker: PhantomData<*const ()> // We need !Send.
}

impl Graphics {
    pub fn new<D: IsDisplay>(display: &D) -> Result<Rc<Self>, GraphicsError> {
        Ok(Rc::new(Self {
            backend: backend::GraphicsBackend::new(display)?,
            _marker: PhantomData
        }))
    }
}

pub struct Program {
    backend: backend::ProgramBackend
}

impl Program {
    #[track_caller]
    pub fn new(gp: &Graphics, shaders: &[Source]) -> Self {
        Self { backend: backend::ProgramBackend::new(&gp, shaders) }
    }
    #[track_caller]
    pub fn uniformloc(&mut self, name: &str) -> crate::Location {
        self.backend.uniformloc(name)
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

    pub fn new<S: IsSurface>(gp: &Rc<Graphics>, window: &S) -> Self {
        Self { backend: backend::SurfaceBackend::new(&gp, window )}
    }

    pub fn resize(&mut self, size: PhysicalSize) {
        self.backend.resize(size);
    }

    pub fn draw<'a>(&mut self, cmd: crate::DrawCommand<'a>) {
        self.backend.draw(cmd);
    }

    /// Blit all contents from the texture onto the surface.
    ///
    /// # Panics
    /// The sizes of the `Texture` and `self` have to be equal.
    #[track_caller]
    pub fn blit(&mut self, texture: &Texture) {
        self.backend.blit(texture);
    }

    /// Swap the buffers and make changes visible to the user.
    pub fn swap(&mut self) {
        self.backend.swap();
    }

}

/// A 2D, RGBA-8 texture.
pub struct Texture {
    backend: backend::TextureBackend
}

impl Texture {
    pub fn maxsize(gp: &Graphics) -> usize {
        backend::TextureBackend::maxsize(&gp)
    }
    /// You can provide `None` for `data`, if you only want to define and allocate the texture.
    pub fn new(gp: &Rc<Graphics>, size: PhysicalSize, data: Option<&[u8]>) -> Self {
        Self { backend: backend::TextureBackend::new(&gp, size, data) }
    }
    pub fn size(&self) -> PhysicalSize {
        self.backend.size()
    }
    pub fn resize(&mut self, size: PhysicalSize, data: Option<&[u8]>) {
        self.backend.resize(size, data)
    }
    pub fn clear(&mut self, values: [f32; 4]) {
        self.backend.clear(values)
    }
    pub fn inspect(&mut self) -> Vec<u8> {
        self.backend.inspect()
    }
    #[track_caller]
    pub fn frombuf(&mut self, src: &[u8], dstrect: PhysicalRect) {
        self.backend.frombuf(src, dstrect)
    }
    #[track_caller]
    pub fn fromtex(&mut self, src: &Texture, srcrect: PhysicalRect, destrect: PhysicalRect) {
        self.backend.fromtex(&src, srcrect, destrect)
    }
    pub fn draw(&mut self, cmd: DrawCommand) {
        self.backend.draw(cmd)
    }
}

pub struct VertexBuffer {
    backend: backend::VertexBufferBackend,
}

impl VertexBuffer {
    pub fn new(gp: &Graphics, layout: &[VertexAttrib]) -> Self {
        Self { backend: backend::VertexBufferBackend::new(gp, layout) }
    }
    #[track_caller]
    pub fn frombuf(&mut self, src: &[u8]) {
        self.backend.frombuf(src);
    }
}

#[derive(Clone)]
#[repr(C)]
pub struct VertexAttrib {
    pub kind: AttribKind,
    pub count: usize,
    pub divisor: Divisor,
    pub loc: Location
}

/// The default attrib is a `F32, count(1), per-vertex, location(0)`.
impl Default for VertexAttrib {
    fn default() -> Self {
        Self { kind: AttribKind::F32, count: 1, divisor: Divisor::PERVERTEX, loc: Location(0) }
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Location(pub usize);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Divisor(pub usize);

impl Divisor {
    pub const PERVERTEX: Self = Self(0);
    pub const PERINSTANCE: Self = Self(1);
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum AttribKind {
    F32,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8
}

#[derive(Clone, Copy)]
pub struct Source<'a> {
    pub kind: SourceKind,
    pub data: &'a str,
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum SourceKind {
    Vertex,
    Fragment
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct DrawOptions {
    pub primitive: Primitive,
    pub blend: BlendMode,
    pub polygon: PolygonMode
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Primitive {
    Triangles
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum BlendMode {
    None,
    OrderedTransparency
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum PolygonMode {
    Filled,
    Outline
}

#[derive(Clone, Copy)]
pub struct TextureAttrib<'a> {
    pub src: &'a Texture,
    pub sampler: crate::Location,
}

#[derive(Clone, Copy)]
pub struct DrawCommand<'a> {
    pub src: &'a VertexBuffer,
    pub program: &'a Program,
    pub textures: &'a [TextureAttrib<'a>],
    pub options: &'a DrawOptions
}

#[derive(Clone, Debug)]
pub struct GraphicsError {
    msg: String,
}

impl fmt::Display for GraphicsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "graphics error: {}", self.msg)
    }
}

impl std::error::Error for GraphicsError {}
