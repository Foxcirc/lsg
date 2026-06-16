
#[repr(C)]
pub struct Graphics;

#[repr(C)]
pub struct Program;

#[repr(C)]
pub struct Surface;

#[repr(C)]
pub struct Texture;

#[repr(C)]
pub struct VertexBuffer;

#[repr(C)]
pub struct SourcesSlice {
    pub ptr: *const Source,
    pub len: usize,
}

#[repr(C)]
pub struct Source {
    pub kind: crate::SourceKind,
    pub data: *const i8,
}

#[repr(C)]
pub struct DrawCommand {
    pub src: *const VertexBuffer,
    pub program: *const Program,
    pub textures: TextureAttribSlice,
    pub options: *const crate::DrawOptions
}

#[repr(C)]
pub struct TextureAttribSlice {
    pub ptr: *const TextureAttrib,
    pub len: usize,
}

#[repr(C)]
pub struct TextureAttrib {
    pub src: *const Texture,
    pub sampler: u32,
}

#[repr(C)]
pub struct ByteSlice {
    pub ptr: *const u8,
    pub len: usize,
}

#[repr(C)]
pub struct VertexAttribSlice {
    pub ptr: *const VertexAttrib,
    pub len: usize,
}

#[repr(C)]
pub struct VertexAttrib {
    pub kind: crate::AttribKind,
    pub count: u32,
    /// `0` => per-vertex
    /// `n` => per-n-instances
    pub divisor: u32,
    pub loc: u32
}
