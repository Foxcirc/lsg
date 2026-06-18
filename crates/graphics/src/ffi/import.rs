
pub use implementation::*;

pub mod definitions {

    use std::ffi::c_void as void;
    use crate::ffi::types;

    #[allow(improper_ctypes)] // We use zero sized structs behind pointers which is fine.
    unsafe extern "C" {

        pub fn graphics_new(display: *const void) -> *mut types::Graphics;
        pub fn graphics_drop(this: *mut types::Graphics);

        pub fn program_new(gp: *mut types::Graphics, sources: types::SourcesSlice) -> *mut types::Program;
        pub fn program_drop(this: *mut types::Program);

        #[cfg(any(target_os = "linux", target_family = "wasm"))]
        pub fn program_uniformloc(this: *mut types::Program, name: *const i8) -> u32;

        pub fn surface_new(gp: *mut types::Graphics, window: *const void) -> *mut types::Surface;
        pub fn surface_drop(this: *mut types::Surface);
        pub fn surface_resize(this: *mut types::Surface, size: common::PhysicalSize);
        pub fn surface_draw(this: *mut types::Surface, cmd: types::DrawCommand);
        pub fn surface_blit(this: *mut types::Surface, texture: *const crate::Texture);
        pub fn surface_swap(this: *mut types::Surface);

        pub fn texture_maxsize(gp: *mut types::Graphics) -> u32;
        pub fn texture_new(gp: *mut types::Graphics, size: common::PhysicalSize, data: types::ByteSlice) -> *mut types::Texture;
        pub fn texture_drop(this: *mut types::Texture);
        pub fn texture_size(this: *mut types::Texture) -> common::PhysicalSize;
        pub fn texture_resize(this: *mut types::Texture, size: common::PhysicalSize, data: types::ByteSlice);
        pub fn texture_clear(this: *mut types::Texture, r: f32, g: f32, b: f32, a: f32);
        pub fn texture_inspect(this: *mut types::Texture, out: types::ByteSlice);
        pub fn texture_frombuf(this: *mut types::Texture, src: types::ByteSlice, dstrect: common::PhysicalRect);
        pub fn texture_fromtex(this: *mut types::Texture, src: *const types::Texture, srcrect: common::PhysicalRect, dstrect: common::PhysicalRect);
        pub fn texture_draw(this: *mut types::Texture, cmd: types::DrawCommand);

        pub fn vertex_buffer_new(gp: *mut types::Graphics, layout: types::VertexAttribSlice) -> *mut types::VertexBuffer;
        pub fn vertex_buffer_drop(this: *mut types::VertexBuffer);
        pub fn vertex_buffer_frombuf(this: *mut types::VertexBuffer, src: types::ByteSlice);
        pub fn vertex_buffer_vertsize(this: *mut types::VertexBuffer) -> u32;

    }

}

pub mod implementation {

    use crate::ffi::types;
    use super::definitions;
    use common::*;
    use std::{ffi::CString, ptr};

    /// Creates a ByteSlice from an Option<&[u8]>.
    fn optional_byteslice(data: Option<&[u8]>) -> types::ByteSlice {
        match data {
            Some(d) => types::ByteSlice { ptr: d.as_ptr(),       len: d.len() },
            None    => types::ByteSlice { ptr: std::ptr::null(), len: 0       },
        }
    }

    pub struct GraphicsBackend {
        inner: *mut types::Graphics,
    }

    impl GraphicsBackend {
        pub fn new<D: IsDisplay>(display: &D) -> Result<Self, crate::GraphicsError> {
            let inner = unsafe { definitions::graphics_new(ptr::from_ref(display).cast()) };
            Ok(Self { inner })
        }
    }

    impl Drop for GraphicsBackend {
        fn drop(&mut self) {
            unsafe { definitions::graphics_drop(self.inner) }
        }
    }

    pub struct ProgramBackend {
        inner: *mut types::Program,
    }

    impl ProgramBackend {
        pub fn new(gp: &crate::Graphics, sources: &[crate::Source]) -> Self {

            let strings0: Vec<CString> = sources.iter()
                .map(|s| CString::new(s.data).unwrap())
                .collect();

            let sources0: Vec<types::Source> = sources.iter()
                .zip(strings0.iter())
                .map(|(s, cstr)| types::Source {
                    kind: s.kind,
                    data: cstr.as_ptr(),
                })
                .collect();

            let slice = types::SourcesSlice {
                ptr: sources0.as_ptr(),
                len: sources0.len()
            };

            let inner = unsafe { definitions::program_new(gp.backend.inner, slice) };
            Self { inner }
        }

        #[cfg(target_os = "linux")]
        #[track_caller]
        pub fn uniformloc(&mut self, name: &str) -> crate::Location {
            let name0 = CString::new(name).expect("Invalid uniform name");
            let loc = unsafe { definitions::program_uniformloc(self.inner, name0.as_ptr()) };
            crate::Location(loc as usize)
        }
    }

    impl Drop for ProgramBackend {
        fn drop(&mut self) {
            unsafe { definitions::program_drop(self.inner) }
        }
    }

    pub struct SurfaceBackend {
        inner: *mut types::Surface,
    }

    impl SurfaceBackend {
        pub fn new<S: IsSurface>(gp: &crate::Graphics, window: &S) -> Self {
            let inner = unsafe { definitions::surface_new(
                gp.backend.inner,
                ptr::from_ref(window).cast()
            ) };
            Self { inner }
        }

        pub fn resize(&mut self, size: PhysicalSize) {
            unsafe { definitions::surface_resize(self.inner, size) }
        }

        pub fn draw<'a>(&self, cmd: crate::DrawCommand<'a>) {

            let attrs0: Vec<types::TextureAttrib> = cmd.textures.iter()
                .map(|it| types::TextureAttrib {
                    src: it.src.backend.inner,
                    sampler: it.sampler.0 as u32,
                }).collect();

            let cmd0 = types::DrawCommand {
                src: cmd.src.backend.inner,
                program: cmd.program.backend.inner,
                textures: types::TextureAttribSlice {
                    ptr: attrs0.as_ptr(),
                    len: attrs0.len(),
                },
                options: cmd.options,
            };

            unsafe { definitions::surface_draw(self.inner, cmd0) }

        }

        pub fn blit(&mut self, texture: &crate::Texture) {
            unsafe { definitions::surface_blit(self.inner, ptr::from_ref(texture)) }
        }

        pub fn swap(&mut self) {
            unsafe { definitions::surface_swap(self.inner) }
        }
    }

    impl Drop for SurfaceBackend {
        fn drop(&mut self) {
            unsafe { definitions::surface_drop(self.inner) }
        }
    }

    pub struct TextureBackend {
        inner: *mut types::Texture,
    }

    impl TextureBackend {
        pub fn maxsize(gp: &crate::Graphics) -> usize {
            unsafe { definitions::texture_maxsize(gp.backend.inner) as usize }
        }

        pub fn new(gp: &crate::Graphics, size: PhysicalSize, data: Option<&[u8]>) -> Self {
            let inner = unsafe { definitions::texture_new(gp.backend.inner, size, optional_byteslice(data)) };
            Self { inner }
        }

        pub fn size(&self) -> PhysicalSize {
            unsafe { definitions::texture_size(self.inner) }
        }

        pub fn resize(&mut self, size: PhysicalSize, data: Option<&[u8]>) {
            unsafe { definitions::texture_resize(self.inner, size, optional_byteslice(data)) }
        }

        pub fn clear(&mut self, values: [f32; 4]) {
            unsafe { definitions::texture_clear(self.inner, values[0], values[1], values[2], values[3]) }
        }

        pub fn inspect(&mut self) -> Vec<u8> {

            let mut out = Vec::new();

            // Since we know the format is RGBA-8 right
            // now, we can predict the needed size easily:
            let size = self.size();
            out.resize(size.w as usize * size.h as usize * 4, 0);

            let slice0 = types::ByteSlice {
                ptr: out.as_ptr(),
                len: out.len()
            };

            unsafe { definitions::texture_inspect(self.inner, slice0) };

            out
        }

        pub fn frombuf(&mut self, src: &[u8], dstrect: PhysicalRect) {
            unsafe { definitions::texture_frombuf(self.inner, optional_byteslice(Some(src)), dstrect) }
        }

        #[track_caller]
        pub fn fromtex(&mut self, src: &TextureBackend, srcrect: PhysicalRect, dstrect: PhysicalRect) {
            unsafe { definitions::texture_fromtex(self.inner, src.inner, srcrect, dstrect) }
        }

        pub fn draw<'a>(&self, cmd: crate::DrawCommand<'a>) {

            let attrs0: Vec<types::TextureAttrib> = cmd.textures.iter()
                .map(|it| types::TextureAttrib {
                    src: it.src.backend.inner,
                    sampler: it.sampler.0 as u32,
                }).collect();

            let cmd0 = types::DrawCommand {
                src: cmd.src.backend.inner,
                program: cmd.program.backend.inner,
                textures: types::TextureAttribSlice {
                    ptr: attrs0.as_ptr(),
                    len: attrs0.len(),
                },
                options: cmd.options,
            };

            unsafe { definitions::texture_draw(self.inner, cmd0) }

        }
    }

    impl Drop for TextureBackend {
        fn drop(&mut self) {
            unsafe { definitions::texture_drop(self.inner) }
        }
    }

    pub struct VertexBufferBackend {
        inner: *mut types::VertexBuffer,
    }

    impl VertexBufferBackend {
        pub fn new(gp: &crate::Graphics, layout: &[crate::VertexAttrib]) -> Self {

            let attribs0: Vec<types::VertexAttrib> = layout.iter().map(|a| types::VertexAttrib {
                kind: a.kind,
                count: a.count as u32,
                divisor: a.divisor.0 as u32,
                loc: a.loc.0 as u32,
            }).collect();

            let slice = types::VertexAttribSlice {
                ptr: attribs0.as_ptr(),
                len: attribs0.len()
            };

            let inner = unsafe { definitions::vertex_buffer_new(gp.backend.inner, slice) };

            Self { inner }
        }

        pub fn frombuf(&self, src: &[u8]) {
            unsafe { definitions::vertex_buffer_frombuf(self.inner, optional_byteslice(Some(src))) }
        }

        pub fn vertsize(&self) -> usize {
            unsafe { definitions::vertex_buffer_vertsize(self.inner) as usize }
        }
    }

    impl Drop for VertexBufferBackend {
        fn drop(&mut self) {
            unsafe { definitions::vertex_buffer_drop(self.inner) }
        }
    }

}
