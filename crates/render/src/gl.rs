
use std::{error::Error as StdError, fmt, iter::{self, once, repeat, zip}, sync::Arc};

use common::*;
use crate::VertexGeometry;

/// A render storage backed by a texture,
/// which can be rendered to.
pub struct GlRenderStorage {
    size: PhysicalSize,
    framebuffer: gl::FrameBuffer,
    texture: gl::Texture,
}

impl GlRenderStorage {

    pub fn new(gl: &GlRenderer, size: PhysicalSize) -> Self {

        gl.ctx.bind(&gl.instance, None);

        let framebuffer = gl::gen_frame_buffer();
        let texture = gl::gen_texture(gl::TextureType::Basic2D);

        gl::tex_image_2d(
            &texture, size,
            gl::GpuColorFormat::Rgba8,
            gl::ColorFormat::Rgba,
            gl::DataType::U8,
            None
        );

        gl::tex_parameter_defaults(&texture); // important, so it can be sampeled
        gl::frame_buffer_texture_2d(&framebuffer, gl::AttachmentPoint::Color0, &texture);

        Self {
            size,
            framebuffer,
            texture
        }

    }

    pub fn resize(&mut self, gl: &GlRenderer, size: PhysicalSize) {

        gl.ctx.bind(&gl.instance, None);

        self.size = size;

        gl::tex_image_2d(
            &self.texture, self.size,
            gl::GpuColorFormat::Rgba8,
            gl::ColorFormat::Rgba,
            gl::DataType::U8,
            None
        );

    }

    /// Copy the data from the GPU over to the CPU.
    ///
    /// The color format is RGBA-8.
    pub fn download(&self) -> Vec<u8> {

        unsafe { gl::read_pixels(
            &self.framebuffer,
            PhysicalRect::new(PhysicalPoint::ZERO, self.size),
            gl::ColorFormat::Rgba, gl::DataType::U8
        ) }

    }

}

/// A "physcial" surface, backed by an actual
/// region which could be on screen, like a window.
///
/// You can copy pixels from a [`GlRenderStorage`]
/// directly into a surface to update it.
pub struct GlSurface {
    inner: egl::Surface,
    // fbo: gl::FrameBuffer,
    // rbo: gl::RenderBuffer,
}

impl GlSurface {

    pub fn new<W: common::IsSurface>(gl: &GlRenderer, window: &W) -> Self {

        let inner = egl::Surface::new(
            &gl.instance, &gl.config, window, window.size(),
        ).expect("cannot create egl surface");

        // // Bind a context for initialization.
        // gl.ctx.bind(&gl.instance, Some(&surface));

        // let fbo = gl::gen_frame_buffer();
        // let rbo = gl::gen_render_buffer();

        // // IMPORTANT: call `render_buffer_storage` before `frame_buffer_render_buffer` (i hate opengl)
        // gl::render_buffer_storage(&rbo, gl::GpuColorFormat::Rgba8, window.size());
        // gl::frame_buffer_render_buffer(&fbo, gl::AttachmentPoint::Color0, &rbo);

        Self {
            inner,
            // fbo,
            // rbo
        }

    }

    pub fn resize(&mut self, _gl: &GlRenderer, size: PhysicalSize) {
        //                   ^^^ we keep this for consistency with the `resize` methods

        self.inner.resize(size);

        // gl.ctx.bind(&gl.instance, Some(&self.inner));

        // gl::render_buffer_storage(&self.rbo, gl::GpuColorFormat::Rgba8, size);

        /*

        For a texture it would look like this:

        let texture = gl::gen_texture(gl::TextureType::Basic2D);
        gl::tex_image_2d(&texture, 0, gl::ColorFormat::Rgba8, size, gl::PixelFormat::Rgba, gl::DataType::UByte);
        gl::tex_parameter_i(&texture, gl::TextureProperty::MagFilter, gl::TexturePropertyValue::Linear);
        gl::tex_parameter_i(&texture, gl::TextureProperty::MinFilter, gl::TexturePropertyValue::Linear);

        gl::frame_buffer_texture_2d(&self.fbo, gl::AttachmentPoint::Color0, &texture, 0);

        */

    }

}

/// Auxilarry struct used to manage textures.
/// Before using a texture with the renderer you have to upload it
/// through this interface.
pub struct GlTextureAtlas {
    /// A 2D texture storing the images.
    texture: gl::Texture,
    /// The current size of the texture.
    size: PhysicalSize,
    /// Which size we can't exceed.
    maxsize: PhysicalSize,
    /// Which images we are currently storing.
    entries: Vec<TextureEntry>,
    /// This associates a `TextureIndex` with an actual
    /// position inside `entries`. We use a mapping since
    /// `entries` is reordered when upsizing the atlas.
    mapping: Vec<u16>,
}

impl GlTextureAtlas {

    pub fn new(renderer: &GlRenderer) -> Self {

        renderer.ctx.bind(&renderer.instance, None);

        let maxsize = gl::get_integer_v(gl::Property::MaxTextureSize) as u16;
        let texture = gl::gen_texture(gl::TextureType::Basic2D);

        // important, so it can be sampeled
        gl::tex_parameter_defaults(&texture);

        Self {
            texture,
            size: PhysicalSize::MIN,
            maxsize: PhysicalSize::quad(maxsize),
            entries: Vec::new(),
            mapping: Vec::new()
        }

    }

    /// This will make an image available to the GPU.
    ///
    /// There is no concept of releasing a single image inside an atlas,
    /// so if you want to release memory you have to drop the whole atlas.
    ///
    /// # Panic
    /// Panics if data length and `size` don't match up.
    pub fn upload(&mut self, renderer: &GlRenderer, data: &[u8], size: PhysicalSize) -> TextureIndex  {

        renderer.ctx.bind(&renderer.instance, None);

        // Find a slot.

        let slot = loop {
            let maybe = self.nextslot(size);
            if let Some(slot) = maybe {
                // We found a slot!
                break slot;
            } else if self.size.w * 2 < self.maxsize.w {
                // Upsize the texture, after checking
                // that the new size would be valid.
                self.upsize();
            } else {
                // If we ran out of space, we return
                // an errornous rect.
                return TextureIndex::ERR
            }
        };

        // Copy the image to the slot.

        let rect = PhysicalRect { pos: slot, size };
        gl::tex_sub_image_2d(&self.texture, rect, gl::ColorFormat::Rgba, gl::DataType::U8, data);

        // Add the slot to our state and return it.

        let mapping = self.mapping.len() as u16;
        let ientry = self.entries.len() as u16;

        self.entries.push(TextureEntry { rect, mapping });
        self.mapping.push(ientry);

        return TextureIndex { inner: mapping as u16 }

    }

    pub(crate) fn get(&self, index: TextureIndex) -> PhysicalRect {
        self.entries[self.mapping[index.inner as usize] as usize].rect
    }

    fn nextslot(&mut self, slotsize: PhysicalSize) -> Option<PhysicalPoint> {

        // We use the layout algorithm to check if and where an
        // additional slot would fit in a layout.

        let mut probe = TextureEntry {
            mapping: 0, // not relevant
            rect: PhysicalRect::new(
                PhysicalPoint::ZERO, // not relevant
                slotsize
            ),
        };

        let iter = self.entries.iter_mut()
            .chain(once(&mut probe));

        let (.., slot) = Self::layout(iter, self.size).last()?; // only `None` if empty
        //  ^^^^ this contains the position BEFORE the slot we want,
        //       but only if it is a valid slot

        if slot.x == i16::MAX {
            None // This means we don't have space.
        } else {
            Some(slot)
        }

    }

    fn upsize(&mut self) {

        // Create a new, bigger texture.

        let newsize = self.size.w + 256;

        let new = gl::gen_texture(gl::TextureType::Basic2D);

        gl::tex_image_2d(
            &new,
            PhysicalSize::quad(newsize),
            gl::GpuColorFormat::Rgba8,
            gl::ColorFormat::Rgba,
            gl::DataType::U8,
            None
        );

        gl::clear_tex_image(&new, &[0.0f32; 4]);

        // We use this chance to sort them aswell, for a more
        // efficient spacial layout.

        self.entries.sort_unstable_by(|lhs, rhs| {
            let ls = lhs.rect.size.w as usize * lhs.rect.size.h as usize;
            let rs = rhs.rect.size.w as usize * rhs.rect.size.h as usize;
            ls.cmp(&rs)
        });

        // We need to update the mapping.

        for (idx, entry) in self.entries.iter().enumerate() {
            self.mapping[entry.mapping as usize] = idx as u16;
        }

        // Copy over the old images to the new texture.

        let srcbuf = gl::gen_frame_buffer();

        gl::frame_buffer_texture_2d(&srcbuf, gl::AttachmentPoint::Color0, &self.texture);

        for (entry, newpos) in Self::layout(self.entries.iter_mut(), self.size) {
            // Copy from the original rect, still stored in the rect
            //  to the new position `newpos`.
            gl::copy_tex_sub_image_2d((&srcbuf, entry.rect.pos), (&new, newpos), entry.rect.size);
            // Make sure to update the size and position of the entry accordingly.
            entry.rect.pos = newpos;
        }

        // After this the atlas is fully present in the new texture.
        // So we exchange it with the old one.

        self.texture = new;

    }

    /// Calculates slot positions inside an atlas based on their sizes and order.
    fn layout<'d>(mut iter: impl Iterator<Item = &'d mut TextureEntry>, size: PhysicalSize) -> impl Iterator<Item = (&'d mut TextureEntry, PhysicalPoint)> {

        let mut rowheight = 0i16;
        let mut nextpos = PhysicalPoint::ZERO;

        iter::from_fn(move || {

            let entry = iter.next()?;

            let w = entry.rect.size.w as i16;
            let h = entry.rect.size.h as i16;

            // wrap row if needed
            if nextpos.x + w > size.w as i16 {
                nextpos.y += rowheight;
                rowheight = 0;
                nextpos.x = 0;
            }

            let pos = nextpos;

            nextpos.x += w;
            rowheight = rowheight.max(h);

            if nextpos.y > size.h as i16 {
                Some((entry, PhysicalPoint::MAX))
            } else {
                Some((entry, pos))
            }

        })

    }

    // /// Overwrite the same texture with a new image of the same size.
    // pub fn update(self: &Arc<Self>, renderer: &GlRenderer, texture: TextureCoords) {

    // }

}

/// A single instance of a shape. This can be used to render the same
/// shape many times with different transformations and textures.
#[derive(Debug, Clone)]
pub struct Instance {
    /// Index into the [`VertexGeometry`]s and then the inner [`Shape`]s.
    pub target: GeometryTarget,
    /// offsetX, offsetY
    pub pos: LogicalPoint,
    /// Scale which is applied to the targeted shape.
    pub size: LogicalSize,
    /// Texture / Color
    pub texture: TextureKind,
}

#[derive(Debug, Clone)]
struct TextureEntry {
    /// The position inside the atlas texture.
    pub rect: PhysicalRect,
    /// Index into `mapping`. Used to update the mapping
    /// accordingly after sorting the entries.
    pub mapping: u16,
}

#[derive(Debug, Clone, Copy)]
pub enum TextureKind {
    /// RGBA
    Color(u8, u8, u8, u8),
    /// Image from TextureAtlas
    Atlas(TextureIndex),
}

#[derive(Debug, Clone, Copy)]
pub struct TextureIndex {
    inner: u16,
}

impl TextureIndex {
    pub const ERR: Self = Self { inner: u16::MAX };
}

#[derive(Debug, Clone)]
pub struct GeometryTarget {
    /// Index into the associated list of vertex gemoetries.
    pub geometry: u16,
    /// Index into the list of shapes of that geometry.
    pub shape: u16,
}

/// Represents multiple instances of shapes together with their vertex information.
#[derive(Debug)]
pub struct DrawableGeometry<'a> {
    pub source: &'a [&'a VertexGeometry],
    pub instances: &'a [Instance],
}

pub struct GlRenderer {
    instance: egl::Instance,
    ctx: egl::Context,
    config: egl::Config,
    shape: ShapeRenderer,
}

impl GlRenderer {

    pub fn new<D: common::IsDisplay>(display: &D) -> Result<Self, RenderError> {

        let instance = egl::Instance::new(display)?;
        gl::load_with(|name| instance.get_proc_address(name))?;

        let config = egl::Config::build()
            .api(egl::Api::OpenGl)
            .version(4, 3)
            .debug(cfg!(test))
            .profile(egl::Profile::Core)
            .finish(&instance)?;

        let ctx = egl::Context::new(&instance, &config)?;

        // bind for initialization
        ctx.bind(&instance, None);

        gl::debug_message_callback(gl::debug_message_default_handler);
        gl::debug_message_control(Some(gl::DebugSeverity::Notification), None, None, true);

        let shape = ShapeRenderer::new()?;

        Ok(Self {
            instance,
            config,
            ctx,
            shape,
        })

    }

    pub fn draw<'b>(&mut self, geometry: &DrawableGeometry<'b>, atlas: &GlTextureAtlas, storage: &GlRenderStorage) {

        self.ctx.bind(&self.instance, None);

        gl::active_texture(0, &atlas.texture);
        gl::resize_viewport(storage.size);
        self.shape.draw(geometry, &storage.framebuffer, storage.size); // draw the new geometry ontop of the old one

    }

    /// Blit all contents from the render storage onto the surface.
    ///
    /// The content will be scaled if sized don't match.
    pub fn blit(&mut self, surface: &GlSurface, source: &GlRenderStorage) {

        self.ctx.bind(&self.instance, Some(&surface.inner));

        let target = (&gl::FrameBuffer::default(), PhysicalRect::MAX);
        let source = (&source.framebuffer, PhysicalRect::MAX);

        gl::blit_frame_buffer(target, source, gl::TexValue::Linear);

    }

    /// Actually make all changes visible to the user.
    pub fn swap(&mut self, surface: &GlSurface) {

        self.ctx.swap(&surface.inner, Damage::all());

    }

}

struct CompositeRenderer {
    // _vao: gl::VertexArray,
    // _vbo: gl::Buffer,
    // _program: gl::LinkedProgram,
}
impl CompositeRenderer {

    pub fn new() -> Result<Self, RenderError> {

        // let program = {

        //     const VERT: &str = include_str!("shader/composite.vert");
        //     const FRAG: &str = include_str!("shader/composite.frag");

        //     // compile the shader program

        //     let vert = gl::create_shader(gl::ShaderType::Vertex,   VERT)?;
        //     let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG)?;

        //     let mut builder = gl::create_program();
        //     gl::attach_shader(&mut builder, vert);
        //     gl::attach_shader(&mut builder, frag);
        //     let program = gl::link_program(builder)?;

        //     let texture = gl::uniform_location(&program, "texture")?;
        //     gl::uniform_1i(&program, texture, 0);

        //     program

        // };

        // let (vao, vbo) = {

        //     let vao = gl::gen_vertex_array();
        //     let vbo = gl::gen_buffer(gl::BufferType::Array);

        //     let f = size_of::<f32>();
        //     gl::vertex_attrib_pointer(&vao, &vbo, gl::AttribLocation::new(0), 2, gl::DataType::F32, false, 2*f, 0);

        //    // a single full screen rect
        //     let vertices: [f32; 12] = [
        //         -1.0, 1.0, 1.0, 1.0, -1.0, -1.0, // upper left triangle
        //         1.0, 1.0, 1.0, -1.0, -1.0, -1.0, // lower right triangle
        //     ];

        //     gl::buffer_data(&vbo, &vertices, gl::DrawHint::Static);

        //     (vao, vbo)

        // };

        Ok(Self {
            // _vao: vao,
            // _vbo: vbo,
            // _program: program,
        })

    }

    pub fn draw(&mut self, source: &gl::FrameBuffer, target: &gl::FrameBuffer, size: PhysicalSize) {

        let rect = PhysicalRect::new(PhysicalPoint::ZERO, size);
        gl::blit_frame_buffer((target, rect), (source, rect), gl::TexValue::Nearest);

    }

}

struct SingularData {
    vao: gl::VertexArray,
    vdata: gl::Buffer,
}

struct InstancedData {
    vao: gl::VertexArray,
    vdata: gl::Buffer, // per-vertex data
    idata: gl::Buffer, // per-instance
    commands: gl::Buffer, // the draw commands
}

/// The builtin curve renderer.
struct ShapeRenderer {
    singular: SingularData,
    instanced: InstancedData,
    prepared: PreparedGeometry,
    program: gl::LinkedProgram,
}

impl ShapeRenderer {

    pub fn new() -> Result<Self, RenderError> {

        const VERT: &str = include_str!("shader/curve.vert");
        const FRAG: &str = include_str!("shader/curve.frag");

        let vert = gl::create_shader(gl::ShaderType::Vertex, VERT).unwrap();
        let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG).unwrap();

        let mut builder = gl::create_program();
        gl::attach_shader(&mut builder, vert);
        gl::attach_shader(&mut builder, frag);
        let program = gl::link_program(builder).unwrap(); // TODO: compile shaders to binary in a build.rs script

        let singular = {
            let vdata = gl::gen_buffer(gl::BufferType::Array);
            let vao = gl::gen_vertex_array();
            gl::vertex_attrib_pointer(&vao, &vdata, 0, 1, gl::DataType::U16, false, 10, 0); // FLAGS
            gl::vertex_attrib_pointer(&vao, &vdata, 1, 1, gl::DataType::I32, false, 10, 2); // x, y
            gl::vertex_attrib_pointer(&vao, &vdata, 2, 1, gl::DataType::U32, false, 10, 6); // u, v, l (texture coords)
            // gl::vertex_attrib_pointer(&vao, &vdata, 0, 3, gl::DataType::F32, false, 9*f, 0*f); // x, y, z TODO: remove Z coordinate as transparency/layering is handeled purely by draw-order
            // gl::vertex_attrib_pointer(&vao, &vdata, 1, 2, gl::DataType::F32, false, 9*f, 3*f); // curveX, curveY
            // gl::vertex_attrib_pointer(&vao, &vdata, 2, 3, gl::DataType::F32, false, 9*f, 5*f); // textureX, textureY, textureLayer
            // gl::vertex_attrib_pointer(&vao, &vdata, 3, 1, gl::DataType::U32, false, 9*f, 8*f); // flags TODO: document, make this loc 2 (swap with above)
            SingularData { vao, vdata }
        };

        let instanced = {
            let vdata = gl::gen_buffer(gl::BufferType::Array);
            let idata = gl::gen_buffer(gl::BufferType::Array);
            let commands = gl::gen_buffer(gl::BufferType::DrawIndirect);
            let vao = gl::gen_vertex_array();
            // let f = size_of::<f32>();
            // // vertex data
            // gl::vertex_attrib_pointer(&vao, &vdata, 0, 2, gl::DataType::F32, false, 5*f, 0*f); // x, y
            // gl::vertex_attrib_pointer(&vao, &vdata, 1, 2, gl::DataType::F32, false, 5*f, 2*f); // curveX, curveY
            // gl::vertex_attrib_pointer(&vao, &vdata, 3, 1, gl::DataType::U32, false, 5*f, 4*f); // flags TODO: document
            // // instance data
            // gl::vertex_attrib_pointer(&vao, &idata, 4, 3, gl::DataType::F32, false, 6*f, 0*f); // offsetX, offsetY, z
            // gl::vertex_attrib_pointer(&vao, &idata, 5, 3, gl::DataType::F32, false, 6*f, 3*f); // textureX, textureY, textureLayer
            // gl::vertex_attrib_divisor(&vao, 4, gl::Divisor::PerInstances(1));
            // gl::vertex_attrib_divisor(&vao, 5, gl::Divisor::PerInstances(1));
            // // default value for attrib that is not passed for instanced shapes
            // // this is used to distingluish between an instanced and non instanced call in the vertex shader
            // gl::vertex_attrib_3f(&vao, 4, -1.0, -1.0, -1.0);
            // gl::vertex_attrib_3f(&vao, 5, -1.0, -1.0, -1.0);
            InstancedData { vao, vdata, idata, commands }
        };

        Ok(Self {
            singular,
            instanced,
            prepared: PreparedGeometry::default(),
            program,
        })

    }

    /// Convert geometry into internal representation.
    fn prepare<'b>(&mut self, geometry: &DrawableGeometry<'b>, size: PhysicalSize) {

        // The layout is packed heavily to minimize memory usage.
        //
        // Layout:
        // FLAGS  | x, y  | u, v, l
        // 16 bit | 16 16 | 12 12 8
        // u16      u32       u32     = a total of 10 bytes per vertex
        //
        // Flags Layout:
        // FILLED/CONVEX/CONCAVE   INSTANCED/NORMAL   VERTEX INDEX   OUTER EDGES
        // 2 bit                   1 bit              2 bit          3 bit

        self.prepared.clear();

        for instance in geometry.instances {

            let inner = &geometry.source[instance.target.geometry as usize];
            let shape = &inner.shapes[instance.target.shape as usize];
            let vertices = &inner.vertices[shape.range()];

            let ivertices = repeat([0, 1, 2] as [u16; 3]).flatten();

            for (vertex, index) in zip(vertices, ivertices) {

                let physical_x = vertex.pos[0] as f64 * (instance.size.w as f64 / 5000.0);
                let physical_y = vertex.pos[1] as f64 * (instance.size.h as f64 / 5000.0);
                //                                                              / ^^^^^^
                //      This is the scaling where 5000 means a 1.0 scale. So for a
                //      filled rect a scale of 5000 would be a 5000x5000 rect.

                let scaled_x = (physical_x * 5000.0) / size.w as f64;
                let scaled_y = (physical_y * 5000.0) / size.h as f64;

                let shifted_x = scaled_x + instance.pos.x as f64;
                let shifted_y = scaled_y + instance.pos.y as f64;

                let packed_pos = 0i32 |
                    ((shifted_y as i32 & 0xFFFF) << 0) | // y
                    ((shifted_x as i32 & 0xFFFF) << 16); // x

                // let packed_texture = 0u32 |
                //     ((0b0      & 255)  << 0) | // l
                //     ((aaaaaaaa & 4095) << 8) | // v
                //     ((bbbbbbbb & 4095) << 20); // u

                let TextureKind::Color(r, g, b, a) = instance.texture else { panic!() };

                let packed_colors = 0u32 |
                    ((a as u32 & 0xFF)  << 0)  | // a
                    ((b as u32 & 0xFF)  << 8)  | // b
                    ((g as u32 & 0xFF)  << 16) | // g
                    ((r as u32 & 0xFF)  << 24);  // r

                let edges = vertex.edges as u16;
                let curve = vertex.curve as u16;

                let flags = 0u16 |
                    ((edges & 0b111) << 0) |
                    ((index & 0b011) << 3) |
                    ((0b0   & 0b001) << 5) | // TODO: no instanced drawing for now
                    ((curve & 0b011) << 6);

                self.prepared.singular.vertices.extend_u16([flags]);
                self.prepared.singular.vertices.extend_i([packed_pos]);
                self.prepared.singular.vertices.extend_u([packed_colors]);

                /*
                self.prepared.singular.vertices.extend_f(pos); // XY
                self.prepared.singular.vertices.extend_f([1.0]); // Z-coordinte
                self.prepared.singular.vertices.extend_f(vertex.cxy.xy()); // Curve XY
                // self.prepared.singular.vertices.extend_f(instance.texture); // texture
                self.prepared.singular.vertices.extend_u([vertex.flags]); // flags
                */

            }

        }

    }

    pub fn draw(&mut self, geometry: &DrawableGeometry, target: &gl::FrameBuffer, size: PhysicalSize) {

        self.prepare(geometry, size);

        gl::enable(gl::Capability::Blend);
        gl::blend_func(gl::BlendFunc::SrcAlpha, gl::BlendFunc::OneMinusSrcAlpha);

        // render all non-instanced shapes
        let r = &self.prepared.singular;
        let len = r.vertices.inner.len();
        if len > 0 {
            gl::buffer_data(&self.singular.vdata, &r.vertices.inner, gl::DrawHint::Dynamic);
            gl::draw_arrays(target, &self.program, &self.singular.vao, gl::Primitive::Triangles, 0, len / 10);
        }

        // // render all instanced shapes
        // let r = &result.instanced;
        // if r.commands.len() > 0 {
        //     gl::buffer_data(&self.instanced.vdata,    &r.vertices.inner,  gl::DrawHint::Dynamic);
        //     gl::buffer_data(&self.instanced.idata,    &r.instances.inner, gl::DrawHint::Dynamic);
        //     gl::buffer_data(&self.instanced.commands, &r.commands,  gl::DrawHint::Dynamic);
        //     gl::draw_arrays_indirect(target, &self.program, &self.instanced.vao, &self.instanced.commands, gl::Primitive::Triangles, 0);
        // }

    }

}

/*

space.add(VertexShape(shape, vertexData));
...
space.add(CachedGeometry(geometry, shapes));
...
space.add(CustomRendering(renderfn));
...

fn renderfn(&self, outputTexture: u32) {

    // assumes a gl-viewport-coords screen space, which will be backed by a small slice of the actual render target

    let out = gl::Texture::from(outputTexture);

    gl::... // cross-platform render



}

// (1) Drawing Child:

let childscene = space.child(dimensions...); // a.k.a subdivide
child.handle(Action::Draw(childscene));

// if you want to do smth with the pixels of this scene, it
// now contains the things that the child wants to draw, and you
// can tell the system to access these in two ways:

let texture = child.texturize();
Instance::new(..., texture);

// what happens like if scene.texturize() is called "recursively" on our own scene

// (2) Simple Version:

let child = // ...popolate child as above

let pixels = scene.render(child);
// pixels is a texture handle which is ready to be read from!

scene.gl(renderfn, pixels);

 */

/// Vertex data which is ready to be rendered.
#[derive(Default)]
struct PreparedGeometry {
    pub singular: SingularPreparedGeometry,
    pub instanced: InstancedPreparedGeometry,
}
impl PreparedGeometry {
    fn clear(&mut self) {
        self.singular.vertices.inner.clear();
        self.instanced.vertices.inner.clear();
        self.instanced.instances.inner.clear();
        self.instanced.commands.clear();
    }
}

#[derive(Default)]
struct SingularPreparedGeometry {
    pub vertices: gl::AttribVec,
}

#[derive(Default)]
struct InstancedPreparedGeometry {
    pub vertices:  gl::AttribVec,
    pub instances: gl::AttribVec,
    pub commands:  Vec<gl::DrawArraysIndirectCommand>,
}

/// An error that occured when rendering.
///
/// Likely an unrecoverable error, like a graphics device reset or
/// missing libraries/functions.
#[derive(Debug)]
pub struct RenderError {
    msg: String,
}

impl RenderError {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}

impl fmt::Display for RenderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "render error, {}", self.msg)
    }
}

impl StdError for RenderError {}

impl From<egl::EglError> for RenderError {
    fn from(value: egl::EglError) -> Self {
        Self::new(format!("egl call failed, {}", value))
    }
}

impl From<gl::ShaderError> for RenderError {
    fn from(value: gl::ShaderError) -> Self {
        Self::new(format!("compiling shader failed, {}", value))
    }
}

impl From<gl::LinkError> for RenderError {
    fn from(value: gl::LinkError) -> Self {
        Self::new(format!("linking shader program failed, {}", value))
    }
}

impl From<gl::UniformUnknown> for RenderError {
    fn from(_: gl::UniformUnknown) -> Self {
        Self::new(format!("cannot query uniform"))
    }
}

impl From<gl::FnsUnknown> for RenderError {
    fn from(_: gl::FnsUnknown) -> Self {
        Self::new(format!("cannot load gl functions"))
    }
}
