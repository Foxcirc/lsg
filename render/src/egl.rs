
use std::{iter::{repeat, zip}};

use common::*;
use crate::VertexGeometry;

pub struct GlSurface {
    inner: egl::v2::Surface,
    fbo: gl::FrameBuffer,
    rbo: gl::RenderBuffer,
}

impl GlSurface {

    pub fn new<W: egl::IsSurface>(gl: &GlRenderer, window: &W, size: Size) -> Result<Self, RenderError> {

        let surface = egl::v2::Surface::new(
            &gl.instance, &gl.config, window, size
        )?;

        // Bind a context for initialization.
        gl.ctx.bind(&gl.instance, &surface)?;

        let fbo = gl::gen_frame_buffer();
        let rbo = gl::gen_render_buffer();

        // IMPORTANT: call `render_buffer_storage` before `frame_buffer_render_buffer` (i hate opengl)
        gl::render_buffer_storage(&rbo, gl::PreciseColorFormat::Rgba8, size);
        gl::frame_buffer_render_buffer(&fbo, gl::AttachmentPoint::Color0, &rbo);

        Ok(Self {
            inner: surface,
            fbo,
            rbo
        })

    }

    pub fn resize(&mut self, gl: &GlRenderer, size: Size) -> Result<(), RenderError> {

        gl.ctx.bind(&gl.instance, &self.inner)?;

        gl::render_buffer_storage(&self.rbo, gl::PreciseColorFormat::Rgba8, size);

        /*

        For a texture it would look like this:

        let texture = gl::gen_texture(gl::TextureType::Basic2D);
        gl::tex_image_2d(&texture, 0, gl::ColorFormat::Rgba8, size, gl::PixelFormat::Rgba, gl::DataType::UByte);
        gl::tex_parameter_i(&texture, gl::TextureProperty::MagFilter, gl::TexturePropertyValue::Linear);
        gl::tex_parameter_i(&texture, gl::TextureProperty::MinFilter, gl::TexturePropertyValue::Linear);

        gl::frame_buffer_texture_2d(&self.fbo, gl::AttachmentPoint::Color0, &texture, 0);

        */

        self.inner.resize(size);

        Ok(())

    }

}

/// Represents multiple instances of shapes together with their vertex information.
#[derive(Debug)]
pub struct DrawableGeometry<'a> {
    pub source: &'a [&'a VertexGeometry],
    pub instances: &'a [Instance],
}

pub struct GlRenderer {
    instance: egl::Instance,
    ctx: egl::v2::Context,
    config: egl::v2::Config,
    shape: ShapeRenderer,
    composite: CompositeRenderer,
}

impl GlRenderer {

    pub fn new<D: egl::IsDisplay>(display: &D) -> Result<Self, RenderError> {

        let instance = egl::Instance::new(display)?;
        gl::load_with(|name| instance.get_proc_address(name))?;

        let config = egl::v2::Config::build()
            .api(egl::v2::Api::OpenGl)
            .version(4, 3)
            .debug(cfg!(test))
            .profile(egl::v2::Profile::Core)
            .finish(&instance)?;

        let ctx = egl::v2::Context::new(&instance, &config)?;

        // bind for initialization
        ctx.bind(&instance, None)?;

        gl::debug_message_callback(gl::debug_message_tracing_handler);
        gl::debug_message_control(Some(gl::DebugSeverity::Notification), None, None, true);

        let shape = ShapeRenderer::new()?;
        let composite = CompositeRenderer::new()?;

        Ok(Self {
            instance,
            config,
            ctx,
            shape,
            composite,
        })

    }

    pub fn draw<'b>(&mut self, geometry: &DrawableGeometry<'b>, surface: &GlSurface) -> Result<(), RenderError> {

        self.ctx.bind(&self.instance, &surface.inner)?;

        let size = surface.inner.size();
        gl::resize_viewport(size);

        gl::clear(&gl::FrameBuffer::default(), 0.0, 0.0, 0.0, 1.0);
        // gl::clear(&surface.fbo, 0.0, 0.0, 0.0, 0.8); // TODO: this should be changed later since we dont want to clear the fbo but instead want to draw ontop of it

        self.shape.draw(geometry, &gl::FrameBuffer::default(), size); // draw the new geometry ontop of the old one
        // self.composite.draw(&surface.fbo, &gl::FrameBuffer::default(), size); // final full-screen composition pass

        self.ctx.swap(&surface.inner, Damage::all())?; // finally swap the buffers

        Ok(())

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

    pub fn draw(&mut self, source: &gl::FrameBuffer, target: &gl::FrameBuffer, size: Size) {

        let rect = Rect::new(PhysicalPoint::ZERO, size);
        gl::blit_frame_buffer((target, rect), (source, rect), gl::FilterValue::Nearest);

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
            gl::vertex_attrib_pointer(&vao, &vdata, 1, 1, gl::DataType::U32, false, 10, 2); // x, y, z
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
    fn prepare<'b>(&mut self, geometry: &DrawableGeometry<'b>, size: Size) {

        // The layout is packed heavily to minimize memory usage.
        // A position of 10,000 is converted to NDC coordinates of 1.0
        //
        // Layout:
        // FLAGS  | x, y, z | u, v, l
        // 16 bit | 12 12 8 | 12 12 8
        // u16      u32       u32     = a total of 10 bytes per vertex
        //
        // Flags Layout:
        // FILLED/CONVEX/CONCAVE   INSTANCED/NORMAL   VERTEX INDEX   OUTER EDGES
        // 2 bit                   1 bit              2 bit          3 bit

        self.prepared.clear();

        for instance in geometry.instances {

            let inner = &geometry.source[instance.target[0]];
            let shape = &inner.shapes[instance.target[1]];
            let vertices = &inner.vertices[shape.range()];

            let ivertices = repeat([0, 1, 2] as [u16; 3]).flatten();

            for (vertex, index) in zip(vertices, ivertices) {

                let pos_x = vertex.pos[0] + instance.pos.x as u16;
                let pos_y = vertex.pos[1] + instance.pos.y as u16;

                let scaled_x = ((pos_x as usize * 4096) / size.w) as u32;
                let scaled_y = ((pos_y as usize * 4096) / size.h) as u32;

                let packed_pos = 0u32 |
                    ((0b0      & 255)  << 0) | // no Z for now
                    ((scaled_y & 4095) << 8) |
                    ((scaled_x & 4095) << 20);

                let edges = vertex.edges as u16;
                let fill = vertex.fill as u16;

                let flags = 0u16 |
                    ((edges & 0b111) << 0) |
                    ((index & 0b011) << 3) |
                    ((0b0   & 0b001) << 5) | // no instanced drawing for now
                    ((fill  & 0b011) << 6);

                self.prepared.singular.vertices.extend_u16([flags]);
                self.prepared.singular.vertices.extend_u([packed_pos]);
                self.prepared.singular.vertices.extend_u([0]); // no u, v, l for now

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

    pub fn draw<'s, 'b>(&'s mut self, geometry: &DrawableGeometry<'b>, target: &gl::FrameBuffer, size: Size) -> Damage<'s> {

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

        Damage::all()

    }

}

/// Vertex data which is ready to be rendered.
#[derive(Default)]
pub struct PreparedGeometry {
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
pub struct SingularPreparedGeometry {
    pub vertices: gl::AttribVec,
}

#[derive(Default)]
pub struct InstancedPreparedGeometry {
    pub vertices:  gl::AttribVec,
    pub instances: gl::AttribVec,
    pub commands:  Vec<gl::DrawArraysIndirectCommand>,
}

/// An error that occured when rendering.
///
/// # Returning this yourself
/// Implements From<&str>, so you can use that to easily construct it.
#[derive(Debug)] // TODO: impl StdError
pub enum RenderError {
    /// Likely unrecoverable error, like a graphics device reset or
    /// missing libraries/functions.
    Fatal(String),
    /// Invalid input was given. Otherwise everything is probably alright.
    InvalidInput(String),
}

impl From<egl::EglError> for RenderError {
    fn from(value: egl::EglError) -> Self {
        Self::Fatal(format!("egl call failed, {}", value))
    }
}

impl From<gl::ShaderError> for RenderError {
    fn from(value: gl::ShaderError) -> Self {
        Self::Fatal(format!("compiling shader failed, {}", value))
    }
}

impl From<gl::LinkError> for RenderError {
    fn from(value: gl::LinkError) -> Self {
        Self::Fatal(format!("linking shader program failed, {}", value))
    }
}

impl From<gl::UniformUnknown> for RenderError {
    fn from(_: gl::UniformUnknown) -> Self {
        Self::Fatal(format!("cannot query uniform"))
    }
}

impl From<gl::FnsUnknown> for RenderError {
    fn from(_: gl::FnsUnknown) -> Self {
        Self::Fatal(format!("cannot load gl functions"))
    }
}
