
use std::mem::size_of;

use crate::CurveGeometry;
use common::*;
use gl::AttribLocation;
use crate::transform;

/// A point in normalized device coordinates.
#[derive(Debug, Clone, Copy)]
pub struct GlPoint {
    pub x: f32,
    pub y: f32,
}

impl GlPoint {

    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    /// Convert to normalized device coordinates using the given window size.
    pub fn from(p: Point, size: Size) -> Self {
        Self {
            x:       2.0 * (p.x as f32 / size.w  as f32) - 1.0,
            y: 1.0 - 2.0 * (p.y as f32 / size.h as f32)
        }
    }

}

pub struct GlSurface {
    surface: egl::v2::Surface,
}

impl GlSurface {

    pub fn new<W: egl::IsSurface>(gl: &GlRenderer, window: &W, size: Size) -> Result<Self, RenderError> {
        let surface = egl::v2::Surface::new(&gl.instance, &gl.config, window, size)?;
        Ok(Self { surface })
    }

    pub fn resize(&mut self, size: Size) {
        self.surface.resize(size)
    }

}

pub struct GlRenderer {
    instance: egl::Instance,
    ctx: egl::v2::Context,
    config: egl::v2::Config,
    pub shape: ShapeRenderer,
    composite: CompositeRenderer,
}

impl GlRenderer {

    pub fn new<D: egl::IsDisplay>(display: &D) -> Result<Self, RenderError> {

        let instance = egl::Instance::new(display)?;
        gl::load_with(|name| instance.get_proc_address(name))?;

        let config = egl::v2::Config::build()
            .api(egl::v2::Api::OpenGl)
            .version(4, 3)
            .debug(cfg!(debug_assertions))
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

    pub fn draw(&mut self, geometry: &CurveGeometry, window: &GlSurface) -> Result<(), RenderError> {

        self.ctx.bind(&self.instance, &window.surface)?;

        let size = window.surface.size();
        gl::resize_viewport(size);

        // we need to update the size before rendering with the ShapeRenderer
        // so self.composite.fbo is initialized properly
        self.composite.update(size);

        self.shape.draw(size, geometry, &self.composite.fbo)?; // draw the new geometry ontop of the old one
        self.composite.draw(&gl::FrameBuffer::default()); // final full-screen composition pass

        self.ctx.swap(&window.surface, Damage::all())?; // finally swap the buffers

        Ok(())

    }

}

struct CompositeRenderer {
    current: Size,
    fbo: gl::FrameBuffer,
    vao: gl::VertexArray,
    #[allow(unused)] // to keep it alive
    vbo: gl::Buffer,
    texture: gl::Texture,
    program: gl::LinkedProgram,
}
impl CompositeRenderer {

    pub fn new() -> Result<Self, RenderError> {

        let program = {

            const VERT: &str = include_str!("shader/composite.vert");
            const FRAG: &str = include_str!("shader/composite.frag");

            // compile the shader program

            let vert = gl::create_shader(gl::ShaderType::Vertex,   VERT)?;
            let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG)?;

            let mut builder = gl::create_program();
            gl::attach_shader(&mut builder, vert);
            gl::attach_shader(&mut builder, frag);
            let program = gl::link_program(builder)?;

            let texture = gl::uniform_location(&program, "texture")?;
            gl::uniform_1i(&program, texture, 0);

            program

        };

        let (vao, vbo) = {

            let vao = gl::gen_vertex_array();
            let vbo = gl::gen_buffer(gl::BufferType::Array);

            let f = size_of::<f32>();
            gl::vertex_attrib_pointer(&vao, &vbo, AttribLocation::new(0), 2, gl::DataType::Float, false, 2*f, 0);

           // a single full screen rect
            let vertices: [f32; 12] = [
                -1.0, 1.0, 1.0, 1.0, -1.0, -1.0, // upper left triangle
                1.0, 1.0, 1.0, -1.0, -1.0, -1.0, // lower right triangle
            ];

            gl::buffer_data(&vbo, &vertices, gl::DrawHint::Static);

            (vao, vbo)

        };

        let fbo = gl::gen_frame_buffer();

        Ok(Self {
            fbo,
            current: Size::new(0, 0),
            vao,
            vbo,
            texture: gl::Texture::invalid(),
            program,
        })

    }

    pub fn update(&mut self, size: Size) {

        // (re)create composition texture if necessary
        if size != self.current {

            let texture = gl::gen_texture(gl::TextureType::D2);
            gl::tex_image_2d(&texture, 0, gl::ColorFormat::Rgba8, size, gl::PixelFormat::Rgba, gl::DataType::UByte);
            gl::tex_parameter_i(&texture, gl::TextureProperty::MagFilter, gl::TexturePropertyValue::Linear);
            gl::tex_parameter_i(&texture, gl::TextureProperty::MinFilter, gl::TexturePropertyValue::Linear);

            gl::frame_buffer_texture_2d(&self.fbo, gl::AttachmentPoint::Color0, &texture, 0);

            // drop the old one and keep the new one alive
            self.texture = texture;
            self.current = size;

        }

    }

    pub fn draw(&mut self, target: &gl::FrameBuffer) {

        gl::disable(gl::Capability::Blend);
        gl::active_texture(0, &self.texture);

        gl::clear(target, 0.0, 0.0, 0.0, 1.0);
        gl::draw_arrays(target, &self.program, &self.vao, gl::Primitive::Triangles, 0, 6);

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
    program: gl::LinkedProgram,
    transformer: transform::Transformer,
}

impl ShapeRenderer {

    fn new() -> Result<Self, RenderError> {

        const VERT: &str = include_str!("shader/curve.vert");
        const FRAG: &str = include_str!("shader/curve.frag");

        let vert = gl::create_shader(gl::ShaderType::Vertex, VERT).unwrap();
        let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG).unwrap();

        let mut builder = gl::create_program();
        gl::attach_shader(&mut builder, vert);
        gl::attach_shader(&mut builder, frag);
        let program = gl::link_program(builder).unwrap(); // TODO: compile shaders to binary in a build.rs script

        let transformer = transform::Transformer::new();

        let singular = {
            let vdata = gl::gen_buffer(gl::BufferType::Array);
            let vao = gl::gen_vertex_array();
            let f = size_of::<f32>();
            gl::vertex_attrib_pointer(&vao, &vdata, 0, 3, gl::DataType::Float, false, 8*f, 0*f); // x, y, z
            gl::vertex_attrib_pointer(&vao, &vdata, 1, 2, gl::DataType::Float, false, 8*f, 3*f); // curveX, curveY
            gl::vertex_attrib_pointer(&vao, &vdata, 2, 3, gl::DataType::Float, false, 8*f, 5*f); // textureX, textureY, textureLayer
            SingularData { vao, vdata }
        };

        let instanced = {
            let vdata = gl::gen_buffer(gl::BufferType::Array);
            let idata = gl::gen_buffer(gl::BufferType::Array);
            let commands = gl::gen_buffer(gl::BufferType::DrawIndirect);
            let vao = gl::gen_vertex_array();
            let f = size_of::<f32>();
            // vertex data
            gl::vertex_attrib_pointer(&vao, &vdata, 0, 2, gl::DataType::Float, false, 4*f, 0*f); // x, y
            gl::vertex_attrib_pointer(&vao, &vdata, 1, 2, gl::DataType::Float, false, 4*f, 2*f); // curveX, curveY
            // instance data
            gl::vertex_attrib_pointer(&vao, &idata, 3, 3, gl::DataType::Float, false, 6*f, 0*f); // offsetX, offsetY, z
            gl::vertex_attrib_pointer(&vao, &idata, 4, 3, gl::DataType::Float, false, 6*f, 3*f); // textureX, textureY, textureLayer
            gl::vertex_attrib_divisor(&vao, 3, gl::Divisor::PerInstances(1));
            gl::vertex_attrib_divisor(&vao, 4, gl::Divisor::PerInstances(1));
            // default value for attrib that is not passed for instanced shapes
            // this is used to distingluish between an instanced and non instanced call in the vertex shader
            gl::vertex_attrib_3f(&vao, 3, -1.0, -1.0, -1.0);
            gl::vertex_attrib_3f(&vao, 4, -1.0, -1.0, -1.0);
            InstancedData { vao, vdata, idata, commands }
        };

        Ok(Self {
            transformer,
            singular,
            instanced,
            program,
        })

    }

    fn draw<'s>(&'s mut self, size: Size, geometry: &CurveGeometry, target: &gl::FrameBuffer) -> Result<Damage<'s>, RenderError> {

        let result = self.transformer.process(geometry, size);

        // assure that we've gotten valid geometry
        result.check().map_err(|msg| RenderError::InvalidInput(msg.into()))?;

        gl::enable(gl::Capability::Blend);
        gl::blend_func(gl::BlendFunc::SrcAlpha, gl::BlendFunc::OneMinusSrcAlpha);

        // render all non-instanced shapes
        let r = &result.singular;
        if result.singular.vertices.len() > 0 {
            gl::buffer_data(&self.singular.vdata, r.vertices, gl::DrawHint::Dynamic);
            gl::draw_arrays(target, &self.program, &self.singular.vao, gl::Primitive::Triangles, 0, r.vertices.len() / 7);
        }

        // render all instanced shapes
        let r = &result.instanced;
        if r.commands.len() > 0 {
            gl::buffer_data(&self.instanced.vdata,    &r.vertices,  gl::DrawHint::Dynamic);
            gl::buffer_data(&self.instanced.idata,    &r.instances, gl::DrawHint::Dynamic);
            gl::buffer_data(&self.instanced.commands, &r.commands,  gl::DrawHint::Dynamic);
            gl::draw_arrays_indirect(target, &self.program, &self.instanced.vao, &self.instanced.commands, gl::Primitive::Triangles, 0);
        }

        Ok(Damage::all())

    }

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
