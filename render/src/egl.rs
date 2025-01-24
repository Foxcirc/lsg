
use std::{fmt, mem::size_of, ops::Range};

use common::*;
use crate::triangulate;

#[derive(Default)]
pub struct CurveGeometry {
    pub points: Vec<CurvePoint>,
    pub shapes: Vec<Shape>,
    pub instances: Vec<Instance>,
}

impl CurveGeometry {
    pub fn clear(&mut self) {
        self.points.clear();
        self.shapes.clear();
        self.instances.clear();
    }
}

/// A point in normalized device coordinates.
#[derive(Debug, Clone, Copy)]
// #[repr(packed)]
pub struct GlPoint {
    pub x: f32,
    pub y: f32,
}

impl GlPoint {
    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
}

/// A point in window coordinates.
#[derive(Debug, Clone, Copy,PartialEq)]
pub struct Point {
    pub x: f32, // TODO: use i16?
    pub y: f32,
}

impl Point {

    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    /// Convert to normalized device coordinates using the given window size.
    pub(crate) fn gl(self, size: Size) -> GlPoint {
        GlPoint {
            x:       2.0 * (self.x as f32 / size.w  as f32) - 1.0,
            y: 1.0 - 2.0 * (self.y as f32 / size.h as f32)
        }
    }

}

/// A point on a curve.
/// Can represent base (on-curve) and control (off-curve) points.
/// The coordinates represent (0, 0) as the top-left corner of the screen,
/// y-flipped, so y is growing downwards.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CurvePoint {
    // positive values: the point is a base point (on-curve)
    // negative values: the point is a control point (off-curve)
    // zero is represented by i16::MAX or respectively -i16::MAX
    x: i16, // TODO: make pub and document the workings of this; that would be really nice
    y: i16,
}

impl fmt::Debug for CurvePoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.kind() {
            write!(f, "Point {{{}, {}}}", self.x(), self.y())
        } else {
            write!(f, "ControlPoint {{{}, {}}}", self.x(), self.y())
        }
    }
}

impl CurvePoint {

    /// Construct a new base point.
    ///
    /// ### Panics
    /// Only accepts positive values that are not i16::MAX.
    #[track_caller]
    pub fn base(mut x: i16, mut y: i16) -> Self {
        debug_assert!(
            x >= 0 && x < i16::MAX && y >= 0 && y < i16::MAX,
            "coordinates must be positive and not i16::MAX"
        ); // TODO: document that this is only enforced in debug
        if x == 0 { x = i16::MAX };
        if y == 0 { y = i16::MAX };
        Self { x, y }
    }

    /// Construct a new control point.
    ///
    /// ### Panics
    /// Only accepts positive values that are not i16::MAX.
    #[track_caller]
    pub fn ctrl(mut x: i16, mut y: i16) -> Self {
        debug_assert!(
            x >= 0 && x < i16::MAX && y >= 0 && y < i16::MAX,
            "coordinates must be positive and not i16::MAX"
        ); // TODO: document that this is only enforced in debug
        if x == 0 { x = i16::MAX };
        if y == 0 { y = i16::MAX };
        Self { x: -x, y: -y }
    }

    #[inline(always)]
    pub fn x(&self) -> i16 {
        self.x.abs() % i16::MAX
    }

    #[inline(always)]
    pub fn y(&self) -> i16 {
        self.y.abs() % i16::MAX
    }

    /// `true`: base point
    /// `false`: control point
    #[inline(always)]
    pub fn kind(&self) -> bool {
        self.x > 0
    }

    /// Convert to basic point. Discarding curve information.
    pub(crate) fn point(self) -> Point {
        Point::new(self.x() as f32, self.y() as f32)
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
/// Be careful not to invalidate any invariants of the fields when
/// you modify their values.
pub struct Shape { // TODO: make this behave more similar to CurvePoint
    // TODO: ^^^ add doc links to "points"
    /// The index into the `points` to specify what points this shape
    /// conist of.
    /// This field also denotes the `kind` of this shape. The sign of the numbers
    /// is used as the indicator.
    /// - positive values means the shape is `singlular`
    /// - negative values means the shape is `instanced`
    pub polygon: Range<i16>,
    /// Range of instances this shape has. `instance.len()` should be one for singular shapes.
    pub instance: Range<u16>,
}

impl Shape {

    #[track_caller]
    pub fn new_singular(polygon: Range<i16>, instance: u16) -> Self {
        debug_assert!(polygon.start >= 0 && polygon.end > 0); // TODO: we have to alow "empty" shapes (where polygon.end == 0), but rn we dont!! which is confusing for users (0..1 is not empty!)
        Self {
            polygon,
            instance: instance..instance + 1,
        }
    }

    #[track_caller]
    pub fn new_instanced(polygon: Range<i16>, instances: Range<u16>) -> Self {
        debug_assert!(polygon.start >= 0 && polygon.end > 0);
        debug_assert!(!instances.len() > 0);
        Self {
            polygon: -polygon.start .. -polygon.end,
            instance: instances,
        }
    }

    pub fn polygon_range(&self) -> Range<i16> {
        self.polygon.start.abs()
        .. self.polygon.end.abs()
    }

    /// The range of instances of this shape.
    /// Will include only one instance for singular shapes.
    pub fn instances_range(&self) -> Range<u16> {
        self.instance.start
        .. self.instance.end
    }

    /// `true`: singular
    /// `false`: instanced
    pub fn is_singular(&self) -> bool {
        // `start` can be zero, `end` cannot and
        // will always be positive or negative
        self.polygon.end > 0
    }

}

#[derive(Debug, Clone)]
pub struct Instance {
    /// offsetX, offsetY, z
    pub pos: [f32; 3], //  TODO: make this be like CurvePoint, an offset in u16 pixels
    /// texture coordinates and layer
    pub texture: [f32; 3],
}

pub struct PerWindow {
    surface: egl::v2::Surface,
}

impl PerWindow {

    pub fn new<W: egl::IsSurface>(gl: &GlRenderer, window: &W, size: Size) -> Result<Self, RenderError> {
        let surface = egl::v2::Surface::new(&gl.lib, &gl.config, window, size)?;
        Ok(Self { surface })
    }

    pub fn resize(&mut self, size: Size) {
        self.surface.resize(size)
    }

}

pub struct GlRenderer {
    lib: egl::Instance,
    ctx: egl::v2::Context,
    config: egl::v2::Config,
    pub shape: ShapeRenderer,
    composite: CompositeRenderer,
}

impl GlRenderer {

    pub fn new<D: egl::IsDisplay>(display: &D) -> Result<Self, RenderError> {

        let lib = egl::Instance::new(display)?;
        gl::load_with(|name| lib.get_proc_address(name))?;

        let config = egl::v2::Config::build()
            .api(egl::v2::Api::OpenGl)
            .version(4, 3)
            .debug(cfg!(debug_assertions))
            .profile(egl::v2::Profile::Core)
            .finish(&lib)?;

        let ctx = egl::v2::Context::new(&lib, &config)?;

        // bind for initialization
        ctx.bind(None)?;

        gl::debug_message_callback(gl::debug_message_tracing_handler);
        gl::debug_message_control(Some(gl::DebugSeverity::Notification), None, None, true);

        let shape = ShapeRenderer::new()?;
        let composite = CompositeRenderer::new()?;

        Ok(Self {
            lib,
            config,
            ctx,
            shape,
            composite,
        })

    }

    pub fn draw(&mut self, window: &PerWindow) -> Result<(), RenderError> {

        let size = window.surface.size();

        self.ctx.bind(&window.surface)?;

        gl::resize_viewport(size);
        self.composite.update(size); // NOTE: this is done here bc we need the texture to be initialized when calling shape.draw

        self.shape.draw(size, &self.composite.fbo)?; // draw the new geometry ontop of the old one
        self.composite.draw(&gl::FrameBuffer::default()); // final full-screen composition pass

        self.ctx.swap(&window.surface, Damage::all())?; // finally swap the buffers

        Ok(())

    }

}

pub struct CompositeRenderer {
    current: Size,
    fbo: gl::FrameBuffer,
    vao: gl::VertexArray,
    #[allow(unused)]
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
            gl::vertex_attrib_pointer(&vao, &vbo, 0, 2, gl::DataType::Float, false, 2*f, 0);

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

pub struct SingularData {
    vao: gl::VertexArray,
    vdata: gl::Buffer,
}

pub struct InstancedData {
    vao: gl::VertexArray,
    vdata: gl::Buffer, // per-vertex data
    idata: gl::Buffer, // per-instance
    commands: gl::Buffer, // the draw commands
}

/// The builtin curve renderer.
pub struct ShapeRenderer {
    /// This buffer stores geometry that will be drawn next frame.
    pub geometry: CurveGeometry,
    singular: SingularData,
    instanced: InstancedData,
    program: gl::LinkedProgram,
    triangulator: triangulate::Triangulator,
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

        let triangulator = triangulate::Triangulator::new();

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
            geometry: CurveGeometry::default(),
            triangulator,
            singular,
            instanced,
            program,
        })

    }

    fn draw<'s>(&'s mut self, size: Size, target: &gl::FrameBuffer) -> Result<Damage<'s>, RenderError> {

        let result = self.triangulator.process(&self.geometry, size);

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
