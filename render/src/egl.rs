
use std::{fmt, mem::size_of, ops::Range};

use common::*;
use crate::triangulate;

#[derive(Default)]
pub struct CurveGeometry {
    pub points: Vec<CurvePoint>,
    pub shapes: Vec<Shape>,
    pub singular: Vec<Instance>,
    pub instances: Vec<Instance>,
}

impl CurveGeometry {
    pub fn clear(&mut self) {
        self.points.clear();
        self.shapes.clear();
        self.singular.clear();
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
    pub fn control(mut x: i16, mut y: i16) -> Self {
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

    /// Convert to normalized device coordinates using the given window size.
    pub(crate) fn gl(self, size: Size) -> GlPoint {
        GlPoint {
            x:       2.0 * (self.x() as f32 / size.w  as f32) - 1.0,
            y: 1.0 - 2.0 * (self.y() as f32 / size.h as f32)
        }
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
    /// This field has different meaning depending on the `kind` of this shape.
    // - singular means instance.start is an index in `singular`, count will be zero
    // - instanced means this will be an index into `instances`
    pub instance: Range<u16>,
}

impl Shape {

    #[track_caller]
    pub fn singular(polygon: Range<i16>, instance: u16) -> Self {
        debug_assert!(polygon.start >= 0 && polygon.end > 0); // TODO: we have to alow "empty" shapes (where polygon.end == 0), but rn we dont!! which is confusing for users (0..1 is not empty!)
        Self {
            polygon,
            instance: instance..instance + 1,
        }
    }

    #[track_caller]
    pub fn instanced(polygon: Range<i16>, instances: Range<u16>) -> Self {
        debug_assert!(polygon.start >= 0 && polygon.end > 0);
        debug_assert!(!instances.len() > 0);
        Self {
            polygon: -polygon.start .. -polygon.end,
            instance: instances,
        }
    }

    pub fn polygon(&self) -> Range<i16> {
        self.polygon.start.abs()
        .. self.polygon.end.abs()
    }

    /// The range of instances of this shape.
    /// Will include only one instance for singular shapes.
    pub fn instances(&self) -> Range<u16> {
        self.instance.start
        .. self.instance.end
    }

    /// `true`: singular
    /// `false`: instanced
    pub fn kind(&self) -> bool {
        // `start` can be zero, `end` cannot and
        // will always be positive or negative
        self.polygon.end > 0
    }

}

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

    pub fn new<S: SubRenderers, W: egl::IsSurface>(gl: &GlRenderer<S>, window: &W, size: Size) -> Result<Self, RenderError> {
        let surface = egl::v2::Surface::new(&gl.lib, &gl.config, window, size)?;
        Ok(Self { surface })
    }

    pub fn resize(&mut self, size: Size) {
        self.surface.resize(size)
    }

}

pub trait SubRenderers {
    // TODO: document the properties of the opengl context given to this OR/AND instead of passing "size", pass a "poperties" parameter
    //  that contains multiple gl/window properties
    fn new() -> Result<Self, RenderError> where Self: Sized;
    fn draw<'s>(&'s mut self, size: Size) -> Result<Damage<'s>, RenderError>;
}

struct CompositeData {
    old: Size,
    fbo: gl::FrameBuffer,
    color: gl::Texture,
    reveal: gl::Texture,
    vao: gl::VertexArray,
    _vbo: gl::Buffer,
    program: gl::LinkedProgram,
}

pub struct GlRenderer<S: SubRenderers> {
    lib: egl::Instance,
    ctx: egl::v2::Context,
    config: egl::v2::Config,
    composite: CompositeData,
    pub inner: S,
}

impl<S: SubRenderers> GlRenderer<S> {
    
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

        // setup fbo and shaders for transparency composition
        let fbo = gl::gen_frame_buffer();

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

            // set the sampler uniforms

            let color = gl::uniform_location(&program, "colorTexture")?;
            let reveal = gl::uniform_location(&program, "revealTexture")?;

            gl::uniform_1i(&program, color, 0); // color texture will be the 0th unit
            gl::uniform_1i(&program, reveal, 1); // reveal texture will be the 1st unit

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

        // setup the sub renderers
        let renderers = S::new()?;

        Ok(Self {
            lib,
            config,
            ctx,
            composite: CompositeData {
                fbo,
                old: Size::new(0, 0),
                color: gl::Texture::invalid(),
                reveal: gl::Texture::invalid(),
                vao,
                _vbo: vbo,
                program,
            },
            inner: renderers,
        })

    }

    pub fn draw(&mut self, window: &PerWindow) -> Result<(), RenderError> {

        let size = window.surface.size();

        self.ctx.bind(&window.surface)?;
        gl::resize_viewport(size);

        // recreate composition textures of necessary
        if size != self.composite.old {

            let color = gl::gen_texture(gl::TextureKind::D2);
            gl::tex_image_2d(&color, 0, gl::ColorFormat::Rgba16F, size, gl::PixelFormat::Rgba, gl::DataType::Float);
            gl::tex_parameter_i(&color, gl::TextureProperty::MagFilter, gl::TexturePropertyValue::Linear);
            gl::tex_parameter_i(&color, gl::TextureProperty::MinFilter, gl::TexturePropertyValue::Linear);
            
            let reveal = gl::gen_texture(gl::TextureKind::D2);
            gl::tex_image_2d(&reveal, 0, gl::ColorFormat::R8, size, gl::PixelFormat::Red, gl::DataType::UByte);
            gl::tex_parameter_i(&reveal, gl::TextureProperty::MagFilter, gl::TexturePropertyValue::Linear);
            gl::tex_parameter_i(&reveal, gl::TextureProperty::MinFilter, gl::TexturePropertyValue::Linear);

            gl::frame_buffer_texture_2d(&self.composite.fbo, gl::AttachmentPoint::Color0, &color, 0);
            gl::frame_buffer_texture_2d(&self.composite.fbo, gl::AttachmentPoint::Color1, &reveal, 0);

            gl::draw_buffers(&self.composite.fbo, &[gl::AttachmentPoint::Color0, gl::AttachmentPoint::Color1]);

            // drop the old ones and keep the new ones alive
            self.composite.color = color;
            self.composite.reveal = reveal;

            self.composite.old = size;
            
        }

        gl::clear_buffer_v(&self.composite.fbo, gl::AttachmentPoint::Color0, &[0.0; 4]);
        gl::clear_buffer_v(&self.composite.fbo, gl::AttachmentPoint::Color1, &[1.0; 4]);

        // draw using the sub renderers
        gl::bind_frame_buffer(&self.composite.fbo); // TODO: we need to pass this into the sub renderers, so sub renderers can make use of their own fbo's

        let damage = self.inner.draw(size)?;

        // final compositing pass
        gl::bind_default_frame_buffer();

        gl::clear(0.0, 0.0, 0.0, 1.0);

        gl::enable(gl::Capability::Blend);
        gl::blend_func(gl::BlendFunc::OneMinusSrcAlpha, gl::BlendFunc::SrcAlpha);
        gl::active_texture(0, &self.composite.color);
        gl::active_texture(1, &self.composite.reveal);

        gl::draw_arrays(&self.composite.program, &self.composite.vao, gl::Primitive::Triangles, 0, 6);

        // finally swap the buffers
        self.ctx.swap(&window.surface, damage)?;

        Ok(())

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
pub struct BuiltinRenderer {
    /// This buffer stores geometry that will be drawn next frame.
    pub geometry: CurveGeometry,
    singular: SingularData,
    instanced: InstancedData,
    program: gl::LinkedProgram,
    triangulator: triangulate::Triangulator,
}

impl BuiltinRenderer {

    /// Add a non-instanced polygon to the current frame.
    // TODO: choose a name, polygon OR shape!
    pub fn add(&mut self, polygon: &[CurvePoint]) {
        let shape = self.geometry.points.len() as i16 .. polygon.len() as i16;
        let instance = self.geometry.instances.len() as u16;
        self.geometry.points.extend_from_slice(polygon);
        self.geometry.shapes.push(Shape::singular(shape, instance));
        self.geometry.instances.push(Instance { pos: [0.0, 0.0, 0.0], texture: [1.0, 1.0, 1.0] });
    }
    
}

/// Usually this trait is implemented for a struct containing multiple sub renderers
/// itself, but this way you can directly use `BuiltinRenderer`.
impl SubRenderers for BuiltinRenderer {

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
            // this is used to distingluish between an instanced and non instanced call in the shader
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

    fn draw<'s>(&'s mut self, size: Size) -> Result<Damage<'s>, RenderError> {
        
        let result = self.triangulator.process(size, &self.geometry);

        // assure that we've gotten valid geometry
        result.check().map_err(|msg| RenderError::InvalidInput(msg.into()))?;

        // render transparent shapes
        gl::depth_mask(false);
        gl::enable(gl::Capability::Blend);
        // gl::blend_func_seperate(
        //     gl::BlendFunc::One, gl::BlendFunc::One, /* rgb */
        //     gl::BlendFunc::Zero, gl::BlendFunc::OneMinusSrcAlpha /* alpha */
        // );
        gl::blend_func_i(gl::AttachmentPoint::Color0, gl::BlendFunc::One, gl::BlendFunc::One);
        gl::blend_func_i(gl::AttachmentPoint::Color1, gl::BlendFunc::Zero, gl::BlendFunc::OneMinusSrcAlpha);

        // render all non-instanced shapes
        let r = &result.singular;
        if result.singular.vertices.len() > 0 {
            gl::buffer_data(&self.singular.vdata, r.vertices, gl::DrawHint::Dynamic);
            gl::draw_arrays(&self.program, &self.singular.vao, gl::Primitive::Triangles, 0, r.vertices.len() / 7);
        }

        // render all instanced shapes
        let r = &result.instanced;
        if r.commands.len() > 0 {
            gl::buffer_data(&self.instanced.vdata,    &r.vertices,  gl::DrawHint::Dynamic);
            gl::buffer_data(&self.instanced.idata,    &r.instances, gl::DrawHint::Dynamic);
            gl::buffer_data(&self.instanced.commands, &r.commands,  gl::DrawHint::Dynamic);
            gl::draw_arrays_indirect(&self.program, &self.instanced.vao, &self.instanced.commands, gl::Primitive::Triangles, 0);
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
