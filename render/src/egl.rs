
use std::{fmt, mem::{size_of, transmute}};

use common::*;

use crate::triangulate;

// trait Widget<R>
// fn draw(&mut self, renderer: &mut R);

/*
There are three (builtin) renderers that can be used by widgets:
- curve renderer
- opengl/metal/whatever (native) renderer
- raw window access
*/

// pub struct BuiltinRenderer {
//     pub curve: CurveRenderer,
//     // pub gl: GlRenderer,
//     // pub raw: RawRenderer,
// }

// pub struct BuiltinShared {
//     pub curve: CurveRenderer,
//     // pub gl: GlRenderer,
//     // pub raw: RawRenderer,
// }

// pub struct {
//     pub curve: CurveRenderer,
//     // pub gl: GlRenderer,
//     // pub raw: RawRenderer,
// }

// pub trait Renderer {
//     type Base;
//     type Buffer: Buffer;
//     fn new(window: &mut );
//     fn render(&mut self, buffer: &mut Self::Buffer);
// }

// pub trait Renderer {
//     type Ctx: Context;
//     type Buf: Buffer;
//     /// Create a new render context for a specific window.
//     fn context(&mut self) -> Self::Ctx;
//     /// Render the buffer contents to whatever destination this
//     /// context renders to.
//     fn render(&mut self, buf: &mut Self::Buf);
// }

// pub trait Buffer {
//     /// Clear the buffer, so it can be reused.
//     fn clear(&mut self);
// }

pub struct CurveGeometry {
    pub points: Vec<CurvePoint>,
    pub shapes: Vec<CurveShape>,
}

impl CurveGeometry {
    pub fn clear(&mut self) {
        self.points.clear();
        self.shapes.clear();
    }
}

/// A point in normalized device coordinates.
#[derive(Debug, Clone, Copy)]
#[repr(packed)]
pub struct GlPoint {
    x: f32,
    y: f32,
}

/// A point on a curve.
/// Can represent base (on-curve) and control (off-curve) points.
/// The coordinates represent (0, 0) as the top-left corner of the screen,
/// y-flipped, so y is growing downwards.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CurvePoint {
    // positive values: the point is a base point (on-curve)
    // negative values: the point is a control point (off-curve)
    x: i16,
    y: i16,
}

impl fmt::Debug for CurvePoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.basic() {
            write!(f, "Point {{{}, {}}}", self.x(), self.y())
        } else {
            write!(f, "ControlPoint {{{}, {}}}", self.x(), self.y())
        }
    }
}

impl CurvePoint {

    /// Construct a new base point.
    /// ### Panic
    /// Cannot accept values below zero.
    #[track_caller]
    pub fn base(x: i16, y: i16) -> Self {
        debug_assert!(x >= 0 && y >= 0);
        Self { x, y }
    }

    /// Construct a new control point.
    /// ### Panic
    /// Cannot accept values below zero.
    #[track_caller]
    pub fn control(x: i16, y: i16) -> Self {
        // TODO: right now you can't have a control point with x or y = 0, bc there is no -0 (i checked it)
        //         is this a problem?
        debug_assert!(x > 0 && y > 0); // TODO: was changed to > 0 bc of ^^
        Self { x: -x, y: -y }
    }

    #[inline(always)]
    pub fn x(&self) -> i16 {
        self.x.abs()
    }

    #[inline(always)]
    pub fn y(&self) -> i16 {
        self.y.abs()
    }

    /// Returns `true` if base point, `false` if control point.
    #[inline(always)]
    pub fn basic(&self) -> bool {
        !self.x.is_negative()
    }

    /// Convert to normalized device coordinates using the given window size.
    pub(crate) fn gl(&self, size: Size) -> GlPoint {
        GlPoint {
            x:       2.0 * (self.x() as f32 / size.width  as f32) - 1.0,
            y: 1.0 - 2.0 * (self.y() as f32 / size.height as f32)
        }
    }

}

#[derive(Debug, Clone, Copy,PartialEq, Eq)]
pub struct CurveShape {
    /// index into `points`
    // TODO: ^^^ add doc links to "points"
    pub idx: u16,
    /// how many points after `idx` this contains
    pub len: u16,
}

impl CurveShape { // TODO: just use a Range<u16>
    pub fn new(idx: u16, len: u16) -> Self {
        Self { idx, len }
    }
}

pub struct CurveShared {
    lib: egl::Instance,
    share: egl::ShareContext,
    /// used for rendering basic triangles
    basic: gl::LinkedProgram,
    /// used for rendering beziér curves
    curve: gl::LinkedProgram,
}

impl CurveShared {
    
    pub fn new<D: egl::Display>(display: &D) -> Result<Self, RenderError> {

        let lib = egl::Instance::new(display)?;
        let share = egl::ShareContext::new(&lib)?;

        // add shared shaders that the curve renderer uses

        let basic = {

            const VERT: &str = include_str!("shader/basic.vert");
            const FRAG: &str = include_str!("shader/basic.frag");

            let vert = gl::create_shader(gl::ShaderType::Vertex, VERT)?;
            let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG)?;

            let mut val = gl::create_program();
            gl::attach_shader(&mut val, vert);
            gl::attach_shader(&mut val, frag);
            gl::link_program(val)?

        };

        let curve = {

            const VERT: &str = include_str!("shader/curve.vert");
            const FRAG: &str = include_str!("shader/curve.frag");

            let vert = gl::create_shader(gl::ShaderType::Vertex, VERT)?;
            let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG)?;

            let mut val = gl::create_program();
            gl::attach_shader(&mut val, vert);
            gl::attach_shader(&mut val, frag);
            gl::link_program(val)?

        };

        Ok(Self {
            lib,
            share,
            basic,
            curve
        })

    }
    
}

/// One-per-window render context.
pub struct CurveRenderer {
    pub size: Size,
    ctx: egl::Context,
    triangulator: triangulate::Triangulator,
    basic: BasicData,
    curve: CurveData,
}

struct BasicData {
    vao: gl::VertexArrayObject,
    vbo: gl::Buffer,
    program: gl::LinkedProgram,
}

struct CurveData {
    vao: gl::VertexArrayObject,
    vbo: gl::Buffer,
    program: gl::LinkedProgram,
}

impl CurveRenderer {

    pub fn new<W: egl::Surface>(shared: &CurveShared, window: &W, size: Size) -> Result<Self, egl::EglError> {

        // TODO: we are not binding any ctx rn :P

        let ctx = egl::Context::new(
            &shared.lib,
            window,
            size,
            Some(&shared.share)
        )?;

        let triangulator = triangulate::Triangulator::new(size);

        let basic = {
            let program = shared.basic.clone();
            let vbo = gl::gen_buffer(gl::BufferType::ArrayBuffer);
            let vao = gl::gen_vertex_array();
            gl::vertex_attribs(&vao, &vbo, 0, 2, gl::DataType::Float, false, 2 * size_of::<f32>(), 0);
            BasicData { vbo, vao, program }
        };

        let curve = {
            let program = shared.curve.clone();
            let vbo = gl::gen_buffer(gl::BufferType::ArrayBuffer);
            let vao = gl::gen_vertex_array();
            gl::vertex_attribs(&vao, &vbo, 0, 2, gl::DataType::Float, false, 2 * size_of::<f32>(), 0);
            CurveData { vbo, vao, program }
        };

        Ok(Self {
            size,
            ctx,
            triangulator,
            basic,
            curve,
        })

    }
    
    pub fn resize(&mut self, size: Size) {
        self.size = size;
        self.triangulator.resize(size);
        gl::resize_viewport(size);
    }
    
    pub fn render(&mut self, geometry: &CurveGeometry) -> Result<(), RenderError> {

        // println!("{:?}", geometry.points);
        let data = self.triangulator.process(geometry)?;

        // draw the basic triangles

        // if !data.basic.is_empty() {
        //     // let buffer = unsafe { transmute(data.basic) }; // TODO: this should be safe, but I wanna avoid it in the best case
        //     let buffer: Vec<f32> = data.basic.into_iter().map(|it| [[it.a.x, it.a.y], [it.b.x, it.b.y], [it.c.x, it.c.y]]).flatten().flatten().collect();
        //     gl::buffer_data(&self.basic.vbo, &buffer, gl::DrawHint::Dynamic);
        //     gl::draw_arrays(&self.basic.program, &self.basic.vao, gl::Primitive::Triangles, 0, buffer.len());
        // }

        // draw the curves

        if !data.curved.is_empty() {
            // let buffer = unsafe { transmute(data.basic) }; // TODO: this should be safe, but I wanna avoid it in the best case
            let buffer: Vec<f32> = data.curved.into_iter().map(|it| [[it.a.x, it.a.y], [it.b.x, it.b.y], [it.c.x, it.c.y]]).flatten().flatten().collect();
            gl::buffer_data(&self.basic.vbo, &buffer, gl::DrawHint::Dynamic);
            gl::draw_arrays(&self.basic.program, &self.basic.vao, gl::Primitive::Triangles, 0, buffer.len());
        }

        Ok(())
        
    }

}

#[derive(Debug)] // TODO: impl StdError
pub enum RenderError {
    Fatal(String),
    InvalidPolygons(triangulate::TriagError),
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

impl From<triangulate::TriagError> for RenderError {
    fn from(value: triangulate::TriagError) -> Self {
        Self::InvalidPolygons(value)
    }
}
