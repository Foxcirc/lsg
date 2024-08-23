
use std::{fmt, mem::size_of};

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

impl GlPoint {
    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
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
    // share: egl::ShareContext,
    // program: gl::LinkedProgram,
    // mode: gl::UniformLocation,
}

impl CurveShared {
    
    pub fn new<D: egl::Display>(display: &D) -> Result<Self, RenderError> {

        let lib = egl::Instance::new(display)?;
        // let share = egl::ShareContext::new(&lib)?;

        // share.bind().expect("TODO");

        // // add shared shaders that the curve renderer uses

        // const VERT: &str = include_str!("shader/curve.vert");
        // const FRAG: &str = include_str!("shader/curve.frag");

        // let vert = gl::create_shader(gl::ShaderType::Vertex, VERT)?;
        // let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG)?;

        // let mut builder = gl::create_program();
        // gl::attach_shader(&mut builder, vert);
        // gl::attach_shader(&mut builder, frag);
        // let program = gl::link_program(builder)?;
        // let mode = gl::uniform_location(&program, "mode")?;

        Ok(Self {
            lib,
            // share,
            // program,
            // mode,
        })

    }
    
}

/// One-per-window render context.
pub struct CurveRenderer {
    pub size: Size,
    ctx: egl::Context,
    triangulator: triangulate::Triangulator,
    /// No uv's.
    // vao1: gl::VertexArrayObject,
    /// With uv's.
    vao2: gl::VertexArrayObject,
    vbo: gl::Buffer,
    program: gl::LinkedProgram,
    // mode: gl::UniformLocation,
}

impl CurveRenderer {

    pub fn new<W: egl::Surface>(shared: &CurveShared, window: &W, size: Size) -> Result<Self, egl::EglError> {

        // TODO: we are not binding any ctx rn :P

        let ctx = egl::Context::new(
            &shared.lib,
            window,
            size,
            None,
        )?;

        ctx.unbind().expect("TODO");
        ctx.bind().expect("TODO");

        const VERT: &str = include_str!("shader/curve.vert");
        const FRAG: &str = include_str!("shader/curve.frag");

        let vert = gl::create_shader(gl::ShaderType::Vertex, VERT).unwrap();
        let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG).unwrap();

        let mut builder = gl::create_program();
        gl::attach_shader(&mut builder, vert);
        gl::attach_shader(&mut builder, frag);
        let program = gl::link_program(builder).unwrap();

        let triangulator = triangulate::Triangulator::new(size);

        let buffer = gl::gen_buffer(gl::BufferType::ArrayBuffer);
        
        // let vao1 = gl::gen_vertex_array();
        // gl::vertex_attrib_pointer(&vao1, &buffer, 0, 2, gl::DataType::Float, false, 2 * size_of::<f32>(), 0);
        
        let vao2 = gl::gen_vertex_array();
        gl::vertex_attrib_pointer(&vao2, &buffer, 0, 2, gl::DataType::Float, false, 4 * size_of::<f32>(), 0);
        gl::vertex_attrib_pointer(&vao2, &buffer, 1, 2, gl::DataType::Float, false, 4 * size_of::<f32>(), 2);

        Ok(Self {
            size,
            ctx,
            triangulator,
            // vao1,
            vao2,
            vbo: buffer,
            program,
            // mode: shared.mode.clone(),
        })

    }
    
    pub fn resize(&mut self, size: Size) {
        // self.ctx.bind().expect("TODO");
        self.size = size;
        self.triangulator.resize(size);
        self.ctx.resize(size);
        gl::resize_viewport(size);
    }
    
    pub fn render(&mut self, geometry: &CurveGeometry) -> Result<(), RenderError> {
        // self.ctx.bind().expect("TODO");
            let p0 = [-0.7f32, -0.2];
            let p1 = [ 0.0f32,  0.2];
            let p2 = [ 0.7f32, -0.2];
            let buffer = [
                p0[0], p0[1], /* pos */ 0.0, 0.0,  /* uv */
                p1[0], p1[1], /* pos */ 0.5, 0.0,  /* uv */
                p2[0], p2[1], /* pos */ 1.0, 1.0,  /* uv */
            ];
            // gl::uniform_1ui(&self.program, self.mode, CONVEX);
            tracing::error!("DRAW");
            gl::buffer_data(&self.vbo, &buffer, gl::DrawHint::Dynamic);
            gl::draw_arrays(&self.program, &self.vao2, gl::Primitive::Triangles, 0, buffer.len() / 4);
            return Ok(());

        const FILLED:  u32 = 1;
        const CONVEX:  u32 = 2;
        const CONCAVE: u32 = 3;

        // println!("{:?}", geometry.points);
        let data = self.triangulator.process(geometry)?;

        // draw the basic triangles

        if data.basic.len() > 0 {
            // let buffer: &[f32] = unsafe { transmute(data.basic) }; // TODO: this should be safe, but I wanna avoid it in the best case
            let buffer: Vec<f32> = data.basic.into_iter().map(|it| [[it.a.x, it.a.y], [it.b.x, it.b.y], [it.c.x, it.c.y]]).flatten().flatten().collect();
            // gl::uniform_1ui(&self.program, self.mode, FILLED);
            gl::buffer_data(&self.vbo, &buffer, gl::DrawHint::Dynamic);
            // gl::draw_arrays(&self.program, &self.vao1, gl::Primitive::Triangles, 0, buffer.len());
            // TODO: enable vao1 again
        }

        // draw the convex curves

        if data.convex.len() > 0 {
            // let buffer: &[f32] = unsafe { transmute(data.convex) }; // TODO: this should be safe, but I wanna avoid it in the best case
            let buffer: Vec<f32> = data.convex.into_iter().map(|it| [[it.a.x, it.a.y, it.uva.x, it.uva.y], [it.b.x, it.b.y, it.uvb.x, it.uvb.y], [it.c.x, it.c.y, it.uvc.x, it.uvc.y]]).flatten().flatten().collect();
            let p0 = [-0.7f32, -0.2];
            let p1 = [ 0.0f32,  0.2];
            let p2 = [ 0.7f32, -0.2];
            let buffer = [
                p0[0], p0[1], /* pos */ 0.0, 0.0,  /* UV */
                p1[0], p1[1], /* pos */ 0.5, 0.0,  /* UV */
                p2[0], p2[1], /* pos */ 1.0, 1.0,  /* UV */
            ];
            // gl::uniform_1ui(&self.program, self.mode, CONVEX);
            gl::buffer_data(&self.vbo, &buffer, gl::DrawHint::Dynamic);
            gl::draw_arrays(&self.program, &self.vao2, gl::Primitive::Triangles, 0, buffer.len());
        }

        if data.concave.len() > 0 {
            // let buffer: &[f32] = unsafe { transmute(data.concave) }; // TODO: this should be safe, but I wanna avoid it in the best case
            let buffer: Vec<f32> = data.convex.into_iter().map(|it| [[it.a.x, it.a.y, it.uva.x, it.uva.y], [it.b.x, it.b.y, it.uvb.x, it.uvb.y], [it.c.x, it.c.y, it.uvc.x, it.uvc.y]]).flatten().flatten().collect();
            // gl::uniform_1ui(&self.program, self.mode, CONCAVE);
            gl::buffer_data(&self.vbo, &buffer, gl::DrawHint::Dynamic);
            gl::draw_arrays(&self.program, &self.vao2, gl::Primitive::Triangles, 0, buffer.len());
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

impl From<gl::UniformUnknown> for RenderError {
    fn from(_: gl::UniformUnknown) -> Self {
        Self::Fatal(format!("cannot query uniform"))
    }
}

impl From<triangulate::TriagError> for RenderError {
    fn from(value: triangulate::TriagError) -> Self {
        Self::InvalidPolygons(value)
    }
}
