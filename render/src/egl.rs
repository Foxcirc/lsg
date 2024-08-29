
use std::{fmt, mem::size_of, ops::Range, thread::sleep_ms};

use common::*;
use crate::{triangulate, OutputGeometry};

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
#[repr(packed)]
pub struct GlPoint {
    pub x: f32,
    pub y: f32,
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

    /// Convert to normalized device coordinates using the given window size.
    pub(crate) fn gl(&self, size: Size) -> GlPoint {
        GlPoint {
            x:       2.0 * (self.x() as f32 / size.width  as f32) - 1.0,
            y: 1.0 - 2.0 * (self.y() as f32 / size.height as f32)
        }
    }

}

    // TODO: ^^^ add doc links to "points"
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Shape { // TODO: make this behave more similar to CurvePoint
    // positive values: the shape is singlular
    // negative values: the shape is instanced
    start: i16,
    range: i16, // TODO: make pub and document behaviour!!! we need pub so we can just do range += 1 and stuff
    // singular: index into `singular` list
    // instanced: lowest index in `instances`, we linear search after this index
    instance: u16,
}

impl Shape {

    /// `start..range` is exlusive
    pub fn singular(start: i16, range: i16, instance: u16) -> Self {
        debug_assert!(start >= 0 && range > 0);
        Self { start, range, instance }
    }

    /// `start..range` is exlusive
    pub fn instanced(start: i16, range: i16, count: u16) -> Self {
        debug_assert!(start >= 0 && range > 0);
        Self { start: -start, range: -range, instance: count }
    }

    pub fn start(&self) -> i16 {
        self.start.abs()
    }

    pub fn range(&self) -> i16 {
        self.range.abs()
    }

    /// `true`: singular
    /// `false`: instanced
    pub fn kind(&self) -> bool {
        // `start` can be zero, `len` cannot and
        // will always be positive or negative
        self.range() > 0
    }

}

pub struct Instance {
    idx: i16,
    // offset: 
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

pub struct SingularData {
    vao: gl::VertexArrayObject,
    vdata: gl::Buffer,
}

pub struct InstancedData {
    vao: gl::VertexArrayObject,
    vdata: gl::Buffer, // per-vertex data
    idata: gl::Buffer, // per-instance
}

/// One-per-window render context.
pub struct CurveRenderer {
    pub size: Size,
    pub ctx: egl::Context, // TODO: remove pub(crate)
    triangulator: triangulate::Triangulator,
    singular: SingularData,
    instanced: InstancedData,
    program: gl::LinkedProgram,
}

impl CurveRenderer {

    pub fn new<W: egl::Surface>(shared: &CurveShared, window: &W, size: Size) -> Result<Self, egl::EglError> {

        let ctx = egl::Context::new(&shared.lib, window, size, None)?;
        ctx.bind()?;

        const VERT: &str = include_str!("shader/curve.vert");
        const FRAG: &str = include_str!("shader/curve.frag");

        let vert = gl::create_shader(gl::ShaderType::Vertex, VERT).unwrap();
        let frag = gl::create_shader(gl::ShaderType::Fragment, FRAG).unwrap();

        let mut builder = gl::create_program();
        gl::attach_shader(&mut builder, vert);
        gl::attach_shader(&mut builder, frag);
        let program = gl::link_program(builder).unwrap();

        let triangulator = triangulate::Triangulator::new(size);

        let singular = {
            let vdata = gl::gen_buffer(gl::BufferType::ArrayBuffer);
            let vao = gl::gen_vertex_array();
            let f = size_of::<f32>();
            gl::vertex_attrib_pointer(&vao, &vdata,
                gl::VertexAttribs::floats(0, 2, 4*f, 0*f));
            gl::vertex_attrib_pointer(&vao, &vdata,
                gl::VertexAttribs::floats(1, 2, 4*f, 2*f));
            SingularData { vao, vdata }
        };

        let instanced = {
            let vdata = gl::gen_buffer(gl::BufferType::ArrayBuffer);
            let idata = gl::gen_buffer(gl::BufferType::ArrayBuffer);
            let vao = gl::gen_vertex_array();
            let f = size_of::<f32>();
            gl::vertex_attrib_pointer(&vao, &vdata,
                gl::VertexAttribs::floats(0, 2, 4*f, 0*f));
            gl::vertex_attrib_pointer(&vao, &vdata,
                gl::VertexAttribs::floats(1, 2, 4*f, 2*f));
            // gl::vertex_attrib_pointer(&vao, &idata, // TODO: layout instance data
            //     gl::VertexAttribs::floats(0, 2, 4 * size, 2 * size));
            // gl::Divisor...
            InstancedData { vao, vdata, idata }
        };

        Ok(Self {
            size,
            ctx,
            triangulator,
            singular,
            instanced,
            program,
        })

    }
    
    pub fn resize(&mut self, size: Size) {
        // self.ctx.bind().expect("TODO");
        self.size = size;
        self.triangulator.resize(size);
        self.ctx.resize(size);
        gl::resize_viewport(size);
    }
    
    pub fn draw(&mut self, geometry: &CurveGeometry) -> Result<(), RenderError> {

        // TODO: bind correct gl CTX

        let result = self.triangulator.process(geometry);
        result.check().map_err(RenderError::InvalidPolygons)?;

        // upload the vertices to the gpu
        gl::buffer_data(&self.singular.vdata, result.singular, gl::DrawHint::Dynamic);
        gl::draw_arrays(&self.program, &self.singular.vao, gl::Primitive::Triangles, 0, result.singular.len() / 4); // TODO: update to vertex size not just len / 4

        // draw the basic triangles
        // if result.basic.len() > 0 {
         // println!("{:?}", data.basic);
        //     gl::draw_arrays(&self.program, &self.vao1, gl::Primitive::Triangles, result.basic.start as usize, result.basic.len());
        // }

        /*

        // draw the convex curves
        if result.convex.len() > 0 {
            // println!("drawing {} convex trigs: {:?}", data.convex.len(), data.convex.get(0).map(|t| [t.a, t.b, t.c]));
            // let buffer: &[f32] = unsafe { transmute(data.convex) }; // TODO: this should be safe, but I wanna avoid it in the best case
            let buffer: Vec<f32> = result.convex.into_iter().map(|it| [[it.a.x, it.a.y, it.uva.x, it.uva.y], [it.b.x, it.b.y, it.uvb.x, it.uvb.y], [it.c.x, it.c.y, it.uvc.x, it.uvc.y]]).flatten().flatten().collect();
            gl::uniform_1ui(&self.program, self.mode, CONVEX);
            gl::buffer_data(&self.vbo, &buffer, gl::DrawHint::Dynamic);
            gl::draw_arrays(&self.program, &self.vao2, gl::Primitive::Triangles, 0, buffer.len());
        }

        // TODO: fix annoying race condition with uniform value write race

        // draw the concave curves
        if result.concave.len() > 0 {
            // println!("drawing {} concave trigs: {:?}", data.concave.len(), data.concave.get(0).map(|t| [t.a, t.b, t.c]));
            // let buffer: &[f32] = unsafe { transmute(data.concave) }; // TODO: this should be safe, but I wanna avoid it in the best case
            let buffer: Vec<f32> = result.concave.into_iter().map(|it| [[it.a.x, it.a.y, it.uva.x, it.uva.y], [it.b.x, it.b.y, it.uvb.x, it.uvb.y], [it.c.x, it.c.y, it.uvc.x, it.uvc.y]]).flatten().flatten().collect();
            gl::uniform_1ui(&self.program, self.mode, CONCAVE);
            gl::buffer_data(&self.vbo, &buffer, gl::DrawHint::Dynamic);
            gl::draw_arrays(&self.program, &self.vao2, gl::Primitive::Triangles, 0, buffer.len());
        }

        */

        self.ctx.swap_buffers(None).expect("TODO");

        Ok(())
        
    }

}

#[derive(Debug)] // TODO: impl StdError
pub enum RenderError {
    Fatal(String),
    InvalidPolygons(&'static str), // TODO: call it shapes or polygons? right now I have both
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
