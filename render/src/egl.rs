
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

// pub struct Renderer {
//     curve: CurveRenderer,
//     gl: GlRenderer,
//     raw: RawRenderer,
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

pub trait Buffer {
    /// Clear the buffer, so it can be reused.
    fn clear(&mut self);
}

pub struct CurveShared {
    lib: egl::Instance,
    share: egl::ShareContext,
    /// used for rendering basic geometry
    basic: gl::LinkedProgram,
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

        Ok(Self {
            lib,
            share,
            basic,
        })

    }
    
}

/// One-per-window render context.
pub struct CurveRenderer {
    pub size: Size,
    ctx: egl::Context,
    triangulator: triangulate::Triangulator,
    vao: gl::VertexArrayObject,
    vbo: gl::Buffer,
    basic: gl::LinkedProgram,
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

        let triangulator = triangulate::Triangulator::new();

        let vbo = gl::gen_buffer(gl::BufferType::ArrayBuffer);
        let vao = gl::gen_vertex_array();
        gl::vertex_attribs(&vao, &vbo, 0, 2, gl::DataType::Float, false, 2 * size_of::<f32>(), 0);

        Ok(Self {
            size,
            ctx,
            triangulator,
            vao,
            vbo,
            basic: shared.basic.clone(),
        })

    }
    
    pub fn resize(&mut self, size: Size) {
        self.size = size;
        gl::resize_viewport(size);
    }
    
    pub fn render(&mut self, geometry: &Geometry) {

        let size = self.size;

        let new: Vec<Vertex> = geometry.verticies.iter().map(|vt|
            Vertex { x: vt.x, y: self.size.height as u16 - vt.y }
        ).collect();

        let trigs = self.triangulator.triangulate(&new).unwrap_or(&[]);
        let data: Vec<f32> = trigs.iter().map(|vt| {
            let new = Vertex { x: vt.x, y: self.size.height as u16 - vt.y };
            Self::absolute_to_gl(size, new)
        }).flatten().collect();

        if !data.is_empty() {
            gl::buffer_data(&self.vbo, &data, gl::DrawHint::Dynamic);
            gl::draw_arrays(&self.basic, &self.vao, gl::Primitive::Triangles, 0, trigs.len());
        }
        
    }

    /// Convert absolute to OpenGl coordinates.
    pub fn absolute_to_gl(size: Size, vt: Vertex) -> [f32; 2] {
        let glx =       2.0 * (vt.x as f32 / size.width  as f32) - 1.0;
        let gly = 1.0 - 2.0 * (vt.y as f32 / size.height as f32);
        [glx, gly]
    }

}

#[test]
fn absolute_to_gl() {
    let size = Size { width: 100, height: 100 };
    // case 1
    let p1 = Vertex { x: 0, y: 0 };
    let r1 = CurveRenderer::absolute_to_gl(size, p1);
    assert_eq!(r1, [-1.0, 1.0]);
    // case 2
    let p2 = Vertex { x: 50, y: 50 };
    let r2 = CurveRenderer::absolute_to_gl(size, p2);
    assert_eq!(r2, [0.0, 0.0]);
    // case 3
    let p3 = Vertex { x: 100, y: 80 };
    let r3 = CurveRenderer::absolute_to_gl(size, p3);
    assert_eq!(r3, [1.0, -0.6]);
}

#[derive(Debug)]
pub enum RenderError {
    Fatal(String),
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

pub struct Geometry {
    /// like a vertex buffer, stores the vertices in a rather un-structured way
    pub verticies: Vec<Vertex>,
    /// like an index buffer, polygons reference the `vertices` to avoid duplication
    pub polygons: Vec<Polygon>,
}

/// Vertex position in screen-space coordinates.
#[derive(Debug, Clone, Copy,PartialEq, Eq)]
pub struct Vertex { // TODO: move to common? TODO: rename to Point
    pub x: u16,
    pub y: u16,
}

#[derive(Debug, Clone, Copy,PartialEq, Eq)]
pub struct Polygon {
    /// index into the `vertices` buffer
    pub idx: u16,
    /// how many vertices to read
    pub len: u16,
}

