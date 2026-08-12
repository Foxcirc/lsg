
//! This workspace crate contains code to render curved shapes on the CPU + GPU.

pub mod atlas;
pub use atlas::*;

use common::*;
use std::{error::Error as StdError, fmt, iter::{repeat, zip}, ops::Range, rc::Rc};

/// Represents multiple instances of shapes,
/// together with their vertex information.
#[derive(Debug)]
pub struct DrawableGeometry<'a> {
    pub source: &'a [&'a crate::VertexGeometry],
    pub instances: &'a [Instance],
}

/// Curve-geometry renderer.
pub struct Renderer {
    pub gp: Rc<graphics::Graphics>,
    vbuf: graphics::VertexBuffer,
    vraw: Vec<u8>,
    program: graphics::Program,
    samplerloc: graphics::Location
}

impl Renderer {

    pub fn new<D: IsDisplay>(display: &D) -> Result<Self, RenderError> {

        use graphics::{VertexAttrib, AttribKind, Location, Divisor};

        let gp = graphics::Graphics::new(display)?;

        const VERT: &str = include_str!("shader/curve.vert");
        const FRAG: &str = include_str!("shader/curve.frag");

        let mut program  = graphics::Program::new(&gp, &[
            graphics::Source { kind: graphics::SourceKind::Vertex,   data: VERT },
            graphics::Source { kind: graphics::SourceKind::Fragment, data: FRAG },
        ]);

        let vbuf = graphics::VertexBuffer::new(&gp, &[
            VertexAttrib { kind: AttribKind::U16, loc: Location(0), count: 1, divisor: Divisor::PERVERTEX }, // FLAGS
            VertexAttrib { kind: AttribKind::I16, loc: Location(1), count: 2, divisor: Divisor::PERVERTEX }, // X, Y
            VertexAttrib { kind: AttribKind::U16, loc: Location(2), count: 2, divisor: Divisor::PERVERTEX }, // U, V
        ]);

        let samplerloc = program.uniformloc("atlas");

        Ok(Self {
            gp,
            vbuf,
            vraw: Vec::with_capacity(1024),
            program,
            samplerloc,
        })

    }

    /// Convert geometry into internal representation.
    fn prepare<'b>(&mut self, geometry: &DrawableGeometry<'b>, atlas: &TextureAtlas, size: PhysicalSize) {

        // The layout is packed heavily to minimize memory usage.
        //
        // Layout:
        // FLAGS  | x, y  | u, v
        // 16 bit | 16 16 | 16 16
        // u16      u16x2   u16x2    = a total of 10 bytes per vertex
        //
        // Flags Layout:
        // FILLED/CONVEX/CONCAVE   INSTANCED/NORMAL   VERTEX INDEX   OUTER EDGES
        // 2 bit                   1 bit              2 bit          3 bit

        self.vraw.clear();

        for instance in geometry.instances {

            // Skip discarded instances. See constant [`Instance::DISCARD`].
            if instance.isdiscard() {
                continue;
            }

            let inner = &geometry.source[instance.target.geometry as usize];
            let shape = &inner.shapes[instance.target.shape as usize];
            let vertices = &inner.vertices[shape.rangeu()];

            let ivertices = repeat([0, 1, 2] as [u16; 3]).flatten();

            for (vertex, index) in zip(vertices, ivertices) {

                let vertex_x = vertex.pos.x;
                let vertex_y = vertex.pos.y;

                let physical_x = vertex_x as f64 * instance.size.x as f64 / 10000.0;
                let physical_y = vertex_y as f64 * instance.size.y as f64 / 10000.0;
                //                                                          ^^^^^^^
                //      This is the scaling where 10,000 means a 1.0 scale. So if...
                //          1. SHAPE = 10,000x10,000 filled rect
                //          2. INSTANCE.SCALE = 100 = (0.01x)
                //      ...then you will get a 100x100 physical-pixel filled rect.

                let transformed_x = physical_x + instance.pos.x as f64;
                let transformed_y = physical_y + instance.pos.y as f64;

                // We convert them into format which is processed by the shader.
                let packed_x = maprange(transformed_x, 0.0..size.x as f64, -2500f64..2500f64) as i16;
                let packed_y = maprange(transformed_y, 0.0..size.y as f64, -2500f64..2500f64) as i16;

                let texture_lhs: u16;
                let texture_rhs: u16;

                let isatlas: bool;

                match instance.texture {

                    TextureKind::Color(r, g, b, a) => {

                        isatlas = false;
                        texture_lhs =
                            ((r as u16 & 0xFF)  << 0)  | // r
                            ((g as u16 & 0xFF)  << 8);   // g
                        texture_rhs =
                            ((b as u16 & 0xFF)  << 0) | // b
                            ((a as u16 & 0xFF)  << 8);  // a

                    },

                    TextureKind::Atlas(index, offset) => {

                        let coords = atlas.get(index);

                        // Project the vertex' position inside the shape onto the texture:
                        let x_low  = coords.point.x as f64;
                        let x_high = coords.size.x as f64 + x_low;
                        let y_low  = coords.point.y as f64;
                        let y_high = coords.size.y as f64 + y_low;
                        let x = maprange(vertex_x as f64, -2500f64..2500f64, x_low..x_high);
                        let y = maprange(vertex_y as f64, -2500f64..2500f64, y_low..y_high);

                        // We can also specify an offset into the texture:
                        let transformed_x = x as i16 + offset.x;
                        let transformed_y = y as i16 + offset.y;

                        // TODO: What should be the behaviour for offsets that make x and y be out of bounds
                        // for the texture (either positive or negative). Rn it might crash or display smth random.

                        isatlas = true;
                        texture_lhs = transformed_x as u16;
                        texture_rhs = transformed_y as u16;

                    }

                };

                let edges = vertex.edges as u16;
                let curve = vertex.curve as u16;

                let flags = 0u16 |
                    ((edges & 0b111) << 0) |
                    ((index & 0b011) << 3) |
                    ((0b0   & 0b001) << 5) | // TODO: no instanced drawing for now
                    ((curve & 0b011) << 6) |
                    ((isatlas as u16 & 0b001) << 8);

                self.vraw.extend(flags.to_ne_bytes());
                self.vraw.extend(packed_x.to_ne_bytes());
                self.vraw.extend(packed_y.to_ne_bytes());
                self.vraw.extend(texture_lhs.to_ne_bytes());
                self.vraw.extend(texture_rhs.to_ne_bytes());

            }

        }

    }

    pub fn draw<'b>(&mut self, geometry: &DrawableGeometry<'b>, atlas: &TextureAtlas, target: &mut graphics::Texture) {

        self.prepare(geometry, atlas, target.size());

        self.vbuf.frombuf(&self.vraw);

        let options = graphics::DrawOptions {
            primitive: graphics::Primitive::Triangles,
            blend: graphics::BlendMode::OrderedTransparency,
            polygon: graphics::PolygonMode::Filled
        };

        let textures = [
            graphics::TextureAttrib {
                src: atlas.texture(),
                sampler: self.samplerloc
            }
        ];

        let cmd = graphics::DrawCommand {
            src: &self.vbuf,
            program: &self.program,
            textures: &textures,
            options: &options
        };

        target.draw(cmd);

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

/// Also called "affine transform" which sounds very cool.
pub(crate) fn maprange(v: f64, lhs: Range<f64>, rhs: Range<f64>) -> f64 {
    rhs.start + ((v - lhs.start) * (rhs.end - rhs.start)) / (lhs.end - lhs.start)
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

impl From<graphics::GraphicsError> for RenderError {
    fn from(value: graphics::GraphicsError) -> Self {
        Self::new(format!("gpu error, {}", value))
    }
}
