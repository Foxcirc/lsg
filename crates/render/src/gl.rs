
use std::{error::Error as StdError, fmt, iter::{repeat, zip}, ops::Range, rc::Rc};

use common::*;

/// Contains the layouting algorithm for the texture atlas,
/// so it can be seperate from the actual GPU calls.
struct AtlasLayout {
    /// The current size of the atlas.
    size: PhysicalSize,
    /// The height of the current row.
    rh: u16,
    /// The position after the current slot.
    cursor: PhysicalPoint,
}

impl AtlasLayout {

    pub fn new(size: PhysicalSize) -> Self {
        Self {
            size,
            rh: 0,
            cursor: PhysicalPoint::ZERO,
        }
    }

    /// Find the point before the next slot of `size`. If the current
    /// layout is not big enough it will return `None`.
    ///
    /// # Errors
    /// If the slot doesn't fit, returns the overshoot. It is not specified
    /// wether the overshoot occured sideways or upwards.
    pub fn advance(&mut self, size: PhysicalSize) -> Result<PhysicalPoint, u16> {

        let PhysicalSize { w, h } = size;

        if self.cursor.x as u16 + w > self.size.w {

            // If we overshoot sideways, we need to grow upwards.

            let incr = self.rh as i16;

            // Move the cursor one row up.
            self.cursor.y += incr;
            self.cursor.x = 0;
            self.rh = 0;

            if self.cursor.x as u16 + w > self.size.w {
                // If we overshoot sideways again, the object is to large.
                Err(self.cursor.x as u16 + w - self.size.w)
            } else if self.cursor.y as u16 + h > self.size.h {
                // If we go out of bounds upwards, there is no space left.
                Err(self.cursor.y as u16 + h - self.size.h)
            } else {

                // We made space.

                let result = self.cursor;

                self.cursor.x += w as i16;
                self.rh = self.rh.max(h);

                Ok(result)

            }

        } else if self.cursor.y as u16 + h > self.size.h {
            // If we overshoot upwards immediatly, the object is to large.
            Err(self.cursor.y as u16 + h - self.size.h)
        } else {

            // This is the simple case, where we actually have space.

            let result = self.cursor;

            self.cursor.x += w as i16;
            self.rh = self.rh.max(h);

            Ok(result)

        }

    }

}

#[test]
fn atlas_layout() {

    let mut layout = AtlasLayout::new(PhysicalSize::new(100, 100));

    // 1. 10x10 squares, all at y=0

    for idx in 0..10 {

        let pt = layout.advance(PhysicalSize::quad(10))
            .expect("must have enough space");

        assert_eq!(pt.y, 0, "y level must be 0");
        assert_eq!(pt.x, idx as i16 * 10, "x must increase in steps of 10");

    }

    // 2. large bar, 80x10, above the sqaures at y=10

    let pt2 = layout.advance(PhysicalSize::new(80, 10))
        .expect("must have enough space (2)");

    assert_eq!(pt2.y, 10, "large bar should be above the squares");
    assert_eq!(pt2.x, 0, "large bar should be at the start of the row");

    // 3. misc squares, above the bar at y=20

    let pt3 = layout.advance(PhysicalSize::quad(30)).unwrap();
    assert_eq!(pt3.y, 20);
    assert_eq!(pt3.x, 0);

    let pt4 = layout.advance(PhysicalSize::quad(50)).unwrap();
    assert_eq!(pt4.y, 20);
    assert_eq!(pt4.x, 30);

    let pt5 = layout.advance(PhysicalSize::quad(20)).unwrap();
    assert_eq!(pt5.y, 20);
    assert_eq!(pt5.x, 80);

    // 3. large bar, above the squares at y=70 (20+50)

    let pt6 = layout.advance(PhysicalSize::new(100, 20)).unwrap();
    assert_eq!(pt6.y, 70);
    assert_eq!(pt6.x, 0);

    // 4. something that shouldn't fit :b

    let inv7 = layout.advance(PhysicalSize::new(55, 15));
    assert_eq!(inv7, Err(5), "large object should not fit");

    let mut layout2 = AtlasLayout::new(PhysicalSize::new(100, 100));
    let pt21 = layout2.advance(PhysicalSize::quad(100))
        .expect("100x100 should fit on a 100x100 layout");
    assert_eq!(pt21.y, 0);
    assert_eq!(pt21.x, 0);

    let mut layout3 = AtlasLayout::new(PhysicalSize::new(100, 100));
    let inv31 = layout3.advance(PhysicalSize::quad(101));
    assert_eq!(inv31, Err(1), "101x101 should not fit on a 100x100 layout");

}

/// Used to manage textures.
///
/// Before using a texture with the renderer you have to upload it
/// through this interface.
pub struct TextureAtlas {
    /// A 2D texture storing the images.
    texture: graphics::Texture,
    /// The current layout, used to place new slots.
    layout: AtlasLayout,
    /// Which size (as a quad) we can't exceed.
    maxsize: u16,
    /// Which images we are currently storing.
    entries: Vec<TextureEntry>,
    /// This associates a `TextureIndex` with an actual
    /// position inside `entries`. We use a mapping since
    /// `entries` is reordered when upsizing the atlas.
    mapping: Vec<u16>,
}

impl TextureAtlas {

    const MININCR: u16 = 256;

    pub fn new(renderer: &Renderer) -> Self {

        let mut this = Self {
            layout: AtlasLayout::new(PhysicalSize::MIN),
            texture: graphics::Texture::new(&renderer.gp, PhysicalSize::quad(1), None),
            maxsize: graphics::Texture::maxsize(&renderer.gp) as u16,
            entries: Vec::new(),
            mapping: Vec::new(),
        };

        this.upsize(renderer, Self::MININCR);

        this

    }

    /// Write an image into the atlas.
    ///
    /// There is no concept of releasing a single image inside an atlas,
    /// so if you want to release memory you have to drop the whole atlas.
    ///
    /// However, you can update a texture once uploaded. See [`GlTextureAtlas::update`].
    ///
    /// # Panic
    /// Panics if data length and `size` don't match up.
    #[track_caller]
    pub fn upload(&mut self, renderer: &Renderer, source: &impl GlWriteToAtlas, size: PhysicalSize) -> TextureIndex  {

        let (index, rect) = self.alloc(renderer, size);
        source.write(&mut self.texture, rect);

        index

    }

    fn alloc(&mut self, renderer: &Renderer, size: PhysicalSize) -> (TextureIndex, PhysicalRect)  {

        // Find a slot or return an error.

        let slot = loop {
            match self.layout.advance(size) {
                Ok(slot) => break slot,
                Err(overshoot) => {
                    let incr = overshoot.max(Self::MININCR);
                    if self.layout.size.w + incr > self.maxsize ||
                       self.layout.size.h + incr > self.maxsize {
                        panic!("The texture-atlas is full.")
                   } else {
                       self.upsize(renderer, incr);
                   }
                }
            }
        };

        // Add the slot to our state and return it.

        let mapping = self.mapping.len() as u16;
        let ientry = self.entries.len() as u16;

        let (index, rect) = (
            TextureIndex { inner: mapping as u16 },
            PhysicalRect { pos: slot, size }
        );

        self.entries.push(TextureEntry { rect, mapping });
        self.mapping.push(ientry);

        (index, rect)

    }

    /// Overwrite the same texture with a new image of the same size.
    #[track_caller]
    pub fn update(&mut self, index: TextureIndex, source: impl GlWriteToAtlas) {

        let orig = self.entries[self.mapping[index.inner as usize] as usize].rect;
        source.write(&mut self.texture, orig);

    }

    /// Copy the atlas' texture from the GPU over to the CPU.
    ///
    /// The color format is RGBA-8.
    pub fn inspect(&mut self) -> Vec<u8> {
        self.texture.inspect()
    }

    /// Get the texture coordinates for a specific index relative
    /// to the atlas texture. These coordinates are in a range from
    /// 0..5000 which map to OpenGL's 0.0 .. 1.0 texture cordinates.
    ///
    /// Also: Why the FUCK are "clipspace" and "texture" coordinates
    /// using two different coordinate systems.
    pub(crate) fn get(&self, index: TextureIndex) -> PhysicalRect {

        let orig = self.entries[self.mapping[index.inner as usize] as usize].rect;

        let x_range = 0f64 .. self.layout.size.w as f64;
        let y_range = 0f64 .. self.layout.size.w as f64;

        const TARGET_RANGE: Range<f64> = 0f64 .. 5000f64;

        PhysicalRect::new2(
            maprange(orig.pos.x  as f64, x_range.clone(), TARGET_RANGE) as i16,
            maprange(orig.pos.y  as f64, y_range.clone(), TARGET_RANGE) as i16,
            maprange(orig.size.w as f64, x_range.clone(), TARGET_RANGE) as u16,
            maprange(orig.size.h as f64, y_range.clone(), TARGET_RANGE) as u16
        )

    }

    fn upsize(&mut self, renderer: &Renderer, incr: u16) {

        // Create a new, bigger texture.

        let mut layout = AtlasLayout::new(PhysicalSize::new(
            self.layout.size.w + incr,
            self.layout.size.h + incr,
        ));

        let mut new = graphics::Texture::new(&renderer.gp, layout.size, None);

        // We use this chance to sort the entries, for a more
        // efficient spacial layout. We also need to update the mapping.

        self.entries.sort_unstable_by(|lhs, rhs| {
            let ls = lhs.rect.size.w as usize * lhs.rect.size.h as usize;
            let rs = rhs.rect.size.w as usize * rhs.rect.size.h as usize;
            ls.cmp(&rs)
        });

        for (idx, entry) in self.entries.iter().enumerate() {
            self.mapping[entry.mapping as usize] = idx as u16;
        }

        // Copy over the old images to the new texture.

        for entry in self.entries.iter_mut() {
            let newpos = layout.advance(entry.rect.size)
                .expect("layout must be valid, since the new entry was not added yet");
            // Copy from the original rect, still stored in the rect to the new position `newpos`.
            new.fromtex(&self.texture, entry.rect, PhysicalRect::new(newpos, entry.rect.size));
            // Make sure to update the position of the entry accordingly.
            entry.rect.pos = newpos;
        }

        // After this the atlas is fully present in the
        // new texture, so we exchange it with the old one.
        // Also we need to update the layout.

        self.texture = new;
        self.layout = layout;

    }

}

pub trait GlWriteToAtlas {
    /// The OpenGL context will be bound.
    fn write(&self, target: &mut graphics::Texture, dstrect: PhysicalRect);
}

impl GlWriteToAtlas for [u8] {
    #[track_caller]
    fn write(&self, target: &mut graphics::Texture, dstrect: PhysicalRect) {
        // Write ourself to the texture at `dstrect`.
        target.frombuf(self, dstrect);
    }
}

impl GlWriteToAtlas for graphics::Texture {
    #[track_caller]
        fn write(&self, target: &mut graphics::Texture, dstrect: PhysicalRect) {
        target.fromtex(self, PhysicalRect::new(PhysicalPoint::ZERO, dstrect.size), dstrect);
        // original: gl::copy_tex_sub_image_2d((&self.fbo, PhysicalPoint::ZERO), (&target, rect.pos), rect.size);
    }
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
    /// Which `mapping` stores our index. Used to update
    /// the mapping accordingly after sorting the entries.
    pub mapping: u16,
}

#[derive(Debug, Clone, Copy)]
pub enum TextureKind {
    /// RGBA
    Color(u8, u8, u8, u8),
    /// Index into TextureAtlas + Offset
    Atlas(TextureIndex, PhysicalPoint),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextureIndex {
    inner: u16,
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
    pub source: &'a [&'a crate::VertexGeometry],
    pub instances: &'a [Instance],
}

// struct SingularData {
//     vbuf: graphics::VertexBuffer,
// }

// struct InstancedData {
//     vao: gl::VertexArray,
//     vdata: gl::Buffer, // per-vertex data
//     idata: gl::Buffer, // per-instance
//     commands: gl::Buffer, // the draw commands
// }

/// The builtin curve renderer.
pub struct Renderer {
    pub gp: Rc<graphics::Graphics>,
    vbuf: graphics::VertexBuffer,
    vraw: Vec<u8>,
    program: graphics::Program,
    samplerloc: graphics::Location
    // instanced: InstancedData,
    // sampler: gl::UniformLocation,
    // program: gl::LinkedProgram,
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

        // let singular = {
        //     let vdata = gl::gen_buffer(gl::BufferType::Array);
        //     let vao = gl::gen_vertex_array();
        //     gl::vertex_attrib_pointer(&vao, &vdata, 0, 1, gl::DataType::U16, false, 10, 0); // FLAGS
        //     gl::vertex_attrib_pointer(&vao, &vdata, 1, 1, gl::DataType::I32, false, 10, 2); // x, y
        //     gl::vertex_attrib_pointer(&vao, &vdata, 2, 1, gl::DataType::U32, false, 10, 6); // u, v, l (texture coords)
        //     // let buf = graphics::VertexBuffer::new([
        //     //     graphcis::Attrib::PerVertex(graphics::DataType::U16, 1),
        //     //     graphcis::Attrib::PerVertex(graphics::DataType::I32, 1),
        //     //     graphcis::Attrib::PerVertex(graphics::DataType::U32, 1),
        //     // ]);
        //     SingularData { vao, vdata }
        // };

        // let instanced = {
        //     let vdata = gl::gen_buffer(gl::BufferType::Array);
        //     let idata = gl::gen_buffer(gl::BufferType::Array);
        //     let commands = gl::gen_buffer(gl::BufferType::DrawIndirect);
        //     let vao = gl::gen_vertex_array();
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
        //     InstancedData { vao, vdata, idata, commands }
        // };

        // let sampler = gl::uniform_location(&program, "atlas")
        //     .expect("cannot find `atlas` uniform");

        Ok(Self {
            gp,
            vbuf,
            vraw: Vec::with_capacity(1024),
            program,
            samplerloc,
            // singular,
            // instanced,
            // sampler,
            // program,
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

            let inner = &geometry.source[instance.target.geometry as usize];
            let shape = &inner.shapes[instance.target.shape as usize];
            let vertices = &inner.vertices[shape.range2()];

            let ivertices = repeat([0, 1, 2] as [u16; 3]).flatten();

            for (vertex, index) in zip(vertices, ivertices) {

                let vertex_x = vertex.pos[0];
                let vertex_y = vertex.pos[1];

                let physical_x = vertex_x as f64 * (instance.size.w as f64 / 5000.0);
                let physical_y = vertex_y as f64 * (instance.size.h as f64 / 5000.0);
                //                                                              / ^^^^^^
                //      This is the scaling where 5000 means a 1.0 scale. So if...
                //          1. SHAPE = 5000x5000 filled rect
                //          2. INSTANCE.SCALE = 100 = (0.02x)
                //      ...then you will get a 100x100 pixel filled rect.

                let scaled_x = (physical_x * 5000.0) / size.w as f64;
                let scaled_y = (physical_y * 5000.0) / size.h as f64;

                let transformed_x = scaled_x as i16 + instance.pos.x;
                let transformed_y = scaled_y as i16 + instance.pos.y;

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
                        let x_low  = coords.pos.x as f64;
                        let x_high = coords.size.w as f64 + x_low;
                        let y_low  = coords.pos.y as f64;
                        let y_high = coords.size.h as f64 + y_low;
                        let x = maprange(vertex_x as f64, 0f64..5000f64, x_low..x_high);
                        let y = maprange(vertex_y as f64, 0f64..5000f64, y_low..y_high);

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
                self.vraw.extend(transformed_x.to_ne_bytes());
                self.vraw.extend(transformed_y.to_ne_bytes());
                self.vraw.extend(texture_lhs.to_ne_bytes());
                self.vraw.extend(texture_rhs.to_ne_bytes());

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

    pub fn draw<'b>(&mut self, geometry: &DrawableGeometry<'b>, atlas: &TextureAtlas, target: &mut graphics::Texture) {

        self.prepare(geometry, atlas, target.size());

        // // Setup blending.
        // gl::enable(gl::Capability::Blend);
        // gl::blend_func(gl::BlendFunc::SrcAlpha, gl::BlendFunc::OneMinusSrcAlpha);

        // // Make atlas texture accessible. It must always be initialized to a valid texture.
        // gl::active_texture(0);
        // gl::uniform_1i(&self.program, self.sampler, 0);
        // gl::bind_texture(&atlas.texture);

        // // Render all non-instanced shapes.
        // let r = &self.prepared.singular;
        // let len = r.vertices.inner.len();
        // if len > 0 {
        //     gl::buffer_data(&self.singular.vdata, &r.vertices.inner, gl::DrawHint::Dynamic);
        //     gl::draw_arrays(target, &self.program, &self.singular.vao, gl::Primitive::Triangles, 0, len / 10);
        // }

        self.vbuf.frombuf(&self.vraw);

        let options = graphics::DrawOptions {
            primitive: graphics::Primitive::Triangles,
            blend: graphics::BlendMode::OrderedTransparency,
            polygon: graphics::PolygonMode::Filled
        };

        let textures = [
            graphics::TextureAttrib { src: &atlas.texture, sampler: self.samplerloc }
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
fn maprange(v: f64, lhs: Range<f64>, rhs: Range<f64>) -> f64 {
    rhs.start + ((v - lhs.start) * (rhs.end - rhs.start)) / (lhs.end - lhs.start)
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

// /// Vertex data which is ready to be rendered.
// #[derive(Default)]
// struct PreparedGeometry {
//     pub singular: SingularPreparedGeometry,
//     // pub instanced: InstancedPreparedGeometry,
// }
// impl PreparedGeometry {
//     fn clear(&mut self) {
//         self.singular.vertices.clear();
//         // self.instanced.vertices.inner.clear();
//         // self.instanced.instances.inner.clear();
//         // self.instanced.commands.clear();
//     }
// }

// #[derive(Default)]
// struct SingularPreparedGeometry {
//     pub vertices: Vec<u8>,
// }

// #[derive(Default)]
// struct InstancedPreparedGeometry {
//     pub vertices:  gl::AttribVec,
//     pub instances: gl::AttribVec,
//     pub commands:  Vec<gl::DrawArraysIndirectCommand>,
// }

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
