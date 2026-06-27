
//! This crate contains all the builtin widgets that come with lsg,
//! aswell as their required backend logic to e.g. parse input data.

pub mod catalogue;
pub use catalogue::*;

use std::sync::Arc;

pub trait Widget {
    fn action(&self, action: Action);
}

/*

Some cool ideas:

let rect = ForceResize::build().size((5000, 5000))
    .inner(Rect::build().color(Color::Red));

let rect = ForceResize::new((5000, 5000), "rect-red-round25")

let rect = ForceResize((5000, 5000), Rect().red().round25());

let rect = widget("ForceResize(5000x5000, Rect(red, round25))");

 */

pub enum Action<'a> {

    Render { space: Space<'a> },
    // RenderCached { space: Space<'a> }, // This would be used to render a singular widget using app.redraw(&mywidget), so the widgets would have to store their last space-parameters (rect) and then restore and render themselves with the same offset+size. If we would use widgets ID's this could be automatic if space.child stored the new widget id somehow, but I dont like mandatory widget ID's.

    MouseMotion { point: common::PhysicalPoint },
    MouseDown { point: common::PhysicalPoint, button: desktop::MouseButton },
    MouseUp { point: common::PhysicalPoint, button: desktop::MouseButton },
    MouseScroll { delta: common::PhysicalPair },

    Unhover,
    Unfocus,

    KeyDown { key: desktop::Key, repeat: bool },
    KeyUp { key: desktop::Key },

    TextInput { chr: char },
    TextCompose { chr: char },
    TextComposeCancel,

}

#[derive(Default)]
pub struct SpaceRenderState {
    /// Widget-added geometries.
    pub blobs: Vec<Arc<common::VertexGeometry>>,
    /// Widget-added vertices.
    pub vertices: common::VertexGeometry,
    /// Widget-added curves.
    pub curves: common::CurveGeometry,
    /// Ordered shape instances, which index
    /// into the various stored geometries.
    pub instances: Vec<common::Instance>,
}

impl SpaceRenderState {
    pub fn clear(&mut self) {
        self.blobs.clear();
        self.vertices.clear();
        self.curves.clear();
        self.instances.clear();
    }
}

pub struct Space<'a> {
    pub state: &'a mut SpaceRenderState,
    pub offset: common::PhysicalPoint,
    pub size: common::PhysicalSize,
}

impl<'a> Space<'a> {

    pub fn data(&mut self, data: Data) -> SpaceKey {

        match data {
            Data::Curves(it) => {

                let target = &mut self.state.curves;

                let start = target.points.len() as u16;
                target.points.extend_from_slice(it);
                let end = target.points.len() as u16;

                target.shapes.push(common::Shape::new(start..end));
                let idx = (target.shapes.len() - 1) as u16;

                // SpaceKey { index, kind: SpaceKeyKind::Curves }
                SpaceKey::Curves { shape: idx }

            },
            Data::Vertices(it) => {

                let target = &mut self.state.vertices;

                let start = target.vertices.len() as u16;
                target.vertices.extend_from_slice(it);
                let end = target.vertices.len() as u16;

                target.shapes.push(common::Shape::new(start..end));
                let idx = (target.shapes.len() - 1) as u16;

                SpaceKey::Vertices { shape: idx }

            },
            Data::Geometry(it) => {

                let items = &mut self.state.blobs;
                items.push(it);
                let geometry = (items.len() + 1) as u16;
                //                        ^^^^
                // we need to adjust because when creating the `DrawableGeometry`
                // the first two geometries will be for our own curves and vertices.

                SpaceKey::Geometry { geometry }

            },
        }

    }

    #[track_caller]
    pub fn instance(&mut self, key: SpaceKey, i: Instance) {

        let new = Self::transform(common::MeasuredRect { point: i.pos, size: i.size }, self.offset, self.size);

        let inner = common::Instance {
            target: key.target(),
            pos: new.point,
            size: new.size,
            texture: i.texture,
        };

        self.state.instances.push(inner);

    }

    pub fn child<'s>(&'s mut self, offset: common::MeasuredPair, size: common::MeasuredSize) -> Space<'s> {

        let new = Self::transform(common::MeasuredRect { point: offset, size }, self.offset, self.size);

        Space {
            state: self.state,
            offset: new.point,
            size: new.size
        }

    }

    fn transform(input: common::MeasuredRect, toffset: common::PhysicalPoint, tscale: common::PhysicalSize) -> common::PhysicalRect {
        let point = common::PhysicalPoint {
            x: toffset.x + match input.point.mx {
                common::Measure::Absolute => input.point.x,
                common::Measure::Relative => common::rescale(input.point.x, tscale.x)
            },
            y: toffset.x + match input.point.my {
                common::Measure::Absolute => input.point.y,
                common::Measure::Relative => common::rescale(input.point.y, tscale.y)
            }
        };
        let size = common::PhysicalSize {
            x: match input.size.mx {
                common::Measure::Absolute => input.size.x,
                common::Measure::Relative => common::rescale(input.size.x, tscale.x)
            },
            y: match input.size.my {
                common::Measure::Absolute => input.size.y,
                common::Measure::Relative => common::rescale(input.size.y, tscale.y)
            }
        };
        common::PhysicalRect { point, size }
    }

}

pub enum Data<'a> {
    Curves(&'a [common::CurvePoint]),
    Vertices(&'a [common::PartialVertex]),
    Geometry(Arc<common::VertexGeometry>)
}

#[derive(Clone, Copy)]
pub enum SpaceKey {
    Curves       { shape: u16 },
    Vertices     { shape: u16 },
    Geometry     { geometry: u16 },
    GeometryFull { geometry: u16, shape: u16 }
}

impl SpaceKey {
    #[track_caller]
    pub fn shape(self, shape: u16) -> Self {
        if let Self::Geometry { geometry } = self {
            Self::GeometryFull { geometry, shape }
        } else {
            panic!("Only used for geometry `SpaceKey`.")
        }
    }
    #[track_caller]
    pub fn target(self) -> common::GeometryTarget {
        use common::GeometryTarget;
        match self {
            Self::Curves       { shape }           => GeometryTarget { geometry: 0, shape },
            Self::Vertices     { shape }           => GeometryTarget { geometry: 1, shape },
            Self::GeometryFull { geometry, shape } => GeometryTarget { geometry, shape },
            Self::Geometry     { .. }              => panic!("Incomplete `SpaceKey`."),
        }
    }
}

pub struct Instance {
    /// offsetX, offsetY
    pub pos: common::MeasuredPair,
    /// Size of the shape in logical pixels.
    pub size: common::MeasuredPair,
    // Texture information.
    pub texture: common::TextureKind,
}
