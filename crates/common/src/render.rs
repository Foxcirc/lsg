
use crate::*;
use std::{fmt, ops::Range};

/// A point with additional curve information.
///
/// Can represent base (on-curve) and control (off-curve) points using
/// a compressed format to save space.
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CurvePoint {
    /// # Layout
    /// [kind, disjoint, x-pos, y-pos]
    ///  1bit  1bit      15bit  15bit
    inner: u32,
}

impl fmt::Debug for CurvePoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.kind() == PointKind::Base {
            write!(f, "BasePoint({}, {})", self.x(), self.y())
        } else {
            write!(f, "CtrlPoint({}, {})", self.x(), self.y())
        }
    }
}

impl CurvePoint {

    pub const ZERO: Self = Self::new(0, 0, PointKind::Base);

    pub const fn base(x: i16, y: i16) -> Self {
        Self::new(x, y, PointKind::Base)
    }

    pub const fn ctrl(x: i16, y: i16) -> Self {
        Self::new(x, y, PointKind::Ctrl)
    }

    /// Creates a new point.
    /// # Panic (debug-assertions)
    /// X and Y must be smaller then i16::MAX / 2 since they
    /// are stored as 15-bit numbers internally.
    pub const fn new(x: i16, y: i16, kind: PointKind) -> Self {

        debug_assert!(x >= i16::MIN / 2 && x <= i16::MAX / 2);
        debug_assert!(y >= i16::MIN / 2 && y <= i16::MAX / 2);

        let f1 = match kind {
            PointKind::Base => 0b0,
            PointKind::Ctrl => 0b1,
        };

        let f2 = 0b0; // not used rn

        let inner = ((f1 as u32 & 0b1) << 0 ) |
                    ((f2 as u32 & 0b1) << 1 ) |
                    ((x as u32 & 0x7fff) << 2 ) |
                    ((y as u32 & 0x7fff) << 17);

        Self { inner }
    }

    pub fn x(self) -> i16 {
        ((((self.inner >> 2) & 0x7fff) as i32) << 17 >> 17) as i16
    }

    pub fn y(self) -> i16 {
        ((((self.inner >> 17) & 0x7fff) as i32) << 17 >> 17) as i16
    }

    pub fn kind(self) -> PointKind {
        let flag = (self.inner >> 0) & 0b1;
        match flag {
            0b0 => PointKind::Base,
            0b1 => PointKind::Ctrl,
            _ => unreachable!()
        }
    }

    /// Lossy conversion, see `new` for more details.
    pub fn fromp(point: MathPoint, kind: PointKind) -> Self {
        Self::new(point.x as i16, point.y as i16, kind)
    }

}

#[test]
fn curvepoint() {

    let p = CurvePoint::new(20, -40, PointKind::Base);

    assert_eq!(p.x(), 20);
    assert_eq!(p.y(), -40);
    assert_eq!(p.kind(), PointKind::Base);

}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PointKind {
    Base,
    Ctrl
}

/// Description of what points or vertices make up a shape.
// TODO: Remove and use a Range<u16>, cause having a "Shape" type just for this is not worth it.
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shape {
    pub start: u16,
    pub end: u16,
}

impl Shape {

    pub const ZERO: Self = Self::new(0..0);

    pub const fn new(target: Range<u16>)    -> Self { Self { start: target.start, end: target.end } }
    pub const fn new2(start: u16, end: u16) -> Self { Self { start,               end             } }

    pub fn range(&self)  -> Range<u16>   { self.start          .. self.end          }
    pub fn rangeu(&self) -> Range<usize> { self.start as usize .. self.end as usize }

}

/// A single instance of a shape. This can be used to render the same
/// shape many times with different transformations and textures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instance {
    /// Index into the [`VertexGeometry`]s and then the inner [`Shape`]s.
    pub target: GeometryTarget,
    /// offsetX, offsetY
    pub pos: PhysicalPair,
    /// Scale which is applied to the targeted shape.
    pub size: PhysicalSize,
    /// Texture / Color
    pub texture: TextureKind,
}

impl Instance {
    /// Special value that signifies that this instance shall be ignored.
    ///
    /// # Why is this useful
    /// In some cases, e.g. when clipping, we want to no longer draw some instances.
    /// Removing them would mean needing to shift elements, which is inefficient.
    pub const DISCARD: Self = Self {
        target: GeometryTarget::DISCARD,
        pos: PhysicalPoint::ZERO,
        size: PhysicalSize::ZERO,
        texture: TextureKind::Color(0, 0, 0, 0)
    };
    /// Check if this instance should be discarded.
    pub fn isdiscard(&self) -> bool {
        self.target == GeometryTarget::DISCARD
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GeometryTarget {
    /// Index into the associated list of vertex gemoetries.
    pub geometry: u16,
    /// Index into the list of shapes of that geometry.
    pub shape: u16,
}

impl GeometryTarget {
    /// See [`Instance::DISCARD`].
    pub const DISCARD: Self = Self {
        geometry: u16::MAX,
        shape: u16::MAX
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextureKind {
    /// RGBA
    Color(u8, u8, u8, u8),
    /// Index into TextureAtlas + Offset
    Atlas(TextureIndex, PhysicalPair), // TODO: Add scaling (make a Transform struct and use it in Instance and here)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextureIndex {
    pub inner: u16,
}

/// Geometry that represents curved polygons as a list of points.
#[derive(Default, Debug)]
pub struct CurveGeometry {
    pub points: Vec<CurvePoint>,
    pub shapes: Vec<Shape>,
}

impl CurveGeometry {

    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.points.clear();
        self.shapes.clear();
    }

    #[track_caller]
    pub fn get(&self, ishape: u16) -> &[CurvePoint] {
        &self.points[self.shapes[ishape as usize].rangeu()]
    }

    /// Utility to add a new shape to the curve geometry and returns its index.
    pub fn add(&mut self, points: &[CurvePoint]) -> u16 {

        // Push points.
        let start = self.points.len() as u16;
        self.points.extend(points);
        let end = self.points.len() as u16;

        // Push new shape.
        self.shapes.push(Shape { start, end });

        // Return index.
        (self.shapes.len() - 1) as u16

    }

}

#[derive(Default, Clone, Copy, Debug)]
#[repr(u8)]
pub enum FillKind {
    #[default]
    Filled = 0,
    Convex = 1,
    Concave = 2
}

/// A simple vertex making up a list of triangles in [`VertexGeometry`].
#[derive(Default, Clone, Copy, Debug)]
pub struct PartialVertex {
    pub pos: PhysicalPoint,
    pub curve: FillKind,
    pub edges: u8,
}

impl PartialVertex {
    pub const ZERO: Self = Self::new(PhysicalPoint::ZERO, FillKind::Filled, 0);
    pub const fn new(pos: PhysicalPoint, curve: FillKind, edges: u8) -> Self {
        Self { pos, curve, edges }
    }
}

/// Geometry that represents curved polygons after triangulation.
#[derive(Default, Debug)]
pub struct VertexGeometry {
    pub vertices: Vec<PartialVertex>,
    pub shapes: Vec<Shape>,
}

impl VertexGeometry {

    pub fn clear(&mut self) {
        self.vertices.clear();
        self.shapes.clear();
    }

    /// Add a vertex shape to this geometry and return the index.
    pub fn add(&mut self, data: &[PartialVertex]) -> u16 {

        // Calculate where our shape will be.
        let start = self.vertices.len() as u16;
        let end = start + data.len() as u16;
        let shape = Shape::new(start..end);

        // Just push the points and the new shape.
        self.vertices.extend_from_slice(data);
        self.shapes.push(shape);

        return (self.shapes.len() - 1) as u16

    }

    /// Get the vertices of a shape.
    pub fn get(&self, ishape: u16) -> &[PartialVertex] {
        &self.vertices[self.shapes[ishape as usize].rangeu()]
    }

}

// #[derive(Clone, Copy, PartialEq, Eq)]
// pub enum ShapeKind {
//     Singular,
//     Instanced,
// }

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub enum IntersectionRelation {
//     /// Non-Intesecting
//     Outside,
//     /// Intesecting
//     Inside,
//     /// Point lies on an edge
//     OnEdge([[MathPoint; 2]; 1]),
//     /// Point lies on a corner.
//     OnCorner([[MathPoint; 2]; 2]),
// }

// impl IntersectionRelation {
//     /// All edges this intersection touched.
//     /// OnEdge => 1 edge
//     /// OnCorner => 2 edges
//     pub fn edges(&self) -> &[[MathPoint; 2]] {
//         match self {
//             Self::Outside | Self::Inside => &[],
//             Self::OnEdge(edge) => edge,
//             Self::OnCorner(corner) => corner,
//         }
//     }
// }

/// Implemented by a type that can provide the platform specific display pointer.
/// ### Safety
/// You must always return a valid pointer.
pub unsafe trait IsDisplay {
    /// ### Platforms
    /// **On Wayland,**
    /// should return a pointer to the `wl-display` proxy object.
    // TODO: add link to example in the desktop crate
    fn ptr(&self) -> *const void;
}

/// Implemented by a type that can provide the platform surface pointer.
/// ### Safety
/// You must always return a valid pointer.
pub unsafe trait IsSurface {
    /// # Platform-Specific
    /// 1. Wayand: should return a pointer to a `wl-surface` proxy object.
    fn ptr(&self) -> *mut void;
    /// Get the current size of the surface. The size must be scaled
    /// using the scaling factor to obtain the true physical size.
    /// Must not be `0` in any dimension.
    fn size(&self) -> PhysicalPair;
    /// Get the current scaling factor of the surface.
    fn scale(&self) -> f64;
}
