
use std::{ffi::c_void as void, fmt, iter, mem::ManuallyDrop, ops::{self, Range}, sync::{Mutex, MutexGuard}};

/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct PhysicalRect {
    pub point: PhysicalPair,
    pub size: PhysicalPair,
}

// TODO: Add MathRect

impl PhysicalRect {
    pub const MAX: Self = Self::new(PhysicalPair::MIN, PhysicalPair::MAX);
    pub const ZERO: Self = Self::new(PhysicalPair::ZERO, PhysicalPair::MIN);
    pub const fn new(point: PhysicalPair, size: PhysicalPair) -> Self {
        Self { point, size }
    }
    pub const fn new2(x: i16, y: i16, w: i16, h: i16) -> Self {
        Self { point: PhysicalPair::new(x, y), size: PhysicalPair::new(w, h) }
    }
    pub fn xmin(&self) -> i16 { self.point.x }
    pub fn xmax(&self) -> i16 { self.point.x + self.size.x }
    pub fn ymin(&self) -> i16 { self.point.y }
    pub fn ymax(&self) -> i16 { self.point.y + self.size.y }
}

// /// A non-negative size, specified in logical coordinates.
// ///
// /// See [`LogicalPoint`].
// #[repr(C)]
// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
// pub struct LogicalSize {
//     pub w: u16,
//     pub h: u16
// }

// impl LogicalSize {
//     pub const ZERO: Self = Self::new(0, 0);
//     pub const INFINITE: Self = Self::new(u16::MAX, u16::MAX);
//     pub const FULL: Self = Self::new(5000, 5000);
//     pub const MIN: Self = Self::new(0, 0);
//     pub const fn new(w: u16, h: u16) -> Self { Self { w, h } }
//     pub const fn quad(wh: u16) -> Self { Self { w: wh, h: wh } }
//     pub const fn physical(&self, scale: f64) -> PhysicalSize {
//         // With a scaling factor of 1.0, 1920 pixels should be 5000 units.
//         const FACTOR: f64 = 1920.0 / 5000.0;
//         PhysicalSize {
//             w: (self.w as f64 * FACTOR * scale).round() as u16,
//             h: (self.h as f64 * FACTOR * scale).round() as u16,
//         }
//     }
// }

// impl From<PhysicalSize> for LogicalSize {
//     fn from(value: PhysicalSize) -> Self {
//         Self::new(value.w, value.h)
//     }
// }

// /// A non-negative size, specified in physical coordinates.
// #[repr(C)]
// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
// pub struct PhysicalSize {
//     pub w: u16,
//     pub h: u16
// }

// impl PhysicalSize {
//     pub const ZERO: Self = Self::new(0, 0);
//     pub const MAX: Self = Self::new(u16::MAX, u16::MAX);
//     pub const MIN: Self = Self::new(0, 0);
//     pub const fn new(w: u16, h: u16) -> Self { Self { w, h } }
//     pub const fn quad(wh: u16) -> Self { Self { w: wh, h: wh } }
//     pub const fn scale(&self, factor: f64) -> PhysicalSize {
//         PhysicalSize {
//             w: (self.w as f64 * factor).round() as u16,
//             h: (self.h as f64 * factor).round() as u16,
//         }
//     }
    // pub const fn logical(&self, scale: f64) -> LogicalSize {
    //     const FACTOR: f64 = 5000.0 / 1920.0;
    //     LogicalSize {
    //         w: (self.w as f64 * FACTOR / scale).round() as u16,
    //         h: (self.h as f64 * FACTOR / scale).round() as u16,
    //     }
    // }
// }

// impl From<LogicalSize> for PhysicalSize {
//     fn from(value: LogicalSize) -> Self {
//         Self::new(value.w, value.h)
//     }
// }

// /// A point, specified in logical coordinates.
// #[repr(C)]
// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
// pub struct LogicalPoint {
//     pub x: i16,
//     pub y: i16,
// }

// impl LogicalPoint {
//     pub const FULL: Self = Self::new(10000, 10000);
//     pub const ZERO: Self = Self::new(0, 0);
//     pub const MAX: Self = Self::new(i16::MAX, i16::MAX);
//     pub const MIN: Self = Self::new(-i16::MAX, -i16::MAX);
//     pub const fn new(x: i16, y: i16) -> Self {
//         Self { x, y }
//     }
// }

// impl From<MathPoint> for LogicalPoint {
//     fn from(value: MathPoint) -> Self {
//         Self::new(value.x as i16, value.y as i16)
//     }
// }

// /// Convert discarding curve information.
// impl From<CurvePoint> for LogicalPoint {
//     fn from(value: CurvePoint) -> Self {
//         Self::new(value.x() as i16, value.y() as i16)
//     }
// }

/// A point, specified in physical coordinates.
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PhysicalPair {
    pub x: i16,
    pub y: i16,
}

impl PhysicalPair {
    pub const ZERO: Self = Self::new(0, 0);
    pub const MAX: Self = Self::new(i16::MAX, i16::MAX);
    pub const MIN: Self = Self::new(-i16::MAX, -i16::MAX);
    pub const fn new(x: i16, y: i16) -> Self { Self { x, y } }
    pub const fn new2([x, y]: [i16; 2]) -> Self { Self { x, y } }
    pub const fn scale(&self, factor: f64) -> PhysicalPair {
        PhysicalPair {
            x: (self.x as f64 * factor).round() as i16,
            y: (self.y as f64 * factor).round() as i16,
        }
    }
}

impl From<MeasuredPair> for PhysicalPair {
    fn from(it: MeasuredPair) -> Self {
        Self::new(it.x, it.y)
    }
}

impl From<CurvePoint> for PhysicalPair {
    fn from(it: CurvePoint) -> Self {
        Self::new(it.x(), it.y())
    }
}

pub type PhysicalPoint = PhysicalPair;
pub type PhysicalSize  = PhysicalPair;

#[derive(Default, Clone, Copy)]
pub enum Measure {
    #[default]
    Absolute,
    Relative,
}

#[derive(Clone, Copy)]
pub struct MeasuredRect {
    pub point: MeasuredPoint,
    pub size: MeasuredSize
}

impl MeasuredRect {
    pub const ZERO: Self = Self {
        point: MeasuredPoint::ZERO,
        size: MeasuredSize::ZERO
    };
}

#[derive(Default, Clone, Copy)]
pub struct MeasuredPair {
    pub x: i16,
    pub y: i16,
    pub mx: Measure,
    pub my: Measure
}

impl MeasuredPair {
    pub const ZERO: Self = Self::new(abs(0), abs(0));
    pub const fn new(x: MeasuredNumber, y: MeasuredNumber) -> Self {
        Self { x: x.v, mx: x.mv, y: y.v, my: y.mv }
    }
}

pub type MeasuredPoint = MeasuredPair;
pub type MeasuredSize  = MeasuredPair;

// TODO: I believe we should work with integer points that have high coordinate values instead of using f32 generally
/// A mathematical point.
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct MathPoint {
    pub x: f32,
    pub y: f32,
}

impl MathPoint {
    pub const ZERO: Self = Self::new(0.0, 0.0);
    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    pub fn xy(&self) -> [f32; 2] {
        [self.x, self.y]
    }

}

impl From<PhysicalPair> for MathPoint {
    fn from(value: PhysicalPair) -> Self {
        Self::new(value.x as f32, value.y as f32)
    }
}

/// Convert discarding curve information.
impl From<CurvePoint> for MathPoint {
    fn from(value: CurvePoint) -> Self {
        Self::new(value.x() as f32, value.y() as f32)
    }
}

impl ops::Mul<f32> for MathPoint {
    type Output = MathPoint;
    fn mul(self, rhs: f32) -> Self::Output {
        Self::new(self.x * rhs, self.y * rhs)
    }
}

impl ops::Add<MathPoint> for MathPoint {
    type Output = MathPoint;
    fn add(self, rhs: MathPoint) -> Self::Output {
        Self::new(self.x + rhs.x, self.y + rhs.y)
    }
}

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
    /// Removing them would mean needing to shift the `Vec`, which is inefficient.
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
    Atlas(TextureIndex, PhysicalPair),
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

#[derive(Clone, Copy)]
pub struct MeasuredNumber {
    pub v: i16,
    pub mv: Measure
}

/// Utility to return a number with Measure::Absoulte.
pub const fn abs(v: i16) -> MeasuredNumber {
    MeasuredNumber { v, mv: Measure::Absolute }
}

/// Utility to return a number with Measure::Relative.
///
/// Also see: [`percent`].
pub const fn rel(v: i16) -> MeasuredNumber {
    MeasuredNumber { v, mv: Measure::Relative }
}

/// Utility to return a number with Measure::Relative.
///
/// This takes a percent-scale number as f32, which it
/// will convert to the permyriad-scale used by the api.
///
/// Also see: [`rel`].
pub const fn percent(val: f32) -> MeasuredNumber {
    rel((val * 100.0) as i16)
}

/// Computes value * scale, but using units per 10,000.
///
/// # Example Conversion
/// +-----------+-----------+-----------+-----------+
/// | Regular % | 25%       | 50%       | 12,5%     |
/// +-----------+-----------+-----------+-----------+
/// | Our Units | 2,500     | 5,000     | 1,250     |
/// +-----------+-----------+-----------+-----------+
pub const fn rescale(value: i16, scale: i16) -> i16 {
    ((value as isize * scale as isize) / 10000isize) as i16
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

#[derive(Default)]
pub struct SmartMutex<T> {
    inner: Mutex<T>,
}

impl<T> SmartMutex<T> {

    pub const fn new(inner: T) -> Self {
        Self { inner: Mutex::new(inner) }
    }

    #[track_caller]
    pub fn lock<'s>(&'s self) -> MutexGuard<'s, T> {
        self.inner.lock().expect("Mutex was poisoned.")
    }

    #[track_caller]
    pub fn with<F, R>(&self, f: F) -> R
        where F: FnOnce(&mut T) -> R {

        f(&mut *self.lock())

    }

    #[track_caller]
    pub fn set(&self, val: T) {
        *self.lock() = val;
    }

}
