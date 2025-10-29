
use std::{fmt, ops::{self, Range}};

/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Rect {
    pub pos: PhysicalPoint,
    pub size: Size,
}

impl Rect {
    pub const INFINITE: Self = Self::new(PhysicalPoint::ORIGIN, Size::INFINITE);
    pub const fn new(pos: PhysicalPoint, size: Size) -> Self {
        Self { pos, size }
    }
    pub const fn new2(x: isize, y: isize, w: usize, h: usize) -> Self {
        Self { pos: PhysicalPoint::new(x, y), size: Size::new(w, h) }
    }
}

#[derive(Debug, Default, Clone, Copy,PartialEq, Eq)]
pub struct Size {
    pub w: usize,
    pub h: usize
}

impl Size {
    pub const INFINITE: Self = Self::new(usize::MAX, usize::MAX);
    pub const fn new(w: usize, h: usize) -> Self { Self { w, h } }
}

/// A point on a window, in physical coordinates.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PhysicalPoint {
    pub x: isize,
    pub y: isize,
}

impl PhysicalPoint {
    pub const ORIGIN: Self = Self::new(0, 0);
    pub const fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }
}

impl From<Point> for PhysicalPoint {
    fn from(value: Point) -> Self {
        Self::new(value.x as isize, value.y as isize)
    }
}

/// Convert discarding curve information.
impl From<CurvePoint> for PhysicalPoint {
    fn from(value: CurvePoint) -> Self {
        Self::new(value.x() as isize, value.y() as isize)
    }
}

/// A mathematical point.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

impl Point {
    pub const ZERO: Self = Self::new(0.0, 0.0);
    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
}

impl From<PhysicalPoint> for Point {
    fn from(value: PhysicalPoint) -> Self {
        Self::new(value.x as f32, value.y as f32)
    }
}

/// Convert discarding curve information.
impl From<CurvePoint> for Point {
    fn from(value: CurvePoint) -> Self {
        Self::new(value.x() as f32, value.y() as f32)
    }
}

impl ops::Mul<f32> for Point {
    type Output = Point;
    fn mul(self, rhs: f32) -> Self::Output {
        Self::new(self.x * rhs, self.y * rhs)
    }
}

/// Area of a window that has to be redrawn.
pub struct Damage<'s> { // TODO: move to renderer?
    /// Empty means full damage.
    pub rects: &'s [Rect],
}

impl<'s> Damage<'s> {
    /// Everything will be redrawn.
    pub fn all() -> Self {
        Self { rects: &[] }
    }
    /// Only the marked rects should be redrawn.
    /// This is only an optimization and the system may choose
    /// to redraw more parts of the window.
    pub fn partial(rects: &'s [Rect]) -> Self {
        Self { rects }
    }
}

/// Geometry that represents a whole scene.
#[derive(Default)]
pub struct CurveGeometry {
    pub points: Vec<CurvePoint>,
    pub shapes: Vec<Shape>,
    pub instances: Vec<Instance>,
}

impl CurveGeometry {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn clear(&mut self) {
        self.points.clear();
        self.shapes.clear();
        self.instances.clear();
    }
}

/// A point with additional curve information
///
/// Can represent base (on-curve) and control (off-curve) points.
/// To save space on the GPU, here are some quirks with `x` and `y`:
/// - **positive**: base point
/// - **negative**: control point
/// - i16::MAX/-i16::MAX means zero
/// Be careful not to invalidate any invariants of the fields when
/// you modify their values.

// TODO: use +-1 to represent zero instead of +-MAX, then we only have to subtract/add one, or do smth different and represent it using u16

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CurvePoint {
    /// - **positive**: base point
    /// - **negative**: control point
    /// - i16::MAX/-i16::MAX means zero
    pub x: i16,
    /// - **positive**: base point
    /// - **negative**: control point
    /// - i16::MAX/-i16::MAX means zero
    /// - y-flipped, so y is growing downwards
    pub y: i16,
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

    /// Construct a new base point.
    ///
    /// ### Panic
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
    /// ### Panic
    /// Only accepts positive values that are not i16::MAX.
    #[track_caller]
    pub fn ctrl(mut x: i16, mut y: i16) -> Self {
        debug_assert!(
            x >= 0 && x < i16::MAX && y >= 0 && y < i16::MAX,
            "coordinates must be positive and not i16::MAX"
        ); // TODO: document that this is only enforced in debug
        if x == 0 { x = i16::MAX };
        if y == 0 { y = i16::MAX };
        Self { x: -x, y: -y }
    }

    pub fn base_from_point(pt: Point) -> Self {
        Self::base(pt.x as i16, pt.y as i16)
    }

    pub fn ctrl_from_point(pt: Point) -> Self {
        Self::ctrl(pt.x as i16, pt.y as i16)
    }

    #[inline(always)]
    pub fn x(&self) -> i16 {
        self.x.abs() % i16::MAX
    }

    #[inline(always)]
    pub fn y(&self) -> i16 {
        self.y.abs() % i16::MAX
    }

    #[inline(always)]
    pub fn kind(&self) -> PointKind {
        match self.x > 0 {
            true => PointKind::Base,
            false => PointKind::Ctrl,
        }
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PointKind {
    Base,
    Ctrl,
}

/// Description of what points and instances make up a shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shape {
    pub polygon: Range<u16>,
    /// Range of instances this shape has.
    /// `shape.instance.len()` must be `1` for singular shapes
    pub instances: Range<u16>,
}

impl Shape {

    #[track_caller]
    pub const fn singular(polygon: Range<u16>, instance: u16) -> Self {
        Self {
            polygon,
            instances: instance..instance + 1,
        }
    }

    /// Special values of `instances.len()`:
    ///   - **0**: ZERO shape
    ///   - **1**: singular shape
    #[track_caller]
    pub const fn instanced(polygon: Range<u16>, instances: Range<u16>) -> Self {
        Self {
            polygon,
            instances,
        }
    }

    pub fn polygon_range(&self) -> Range<u16> {
        self.polygon.start
        .. self.polygon.end
    }

    /// The range of instances of this shape.
    /// Will include only one instance for singular shapes.
    pub fn instances_range(&self) -> Range<u16> {
        self.instances.start
        .. self.instances.end
    }

    pub fn kind(&self) -> ShapeKind {
        match self.instances.len() <= 1 {
            true => ShapeKind::Singular,
            false => ShapeKind::Instanced,
        }
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntersectionRelation {
    /// Non-Intesecting
    Outside,
    /// Intesecting
    Inside,
    /// Point lies on an edge
    OnEdge(TriangleEdge),
    /// Point lies on a corner.
    OnCorner(TriangleCorner),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriangleEdge {
    AB,
    BC,
    CA
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriangleCorner {
    A,
    B,
    C
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ShapeKind {
    Singular,
    Instanced,
}

/// A single instance of a shape. This can be used to render the same
/// shape many times in different positions and with a different texture.
#[derive(Debug, Clone)]
pub struct Instance {
    /// offsetX, offsetY, z
    pub pos: [f32; 3], //  TODO: make this be like CurvePoint, an offset in u16 pixels
    /// texture coordinates and layer
    pub texture: [f32; 3],
}

/// A point in normalized device coordinates.
// todo: rename to smth like NormalizedPoint
#[derive(Debug, Clone, Copy)]
pub struct GlPoint {
    pub x: f32,
    pub y: f32,
}

impl GlPoint {

    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    /// Convert to normalized device coordinates using the given window size.
    pub fn from(p: Point, size: Size) -> Self {
        Self {
            x:       2.0 * (p.x as f32 / size.w  as f32) - 1.0,
            y: 1.0 - 2.0 * (p.y as f32 / size.h as f32)
        }
    }

}
