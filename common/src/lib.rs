
use std::{fmt, ops::{self, Range}};

/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Rect {
    pub pos: PhysicalPoint,
    pub size: Size,
}

impl Rect {
    pub const INFINITE: Self = Self::new(PhysicalPoint::ZERO, Size::INFINITE);
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
    pub const ZERO: Self = Self::new(0, 0);
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

// TODO: I believe we should work with integer points that have high coordinate values (eg. i32::MAX / 2 being the right side of the screen) instead of using f32 generally
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

    pub fn xy(&self) -> [f32; 2] {
        [self.x, self.y]
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

impl ops::Add<Point> for Point {
    type Output = Point;
    fn add(self, rhs: Point) -> Self::Output {
        Self::new(self.x + rhs.x, self.y + rhs.y)
    }
}

/// Area of a window that has to be redrawn.
pub struct Damage<'s> {
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

/// A point with additional curve information.
///
/// Can represent base (on-curve) and control (off-curve) points using
/// a compressed format to save space.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct CurvePoint {
    /// # Layout
    /// [kind, ---, x-pos, y-pos]
    ///  1bit  1bit 15bit  15bit
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

    /// Creates a new point. The point will be `Visible` by default.
    /// # Panic (debug-assertions)
    /// X and Y must be smaller then u16::MAX / 2 since they
    /// are stored as 15-bit numbers internally.
    pub const fn new(x: u16, y: u16, kind: PointKind) -> Self {

        debug_assert!(x < 32767, "x must be < u16::MAX / 2");
        debug_assert!(y < 32767, "y must be < u16::MAX / 2");

        let f1 = match kind {
            PointKind::Base => 0b00,
            PointKind::Ctrl => 0b01,
        };

        let f2 = 0b0; // not used rn

        let inner = ((f1 as u32 & 0b1)    << 0 ) |
                    ((f2 as u32 & 0b1)    << 1 ) |
                    ((x  as u32 & 0x7fff) << 2 ) |
                    ((y  as u32 & 0x7fff) << 17);

        Self { inner }
    }

    pub fn x(&self) -> u16 {
        ((self.inner >> 2) & 0x7fff) as u16
    }

    pub fn y(&self) -> u16 {
        ((self.inner >> 17) & 0x7fff) as u16
    }

    pub fn kind(&self) -> PointKind {
        let flag = (self.inner >> 0) & 0b1;
        match flag {
            0b0 => PointKind::Base,
            0b1 => PointKind::Ctrl,
            _ => unreachable!()
        }
    }

}

/// Lossy conversion, see `new` for more details.
impl CurvePointFrom<PhysicalPoint> for CurvePoint {
    #[track_caller]
    fn convert(point: PhysicalPoint, kind: PointKind) -> Self {
        Self::new(point.x as u16, point.y as u16, kind)
    }
}

/// Lossy conversion, see `new` for more details.
impl CurvePointFrom<Point> for CurvePoint {
    #[track_caller]
    fn convert(point: Point, kind: PointKind) -> Self {
        Self::new(point.x as u16, point.y as u16, kind)
    }
}

pub trait CurvePointFrom<T> {
    fn convert(t: T, kind: PointKind) -> Self;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PointKind {
    Base,
    Ctrl,
}

/// Description of what points or vertices make up a shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shape {
    pub target: Range<u16>,
}

impl Shape {

    pub const ZERO: Self = Self::new(0..0);

    pub const fn new(target: Range<u16>) -> Self {
        Self { target }
    }

    pub fn range(&self) -> Range<usize> {
        self.target.start as usize .. self.target.end as usize
    }

}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ShapeKind {
    Singular,
    Instanced,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntersectionRelation {
    /// Non-Intesecting
    Outside,
    /// Intesecting
    Inside,
    /// Point lies on an edge
    OnEdge([[Point; 2]; 1]),
    /// Point lies on a corner.
    OnCorner([[Point; 2]; 2]),
}

impl IntersectionRelation {
    /// All edges this intersection touched.
    /// OnEdge => 1 edge
    /// OnCorner => 2 edges
    pub fn edges(&self) -> &[[Point; 2]] {
        match self {
            Self::Outside | Self::Inside => &[],
            Self::OnEdge(edge) => edge,
            Self::OnCorner(corner) => corner,
        }
    }
}

/// A single instance of a shape. This can be used to render the same
/// shape many times in different positions and with a different texture.
#[derive(Debug, Clone)]
pub struct Instance {
    /// Index into the \[[`VertexGeometry`]\] and then the inner \[[`Shape`]\].
    pub target: [usize; 2],
    /// offsetX, offsetY
    pub pos: Point, //  TODO: make this be like CurvePoint, an offset in u16 pixels
    /// texture coordinates and layer
    pub texture: [f32; 3],
}

/// A point in normalized device coordinates.
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
    pub fn convert(p: Point, size: Size) -> Self {
        Self {
            x:       2.0 * (p.x as f32 / size.w  as f32) - 1.0,
            y: 1.0 - 2.0 * (p.y as f32 / size.h as f32)
        }
    }

    pub fn xy(&self) -> [f32; 2] {
        [self.x, self.y]
    }

}
