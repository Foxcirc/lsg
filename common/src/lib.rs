
use std::{fmt, ops::Range};

/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Rect {
    pub pos: TodoFuckMePoint,
    pub size: Size,
}

impl Rect {
    pub const INFINITE: Self = Self::new(TodoFuckMePoint::ORIGIN, Size::INFINITE);
    pub const fn new(pos: TodoFuckMePoint, size: Size) -> Self {
        Self { pos, size }
    }
    pub const fn new2(x: i32, y: i32, w: u32, h: u32) -> Self {
        Self { pos: TodoFuckMePoint::new(x, y), size: Size::new(w, h) }
    }
}

#[derive(Debug, Default, Clone, Copy,PartialEq, Eq)]
pub struct Size {
    pub w: u32,
    pub h: u32
}

impl Size {
    pub const INFINITE: Self = Self::new(u32::MAX, u32::MAX);
    pub const fn new(w: u32, h: u32) -> Self { Self { w, h } }
}

/// A point in physical window coordinates.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TodoFuckMePoint { // TODO: remove Point and make this the default and clean up the mess in egl/shaper (sometimes using float sometimes curvePoint sometimes I think why am I doing this)
    pub x: i32,
    pub y: i32,
}

impl TodoFuckMePoint {
    pub const ORIGIN: Self = Self::new(0, 0);
    pub const fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

/// A point in physical window coordinates.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Point {
    pub x: f32, // TODO: use i16?
    pub y: f32,
}

impl Point {
    pub const ZERO: Self = Self::new(0.0, 0.0);
    pub const ORIGIN: Self = Self::ZERO;
    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
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

/// A point on a curve.
/// Can represent base (on-curve) and control (off-curve) points.
/// The coordinates are in physical window coordinates but to save space, there are some quirks:
/// - **positive**: base point
/// - **negative**: control point
/// - i16::MAX/-i16::MAX means zero
/// - y-flipped, so y is growing downwards
/// Be careful not to invalidate any invariants of the fields when
/// you modify their values.
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
        if self.is_base() {
            write!(f, "BasePoint {{{}, {}}}", self.x(), self.y())
        } else {
            write!(f, "CtlrPoint {{{}, {}}}", self.x(), self.y())
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
    pub fn ctrl(mut x: i16, mut y: i16) -> Self {
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

    #[inline(always)]
    pub fn is_base(&self) -> bool {
        self.x > 0
    }

}

/// Convert to basic point. Discarding curve information.
impl From<CurvePoint> for Point {
    fn from(value: CurvePoint) -> Self {
        Self::new(value.x() as f32, value.y() as f32)
    }
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
    pub const fn new_singular(polygon: Range<u16>, instance: u16) -> Self {
        Self {
            polygon,
            instances: instance..instance + 1,
        }
    }

    /// Special values of `instances.len()`:
    ///   - **0**: ZERO shape
    ///   - **1**: singular shape
    #[track_caller]
    pub const fn new_instanced(polygon: Range<u16>, instances: Range<u16>) -> Self {
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

    /// `true`: singular, or zero
    /// `false`: instanced
    pub fn is_singular(&self) -> bool {
        self.instances.len() <= 1
    }

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
