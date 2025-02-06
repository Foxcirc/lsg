
use std::{fmt, ops::Range};
use common::*;

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
/// Be careful not to invalidate any invariants of the fields when
/// you modify their values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shape {
    // TODO: ^^^ add doc links to "points"
    /// The index into the `points` to specify what points this shape
    /// conist of.
    /// - range.end **positive**: singular shape
    /// - range.end **negative**: instanced shape
    /// - zero: empty shape
    pub polygon: Range<i16>,
    /// Range of instances this shape has.
    /// `shape.instance.len()` should be one for singular shapes.
    pub instances: Range<u16>,
}

impl Shape {

    #[track_caller]
    pub fn new_singular(polygon: Range<i16>, instance: u16) -> Self {
        debug_assert!(polygon.start >= 0 && polygon.end > 0); // TODO: we have to alow "empty" shapes (where polygon.end == 0), but rn we dont!! which is confusing for users (0..1 is not empty!)
        Self {
            polygon,
            instances: instance..instance + 1,
        }
    }

    #[track_caller]
    pub fn new_instanced(polygon: Range<i16>, instances: Range<u16>) -> Self {
        debug_assert!(polygon.start >= 0 && polygon.end > 0);
        debug_assert!(!instances.len() > 0);
        Self {
            polygon: -polygon.start .. -polygon.end,
            instances,
        }
    }

    pub fn polygon_range(&self) -> Range<i16> {
        self.polygon.start.abs()
        .. self.polygon.end.abs()
    }

    /// The range of instances of this shape.
    /// Will include only one instance for singular shapes.
    pub fn instances_range(&self) -> Range<u16> {
        self.instances.start
        .. self.instances.end
    }

    /// `true`: singular
    /// `false`: instanced
    pub fn is_singular(&self) -> bool {
        // `start` can be zero, `end` cannot and
        // will always be positive or negative
        self.polygon.end > 0
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
