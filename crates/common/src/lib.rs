
use std::{ffi::c_void as void, fmt, iter, mem::ManuallyDrop, ops::{self, Range}, sync::{Mutex, MutexGuard}};

/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct PhysicalRect {
    pub point: PhysicalPair,
    pub size: PhysicalPair,
}

impl PhysicalRect {
    pub const MAX: Self = Self::new(PhysicalPair::MIN, PhysicalPair::MAX);
    pub const ZERO: Self = Self::new(PhysicalPair::ZERO, PhysicalPair::MIN);
    pub const fn new(point: PhysicalPair, size: PhysicalPair) -> Self {
        Self { point, size }
    }
    pub const fn new2(x: i16, y: i16, w: i16, h: i16) -> Self {
        Self { point: PhysicalPair::new(x, y), size: PhysicalPair::new(w, h) }
    }
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
    pub const fn new(x: i16, y: i16) -> Self {
        Self { x, y }
    }
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
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct GeometryTarget {
    /// Index into the associated list of vertex gemoetries.
    pub geometry: u16,
    /// Index into the list of shapes of that geometry.
    pub shape: u16,
}

#[derive(Debug, Clone, Copy)]
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

    /// Add a new shape to the curve geometry and returns its index.
    ///
    /// # Notes
    /// The shape is automatically lowered to a simpler representation,
    /// so the data it easier to introspect and work with for widgets:
    /// - Cubic curves are lowered to quadratic ones.
    /// - Intersected quadratic curves are split,
    ///   so they can be correctly triangulated later.
    pub fn add(&mut self, points: &[CurvePoint]) -> usize {

        // The section is always an extension of the last current point,
        // so e.g. `Quadratic` only contains the new control and end point.

        let start = self.points.len() as u16;

        for section in Self::sections(points) {

            match section {

                CurveSection::Line([(.., a), ..]) => self.points.push(
                    CurvePoint::base(a.x, a.y)
                ),

                CurveSection::Quadratic([(ia, a), (ib, b), (ic, c)]) => {

                    // We need to check for intersections with any other points in the shape.
                    let abc = [a, b, c].map(MathPoint::from);
                    let intersected = points.iter().enumerate().any(|(idx, it)|
                        (idx as u16 != ia && idx as u16 != ib && idx as u16 != ic) &&
                        triangle_intersects_point(abc.map(MathPoint::from), MathPoint::from(*it))
                    );

                    if intersected {
                        for [a, b, ..] in splitquadratic4(abc) {
                            self.points.extend_from_slice(&[
                                CurvePoint::fromp(a, PointKind::Base),
                                CurvePoint::fromp(b, PointKind::Ctrl),
                            ]);
                        }
                    } else {
                        self.points.extend_from_slice(&[
                            CurvePoint::base(a.x, a.y),
                            CurvePoint::ctrl(b.x, b.y)
                        ])
                    }

                },

                CurveSection::Cubic(it) => {

                    // Lower the cubic curve into quadratic curves.
                    let cubic = it.map(|(.., it)| MathPoint::from(it));

                    for [a, b, ..] in lowercubic(cubic) {
                        self.points.extend_from_slice(&[
                            CurvePoint::fromp(a, PointKind::Base),
                            CurvePoint::fromp(b, PointKind::Ctrl),
                        ]);
                    }

                }

            }
        }

        let end = self.points.len() as u16;

        self.shapes.push(Shape { start, end });
        self.shapes.len() - 1

    }

    pub fn sections(points: &[CurvePoint]) -> impl Iterator<Item = CurveSection> {

        use PointKind::*;

        let len = points.len();

        // To resolve cases where the points start with a `Ctrl` instead
        // of `Base` point we use this to shift the iterator around a bit.
        let mut offset = 0;

        // Inspect the start of our shape.
        if len > 0 {

            let kinds = [
                points[0 % len].kind(),
                points[1 % len].kind(),
            ];

            match kinds {
               [Ctrl, Ctrl] => offset = 2,
               [Ctrl, Base] => offset = 1,
               [Base, ..] => offset = 0,
            }

        }

        let mut idx = offset;

        iter::from_fn(move || {

            if idx == len + offset {
                return None
            }

            let incr;
            let result;

            let indices @ [ia, ib, ic, id] = [
                ((idx + 0) % len) as u16,
                ((idx + 1) % len) as u16,
                ((idx + 2) % len) as u16,
                ((idx + 3) % len) as u16
            ];

            let sub = indices.map(|idx| points[idx as usize]);
            let kinds = sub.map(CurvePoint::kind);
            let [a, b, c, d] = sub.map(PhysicalPoint::from);

            match kinds {
                // LINE:
                [Base, Base, ..] => {
                    incr = 1;
                    result = CurveSection::Line([(ia, a), (ib, b)]);
                },
                // QUADRATIC CURVE:
                [Base, Ctrl, Base, ..] => {
                    incr = 2;
                    result = CurveSection::Quadratic([(ia, a), (ib, b), (ic, c)]);
                },
                // CUBIC CURVE:
                [Base, Ctrl, Ctrl, Base] => {
                    incr = 3;
                    result = CurveSection::Cubic([(ia, a), (ib, b), (ic, c), (id, d)]);
                },
                // INVALID (3+ CTRL POINTS):
                invalid => panic!("Invalid points in shape: {:?}", invalid),
            }

            idx += incr;
            return Some(result);

        })

    }

}

/// Area of the triangle ABC.
// TODO: could use signed area (remove "abs") to also get the convexity from this (for svg the stuff)
fn triangle_area([a, b, c]: [MathPoint; 3]) -> f32 {
    (((b.x - a.x) as f32 * (c.y - a.y) as f32 -
      (c.x - a.x) as f32 * (b.y - a.y) as f32).abs()) * 0.5
}

/// If `point` lies within the triangle `trig`.
///
/// Considers points that lie exactly on an edge as outside.
fn triangle_intersects_point([a, b, c]: [MathPoint; 3], point: MathPoint) -> /* IntersectionRelation */ bool {

    let abc = triangle_area([a, b, c]);

    let pab = triangle_area([point, a, b]);
    let pbc = triangle_area([point, b, c]);
    let pca = triangle_area([point, c, a]);

    let total = pab + pbc + pca;

    // small epsilon, to account for precision errors
    const EPS: f32 = 1e-6;

    // if (total - abc).abs() < EPS {
    //     match (pab < EPS, pbc < EPS, pca < EPS) {
    //         // point is inside
    //         (false, false, false) => return IntersectionRelation::Inside,
    //         // point lies on one edge
    //         (true, false, false) => return IntersectionRelation::OnEdge([[a, b]]),
    //         (false, true, false) => return IntersectionRelation::OnEdge([[b, c]]),
    //         (false, false, true) => return IntersectionRelation::OnEdge([[c, a]]),
    //         // point lies on two edges (= on a corner)
    //         (true, true, false) => return IntersectionRelation::OnCorner([[a, b], [b, c]]), // corner B
    //         (false, true, true) => return IntersectionRelation::OnCorner([[b, c], [c, a]]), // corner C
    //         (true, false, true) => return IntersectionRelation::OnCorner([[c, a], [a, b]]), // corner A
    //         // deformed triangle
    //         (true, true, true) => return IntersectionRelation::Outside,
    //     }
    // } else {
    //     IntersectionRelation::Outside
    // }

    (total - abc).abs() < EPS && // general area check
    pab > EPS && pbc > EPS && pca > EPS // points on an edge should be considered outside

}


fn lowercubic(c: [MathPoint; 4]) -> [[MathPoint; 3]; 4] {

    let [x, y] = splitcubic(c, 0.5);
    let [p, q] = splitcubic(x, 0.5);
    let [r, s] = splitcubic(y, 0.5);

    [p, q, r, s].map(|[a, b, c, d]| {
        // Degree reduce from cubic to quadratic, by averaging.
        let averaged = MathPoint {
            x: -0.25*a.x + 0.75*b.x + 0.75*c.x -0.25*d.x,
            y: -0.25*a.y + 0.75*b.y + 0.75*c.y -0.25*d.y
        };

        [a, averaged, d]

    })

}

fn lerp(p1: MathPoint, p2: MathPoint, t: f32) -> MathPoint {
    MathPoint::new(
        p1.x as f32 + (p2.x as f32 - p1.x as f32) * t,
        p1.y as f32 + (p2.y as f32 - p1.y as f32) * t
    )
}

fn splitcubic([a, b, c, d]: [MathPoint; 4], t: f32) -> [[MathPoint; 4]; 2] {
    let p1  = lerp(a, b, t);
    let p2  = lerp(b, c, t);
    let p3  = lerp(c, d, t);
    let p12 = lerp(p1, p2, t);
    let p23 = lerp(p2, p3, t);
    let p   = lerp(p12, p23, t);
    [[a, p1, p12, p], [p, p23, p3, d]]
    // -- curve1 --    -- curve2  --
}

fn splitquadratic([a, b, c]: [MathPoint; 3], t: f32) -> [[MathPoint; 3]; 2] {
    let q1 = lerp(a, b, t);
    let q2 = lerp(b, c, t);
    let r0 = lerp(q1, q2, t);
    [[a, q1, r0], [r0, q2, c]]
    //  curve1       curve2
}

fn splitquadratic4(abc: [MathPoint; 3]) -> [[MathPoint; 3]; 4] {
    let [x, y] = splitquadratic(abc, 0.5);
    let [p, q] = splitquadratic(x, 0.5);
    let [r, s] = splitquadratic(y, 0.5);
    [p, q, r, s]
}

pub enum CurveSection {
    Line      ([(u16, PhysicalPoint); 2]),
    Quadratic ([(u16, PhysicalPoint); 3]),
    Cubic     ([(u16, PhysicalPoint); 4])
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
    pub pos: [u16; 2], // TODO: should be i16 I think (to also allow negative offsets)
    pub curve: FillKind,
    pub edges: u8,
}

impl PartialVertex {
    pub const fn new(pos: [u16; 2], curve: FillKind, edges: u8) -> Self {
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
