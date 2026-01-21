
use std::{fmt, future, ops::{self, Range}, sync::{Mutex, MutexGuard}, task, ffi::c_void as void};

/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Rect {
    pub pos: LogicalPoint,
    pub size: LogicalSize,
}

impl Rect {
    pub const INFINITE: Self = Self::new(LogicalPoint::ZERO, LogicalSize::INFINITE);
    pub const fn new(pos: LogicalPoint, size: LogicalSize) -> Self {
        Self { pos, size }
    }
    pub const fn new2(x: i16, y: i16, w: u16, h: u16) -> Self {
        Self { pos: LogicalPoint::new(x, y), size: LogicalSize::new(w, h) }
    }
}

/// A non-negative size, specified in logical coordinates.
///
/// See [`LogicalPoint`].
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct LogicalSize {
    pub w: u16,
    pub h: u16
}

impl LogicalSize {
    pub const INFINITE: Self = Self::new(u16::MAX, u16::MAX);
    pub const MAX: Self = Self::new(5000, 5000);
    pub const fn new(w: u16, h: u16) -> Self { Self { w, h } }
    pub const fn physical(&self, scale: f32) -> PhysicalSize {

        // With a scaling factor of 1.0, 1920 pixels should be 5000 units.
        const FACTOR: f32 = 5000.0 / 1920.0;

        PhysicalSize {
            w: (self.w as f32 * FACTOR * scale).round() as u16,
            h: (self.h as f32 * FACTOR * scale).round() as u16,
        }

    }
}

// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
// /// A non-negative size, specified in physical coordinates.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PhysicalSize {
    pub w: u16,
    pub h: u16
}

/// A point, specified in logical coordinates.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct LogicalPoint {
    pub x: i16,
    pub y: i16,
}

impl LogicalPoint {
    pub const ZERO: Self = Self::new(0, 0);
    pub const MAX: Self = Self::new(10000, 10000);
    pub const INFINITE: Self = Self::new(i16::MAX, i16::MAX);
    pub const fn new(x: i16, y: i16) -> Self {
        Self { x, y }
    }
}

impl From<MathPoint> for LogicalPoint {
    fn from(value: MathPoint) -> Self {
        Self::new(value.x as i16, value.y as i16)
    }
}

/// Convert discarding curve information.
impl From<CurvePoint> for LogicalPoint {
    fn from(value: CurvePoint) -> Self {
        Self::new(value.x() as i16, value.y() as i16)
    }
}

// TODO: I believe we should work with integer points that have high coordinate values (eg. i32::MAX / 2 being the right side of the screen) instead of using f32 generally
/// A mathematical point.
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

impl From<LogicalPoint> for MathPoint {
    fn from(value: LogicalPoint) -> Self {
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

    pub fn x(&self) -> i16 {
        ((((self.inner >> 2) & 0x7fff) as i32) << 17 >> 17) as i16
    }

    pub fn y(&self) -> i16 {
        ((((self.inner >> 17) & 0x7fff) as i32) << 17 >> 17) as i16
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

#[test]
fn curvepoint() {

    let p = CurvePoint::new(20, -40, PointKind::Base);

    assert_eq!(p.x(), 20);
    assert_eq!(p.y(), -40);
    assert_eq!(p.kind(), PointKind::Base);

}

/// Lossy conversion, see `new` for more details.
impl CurvePointFrom<LogicalPoint> for CurvePoint {
    #[track_caller]
    fn convert(point: LogicalPoint, kind: PointKind) -> Self {
        Self::new(point.x as i16, point.y as i16, kind)
    }
}

/// Lossy conversion, see `new` for more details.
impl CurvePointFrom<MathPoint> for CurvePoint {
    #[track_caller]
    fn convert(point: MathPoint, kind: PointKind) -> Self {
        Self::new(point.x as i16, point.y as i16, kind)
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
    OnEdge([[MathPoint; 2]; 1]),
    /// Point lies on a corner.
    OnCorner([[MathPoint; 2]; 2]),
}

impl IntersectionRelation {
    /// All edges this intersection touched.
    /// OnEdge => 1 edge
    /// OnCorner => 2 edges
    pub fn edges(&self) -> &[[MathPoint; 2]] {
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
    /// Index into the [`VertexGeometry`]s and then the inner [`Shape`]s.
    pub target: [usize; 2], // TODO: make this a struct with named fields not just a [usize; 2]
    /// offsetX, offsetY
    pub pos: LogicalPoint,
    /// Scale which is applied to the targeted shape.
    pub size: LogicalSize,
    // /// texture coordinates and layer
    // pub texture: [f32; 3],
}

pub struct InstanceTarget {
    /// Index into the associated list of vertex gemoetries.
    pub geometry: u16,
    /// Index into the list of shapes of that geometry.
    pub shape: u16,
}

/// A point in normalized device coordinates.
// #[derive(Debug, Clone, Copy)]
// pub struct GlPoint {
//     pub x: f32,
//     pub y: f32,
// }
//
// impl GlPoint {
//
//     pub fn new(x: f32, y: f32) -> Self {
//         Self { x, y }
//     }
//
//     /// Convert to normalized device coordinates using the given window size.
//     pub fn convert(p: Point, size: Size) -> Self {
//         Self {
//             x:       2.0 * (p.x as f32 / size.w  as f32) - 1.0,
//             y: 1.0 - 2.0 * (p.y as f32 / size.h as f32)
//         }
//     }
//
//     pub fn xy(&self) -> [f32; 2] {
//         [self.x, self.y]
//     }
//
// }

/// Implemented by a type that can provide the platform specific display pointer.
/// ### Safety
/// You must always return a valid pointer.
pub unsafe trait IsDisplay {
    /// ### Platforms
    /// **On Wayland,**
    /// should return a pointer to the `wl-display` proxy object.
    // TODO: add link to example in the desktop crate
    fn ptr(&self) -> *mut void;
}

/// Implemented by a type that can provide the platform surface pointer.
/// ### Safety
/// You must always return a valid pointer.
// TODO: when can the surface wayland object be dropped?
pub unsafe trait IsSurface {
    /// ### Platforms
    /// **On Wayland,**
    /// should return a pointer to a `wl-surface` proxy object.
    // TODO: add link to example in the desktop crate
    fn ptr(&self) -> *mut void;
}

pub struct SmartMutex<T> {
    inner: Mutex<T>,
}

impl<T> SmartMutex<T> {

    pub fn new(inner: T) -> Self {
        Self { inner: Mutex::new(inner) }
    }

    pub fn lock<'s>(&'s self) -> MutexGuard<'s, T> {
        self.inner.lock().expect("mutex was poisoned")
    }

    pub fn with<F, R>(&self, f: F) -> R
        where F: FnOnce(&mut T) -> R {

        f(&mut *self.lock())

    }

    pub fn set(&self, val: T) {
        *self.lock() = val;
    }

}

pub struct EventChannel<T> {
    inner: SmartMutex<EventChannelInner<T>>,
}

struct EventChannelInner<T> {
    waker: Option<task::Waker>,
    event: Option<T>,
}

impl<T> Default for EventChannel<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> EventChannel<T> {

    pub fn new() -> Self {
        Self {
            inner: SmartMutex::new(EventChannelInner {
                waker: None,
                event: None
            })
        }
    }

    pub fn send(&self, event: T) {

        let mut inner = self.inner.lock();

        inner.event = Some(event);

        if let Some(waker) = &inner.waker {
            waker.wake_by_ref();
        }

    }

    pub async fn listen(&self) -> T {
        future::poll_fn(|cx| { self.poll(cx) }).await
    }

    pub fn poll(&self, cx: &mut task::Context<'_>) -> task::Poll<T> {

        let mut inner = self.inner.lock();

        if let Some(it) = inner.event.take() {

            task::Poll::Ready(it)

        } else {

            match &mut inner.waker {
                Some(it) => it.clone_from(cx.waker()),
                None => inner.waker = Some(cx.waker().clone())
            }

            task::Poll::Pending

        }

    }

}
