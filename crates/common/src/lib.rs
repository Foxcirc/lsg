
use std::{collections::VecDeque, convert::identity, ffi::c_void as void, fmt, future, marker::PhantomData, ops::{self, Range}, pin::{Pin, pin}, sync::{Mutex, MutexGuard}, task};

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
    /// Get the current size of the surface.
    /// Must not be `0` in any dimension.
    fn size(&self) -> LogicalSize;
}

pub struct SmartMutex<T> {
    inner: Mutex<T>,
}

impl<T> SmartMutex<T> {

    pub const fn new(inner: T) -> Self {
        Self { inner: Mutex::new(inner) }
    }

    #[track_caller]
    pub fn lock<'s>(&'s self) -> MutexGuard<'s, T> {
        self.inner.lock().expect("mutex was poisoned")
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

// pub struct EventChannel<T> {
//     inner: SmartMutex<EventChannelInner<T>>,
// }

// impl<T> Default for EventChannel<T> {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// struct EventChannelInner<T> {
//     events: VecDeque<T>,
//     waker: Option<task::Waker>,
// }

// impl<T> EventChannel<T> {

//     pub const fn new() -> Self {
//         Self {
//             inner: SmartMutex::new(EventChannelInner {
//                 events: VecDeque::new(),
//                 waker: None,
//             }),
//         }
//     }

//     /// Returns `true` if this channel has any listeners.
//     pub fn active(&self) -> bool {
//         self.inner.with(|it| it.waker.is_some())
//     }

//     pub fn len(&self) -> usize {
//         self.inner.with(|it| it.events.len())
//     }

//     pub fn send(&self, event: T) {

//         let mut inner = self.inner.lock();

//         inner.events.push_back(event);

//         if let Some(waker) = &inner.waker {
//             waker.wake_by_ref();
//         }

//     }

// }

// impl<T> Future for &EventChannel<T> {

//     type Output = T;

//     fn poll(self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<T> {

//         let mut inner = self.inner.lock();

//         let maybe = inner.events.pop_front();

//         if let Some(it) = maybe {
//             task::Poll::Ready(it)
//         } else {

//             let old = &mut inner.waker;
//             let new = cx.waker();

//             let same = old.as_mut()
//                 .map(|it| it.will_wake(new))
//                 .unwrap_or_default();

//             if !same {
//                 *old = Some(new.clone());
//             }

//             task::Poll::Pending
//         }

//     }

// }

pub struct EventBroadcaster<T: Clone> {
    inner: SmartMutex<EventBroadcasterInner<T>>,
}

impl<T: Clone> Default for EventBroadcaster<T> {
    fn default() -> Self {
        Self::new()
    }
}

struct EventBroadcasterInner<T: Clone> {
    events: VecDeque<Event<T>>,
    wakers: Vec<Option<task::Waker>>,
    listeners: u16, // currently active listeners
    tick: u16, // incremental counter, used to avoid double-reading an event
}

#[derive(Clone)]
struct Event<T: Clone> {
    value: T,
    pending: u16, // listeners that have yet to respond
    tick: u16, // which tick this event belongs to
}

impl<T: Clone> EventBroadcaster<T> {

    pub const fn new() -> Self {
        Self {
            inner: SmartMutex::new(EventBroadcasterInner {
                events: VecDeque::new(),
                wakers: Vec::new(),
                listeners: 0,
                tick: 0,
            }),
        }
    }

    /// Returns `true` if this channel has any listeners.
    pub fn active(&self) -> bool {
        self.inner.with(|it| it.listeners > 0)
    }

    pub fn len(&self) -> usize {
        self.inner.with(|it| it.events.len())
    }

    pub fn send(&self, event: T) {

        // Don't send anything if there are no listeners, as
        // this would create events which can never be consumed.
        if !self.active() {
            return
        }

        let mut inner = self.inner.lock();

        inner.tick = inner.tick.wrapping_add(1);

        let pending = inner.listeners;
        let tick = inner.tick;
        inner.events.push_back(Event {
            value: event,
            pending,
            tick,
        });

        let alive = inner.wakers.iter()
            .filter_map(Option::as_ref);

        for waker in alive {
            waker.wake_by_ref()
        }

    }

    pub fn listen<'s>(&'s self) -> BroadcastFuture<'s, T> {

        let mut inner = self.inner.lock();

        inner.listeners += 1;

        let slot = inner.wakers.iter().enumerate()
            .find(|(.., it)| it.is_none())
            .map(|(idx, ..)| idx)
            .unwrap_or_else(|| {
                inner.wakers.push(None);
                inner.wakers.len() - 1
            });

        BroadcastFuture {
            channel: self,
            slot: slot as u16,
            tick: inner.tick,
        }

    }

}

impl EventBroadcaster<()> {
    /// Convenience method for events with no data.
    pub fn fire(&self) {
        self.send(());
    }
}

pub struct BroadcastFuture<'a, T: Clone> {
    channel: &'a EventBroadcaster<T>,
    slot: u16,
    tick: u16, // only acknowledge events newer then this tick
}

impl<'a, T: Clone> Drop for BroadcastFuture<'a, T> {
    fn drop(&mut self) {
        let mut inner = self.channel.inner.lock();
        inner.wakers[self.slot as usize] = None;
        inner.listeners -= 1;
    }
}

impl<'a, T: Clone> BroadcastFuture<'a, T> {
    pub async fn next(&mut self) -> T {
        self.await
    }
}

impl<'a, T: Clone> Future for &mut BroadcastFuture<'a, T> {

    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut task::Context) -> task::Poll<T> {

        let this = self.get_mut();
        let mut inner = this.channel.inner.lock();

        let maybe = inner.events.iter_mut()
            .find(|it| it.tick > this.tick);

        // if we found an event, read it now

        if let Some(it) = maybe {

            // this event was now read by us
            this.tick = it.tick;
            it.pending -= 1;

            let expired = it.pending == 0;

            let result = it.value.clone();

            if expired {
                // remove the event if it was consumed by all listeners
                drop(inner.events.pop_front());
            }

            task::Poll::Ready(result)

        } else {

            let old = &mut inner.wakers[this.slot as usize];
            let new = cx.waker();

            let same = old.as_mut()
                .map(|it| it.will_wake(new))
                .unwrap_or_default();

            if !same {
                *old = Some(new.clone());
            }

            task::Poll::Pending

        }

    }

}

#[test]
fn channels() {

    use futures_lite::future::block_on;

    // let single = EventChannel::new();

    // single.send(1);
    // single.send(2);
    // single.send(3);

    // block_on(async move {
    //     assert_eq!((&single).await, 1);
    //     assert_eq!((&single).await, 2);
    //     assert_eq!((&single).await, 3);
    // });

    let multi = EventBroadcaster::new();

    multi.send(0);

    let mut listener1 = multi.listen();

    multi.send(1);

    let mut listener2 = multi.listen();

    multi.send(2);
    multi.send(3);

    block_on(async move {

        // Both should receive only events that
        // happen after their creation.

        // listener1:
        assert_eq!((&mut listener1).await, 1);
        assert_eq!((&mut listener1).await, 2);
        assert_eq!((&mut listener1).await, 3);

        // listener2:
        assert_eq!((&mut listener2).await, 2);
        assert_eq!((&mut listener2).await, 3);

    });

    // the listeners were moved and dropped
    assert_eq!(multi.active(), false);
    assert_eq!(multi.len(), 0);


}
