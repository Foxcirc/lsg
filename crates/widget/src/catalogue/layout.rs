
use std::{iter::zip, mem::swap};

use common::SmartMutex;
use crate::*;

pub struct Placement<W: Widget> {
    pub rect: common::MeasuredRect,
    pub inner: W,
}

impl<W: Widget> Widget for Placement<W> {
    fn action(&self, layout: Layout, action: Action) {
        let clayout = layout.child(self.rect);
        if let Some(it) = action.cascade(clayout) {
            self.inner.action(clayout, it);
        }
    }
}

pub struct Many<W: Widget> {
    pub inner: SmartMutex<Vec<W>>,
}

impl<W: Widget> Many<W> {
    pub fn add(&self, w: W) {
        self.inner.lock().push(w);
    }
}

impl<W: Widget> Widget for Many<W> {
    fn action(&self, layout: Layout, action: Action) {
        for entry in &*self.inner.lock() {
            entry.action(layout, action);
        }
    }
}

pub struct Cols2<W1: Widget, W2: Widget> {
    pub w1: SmartMutex<(common::MeasuredNumber, W1)>,
    pub w2: SmartMutex<(common::MeasuredNumber, W2)>,
}

impl<W1: Widget, W2: Widget> Cols2<W1, W2> {
    pub fn new(
        w1: (common::MeasuredNumber, W1),
        w2: (common::MeasuredNumber, W2)
    ) -> Self {
        Self {
            w1: SmartMutex::new(w1),
            w2: SmartMutex::new(w2)
        }
    }
}

impl<W1: Widget, W2: Widget> Widget for Cols2<W1, W2> {
    fn action(&self, layout: Layout, action: Action) {
        let mut offset: i16 = 0;
        implcol(layout, action, &*self.w1.lock(), &mut offset);
        implcol(layout, action, &*self.w2.lock(), &mut offset);
    }
}

pub struct Cols3<W1: Widget, W2: Widget, W3: Widget> {
    pub w1: SmartMutex<(common::MeasuredNumber, W1)>,
    pub w2: SmartMutex<(common::MeasuredNumber, W2)>,
    pub w3: SmartMutex<(common::MeasuredNumber, W3)>,
}

impl <W1: Widget, W2: Widget, W3: Widget> Cols3<W1, W2, W3> {
    pub fn new(
        w1: (common::MeasuredNumber, W1),
        w2: (common::MeasuredNumber, W2),
        w3: (common::MeasuredNumber, W3)
    ) -> Self {
        Self {
            w1: SmartMutex::new(w1),
            w2: SmartMutex::new(w2),
            w3: SmartMutex::new(w3)
        }
    }
}

impl<W1: Widget, W2: Widget, W3: Widget> Widget for Cols3<W1, W2, W3> {
    fn action(&self, layout: Layout, action: Action) {
        let mut offset: i16 = 0;
        implcol(layout, action, &*self.w1.lock(), &mut offset);
        implcol(layout, action, &*self.w2.lock(), &mut offset);
        implcol(layout, action, &*self.w3.lock(), &mut offset);
    }
}

pub struct Cols<W: Widget> {
    pub inner: SmartMutex<Vec<(common::MeasuredNumber, W)>>
}

impl<W: Widget> Cols<W> {
    pub fn new(inner: Vec<(common::MeasuredNumber, W)>) -> Self {
        Self { inner: SmartMutex::new(inner) }
    }
}

impl<W: Widget> Widget for Cols<W> {
    fn action(&self, layout: Layout, action: Action) {

        let mut offset: i16 = 0;

        for slot in &*self.inner.lock() {
            implcol(layout, action, slot, &mut offset);
        }

    }
}

fn implcol<W: Widget>(layout: Layout, action: Action, slot: &(common::MeasuredNumber, W), offset: &mut i16) {

    let (cwidth, cwidget) = slot;

    let clayout = layout.child(common::MeasuredRect {
        point: common::MeasuredPoint::new(common::abs(*offset), common::abs(0)),
        size: common::MeasuredSize::new(*cwidth, common::abs(layout.height()))
    });

    *offset += clayout.width();

    if let Some(it) = action.cascade(layout) {
        cwidget.action(clayout, it);
    }
}

pub struct Rows<W: Widget> {
    pub inner: SmartMutex<Vec<(common::MeasuredNumber, W)>>
}

impl<W: Widget> Rows<W> {
    pub fn new(inner: Vec<(common::MeasuredNumber, W)>) -> Self {
        Self { inner: SmartMutex::new(inner) }
    }
}

impl<W: Widget> Widget for Rows<W> {
    fn action(&self, layout: Layout, action: Action) {

        let mut offset: i16 = 0;

        for (cheight, cwidget) in &*self.inner.lock() {

            let clayout = layout.child(common::MeasuredRect {
                point: common::MeasuredPoint::new(common::abs(0), common::abs(offset)),
                size: common::MeasuredSize::new(common::abs(layout.width()), *cheight)
            });

            offset += clayout.height();

            if let Some(it) = action.cascade(layout) {
                cwidget.action(clayout, it);
            }

        }

    }
}

/// Clips its children to avoid their geometry escaping bounds.
///
/// A clipping check is run on all child geometry and instances are
/// adjusted so that their geometry stops at the layout bounds.
///
/// This widget should only be used if you know the child is going to
/// draw out of bounds on purpose.
pub struct Clip<W: Widget> {
    pub inner: W,
    bufs: SmartMutex<ClipBufs>,
}

struct ClipBufs {
    pub curves0: Vec<common::CurvePoint>,
    pub curves1: Vec<common::CurvePoint>,
}

impl ClipBufs {
    pub fn new() -> Self {
        Self {
            curves0: Vec::with_capacity(24),
            curves1: Vec::with_capacity(24),
        }
    }
}

impl<W: Widget> Clip<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner,
            bufs: SmartMutex::new(ClipBufs::new()),
        }
    }
}

impl<W: Widget> Widget for Clip<W> {
    fn action(&self, layout: Layout, action: Action) {

        // The `Clip` widget is purely for visual cleanliness and doesn't need
        // to care about generally affecting the widget layouting process.

        if let Action::Render { space } = action {

            let start = space.state.lock()
                .instances.len();

            // Let the childs render, next we will inspect their output.
            self.inner.action(layout, Action::Render { space });

            let SpaceRenderState { blobs, vertices, instances }
                = &mut *space.state.lock();

            // Iterate over the instances added by the child.
            for instance in &mut instances[start..] {

                // We do bounding box checks on the instances:
                //
                // The checks are based on the fact, that an instance
                // should always be smaller then its bounding box.
                //
                // +---------------------+-----------+
                // | case                | action    |
                // +---------------------+-----------+
                // | completely inside   | keep      |
                // | completely outside  | discard   |
                // | touching bounds     | clip      |
                // +---------------------+-----------+

                let rect = common::PhysicalRect {
                    point: instance.pos,
                    size: instance.size
                };

                if rect_inside_rect(rect, layout.bounds) {
                    // If completely inside, we can keep it as-is.
                    continue
                }

                else if !rect_intersects_rect(rect, layout.bounds) {
                    // If completely outside, discard it.
                    *instance = common::Instance::DISCARD;
                }

                else {

                    // Otherwise this instance needs to be clipped.

                    if instance.target.geometry == 0 {
                        // `0` means index into `state.curves`.

                        // A Sutherland-Hogman style algorithm is used, which clips the
                        // geometry against the 4 edges of the bounding rect individually.

                        const EDGES: [EdgeEquations; 4] = [
                            EdgeEquations::LEFT,
                            EdgeEquations::RIGHT,
                            EdgeEquations::BOTTOM,
                            EdgeEquations::TOP
                        ];

                        let ClipBufs { curves0, curves1 } = &mut *self.bufs.lock();

                        let points = curves.get(instance.target.shape);

                        // Initial setup for our multi-pass algorithm.
                        curves0.extend_from_slice(points);

                        for edge in EDGES {

                            // Clear our output.
                            curves1.clear();

                            for section in common::CurveGeometry::sections(points) {

                                match section {

                                    common::CurveSection::Line([(.., p0), (.., p1)]) => {

                                        match (
                                            (edge.inside)(p0, layout.bounds),
                                            (edge.inside)(p1, layout.bounds)
                                        ) {
                                            // staying outside:
                                            (false, false) => (), // discard
                                            // staying inside:
                                            (true, true) => {
                                                curves1.push(common::CurvePoint::base(p0.x, p0.y));
                                            },
                                            // moving outside:
                                            (true, false) => {
                                                let mid = (edge.iline)([p0, p1], layout.bounds);
                                                curves1.push(common::CurvePoint::base(p0.x, p0.y));
                                                curves1.push(common::CurvePoint::base(mid.x, mid.y));
                                            },
                                            // moving inside:
                                            (false, true) => {
                                                let mid = (edge.iline)([p0, p1], layout.bounds);
                                                curves1.push(common::CurvePoint::base(mid.x, mid.y));
                                            }
                                        }

                                    },

                                    common::CurveSection::Quadratic([(.., p0), (.., ctrl), (.., p1)]) => {

                                        use common::PointKind;

                                        match (
                                            (edge.inside)(p0,   layout.bounds),
                                            (edge.inside)(ctrl, layout.bounds),
                                            (edge.inside)(p1,   layout.bounds)
                                        ) {
                                            // staying outside:
                                            (false, false, false) => (), // discard
                                            // staying inside:
                                            (true, true, true) => {
                                                curves1.push(common::CurvePoint::base(p0.x,   p0.y));
                                                curves1.push(common::CurvePoint::ctrl(ctrl.x, ctrl.y));
                                            },
                                            // (possibly) moving outside:
                                            (true, ..) => {
                                                let curve = [p0, ctrl, p1].map(common::MathPoint::from);
                                                let intersections = (edge.icurve)([p0, ctrl, p1], layout.bounds);
                                                match intersections {
                                                    // not actually moving outside:
                                                    CurveIntersections::None => {
                                                        curves1.push(common::CurvePoint::base(p0.x,   p0.y));
                                                        curves1.push(common::CurvePoint::ctrl(ctrl.x, ctrl.y));
                                                    },
                                                    // moving outside:
                                                    CurveIntersections::One([t]) => {
                                                        let [inner, ..] = common::splitquadratic(curve, t);
                                                        curves1.push(common::CurvePoint::fromp(inner[0], PointKind::Base));
                                                        curves1.push(common::CurvePoint::fromp(inner[1], PointKind::Ctrl));
                                                        curves1.push(common::CurvePoint::fromp(inner[2], PointKind::Base));
                                                    },
                                                    // moving outside and back inside:
                                                    CurveIntersections::Two([t1, t2]) => {
                                                        let [inner1, .., inner2] = common::splitquadratic3(curve, t1, t2);
                                                        // inner section 1:
                                                        curves1.push(common::CurvePoint::fromp(inner1[0], PointKind::Base));
                                                        curves1.push(common::CurvePoint::fromp(inner1[1], PointKind::Ctrl));
                                                        curves1.push(common::CurvePoint::fromp(inner1[2], PointKind::Base));
                                                        // inner section 2:
                                                        curves1.push(common::CurvePoint::fromp(inner2[0], PointKind::Base));
                                                        curves1.push(common::CurvePoint::fromp(inner2[1], PointKind::Ctrl));
                                                        curves1.push(common::CurvePoint::fromp(inner2[2], PointKind::Base));
                                                    }
                                                }
                                            },
                                            // (possibly) moving inside:
                                            (false, ..) => {
                                                let curve = [p0, ctrl, p1].map(common::MathPoint::from);
                                                let intersections = (edge.icurve)([p0, ctrl, p1], layout.bounds);
                                                match intersections {
                                                    // not actually moving inside:
                                                    CurveIntersections::None => (), // discard
                                                    // moving inside:
                                                    CurveIntersections::One([t]) => {
                                                        let [.., inner] = common::splitquadratic(curve, t);
                                                        curves1.push(common::CurvePoint::fromp(inner[0], PointKind::Base));
                                                        curves1.push(common::CurvePoint::fromp(inner[1], PointKind::Ctrl));
                                                    },
                                                    // moving inside and back outside:
                                                    CurveIntersections::Two([t1, t2]) => {
                                                        let [_, inner, _] = common::splitquadratic3(curve, t1, t2);
                                                        curves1.push(common::CurvePoint::fromp(inner[0], PointKind::Base));
                                                        curves1.push(common::CurvePoint::fromp(inner[1], PointKind::Ctrl));
                                                        curves1.push(common::CurvePoint::fromp(inner[2], PointKind::Base));
                                                    }
                                                }
                                            }
                                        }

                                    }

                                    common::CurveSection::Cubic(..) => unreachable!()

                                }

                            }

                            // One passes output shall be input to the next.
                            swap(curves0, curves1);

                        }

                        // The final clipped shape is contained in curves0.
                        let idx = curves.add(curves0);
                        instance.target.shape = idx;

                    } else if instance.target.geometry == 1 {
                        // `1` means index into `state.vertices`.
                    } else {
                        // otherwise, index into another geometry
                    }

                }

            }


        } else {
            self.inner.action(layout, action);
        }

    }
}

enum AddPoints {
    None,
    One([common::PhysicalPoint; 1]),
    Two([common::PhysicalPoint; 2])
}

struct EdgeEquations {
    /// Get if a point is inside the bounds.
    pub inside: fn(point: common::PhysicalPoint, bounds: common::PhysicalRect) -> bool,
    /// Get the intersection point of the edge and the bounds.
    pub iline: fn(edge: [common::PhysicalPoint; 2], bounds: common::PhysicalRect) -> common::PhysicalPoint,
    /// Get the intersection point(s) of the curve and the bounds.
    pub icurve: fn(curve: [common::PhysicalPoint; 3], bounds: common::PhysicalRect) -> CurveIntersections
}

impl EdgeEquations {

    pub const LEFT: Self = Self {
        inside: |point, bounds| point.x >= bounds.point.x,
        iline: |[p0, p1], bounds| common::PhysicalPoint::new(
            bounds.point.x, p0.y + (p1.y - p0.y) * ((bounds.point.x - p0.x) / (p1.x -p0.x))
        ),
        icurve: |[p0, ctrl, p1], bounds| solve_quadratic_bezier_1d(p0.x as f32, ctrl.x as f32, p1.x as f32, bounds.point.x as f32)
    };

    pub const RIGHT: Self = Self {
        inside: |point, bounds| point.x <= bounds.point.x + bounds.size.x,
        iline: |[p0, p1], bounds| {
            let xmax = bounds.point.x + bounds.size.x;
            common::PhysicalPoint::new(
                xmax, p0.y + (p1.y - p0.y) * ((xmax - p0.x) / (p1.x - p0.x))
            )
        },
        icurve: |[p0, ctrl, p1], bounds| solve_quadratic_bezier_1d(p0.x as f32, ctrl.x as f32, p1.x as f32, bounds.point.x as f32 + bounds.size.x as f32)
    };

    pub const BOTTOM: Self = Self {
        inside: |point, bounds| point.y >= bounds.point.y,
        iline: |[p0, p1], bounds| common::PhysicalPoint::new(
            p0.x + (p1.x - p0.x) * ((bounds.point.y - p0.y) / (p1.y - p0.y)), bounds.point.y
        ),
        icurve: |[p0, ctrl, p1], bounds| solve_quadratic_bezier_1d(p0.y as f32, ctrl.y as f32, p1.y as f32, bounds.point.y as f32)
    };

    pub const TOP: Self = Self {
        inside: |point, bounds| point.y <= bounds.point.y + bounds.size.y,
        iline: |[p0, p1], bounds| {
            let ymax = bounds.point.y + bounds.size.y;
            common::PhysicalPoint::new(
                p0.x + (p1.x - p0.x) * ((ymax - p0.y) / (p1.y - p0.y)), ymax
            )
        },
        icurve: |[p0, ctrl, p1], bounds| solve_quadratic_bezier_1d(p0.y as f32, ctrl.y as f32, p1.y as f32, bounds.point.y as f32 + bounds.size.y as f32)
    };

}

enum CurveIntersections {
    None,
    One([f32; 1]),
    Two([f32; 2])
}

fn solve_quadratic_bezier_1d(p0: f32, ctrl: f32, p1: f32, target: f32) -> CurveIntersections {

    // I hate math.

    let a = p0 - 2.0 * ctrl + p1;
    let b = 2.0 * (ctrl - p0);
    let c = p0 - target;

    let mut roots = CurveIntersections::None;

    if a.abs() < 1e-6 {

        if b.abs() > 1e-6 {
            let t = -c / b;
            if (0.0..=1.0).contains(&t) {
                roots = CurveIntersections::One([t])
            }
        }

    } else {

        let discriminant = b * b - 4.0 * a * c;

        if discriminant >= 0.0 {

            let sqrt_d = discriminant.sqrt();
            let t1 = (-b - sqrt_d) / (2.0 * a);
            let t2 = (-b + sqrt_d) / (2.0 * a);

            let t1_valid = (0.0..=1.0).contains(&t1);
            let t2_valid = (0.0..=1.0).contains(&t2);

            match (t1_valid, t2_valid) {
                (true, true) => {
                    // Make sure the smaller one is first.
                    match t1 <= t2 {
                        true  =>  roots = CurveIntersections::Two([t1, t2]),
                        false =>  roots = CurveIntersections::Two([t2, t1])
                    };
                }
                (true, false) => {
                    roots = CurveIntersections::One([t1]);
                }
                (false, true) => {
                    roots = CurveIntersections::One([t2]);
                }
                (false, false) => {}
            }
        }
    }

    roots

}

/// Returns `true` if `inner` is contained completely inside `outer`.
fn rect_inside_rect(inner: common::PhysicalRect, outer: common::PhysicalRect) -> bool {
    inner.point.x >= outer.point.x &&
    inner.point.y >= outer.point.y &&
    (inner.point.x + inner.size.x) <= (outer.point.x + outer.size.x) &&
    (inner.point.y + inner.size.y) <= (outer.point.y + outer.size.y)
}

/// Returns `true` if the rects are intersecting and `false` if `inner` is completely outside `outer`.
fn rect_intersects_rect(inner: common::PhysicalRect, outer: common::PhysicalRect) -> bool {
        inner.point.x                < outer.point.x + outer.size.x &&
        inner.point.y                < outer.point.y + outer.size.y &&
        inner.point.x + inner.size.x > outer.point.x                &&
        inner.point.y + inner.size.y > outer.point.y
}

// enum RectEdge {
//     Left,
//     Right,
//     Top,
//     Bottom,
// }

// fn rect_intersects_point_at(rect: &common::PhysicalRect, line: [common::MathPoint; 2]) -> Option<(RectEdge, common::MathPoint)> {
//     // TODO:                               ^^^^^^^^^^^^ impl something like "MathRect"

//     // From the line we get a origin and direction vector.
//     let origin = line[0];
//     let dir = common::MathPoint {
//         x: line[1].x - line[0].x,
//         y: line[1].y - line[0].y
//     };

//     // Convert the rect into the representation we need.
//     let rect0 = common::MathPoint::from(rect.point);
//     let rect1 = common::MathPoint {
//         x: (rect.point.x + rect.size.x) as f32,
//         y: (rect.point.y + rect.size.y) as f32
//     };

//     let tx1 = (rect0.x - origin.x) / dir.x;
//     let tx2 = (rect1.x - origin.x) / dir.x;
//     let ty1 = (rect0.y - origin.y) / dir.y;
//     let ty2 = (rect1.y - origin.y) / dir.y;

//     let tx_near = tx1.min(tx2);
//     let tx_far  = tx1.max(tx2);
//     let ty_near = ty1.min(ty2);
//     let ty_far  = ty1.max(ty2);

//     let t_near = tx_near.max(ty_near);
//     let t_far = tx_far.min(ty_far);

//     // If t_near is NaN (start point on boundary), we treat it as 0.0
//     let t_near = if t_near.is_nan() { 0.0 } else { t_near };

//     if t_near > t_far || t_far < 0.0 || t_near > 1.0 {
//         return None;
//     }

//     let hit_point = MathPoint {
//         x: origin.x + t_near * dir.x,
//         y: origin.y + t_near * dir.y,
//     };

//     let edge = if tx_near > ty_near {
//         if dir.x > 0.0 { Edge::Left } else { Edge::Right }
//     } else {
//         if dir.y > 0.0 { Edge::Top } else { Edge::Bottom }
//     };

//     Some((edge, hit_point))
// }

// pub struct Scrollable<W: Widget> {
//     pub inner: W,
//     pub rect: SmartMutex<common::PhysicalPoint>,
// }

// impl<W: Widget> Scrollable<W> {
//     pub fn new(inner: W) -> Self {
//         Self { inner, point: SmartMutex::new(common::PhysicalPoint::ZERO) }
//     }
// }

// impl<W: Widget> Widget for Scrollable<W> {
//     fn action(&self, layout: Layout, action: Action) {

//         let point = self.point.lock();

//         // layout.clip = true;

//         let clayout =

//     }
// }
