
use std::mem::swap;

use common::SmartMutex;
use crate::*;

pub struct Placement<W: Widget> {
    pub inner: W,
    pub rect: SmartMutex<common::MeasuredRect>,
}

impl<W: Widget> Placement<W> {
    pub fn new(inner: W, rect: common::MeasuredRect) -> Self {
        Self {
            inner,
            rect: SmartMutex::new(rect)
        }
    }
}

impl<W: Widget> Widget for Placement<W> {
    fn action(&self, cx: Context) -> Response {
        let rect = *self.rect.lock();
        cx.child(rect, &self.inner)
    }
}

pub struct Offset<W: Widget> {
    pub inner: W,
    pub rect: SmartMutex<common::PhysicalRect>,
}

impl<W: Widget> Offset<W> {
    pub fn new(inner: W, rect: common::PhysicalRect) -> Self {
        Self {
            inner,
            rect: SmartMutex::new(rect)
        }
    }
}

impl<W: Widget> Widget for Offset<W> {
    fn action(&self, cx: Context) -> Response {
        let rect = *self.rect.lock();
        let mut newbounds = cx.layout;
        newbounds.point.x += rect.point.x;
        newbounds.point.y += rect.point.y;
        newbounds.size.x += rect.size.x;
        newbounds.size.y += rect.size.y;
        cx.child(newbounds.into(), &self.inner)
    }
}

pub struct Many<W: Widget> {
    pub inner: SmartMutex<Vec<W>>,
}

impl<W: Widget> Many<W> {
    pub fn new() -> Self {
        Self { inner: SmartMutex::new(Vec::new()) }
    }
    pub fn add(&self, w: W) {
        self.inner.lock().push(w);
    }
}

impl<W: Widget> Widget for Many<W> {
    fn action(&self, cx: Context) -> Response {
        let mut resp = Response::Bubble;
        for entry in &*self.inner.lock() {
            let iresp = entry.action(cx);
            resp.and(iresp);
        }
        resp
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
    fn action(&self, cx: Context) -> Response {
        let mut resp = Response::Bubble;
        let mut offset: i16 = 0;
        resp.and(implcol(cx, &*self.w1.lock(), &mut offset));
        resp.and(implcol(cx, &*self.w2.lock(), &mut offset));
        resp
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
    fn action(&self, cx: Context) -> Response {
        let mut resp = Response::Bubble;
        let mut offset: i16 = 0;
        resp.and(implcol(cx, &*self.w1.lock(), &mut offset));
        resp.and(implcol(cx, &*self.w2.lock(), &mut offset));
        resp.and(implcol(cx, &*self.w3.lock(), &mut offset));
        resp
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
    fn action(&self, cx: Context) -> Response {

        let mut resp = Response::Bubble;
        let mut offset: i16 = 0;

        for slot in &*self.inner.lock() {
            let iresp = implcol(cx, slot, &mut offset);
            resp.and(iresp);
        }

        resp

    }
}

fn implcol<W: Widget>(cx: Context, slot: &(common::MeasuredNumber, W), offset: &mut i16) -> Response {

    let (cwidth, cwidget) = slot;

    let crect = cx.transform(common::MeasuredRect {
        point: common::MeasuredPoint::new(common::abs(*offset), common::abs(0)),
        size:  common::MeasuredSize::new(*cwidth, common::abs(cx.height()))
    });


    *offset += crect.size.x;

    cx.child(crect.into(), cwidget)

}

pub struct Rows<W: Widget> {
    pub inner: SmartMutex<Vec<(common::MeasuredNumber, W)>>
}

/// Clips its children to avoid their geometry escaping bounds.
///
/// A clipping check is run on all child geometry and instances are
/// adjusted so that their geometry stops at the layout bounds.
///
/// Only needed if the child will purposefully draw out of bounds.
pub struct Clip<W: Widget> {
    pub inner: W,
    bufs: SmartMutex<ClipBufs>,
}

impl<W: Widget> Clip<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner,
            bufs: SmartMutex::default(),
        }
    }
}

impl<W: Widget> Widget for Clip<W> {
    fn action(&self, cx: Context) -> Response {

        if let Action::Render { out: locked } = cx.action {

            let start = locked.inner.lock()
                .instances.len();

            // Let the child render.
            let resp = self.inner.action(cx);

            let RenderOutputInner { geometry, instances, .. } =
                &mut *locked.inner.lock();

            // Iterate over the instances added by the child.
            for instance in &mut instances[start..] {

                // We do bounding box checks on the instances:
                //
                // The checks are based on the fact, that an instance
                // shall always be smaller then its bounding box.

                let rect = common::PhysicalRect {
                    point: instance.pos,
                    size: instance.size
                };

                // COMPLETELY INSIDE => KEEP
                if rect_inside_rect(rect, cx.layout) {
                    // Do nothing.
                }

                // COMPLETELY OUTSIDE => DISCARD
                else if !rect_intersects_rect(rect, cx.layout) {
                    *instance = common::Instance::DISCARD;
                }

                // INTERSECTING => CLIP
                else {

                    if instance.target.geometry == 0 {
                        // `0` means index into the default geometry

                        let bufs = &mut *self.bufs.lock();
                        let shape = geometry.get(instance.target.shape);

                        clip_triangle_shape_to_rect(bufs, shape, cx.layout);

                        // Finally, redirect the instance to our new shape.
                        let idx = geometry.add(&bufs.newshape);
                        instance.target.shape = idx;

                    } else {
                        // otherwise, index into another geometry
                        todo!();
                    }

                }

            }

            resp

        } else {
            self.inner.action(cx)
        }

    }
}

/// Clip the triangle shape to the bounds and write the result into the provided bufs.
///
/// Will clear the bufs and overwrite its contents.
fn clip_triangle_shape_to_rect(
    ClipBufs { buf0, buf1, newshape }: &mut ClipBufs,
    shape: &[common::PartialVertex],
    bounds: common::PhysicalRect
) {

    buf0.clear();
    buf1.clear();
    newshape.clear();

    let (triangles, ..) = shape.as_chunks::<3>();
    for triangle in triangles {

        // Initialize buf0.
        buf0.extend_from_slice(triangle);

        const EDGES: [EdgeEquation; 4] = [
            EdgeEquation::Left,
            EdgeEquation::Right,
            EdgeEquation::Bottom,
            EdgeEquation::Top
        ];

        for edge in EDGES {

            let (subtriangles, ..) = buf0
                .as_chunks::<3>();

            // Clip arr0 into arr1.
            for subtriangle @ [a, b, c] in subtriangles {

                let isinside = subtriangle
                    .map(|it| edge.inside(it.pos, bounds));

                match subtriangle[0].curve {

                    common::FillKind::Filled => {

                        match isinside {

                            // Keep as-is.
                            [true, true, true] => buf1.extend(*subtriangle),

                            // Do nothing.
                            [false, false, false] => (),

                            // One inside cases:

                            [true,  false, false] => buf1.extend(gen_triangle_one_inside(*a, [*b, *c], edge, bounds)),
                            [false, true,  false] => buf1.extend(gen_triangle_one_inside(*b, [*a, *c], edge, bounds)),
                            [false, false, true]  => buf1.extend(gen_triangle_one_inside(*c, [*a, *b], edge, bounds)),

                            // Two inside cases:

                            [false, true,  true]  => buf1.extend(gen_triangles_two_inside([*b, *c], *a, edge, bounds)),
                            [true,  false, true]  => buf1.extend(gen_triangles_two_inside([*a, *c], *b, edge, bounds)),
                            [true,  true,  false] => buf1.extend(gen_triangles_two_inside([*a, *b], *c, edge, bounds))
                        }

                    },

                    _ => todo!()

                }

            }

            swap(buf0, buf1);
            buf1.clear();

        }

        // Copy over the new triangles to
        // the shape we are constructing...
        newshape.extend_from_slice(buf0);

    }
}

fn gen_triangle_one_inside(inp: common::PartialVertex, [out0, out1]: [common::PartialVertex; 2], edge: EdgeEquation, bs: common::PhysicalRect) -> [common::PartialVertex; 3] {
    let edge0 = [inp.pos, out0.pos];
    let edge1 = [inp.pos, out1.pos];
    let t0 = edge.iline(edge0, bs);
    let t1 = edge.iline(edge1, bs);
    let i0 = interpolated_point_on_line(edge0, t0);
    let i1 = interpolated_point_on_line(edge1, t1);
    // TODO: pass along outside edge info correctly...
    [common::PartialVertex::new(inp.pos, common::FillKind::Filled, 0),
     common::PartialVertex::new(i0,      common::FillKind::Filled, 0),
     common::PartialVertex::new(i1,      common::FillKind::Filled, 0)]
}

fn gen_triangles_two_inside([in0, in1]: [common::PartialVertex; 2], outp: common::PartialVertex, edge: EdgeEquation, bs: common::PhysicalRect) -> [common::PartialVertex; 6] {
    let edge0 = [in0.pos, outp.pos];
    let edge1 = [in1.pos, outp.pos];
    let t0 = edge.iline(edge0, bs);
    let t1 = edge.iline(edge1, bs);
    let i0 = interpolated_point_on_line(edge0, t0);
    let i1 = interpolated_point_on_line(edge1, t1);
    [common::PartialVertex::new(in0.pos, common::FillKind::Filled, 0),
     common::PartialVertex::new(i0,      common::FillKind::Filled, 0),
     common::PartialVertex::new(in1.pos, common::FillKind::Filled, 0),
     common::PartialVertex::new(i0,      common::FillKind::Filled, 0),
     common::PartialVertex::new(i1,      common::FillKind::Filled, 0),
     common::PartialVertex::new(in1.pos, common::FillKind::Filled, 0)]
}

#[derive(Default)]
struct ClipBufs {
    buf0: Vec<common::PartialVertex>,
    buf1: Vec<common::PartialVertex>,
    newshape: Vec<common::PartialVertex>
}

// enum AddPoints {
//     None,
//     One([common::PhysicalPoint; 1]),
//     Two([common::PhysicalPoint; 2])
// }

#[derive(Clone, Copy)]
enum EdgeEquation {
    Left,
    Right,
    Bottom,
    Top
}

impl EdgeEquation {

    pub fn inside(self, point: common::PhysicalPoint, bs: common::PhysicalRect) -> bool {
        match self {
            Self::Left   => point.x >= bs.xmin(),
            Self::Right  => point.x <= bs.xmax(),
            Self::Bottom => point.y >= bs.ymin(),
            Self::Top    => point.y <= bs.ymax()
        }
    }
    pub fn iline(self, [p0, p1]: [common::PhysicalPoint; 2], bs: common::PhysicalRect) -> f32 {
        match self {
            Self::Left   => (bs.xmin() - p0.x) as f32 / (p1.x - p0.x) as f32,
            Self::Right  => (bs.xmax() - p0.x) as f32 / (p1.x - p0.x) as f32,
            Self::Bottom => (bs.ymin() - p0.y) as f32 / (p1.y - p0.y) as f32,
            Self::Top    => (bs.ymax() - p0.y) as f32 / (p1.y - p0.y) as f32,
        }
    }

    pub fn icurve(self, [p0, ctrl, p1]: [common::PhysicalPoint; 3], bs: common::PhysicalRect) -> CurveIntersections {
        match self {
            Self::Left   => solve_quadratic_bezier_1d(p0.x as f32, ctrl.x as f32, p1.x as f32, bs.xmin() as f32),
            Self::Right  => solve_quadratic_bezier_1d(p0.x as f32, ctrl.x as f32, p1.x as f32, bs.xmax() as f32),
            Self::Bottom => solve_quadratic_bezier_1d(p0.y as f32, ctrl.y as f32, p1.y as f32, bs.ymin() as f32),
            Self::Top    => solve_quadratic_bezier_1d(p0.y as f32, ctrl.y as f32, p1.y as f32, bs.ymax() as f32)
        }
    }

}

fn interpolated_point_on_line([p0, p1]: [common::PhysicalPoint; 2], t: f32) -> common::PhysicalPoint {
    common::PhysicalPoint {
        x: (p0.x as f32 + t * (p1.x - p0.x) as f32) as i16,
        y: (p0.y as f32 + t * (p1.y - p0.y) as f32) as i16
    }
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

pub struct Scrollable<W: Widget> {
    pub inner: Offset<W>,
}

impl<W: Widget> Scrollable<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner: Offset::new(inner, common::PhysicalRect::ZERO),
        }
    }
}

impl<W: Widget> Widget for Scrollable<W> {
    fn action(&self, cx: Context) -> Response {

        if let Action::MouseScroll { delta, .. } = cx.action {

            let iresp = self.inner.action(cx);

            if let Response::Bubble = iresp {
                // Adjust the offset.
                self.inner.rect.with(|it| {
                    it.point.x -= delta.x / 200;
                    it.point.y += delta.y / 200;
                });
                // Redraw, to make our changes visible.
                cx.caps.redraw();
            }

            Response::Handeled

        } else {
            self.inner.action(cx)
        }

    }
}

/*

ARCHIVE: This code was used to clip a shape defined by &[CurvePoint] into a clipping rect.

NOTE: Writing it made me realize that it is a better approach to simplify the final shape
      geometry as much as possible so that algorithms that operate on the geometry can
      be as efficient as possible. As such, geometry is now triangulated individually before
      being added to the `Space`, since triangulation converts a *deptendent* list of points
      into an *indipendent* list of triangles.

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


 */
