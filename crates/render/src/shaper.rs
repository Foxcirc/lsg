
//! CPU triangulation algorithms and handling of curves.
//!
//! The algorithms are purposely written in a way that is similar to the
//! compute shader implementation.

use std::{convert::identity, f32::consts::PI, iter::once};
use bv::BitVec;
use common::*;

/// Geometry that represents curved polygons as a list of points.
#[derive(Default)]
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
    /// x, y
    pub pos: [u16; 2],
    /// fillKind
    pub fill: FillKind,
    /// bitflags, which edges are outer edges
    pub edges: u8,
}

impl PartialVertex {
    pub const fn new(pos: [u16; 2], fill: FillKind, edges: u8) -> Self {
        Self { pos, fill, edges }
    }
}

/// Geometry that represents curved polygons after triangulation.
#[derive(Debug, Default)]
pub struct VertexGeometry {
    pub vertices: Vec<PartialVertex>,
    pub shapes: Vec<Shape>,
}

pub struct SingularData {
    pub vertices: gl::AttribVec,
}

pub struct InstancedData {
    pub vertices:  gl::AttribVec,
    pub instances: gl::AttribVec,
    pub commands:  Vec<gl::DrawArraysIndirectCommand>,
}

struct CurvesState<'a> {
    removed: &'a mut BitVec,
    out: &'a mut Vec<PartialVertex>,
}

struct TriangulationState<'a> {
    removed: &'a mut BitVec,
    ears: &'a mut BitVec,
    out: &'a mut Vec<PartialVertex>,
}

pub struct GeometryShaper {
    lower: LoweringPass,
    trig: TriangulationPass,
}

impl GeometryShaper {

    pub fn process<'s>(&'s mut self, geometry: &CurveGeometry) -> &'s VertexGeometry {

        let lowered = self.lower.process_geometry(geometry);
        let result = self.trig.process_geometry(lowered);

        result

    }

    pub fn new() -> Self {
        Self {
            lower: LoweringPass::new(),
            trig: TriangulationPass::new(),
        }
    }

}

/// This pass performs lowering of curve data so the later algorithms can be simpler.
///
/// More concretely this currently does following things:
/// - Convert all cubic into quadratic beziér curves
/// - Split up curve triangles that intersect with the shape
struct LoweringPass {
    output: CurveGeometry,
    errors: usize,
}
impl LoweringPass {

    fn new() -> Self {
        Self {
            output: CurveGeometry::default(),
            errors: 0,
        }
    }

    /// Returns a reference to the updated geometry.
    fn process_geometry<'s>(&'s mut self, geometry: &CurveGeometry) -> &'s CurveGeometry {

        self.output.clear();

        for shape in &geometry.shapes {
            match self.process_one_shape(shape, geometry) {
                Ok(..) => (),
                Err(..) => self.errors += 1,
            }
        }

        &self.output

    }

    fn process_one_shape(&mut self, shape: &Shape, geometry: &CurveGeometry) -> Result<(), ()> {

        let Some(points) = geometry.points.get(shape.range()) else { return Err(()) };

        if points.len() < 3 { return Err(()) }

        // save the start index so we later know which geometry belongs to the current shape
        let shape_start = self.output.points.len() as u16;

        let mut idx = 0;
        loop {

            let len = points.len();

            // get the point at idx and the four points "after" it in the polygon
            let section = [
                points[(idx + 0) % len],
                points[(idx + 1) % len],
                points[(idx + 2) % len],
                points[(idx + 3) % len],
            ];

            let [a, b, c, d] = section.map(|it| Point::from(it));

            let increment; // how many points to skip for the next iteration

            // 1: quadratic curve
            if section[0].kind() == PointKind::Base &&
               section[1].kind() == PointKind::Ctrl &&
               section[2].kind() == PointKind::Base {

                increment = 2;

                let intersected = points.iter().any(|it|
                    !section.contains(it) && TriangulationPass::triangle_intersects_point([a, b, c], Point::from(*it)) == IntersectionRelation::Inside
                );

                if intersected {

                    let [x, y] = TriangulationPass::split_quadratic([a, b, c], 0.5);
                    let [p, q] = TriangulationPass::split_quadratic(x, 0.5);
                    let [r, s] = TriangulationPass::split_quadratic(y, 0.5);

                    self.output.points.extend_from_slice(&[CurvePoint::convert(p[0], PointKind::Base), CurvePoint::convert(p[1], PointKind::Ctrl)]);
                    self.output.points.extend_from_slice(&[CurvePoint::convert(q[0], PointKind::Base), CurvePoint::convert(q[1], PointKind::Ctrl)]);
                    self.output.points.extend_from_slice(&[CurvePoint::convert(r[0], PointKind::Base), CurvePoint::convert(r[1], PointKind::Ctrl)]);
                    self.output.points.extend_from_slice(&[CurvePoint::convert(s[0], PointKind::Base), CurvePoint::convert(s[1], PointKind::Ctrl)]);

                } else {
                    self.output.points.extend_from_slice(&section[..2]);
                }

            // 2: cubic curve
            } else if section[0].kind() == PointKind::Base &&
                      section[1].kind() == PointKind::Ctrl &&
                      section[2].kind() == PointKind::Ctrl &&
                      section[3].kind() == PointKind::Base {

                // we split the cubic curve into pieces and
                // then degree reduce it to a quadratic curve

                increment = 3;

                let [x, y] = TriangulationPass::split_cubic([a, b, c, d], 0.5);
                let [p, q] = TriangulationPass::split_cubic(x, 0.5);
                let [r, s] = TriangulationPass::split_cubic(y, 0.5);

                for curve in [p, q, r, s] {

                    // degree reduce by averaging
                    let new_ctrl_point_x = -0.25*curve[0].x + 0.75*curve[1].x + 0.75*curve[2].x -0.25*curve[3].x;
                    let new_ctrl_point_y = -0.25*curve[0].y + 0.75*curve[1].y + 0.75*curve[2].y -0.25*curve[3].y;

                    self.output.points.extend_from_slice(&[
                        CurvePoint::convert(curve[0], PointKind::Base),
                        CurvePoint::new(new_ctrl_point_x as u16, new_ctrl_point_y as u16, PointKind::Ctrl)
                    ]);

                }

            // 3: invalid case
            } else if section[0].kind() == PointKind::Ctrl &&
                      section[1].kind() == PointKind::Ctrl &&
                      section[2].kind() == PointKind::Ctrl {

                // todo!("error: three contol points in a row");
                return Err(())


            // 4: "normal line" case
            } else {
                self.output.points.push(section[0]);
                increment = 1;
            }

            // TODO: what happens if a point is duplicated in the input? does it triangulate correctly? yes

            idx += increment;
            if idx >= len { break };

        }

        let shape_end = self.output.points.len() as u16;

        // generate the output shape, with updated indices
        self.output.shapes.push(Shape::new(shape_start..shape_end));

        Ok(())


    }

}

struct TriangulationPass {
    /// During ear-clipping, stores which vertices are valid ears.
    ears: BitVec<usize>,
    /// During ear-clipping, stores which vertices have been removed already.
    removed: BitVec<usize>,
    /// The output of the triangulation.
    result: VertexGeometry,
}

impl TriangulationPass {

    pub fn new() -> Self {
        Self {
            ears: BitVec::new(),
            removed: BitVec::new(),
            result: VertexGeometry::default(),
        }
    }

    pub fn process_geometry<'s>(&'s mut self, geometry: &CurveGeometry) -> &'s VertexGeometry {

        // reset the state
        self.result.vertices.clear();
        self.result.shapes.clear();

        // append the shape's triangles
        for shape in &geometry.shapes {

            // geometry is valid up to this index
            let start = self.result.vertices.len();

            // generate the mesh
            match self.process_one_shape(shape, geometry) {
                Ok(..) => {
                    let range = start as u16 .. self.result.vertices.len() as u16;
                    self.result.shapes.push(Shape::new(range));
                },
                // restore the valid geometry on error
                Err(..) => {
                    self.result.vertices.truncate(start);
                    self.result.shapes.push(Shape::ZERO);
                }
            }
        }

        &self.result

    }

    fn process_one_shape(&mut self, shape: &Shape, geometry: &CurveGeometry) -> Result<(), ()> {

        let points = geometry.points.get(shape.range())
            .unwrap_or_default();

        if points.len() < 3 {
            return Err(())
        }

        // reset our state
        self.ears.clear();
        self.removed.clear();
        self.ears.resize(points.len() as u64, false);
        self.removed.resize(points.len() as u64, false);

        self.triangulate_curves(points);
        self.triangulate_body(points)?;

        // if shape.kind() == ShapeKind::Instanced {
        //     for it in instances {
        //         self.instanced.instances.extend_f([
        //             it.pos[0], it.pos[1], it.pos[2], // offsetX, offsetY, z
        //             it.texture[0], it.texture[1], it.texture[2],
        //         ]);
        //         self.instanced.commands.push(gl::DrawArraysIndirectCommand::new(
        //             (self.instanced.vertices.inner.len() - start) / 5, // vertex count
        //             instances.len(), // instance count
        //             start / 5, // start index
        //         ))
        //     }
        // }

        Ok(())

    }

    /// Calculate triangles that should be rendered as curves.
    fn triangulate_curves(&mut self, points: &[CurvePoint]) {

        let len = points.len();
        debug_assert!(len >= 3);

        // make curve triangles out of quadratic curves (B-C-B)

        let mut idx = 0;
        loop {

            let section = [
                points[(idx + 0) % len],
                points[(idx + 1) % len],
                points[(idx + 2) % len],
            ];

            let abc = section.map(|it| Point::from(it));

            let increment; // how many points to skip for the next iteration

            // quadratic curve
            if section[0].kind() == PointKind::Base &&
               section[1].kind() == PointKind::Ctrl &&
               section[2].kind() == PointKind::Base {

                increment = 2;

                let convex = Self::convex(abc);
                let fill = match convex {
                    true => FillKind::Convex,
                    false => FillKind::Concave,
                };

                // mark all convex curved triangles as removed,
                // so they are not triangulated later
                self.removed.set((idx + 1) as u64, convex);
                //                 ^^^^^^ idx of the ctrl point

                Self::generate_triangle(abc, [false; 3], fill, &mut self.result.vertices);

            // simple line, which can be skipped
            } else {
                increment = 1; // TODO: check for three control points in a row, which would be invalid. right now just causes the control points to be treated as base points
            }

            idx += increment;
            if idx >= len { break };

        }

        // Ok(())

    }

    /// Ear-clipping triangulation for a single polygon.
    ///
    // TODO(DOC): link to docs on the triangulation compute shader on github
    fn triangulate_body(&mut self, points: &[CurvePoint]) -> Result<(), ()> {

        let len = points.len();
        debug_assert!(len >= 3);

        // calculate initial ear state for every point

        for idx in 0..len {
            let inices = Self::neighbours(&self.removed, idx);
            let ear = Self::ear(inices, points);
            self.ears.set(idx as u64, ear);
        }

        // remove ears and recalculate neighbours

        // eprintln!("points: {:?}", points);

        #[cfg(debug_assertions)]
        let mut debug_ears_generated = 0; // TODO: is this a decent debugging mechanism

        let mut changes = false; // used to check for errors
        let mut counter = 0;

        loop {

            if counter < len { // increment counter
                counter += 1;
            } else { // reset counter
                if !changes { return Err(()) }
                changes = false;
                counter = 1;
            }

            let idx = counter - 1;

            // skip all removed points
            if self.removed[idx as u64] {
                continue
            }

            if self.ears[idx as u64] {

                let [ia, ib, ic] = Self::neighbours(&self.removed, idx);
                let abc = [ia, ib, ic].map(|it| Point::from(points[it]));
                if ia == ic { // only two points were left
                    break
                };

                // we do not generate verticies for zero-area triangles
                if Self::triangle_area(abc).abs() > 0.0 { // TODO: can we make this more efficient and calc the area only once??

                    // We need to compute adjacency information for every edge of the triangle,
                    // since we only want to anti-alias edges which are actually on the outside of the shape.
                    let outers = Self::check_outer_edges([ia, ib, ic], points);

                    // Generate the filled inner triangle.
                    Self::generate_triangle(abc, outers, FillKind::Filled, &mut self.result.vertices);

                }

                // Mark the point as removed.
                self.removed.set(ib as u64, true);

                // Recalculate left neightbour.
                let indices = Self::neighbours(&self.removed, ia);
                let ear = Self::ear(indices, points);
                self.ears.set(ia as u64, ear);

                // Recalculate right neightbour.
                let indices = Self::neighbours(&self.removed, ic);
                let ear = Self::ear(indices, points);
                self.ears.set(ic as u64, ear);

                #[cfg(debug_assertions)] {
                    debug_ears_generated += 1;
                    if debug_ears_generated == unsafe { crate::SHAPE_TAKE_PART } {
                        break
                    }
                }

                changes = true;

            }

        }

        Ok(())

    }

    fn generate_triangle(points: [Point; 3], outers: [bool; 3], fill: FillKind, out: &mut Vec<PartialVertex>) {

        let flags = ((outers[0] as u8) << 2) |
                    ((outers[1] as u8) << 1) |
                    ((outers[2] as u8) << 0);

        for point in points {
            out.push(PartialVertex::new([point.x as u16, point.y as u16], fill, flags));
        }

    }

    /// Computes the direct neightbours of this index, wrapping around if neccessary.
    #[track_caller]
    pub(self) fn direct_neighbours(len: usize, idx: usize) -> [usize; 2] {
        if idx == 0 {
            [len - 1, idx + 1]
        } else if idx == len - 1 {
            [idx - 1, 0]
        } else {
            debug_assert!(idx <= len, "idx should be <= len");
            [idx - 1, idx + 1]
        }
    }

    /// Computes the neightbours of this index, wrapping around if neccessary and
    /// considering which other items have already been marked as removed.
    ///
    /// # Example
    /// Assuming a dataset of length `4` some example outputs of this function would be:
    // +-----+---------+-------------+
    // | idx | removed | neightbours |
    // +-----+---------+-------------+
    // |   1 | 0 0 0 0 | [0, 1, 2]   |
    // |   1 | 1 0 0 0 | [3, 1, 2]   |
    // |   0 | 0 0 0 0 | [3, 0, 1]   |
    // +-----+---------+-------------+
    //
    /// # Caution
    /// Infinitely loops if all indices are marked as removed.
    pub(self) fn neighbours(removed: &BitVec, idx: usize) -> [usize; 3] {
        // TODO: to optimize this: store a list of neightbours for every point (which is two u8 offsets, one for left on for the right neightbour) and update that list instead of "removing" points

        let len = removed.len(); // removed.len() == polygon.len()

        // #[cfg(debug_assertions)]
        // {
        //     let mut count = 0;
        //     for idx in 0..len { if removed[idx] { count += 1 } }
        //     assert!(len > 2, "`neighbours` called with < 3 elements");
        //     assert!(count <= len - 2, "`neighbtbours` called with < 2 elements alive (just {} out of {:?})", len - count, removed);
        // }

        let mut indices: [usize; 3] = [0; 3];

        // the point we are concerned about
        indices[1] = idx;

        // the right neighbour
        let mut counter: u64 = idx as u64 + 1;
        loop {
            if counter > len - 1 { counter = 0 } // wrap-around
            if !removed[counter] { break }
            counter += 1;
        }

        indices[2] = counter as usize;

        // the left neighbour
        let mut counter: isize = idx as isize - 1;
        loop {
            if counter < 0 { counter = len as isize - 1} // wrap-around
            if !removed[counter as u64] { break }
            counter -= 1;
        }

        indices[0] = counter as usize;

        indices

    }

    fn ear(indices: [usize; 3], polygon: &[CurvePoint]) -> bool {

        let abc = indices.map(|it| Point::from(polygon[it]));

        // A. short curcuit if the triangle has zero area.
        //
        // we want to treat these as valid ears because this allows
        // rendering seemingly disconnected areas as one shape
        // by connecting them using an invisible zero area line.
        let area = Self::triangle_area(abc);
        if area < 1e-6 { // account for precision errors
            return true
        }

        // B. short curcuit if it is concave.
        let convex = Self::convex(abc);
        if !convex {
            return false
        }

        // C. otherwise test for any intersections.
        for (pidx, point) in polygon.iter().enumerate() {

            if !indices.contains(&pidx) && // dont include the points that make up the ear
                Self::triangle_intersects_point(abc, Point::from(*point)) == IntersectionRelation::Inside
            {
                return false
            }

            /*

            let allowed = match Self::triangle_intersects_point([a, b, c], *point) {

                // if there are points lying within this ear, it is invalid
                IntersectionRelation::Inside => false,

                // if there are are points lying on the edge or a corner of this ear it is only
                // valid if their connecting edges also lie outside the ear. to illustrate this:
                //
                //              o                              o       P
                //            /   \                          /   \   .
                //      P   .  .  . X                      /       X  .  .  .  P
                //        /       .   \                  /           \
                //      /       .       \              /               \
                //    O - - - . - - - - - O          O - - - - - - - - - O
                //         P
                //      OOO INVALID EAR                  OOO VALID EAR
                //
                // the point X in the left triangle would make the ear invalid because the edges . . . . connecting
                // it to it's neightbour P are inside the ear. the same point X in the right triangle would be valid
                // because the edges . . . . are outside the ear.
                relation @ IntersectionRelation::OnEdge(..) |
                relation @ IntersectionRelation::OnCorner(..) => {

                    // touched edges
                    let edges = relation.edges();

                    // we use the direct neightbours here because we are interested in the
                    // edges this point is guaranteed to be "connected" to at the end.
                    // we don't care about the edges of the actual ears generated.
                    let nbs = Self::direct_neighbours(removed.len() as usize, pidx)
                        .map(|idx| polygon[idx]);

                    // - the vector to ANY neightbour is not allowed to move into the ear
                    // - the vector must must inside the ear in respect to ALL edges
                    // - moving to the LEFT of the edge means moving INSIDE the ear
                    let invalid = nbs.iter().any(|nb| edges.iter().all(|edge|
                        // Y-Inverted version (MovingLeft vs. MovingRight)
                        two_vector_relation(*edge, [*point, *nb]) == TwoVectorRelation::MovingLeft
                    ));

                    !invalid

                },

                IntersectionRelation::Outside => true,

            };

            if !allowed {
                return false
            }

            */

        };

        true

    }


    /// Check if the three points are convex, assuming counter clockwise orientation.
    /// Y-flipped version.
    fn convex(neighbours: [Point; 3]) -> bool {

        let [a, b, c] = neighbours;

        let ba = [a.x as i32 - b.x as i32, -(a.y as i32 - b.y as i32)];
        let bc = [c.x as i32 - b.x as i32, -(c.y as i32 - b.y as i32)];
        //                                     ^ this minus adjusts it for being y-flipped

        // calcualte the angle BA to BC

        let dot = (bc[0] * ba[0] + bc[1] * ba[1]) as f32;
        let det = (bc[0] * ba[1] - bc[1] * ba[0]) as f32;
        let signed = det.atan2(dot) * 180.0/PI;

        let angle = if signed.is_sign_negative() {
            360.0 + signed
        } else {
            signed
        };

        // check if any other vertex is inside the triangle ABC

        angle < 180.0

    }

    /// Area of the triangle ABC.
    /// Y-flipped version.
    fn triangle_area([a, b, c]: [Point; 3]) -> f32 {
        ((a.x as f32 * (c.y as f32 - b.y as f32) +
          b.x as f32 * (a.y as f32 - c.y as f32) +
          c.x as f32 * (b.y as f32 - a.y as f32)) / 2.0).abs()
    //                  ^^^ the subtracion is fipped to account for the y-flip
    }

    /// If `point` lies within the triangle `trig`.
    /// Y-flipped version.
    ///
    /// Considers points that lie exactly on an edge as outside.
    // TODO: I am keeping this function around until further testing is done since special handling
    // for cases where the point lies on an EDGE/CORNER may be needed for rare edge cases,
    // but in general I wanna revert it back to returning a simple boolean
    fn triangle_intersects_point([a, b, c]: [Point; 3], point: Point) -> IntersectionRelation {

        let abc = Self::triangle_area([a, b, c]);

        let pab = Self::triangle_area([point, a, b]);
        let pbc = Self::triangle_area([point, b, c]);
        let pca = Self::triangle_area([point, c, a]);

        let total = pab + pbc + pca;

        // small epsilon, to account for precision errors
        const EPS: f32 = 1e-6;

        if (total - abc).abs() < EPS {
            match (pab < EPS, pbc < EPS, pca < EPS) {
                // point is inside
                (false, false, false) => return IntersectionRelation::Inside,
                // point lies on one edge
                (true, false, false) => return IntersectionRelation::OnEdge([[a, b]]),
                (false, true, false) => return IntersectionRelation::OnEdge([[b, c]]),
                (false, false, true) => return IntersectionRelation::OnEdge([[c, a]]),
                // point lies on two edges (= on a corner)
                (true, true, false) => return IntersectionRelation::OnCorner([[a, b], [b, c]]), // corner B
                (false, true, true) => return IntersectionRelation::OnCorner([[b, c], [c, a]]), // corner C
                (true, false, true) => return IntersectionRelation::OnCorner([[c, a], [a, b]]), // corner A
                // deformed triangle
                (true, true, true) => return IntersectionRelation::Outside,
            }
        } else {
            IntersectionRelation::Outside
        }

        // (total - abc).abs() < EPS // && // general area check
        // pab > EPS && pbc > EPS && pca > EPS // points on an edge should be considered outside

    }

    fn lerp(p1: Point, p2: Point, t: f32) -> Point {
        Point::new(
            p1.x as f32 + (p2.x as f32 - p1.x as f32) * t,
            p1.y as f32 + (p2.y as f32 - p1.y as f32) * t
        )
    }

    // Function to split a quadratic bézier curve at t
    fn split_quadratic([a, b, c]: [Point; 3], t: f32) -> [[Point; 3]; 2] {
        let q1 = Self::lerp(a, b, t);
        let q2 = Self::lerp(b, c, t);
        let r0 = Self::lerp(q1, q2, t);
        [[a, q1, r0], [r0, q2, c]]
        //  curve1       curve2
    }

    fn split_cubic([a, b, c, d]: [Point; 4], t: f32) -> [[Point; 4]; 2] {
        let p1  = Self::lerp(a, b, t);
        let p2  = Self::lerp(b, c, t);
        let p3  = Self::lerp(c, d, t);
        let p12 = Self::lerp(p1, p2, t);
        let p23 = Self::lerp(p2, p3, t);
        let p   = Self::lerp(p12, p23, t);
        [[a, p1, p12, p], [p, p23, p3, d]]
        // -- curve1 --    -- curve2  --
    }

    fn bezier_point([a, b, c]: [Point; 3], t: f32) -> Point {
        let x = (1.0 - t) * (1.0 - t) * a.x + 2.0 * (1.0 - t) * t * b.x + t * t * c.x;
        let y = (1.0 - t) * (1.0 - t) * a.y + 2.0 * (1.0 - t) * t * b.y + t * t * c.y;
        Point { x, y }
    }

    fn favoured_split_point(curve: [Point; 3], p: Point) -> f32 {

        #![allow(non_snake_case)]

        // coefficients of implicit line equation
        // implicit form: A*x + B*y + C = 0
        let A = p.y - curve[0].y;
        let B = curve[0].x - p.x;
        let C = curve[0].y * p.x - curve[0].x * p.y;

        // coefficients for the line-curve intersection quadratic eqation
        // implicit form: A*x(t) + B*y(t) + C = 0
        // final equation: a*t² + b*t + c = 0
        let a = A * (curve[2].x - 2.0*curve[1].x + curve[0].x) + B * (curve[2].y - 2.0*curve[1].y + curve[0].y);
        let b = A * (2.0*curve[1].x - 2.0*curve[0].x) + B * (2.0*curve[1].y - 2.0*curve[0].y);
        let c = A * curve[0].x + B * curve[0].y + C;

        // find the roots

        let inner = (b.powi(2) - 4.0*a*c).sqrt();
        debug_assert!(!inner.is_nan(), "closest point must not be on the end of the curve"); // TODO: remove and clamp to 0

        let root1 = (-b + inner) / (2.0*a);
        let _root2 = (-b - inner) / (2.0*a); // TODO: (?) check the distance and choose the closest root like in closest_split_point

        root1

    }

    fn closest_split_point(curve: [Point; 3], p: Point) -> f32 {

        // get coefficients for this curve
        let [a, b, c, d] = Self::distance_derivative_coefficients(curve, p);

        // solve the cubic equation for t values
        let roots = Self::solve_distance_derivative(a, b, c, d);

        // NOTE: for our case, where the point lies inside the curves "belly", there will
        //       be only one root and the closest point will not be at the ends

        // this would be for the general case

        let iter = roots.into_iter()
            .filter_map(identity)
            .chain(once(0.0))
            .chain(once(1.0));

        // find the root that gives the minimum distance
        let mut closest_t = 0.0;
        let mut closest_dist = f32::MAX;

        for t in iter {

            // only consider t in the valid range [0, 1]
            if t >= 0.0 && t <= 1.0 {
                let curve_point = Self::bezier_point(curve, t);
                let dist = (curve_point.x - p.x).powi(2) + (curve_point.y - p.y).powi(2);
                if dist < closest_dist {
                    closest_dist = dist;
                    closest_t = t;
                }
            }
        }

        closest_t



    }

    fn distance_derivative_coefficients([a, b, c]: [Point; 3], p: Point) -> [f32; 4] {

        #![allow(non_snake_case)]

        // source: https://www.primescholars.com/articles/an-algorithm-for-computing-the-shortest-distance-between-a-point-and-quadratic-bezier-curve.pdf
        // D'(x) = 4At³ * 12Bt² * 4Ct * 4D
        // where A, B, C, D are d4, ... d1 respectively

        let A = (a.x - 2.0*b.x + c.x).powi(2)       + (a.y - 2.0*b.y + c.y).powi(2);
        let B = (a.x - 2.0*b.x + c.x) * (b.x - a.x) + (a.y - 2.0*b.y + c.y) * (b.y - a.y);
        let C = (a.x - 2.0*b.x + c.x) * (a.x - p.x) + (a.y - 2.0*b.y + c.y) * (a.y - p.y)      + 2.0*(b.x - a.x).powi(2) + 2.0*(b.y - a.y).powi(2);
        let D = (b.x - a.x) * (a.x - p.x)           + (b.y - a.y) * (a.y - p.y);

        [4.0*A, 12.0*B, 4.0*C, 4.0*D]

    }

    /// sloves a cubic equation
    fn solve_distance_derivative(a: f32, b: f32, c: f32, d: f32) -> [Option<f32>; 3] {

        #![allow(non_snake_case)]

        // normalize coefficients, so at³ => t³
        let p = b / a;
        let q = c / a;
        let r = d / a;

        // convert to a depressed cubic u^3 + A*u + B = 0
        let A = (3.0 * q - p * p) / 9.0;
        let B = (9.0 * p * q - 27.0 * r - 2.0 * p * p * p) / 54.0;

        // compute discriminant
        let delta = A.powi(3) + B.powi(2);

        if delta > 0.0 {

            // one real root, two complex roots
            let sqrt_delta = delta.sqrt();
            let C = (B + sqrt_delta).cbrt();
            let D = (B - sqrt_delta).cbrt();
            let u1 = C + D;

            // shift u back to t
            let t1 = u1 - p / 3.0;

            [Some(t1), None, None]

        } else if delta == 0.0 {

            // triple real root case
            let C = B.cbrt();
            let u1 = 2.0 * C;
            let u2 = -C;

            let t1 = u1 - p / 3.0;
            let t2 = u2 - p / 3.0;

            [Some(t1), Some(t2), None]

        } else {

            // three real roots, trigonometric solution
            let theta = (B / (-A * A * A).sqrt()).acos();
            let val = 2.0 * (-A).sqrt();

            let u1 = val *  (theta / 3.0).cos();
            let u2 = val * ((theta + 2.0 * PI) / 3.0).cos();
            let u3 = val * ((theta + 4.0 * PI) / 3.0).cos();

            let t1 = u1 - p / 3.0;
            let t2 = u2 - p / 3.0;
            let t3 = u3 - p / 3.0;

            [Some(t1), Some(t2), Some(t3)]

        }

    }

    fn check_outer_edges([ia, ib, ic]: [usize; 3], points: &[CurvePoint]) -> [bool; 3] {
        let mut result = [false; 3];
        let edges = [[ia, ib], [ib, ic], [ic, ia]];
        for (idx, [x, y]) in edges.iter().enumerate() {
            // check if the points are direct neightbours
            let diff = x.abs_diff(*y);
            if (diff == 1 || diff == points.len() - 1) &&
               points[*x].kind() != PointKind::Ctrl &&
               points[*y].kind() != PointKind::Ctrl {
                // this is an outside edge
                result[idx] = true;
            }
        }
        result
    }

}

/// Gives information about the way the vector `rhs` points in relation to `lhs`.
/// # Example
/// - - - - - - - - - - > lhs
///     ⟍
///       ⟍
///         ⟍
///           ↘ rhs
/// Here the `rhs` has the `MovingRight` relation to `lhs`.
fn two_vector_relation(lhs: [CurvePoint; 2], rhs: [CurvePoint; 2]) -> TwoVectorRelation {

    // compute the normal vector of the edge we are touching. this will
    // be the LEFT normal vector since we are using the [-y, x] form.
    let normal = [-(lhs[1].y() as f32 - lhs[0].y() as f32), (lhs[1].x() as f32 - lhs[0].x() as f32)];

    let dot = ((rhs[1].x() as f32 - rhs[0].x() as f32) * normal[0] as f32) +
              ((rhs[1].y() as f32 - rhs[0].y() as f32) * normal[1] as f32);

    // dot product rules:
    // dot == 0 → parallel
    // dot <  0 → sharp angle
    // dot >  0 → wide angle

    const EPS: f32 = 1e-6;

    if dot < EPS && dot > -EPS {
        TwoVectorRelation::Parallel
    } else if dot < EPS {
        TwoVectorRelation::MovingLeft
    } else {
        TwoVectorRelation::MovingRight
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TwoVectorRelation {
    MovingLeft,
    MovingRight,
    Parallel,
}

#[test]
fn neighbours() {

    let mut bits = BitVec::<usize>::new();
    bits.resize(10, false);

    assert_eq!(TriangulationPass::neighbours(&bits, 4), [3, 4, 5]);
    assert_eq!(TriangulationPass::neighbours(&bits, 0), [9, 0, 1]);
    assert_eq!(TriangulationPass::neighbours(&bits, 9), [8, 9, 0]);

}
