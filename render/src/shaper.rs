
//! CPU triangulation algorithms and handling of curves.
//!
//! The algorithms are purposely written in a way that is similar to the
//! compute shader implementation.

use std::{convert::identity, f32::consts::PI, iter::once};
use bv::BitVec;
use common::*;

// TODO: Idea for interop with this an my gl library: Add a struct that is a kind of of "description" of the vertex data layout
// used by an algorithm. Basically Transformer provides this kind of description which the renderer then uses to setup its buffers in a simple way
// this way layout difference bugs would be prevented
// is this a thing that could be awesome?

/// Output after processing a polygon.
pub struct OutputGeometry<'a> {
    pub singular: SingularOutputGeometry<'a>,
    pub instanced: InstancedOutputGeometry<'a>,
    /// contains error messages for shapes that were invalid.
    /// invalid shapes are discarded
    pub errors: usize,
}

impl<'a> OutputGeometry<'a> {
    pub fn check(&self) -> Result<(), String> {
        if self.errors == 1 {
            Err(format!("{} invalid shape", self.errors))
        } else if self.errors > 1 {
            Err(format!("{} invalid shapes", self.errors))
        } else {
            Ok(())
        }
    }
}

pub struct SingularOutputGeometry<'a> {
    pub vertices: &'a [f32],
}

pub struct InstancedOutputGeometry<'a> {
    pub vertices:  &'a [f32],
    pub instances: &'a [f32],
    pub commands:  &'a [gl::DrawArraysIndirectCommand],
}

pub struct SingularData {
    pub vertices: Vec<f32>,
}

pub struct InstancedData {
    pub vertices:  Vec<f32>,
    pub instances: Vec<f32>,
    pub commands:  Vec<gl::DrawArraysIndirectCommand>,
}

#[derive(Clone, Copy)]
struct ShapeMetadata<'a> {
    pub kind: ShapeKind,
    /// only needed for singular shapes here
    pub instance: &'a Instance,
    /// window size
    pub size: Size,
}

struct CurvesState<'a> {
    removed: &'a mut BitVec,
    out: &'a mut Vec<f32>,
}

struct TriangulationState<'a> {
    removed: &'a mut BitVec,
    ears: &'a mut BitVec,
    out: &'a mut Vec<f32>,
}

pub struct GeometryShaper {
    lower: LoweringPass,
    trig: TriangulationPass,
}

impl GeometryShaper {

    pub fn process<'s>(&'s mut self, geometry: &CurveGeometry, size: Size) -> OutputGeometry<'s> {

        let lowered = self.lower.process_geometry(geometry);
        let result = self.trig.process_geometry(lowered, size);

        result

    }

    pub(crate) fn new() -> Self {
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

        let range = shape.polygon_range();
        let points = geometry.points.get(range.start as usize .. range.end as usize)
            .unwrap_or_default(); // TODO: soft fail or hard error here?

        if points.len() < 3 { return Err(()) }

        let range = shape.instances_range();
        let instances = geometry.instances.get(range.start as usize .. range.end as usize)
            .unwrap_or_default();

        // we just hand the instances through, as we only need to modify the actual shapes
        self.output.instances.extend_from_slice(instances);

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

                // append the points of the curve, which can be considered
                // a "curve triangle" in this case
                self.output.points.extend(section);

                // if points are intersecting the curve triangle, we have
                // to split up the triangle to avoid artifacts

                // the triangle and then its sub-triangles are tested
                // recursively if they are still intersecting.
                // this is done in a somewhat cumbersome way to make it
                // possible to preallocate output space on the gpu

                let mut max_depth = 1; // number of curve triangles that were generated
                let mut was_split = true;
                while was_split {

                    was_split = false;
                    let len = self.output.points.len();

                    // iterate through the already generated triangles
                    for current_depth in 0..max_depth {

                        let [isa, isb, isc] = [
                            len - max_depth * 3 + current_depth * 3,     // 1. point
                            len - max_depth * 3 + current_depth * 3 + 1, // 2. point
                            len - max_depth * 3 + current_depth * 3 + 2  // 3. point
                        ];

                        let [sa, sb, sc] = [self.output.points[isa], self.output.points[isb], self.output.points[isc]];

                        // check for any intersections
                        for point in points.iter() {
                            if ![sa, sb, sc].contains(point) &&
                                TriangulationPass::triangle_intersects_point([sa, sb, sc], *point) {

                                was_split = true;
                                max_depth += 1;

                                // split the curve triangle
                                let split_at = TriangulationPass::closest_split_point([sa.into(), sb.into(), sc.into()], (*point).into());
                                let [first, second] = TriangulationPass::split_quadratic([sa.into(), sb.into(), sc.into()], split_at); // TODO: we need to split "right before" test how small EPS can get, rn sadly not much smaller then 0.1 :/ which is unacceptable for font rendering

                                // insert the first curve triangle by replacing the original sub triangle
                                self.output.points[isa] = CurvePoint::base(first[0].x as i16, first[0].y as i16);
                                self.output.points[isb] = CurvePoint::ctrl(first[1].x as i16, first[1].y as i16);
                                self.output.points[isc] = CurvePoint::base(first[2].x as i16, first[2].y as i16);

                                // insert the second curve triangle at the end
                                self.output.points.push(CurvePoint::base(second[0].x as i16, second[0].y as i16));
                                self.output.points.push(CurvePoint::ctrl(second[1].x as i16, second[1].y as i16));
                                self.output.points.push(CurvePoint::base(second[2].x as i16, second[2].y as i16));

                            }
                        }

                    }

                }

                self.output.points.pop(); // pop the last point since it will be addad again next iteration

            // 2: cubic curve
            } else if section[0].kind() == PointKind::Base &&
                      section[0].kind() == PointKind::Ctrl &&
                      section[0].kind() == PointKind::Ctrl &&
                      section[0].kind() == PointKind::Base {

                // we split the cubic curve into pieces and
                // then degree reduce it to a quadratic curve

                increment = 3;

                let critical_points = [0.25, 0.5, 0.75, 1.0];
                // let critical_points: Vec<f64> = (0..6).into_iter().rev().map(|it| 1.0 / it as f64).collect();
                let mut previous_t_value = 0.0;
                let mut curve_to_split = [a.into(), b.into(), c.into(), d.into()];

                // each iteration, we "chop off" the segment up to t_value
                for t_value in critical_points {

                    // calculate where we have to split the already shrunken curve
                    let normalized_t_value = (t_value - previous_t_value) / (1.0 - previous_t_value);

                    let [curve1, curve2] = TriangulationPass::split_cubic(curve_to_split, normalized_t_value as f32);

                    // degree reduce by averaging
                    let new_ctrl_point_x = -0.25*curve1[0].x + 0.75*curve1[1].x + 0.75*curve1[2].x -0.25*curve1[3].x;
                    let new_ctrl_point_y = -0.25*curve1[0].y + 0.75*curve1[1].y + 0.75*curve1[2].y -0.25*curve1[3].y;

                    // push p1 and p2. the last point will be pushed on in the next iteration
                    self.output.points.push(CurvePoint::base(curve1[0].x as i16, curve1[0].y as i16)); // TODO: this cast is soooooo dirty, f32 to i16? realy???
                    self.output.points.push(CurvePoint::ctrl(new_ctrl_point_x as i16, new_ctrl_point_y as i16));

                    curve_to_split = curve2;
                    previous_t_value = t_value;

                }

            // 3: invalid case
            } else if section[0].kind() == PointKind::Ctrl &&
                      section[0].kind() == PointKind::Ctrl &&
                      section[0].kind() == PointKind::Ctrl {

                todo!("error: three contol points in a row");


            // 4: "normal line" case
            } else {
                self.output.points.push(section[0]);
                increment = 1;
            }

            // TODO: what happens if a point is duplicated in the input? does it triangulate correctly?

            idx += increment;
            if idx >= len { break };

        }

        let shape_end = self.output.points.len() as u16;

        // generate the output shape, with updated indices
        match shape.kind() == ShapeKind::Singular {
            true  => self.output.shapes.push(Shape::singular(shape_start..shape_end, shape.instances_range().start)),
            false => self.output.shapes.push(Shape::instanced(shape_start..shape_end, shape.instances_range())),
            //                                                                             ^^^^ just hand the instances indices through
        }

        Ok(())


    }

}

struct TriangulationPass {
    // state during triangulation
    ears: BitVec<usize>,
    removed: BitVec<usize>,
    // outputs
    singular: SingularData,
    instanced: InstancedData,
    errors: usize,
}

impl TriangulationPass {

    pub fn new() -> Self {
        Self {
            ears: BitVec::new(),
            removed: BitVec::new(),
            singular: SingularData { vertices: Vec::new() },
            instanced: InstancedData { vertices: Vec::new(), instances: Vec::new(), commands: Vec::new() },
            errors: 0,
        }
    }

    pub fn process_geometry<'s>(&'s mut self, geometry: &CurveGeometry, size: Size) -> OutputGeometry<'s> {

        // reset the state
        self.singular.vertices.clear();
        self.instanced.vertices.clear();
        self.instanced.instances.clear();
        self.instanced.commands.clear();
        self.errors = 0;

        // append the shape's triangles
        for shape in &geometry.shapes {

            // geometry is valid up to this index
            let start = [
                self.singular.vertices.len(),
                self.instanced.vertices.len(),
            ];

            // generate the mesh now
            match self.process_one_shape(shape, geometry, size) {
                Ok(..) => (),
                // restore the valid geometry on error
                Err(..) => {
                    self.singular.vertices.truncate(start[0]);
                    self.instanced.vertices.truncate(start[1]);
                    self.errors += 1;
                }
            }
        }

        OutputGeometry {
            singular: SingularOutputGeometry {
                vertices: &self.singular.vertices,
            },
            instanced: InstancedOutputGeometry {
                vertices: &self.instanced.vertices,
                instances: &self.instanced.instances,
                commands: &self.instanced.commands,
            },
            errors: self.errors,
        }

    }

    fn process_one_shape(&mut self, shape: &Shape, geometry: &CurveGeometry, size: Size) -> Result<(), ()> {

        let range = shape.polygon_range();
        let points = geometry.points.get(range.start as usize .. range.end as usize)
            .unwrap_or_default();

        if points.len() < 3 {
            return Err(())
        }

        let range = shape.instances_range();
        let instances = geometry.instances.get(range.start as usize .. range.end as usize)
            .unwrap_or_default();

        if instances.len() == 0 {
            return Err(())
        }

        // save at what position we were in the instances list
        let start = self.instanced.vertices.len();

        // the shape kind determines which kind of vertices are generated
        let meta = ShapeMetadata {
            kind: shape.kind(),
            instance: &instances[0],
            size
        };

        // reset our state
        self.ears.clear();
        self.removed.clear();
        self.ears.resize(points.len() as u64, false);
        self.removed.resize(points.len() as u64, false);

        Self::triangulate_curves(points, meta, CurvesState {
            removed: &mut self.removed,
            out: match shape.kind() {
                ShapeKind::Singular => &mut self.singular.vertices,
                ShapeKind::Instanced => &mut self.instanced.vertices,
            }
        })?;

        Self::triangulate_body(points, meta, TriangulationState {
            removed: &mut self.removed,
            ears: &mut self.ears,
            out: match shape.kind() {
                ShapeKind::Singular => &mut self.singular.vertices,
                ShapeKind::Instanced => &mut self.instanced.vertices,
            }
        })?;

        if shape.kind() == ShapeKind::Instanced {
            for it in instances {
                self.instanced.instances.extend([
                    it.pos[0], it.pos[1], it.pos[2], // offsetX, offsetY, z
                    it.texture[0], it.texture[1], it.texture[2],
                ]);
                self.instanced.commands.push(gl::DrawArraysIndirectCommand::new(
                    (self.instanced.vertices.len() - start) / 4, // vertex count
                    instances.len(), // instance count
                    start / 4, // start index
                ))
            }
        }

        Ok(())

    }

    /// Calculate triangles that should be rendered as curves.
    fn triangulate_curves(points: &[CurvePoint], meta: ShapeMetadata, mut state: CurvesState) -> Result<(), ()> {

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

            let [a, b, c] = section.map(|it| Point::from(it));

            let increment; // how many points to skip for the next iteration

            // quadratic curve
            // (cubic curve has been lowered away!)
            if section[0].kind() == PointKind::Base &&
               section[1].kind() == PointKind::Ctrl &&
               section[2].kind() == PointKind::Base {

                increment = 2;

                // render a the curve triangle. the range of the "uvs" decides
                // which side of the curve get's filled by the shader

                let convex = Self::convex([a.into(), b.into(), c.into()]);

                // mark all convex curved triangles as removed,
                // so they are not triangulated later
                state.removed.set((idx + 1) as u64, convex);
                //                 ^^^^^^ idx of the ctrl point

                Self::generate_triangle(
                    a.into(), b.into(), c.into(), Self::uvs_for_convexity(convex),
                    meta, &mut state.out
                );

            // simple line, which can be skipped
            } else {
                increment = 1; // TODO: check for three control points in a row, which would be invalid. right now just causes the control points to be treated as base points
            }

            idx += increment;
            if idx >= len { break };

        }

        Ok(())

    }

    /// Ear-clipping triangulation for a single polygon.
    ///
    // TODO(DOC): link to docs on the triangulation compute shader on github
    fn triangulate_body(points: &[CurvePoint], meta: ShapeMetadata, mut state: TriangulationState) -> Result<(), ()> {

        let len = points.len();
        debug_assert!(len >= 3);

        // calculate initial ear state for every point

        for idx in 0..len {
            let ear = Self::ear(points, &state.removed, idx);
            state.ears.set(idx as u64, ear);
        }

        // remove ears and recalculate neighbours

        let mut changes = false; // used to check for errors
        let mut counter = 0;
        loop {

            if counter < len {
                counter += 1;
            } else {
                // if !changes { return Err("polygon not ccw or intersecting lines") };
                if !changes { return Err(()) }
                changes = false;
                counter = 1;
            }

            let idx = counter - 1;

            // skip all removed points
            if state.removed[idx as u64] {
                continue
            }

            if state.ears[idx as u64] {

                let [ia, ib, ic] = Self::neighbours(&state.removed, idx);
                if ia == ic { // only two points were left
                    break
                };

                Self::generate_triangle(
                    points[ia].into(), points[ib].into(), points[ic].into(),
                    Self::FILLED, meta, &mut state.out
                );

                // mark the point as self.removed
                state.removed.set(ib as u64, true);

                // recalculate the neighbors
                let ear = Self::ear(points, &state.removed, ia);
                state.ears.set(ia as u64, ear);
                let ear = Self::ear(points, &state.removed, ic);
                state.ears.set(ic as u64, ear);

                changes = true;

            }

        }

        Ok(())

    }

    const CONVEX:  [f32; 6] = [0.5, 0.5, 0.75, 0.5, 1.0, 1.0];
    const CONCAVE: [f32; 6] = [0.0, 0.0, 0.25, 0.0, 0.5, 0.5];
    const FILLED:  [f32; 6] = [2.0; 6]; // (2.0 indicates non-curve triangle)

    fn generate_triangle(a: Point, b: Point, c: Point, uvs: [f32; 6], meta: ShapeMetadata, out: &mut Vec<f32>) {

        let i = meta.instance;
        let [ga, gb, gc] = [GlPoint::from(a, meta.size), GlPoint::from(b, meta.size), GlPoint::from(c, meta.size)];

        match meta.kind {
            ShapeKind::Singular => out.extend([
                ga.x + i.pos[0], ga.y + i.pos[1], i.pos[2], uvs[0], uvs[1], i.texture[0], i.texture[1], i.texture[2],
                gb.x + i.pos[0], gb.y + i.pos[1], i.pos[2], uvs[2], uvs[3], i.texture[0], i.texture[1], i.texture[2],
                gc.x + i.pos[0], gc.y + i.pos[1], i.pos[2], uvs[4], uvs[5], i.texture[0], i.texture[1], i.texture[2]
            ]),
            ShapeKind::Instanced => out.extend([
                ga.x, ga.y, uvs[0], uvs[1],
                gb.x, gb.y, uvs[2], uvs[3],
                gc.x, gc.y, uvs[4], uvs[5]
            ]),
        }

    }

    /// Infinitely loops if all indices are marked as removed.
    pub(self) fn neighbours(removed: &BitVec, idx: usize) -> [usize; 3] {

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

    /// Check if the point at `idx` is an ear, accounting for removed neighbours.
    /// Y-flipped version.
    fn ear(polygon: &[CurvePoint], removed: &BitVec, idx: usize) -> bool {

        let [ia, ib, ic] = Self::neighbours(removed, idx);
        let [a, b, c] = [polygon[ia], polygon[ib], polygon[ic]];

        // short curcuit if the triangle has zero area.
        // we wan't to trat these as ears because this allows ren-
        // dering seemingly disconnected areas as one shape
        let abc = Self::triangle_area(a, b, c);
        if abc < 1e-6 { // account for precision errors
            return true
        }

        // short curcuit if it is concave
        let convex = Self::convex([a.into(), b.into(), c.into()]);
        if !convex {
            return false
        }

        let mut intersects = false;
        for point in polygon.iter() {
            if ![a, b, c].contains(point) {
                intersects |= Self::triangle_intersects_point([a, b, c], *point)
            }
        }

        !intersects

    }


    /// Check if the three points are convex, assuming counter clockwise orientation.
    /// Y-flipped version.
    fn convex(neighbours: [Point; 3]) -> bool { // TODO: make all function that don't care about off/on curve take Point

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
    fn triangle_area(a: CurvePoint, b: CurvePoint, c: CurvePoint) -> f32 {
        ((a.x() as f32 * (c.y() as f32 - b.y() as f32) +
          b.x() as f32 * (a.y() as f32 - c.y() as f32) +
          c.x() as f32 * (b.y() as f32 - a.y() as f32)) / 2.0).abs()
    //                  ^^^ the subtracion is fipped to account for the y-flip
    }

    /// If `point` lies within the triangle `trig`.
    /// Y-flipped version.
    fn triangle_intersects_point(trig: [CurvePoint; 3], point: CurvePoint) -> bool { // TODO: use Point not CurvePoint

        let abc = Self::triangle_area(trig[0], trig[1], trig[2]);

        let pab = Self::triangle_area(point, trig[0], trig[1]);
        let pbc = Self::triangle_area(point, trig[1], trig[2]);
        let pca = Self::triangle_area(point, trig[2], trig[0]);

        let total = pab + pbc + pca;

        // using a small epsilon to account for floating-point precision errors
        (total - abc).abs() < 1e-6

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

    fn uvs_for_convexity(convex: bool) -> [f32; 6] {
        match convex {
            true => Self::CONVEX,
            false => Self::CONCAVE,
        }
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

}

#[test]
fn test_solving_roots() {

    fn approx_equal(a: Option<f32>, b: f32, epsilon: f32) -> bool {
        if let Some(v) = a {
            (v - b).abs() < epsilon
        } else {
            false
        }
    }

    // Case: t^3 - 6t^2 + 11t - 6 = 0 (root at t = 1)
    let roots = TriangulationPass::solve_distance_derivative(1.0, -6.0, 11.0, -6.0); // TODO: move all out of TrigPass
    assert!(approx_equal(roots[0], 3.0, 1e-4), "the result was {:?} (first root should be {})", roots, 1.0);

    // Case: t^3 + t^2 + t + 1 = 0 (root at t = -1)
    let roots = TriangulationPass::solve_distance_derivative(1.0, 1.0, 1.0, 1.0);
    assert!(approx_equal(roots[0], -1.0, 1e-4), "the result was {:?} (first root should be {})", roots, -1.0);

    // Case: t^3 - 3t^2 + 3t - 1 = 0 (root at t = 1)
    let roots = TriangulationPass::solve_distance_derivative(1.0, -3.0, 3.0, -1.0);
    assert!(approx_equal(roots[0], 1.0, 1e-4), "the result was {:?} (first root should be {})", roots, 1.0);

    // Case: t^3 + 2t^2 + 3t + 4 = 0 (root near t = -1.6506)
    let roots = TriangulationPass::solve_distance_derivative(1.0, 2.0, 3.0, 4.0);
    assert!(approx_equal(roots[0], -1.6506, 1e-4), "the result was {:?} (first root should be {})", roots, -1.6506);

    // Case: t^3 + 3t^2 + 3t + 1 = 0 (root near t = 1)
    let roots = TriangulationPass::solve_distance_derivative(1.0, 3.0, 3.0, 1.0);
    assert!(approx_equal(roots[0], 1.0, 1e-4), "the result was {:?} (first root should be {})", roots, -0.8794);

    // Case: t^3 - 4t^2 + 5t - 2 = 0 (root near t = 0.5858)
    let roots = TriangulationPass::solve_distance_derivative(1.0, -4.0, 5.0, -2.0);
    assert!(approx_equal(roots[0], 0.5858, 1e-4), "the result was {:?} (first root should be {})", roots, 0.5858);

    // Case: t^3 - 2t^2 + t - 2 = 0 (root near t = 1.8794)
    let roots = TriangulationPass::solve_distance_derivative(1.0, -2.0, 1.0, -2.0);
    assert!(approx_equal(roots[0], 1.8794, 1e-4), "the result was {:?} (first root should be {})", roots, 1.8794);

    // Case: t^3 - 5t^2 + 8t - 4 = 0 (root near t = 2.1468)
    let roots = TriangulationPass::solve_distance_derivative(1.0, -5.0, 8.0, -4.0);
    assert!(approx_equal(roots[0], 2.1468, 1e-4), "the result was {:?} (first root should be {})", roots, 2.1468);

    // Case: t^3 + 6t^2 + 9t + 4 = 0 (root near t = -0.6604)
    let roots = TriangulationPass::solve_distance_derivative(1.0, 6.0, 9.0, 4.0);
    assert!(approx_equal(roots[0], -0.6604, 1e-4), "the result was {:?} (first root should be {})", roots, -0.6604);

    // Case: t^3 + t^2 - 3t - 2 = 0 (root near t = 1.0000)
    let roots = TriangulationPass::solve_distance_derivative(1.0, 1.0, -3.0, -2.0);
    assert!(approx_equal(roots[0], 1.0000, 1e-4), "the result was {:?} (first root should be {})", roots, 1.0000);

    // Case: t^3 - 1.5t^2 + 0.75t - 0.25 = 0 (root near t = 0.5)
    let roots = TriangulationPass::solve_distance_derivative(1.0, -1.5, 0.75, -0.25);
    assert!(approx_equal(roots[0], 0.5, 1e-4), "the result was {:?} (first root should be {})", roots, 0.5);

    // Case: t^3 + 0.5t^2 + 0.5t + 0.25 = 0 (root near t = -0.5)
    let roots = TriangulationPass::solve_distance_derivative(1.0, 0.5, 0.5, 0.25);
    assert!(approx_equal(roots[0], -0.5, 1e-4), "the result was {:?} (first root should be {})", roots, -0.5);

}

#[test]
fn neighbours() {

    let mut bits = BitVec::<usize>::new();
    bits.resize(10, false);

    assert_eq!(
        TriangulationPass::neighbours(&bits, 4),
        [3, 4, 5]
    );

    assert_eq!(
        TriangulationPass::neighbours(&bits, 0),
        [9, 0, 1]
    );

    assert_eq!(
        TriangulationPass::neighbours(&bits, 9),
        [8, 9, 0]
    );

}
