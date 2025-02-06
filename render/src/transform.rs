
//! CPU triangulation algorithms and handling of Curves.
//! Preparing the geometry for rendering.

use std::{convert::identity, f32::consts::PI, iter::once};
use bv::BitVec;
use common::*;

use crate::{CurveGeometry, CurvePoint, GlPoint, Instance, Shape};

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
enum ShapeKind<'a> {
    /// will be inserted per-vertex
    Singular(&'a Instance),
    /// will only be inserted per-instance
    Instanced,
}

#[derive(Clone, Copy)]
struct ShapeMetadata<'a> {
    pub kind: ShapeKind<'a>,
    /// Window size
    pub size: Size,
}

pub struct Transformer {
    lower: LoweringPass,
    trig: TriangulationPass,
}

impl Transformer {

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
pub struct LoweringPass {
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

            // geometry is valid up to this index
            // let start = [
            //     self.lowered.points.len(),
            //     self.lowered..len(),
            // ];

            match self.process_one_shape(shape, geometry) {
                Ok(..) => (),
                // restore the valid geometry on error
                Err(..) => {
                    // self.singular.vertices.truncate(start[0]);
                    // self.instanced.vertices.truncate(start[1]);
                    self.errors += 1;
                }
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

        // we just hand the instances through, as we only need to modify
        // the actual shapes.
        self.output.instances.extend_from_slice(instances);

        let mut output_len = points.len();
        let mut idx = 0;
        loop {

            let len = points.len();

            // get the point at idx and the four points "after" it in the polygon
            let [ia, ib, ic, id] = [
                (idx + 0) % len,
                (idx + 1) % len,
                (idx + 2) % len,
                (idx + 3) % len,
            ];

            let [a, b, c, d] = [
                points[ia], points[ib],
                points[ic], points[id]
            ];

            let increment; // how many points to skip for the next iteration

            if a.is_base() && !b.is_base() && c.is_base() { // B-C-B

                increment = 2;

                // appends the points of the curve, which can be considered
                // a "curve triangle" in this case

                self.output.points.push(a);
                self.output.points.push(b);
                self.output.points.push(c);

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
                                let split_at = TriangulationPass::was_it_this_easy_all_along([sa.into(), sb.into(), sc.into()], (*point).into());
                                let [first, second] = TriangulationPass::split_quadratic([sa.into(), sb.into(), sc.into()], split_at - 0.01); // TODO: we need to split "right before" test how small EPS can get, rn sadly not much smaller then 0.1 :/ which is unacceptable for font rendering

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

                let new_points = (max_depth - 1) * 3;
                output_len += new_points;

                // for point in points.iter() {
                //     if ![a, b, c].contains(point) &&
                //         TrigPass::triangle_intersects_point([a, b, c], *point) {

                //         // find the correct sub curve
                //         'subs: for current_depth in 0..max_depth {

                //             let points_len = self.lowered.points.len();
                //             let sub_triangle = &mut self.lowered.points[points_len - 3 * (current_depth + 1) .. points_len - 3 * current_depth];
                //             let [sa, sb, sc] = sub_triangle.try_into().expect("get sub triangle");

                //             if TrigPass::triangle_intersects_point([sa, sb, sc], *point) {

                //                 let split_at = TrigPass::was_it_this_easy_all_along([sa.point(), sb.point(), sc.point()], point.point());
                //                 // dbg!(split_at);
                //                 // let split_at = TrigPass::closest_point_on_curve([sa.point(), sb.point(), sc.point()], point.point());
                //                 // let split_at = TrigPass::approx_split_point([sa.point(), sb.point(), sc.point()], point.point());
                //                 let curves = TrigPass::split_quadratic([sa.point(), sb.point(), sc.point()], split_at - 0.01);

                //                 // insert the first curve by replacing the original sub triangle
                //                 sub_triangle[0] = CurvePoint::base(curves[0][0].x as i16, curves[0][0].y as i16);
                //                 sub_triangle[1] = CurvePoint::ctrl(curves[0][1].x as i16, curves[0][1].y as i16);
                //                 sub_triangle[2] = CurvePoint::base(curves[0][2].x as i16, curves[0][2].y as i16);

                //                 // insert the second curve at the end
                //                 self.lowered.points.push(CurvePoint::base(curves[1][0].x as i16, curves[1][0].y as i16));
                //                 self.lowered.points.push(CurvePoint::ctrl(curves[1][1].x as i16, curves[1][1].y as i16));
                //                 self.lowered.points.push(CurvePoint::base(curves[1][2].x as i16, curves[1][2].y as i16));

                //                 new_len += 3; // three new points were added
                //                 max_depth += 1;

                //                 break 'subs

                //             }


                //         }

                //     }
                // }

                self.output.points.pop(); // pop the last point since it will be addad again next iteration

            } else if a.is_base() && !b.is_base() && !c.is_base() && d.is_base() {
                todo!("cubic curves are not yes implemented");
                // TODO: Dont forget to also split up intersected curve trigs here
            } else if !a.is_base() && !b.is_base() && !c.is_base() {
                todo!("error: three contol points in a row");
            } else {
                // normal "line"
                self.output.points.push(a);
                increment = 1;
            }

            // TODO: what happens if a point is duplicated in the input? does it triangulate correctly?

            idx += increment;
            if idx >= len { break };

        }

        // generate the output shape, with updated indices

        let start = shape.polygon_range().start;
        let end = start + output_len as i16;

        match shape.is_singular() {
            true  => self.output.shapes.push(Shape::new_singular(start..end, shape.instances_range().start)),
            false => self.output.shapes.push(Shape::new_instanced(start..end, shape.instances_range())),
            //                                                                 ^^^^ just hand the instances indices through
        }

        Ok(())


    }

}

pub struct TriangulationPass { // TODO: make pub(crate)
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

    /// Prepares the polygons for rendering.
    ///
    /// # Processing Steps
    /// - Convert coordinates to OpenGl screen space.
    /// - Triangulate the polygon using ear-clipping, with some restrictions.
    /// - Output extra triangles for the curved parts. TODO: check all the doc comments. rn they arent useful and mostly wrong anyways
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
            // return Err("shape with less then three points")
            return Err(())
        }

        let range = shape.instances_range();
        let instances = geometry.instances.get(range.start as usize .. range.end as usize)
            .unwrap_or_default();

        if instances.len() == 0 {
            // return Err("shape without any instances")
            return Err(())
        }

        // save at what position we were in the instances list
        let start = self.instanced.vertices.len();

        // this determines which kind of vertices are generated
        let kind = match shape.is_singular() {
            true => ShapeKind::Singular(&instances[0]),
            false => ShapeKind::Instanced,
        };

        let meta = ShapeMetadata { kind, size };

        // reset our state
        self.ears.clear();
        self.removed.clear();
        self.ears.resize(points.len() as u64, false);
        self.removed.resize(points.len() as u64, false);

        Self::triangulate_curves(points, meta, CurvesState {
            removed: &mut self.removed,
            out: if shape.is_singular() { &mut self.singular.vertices }
                 else { &mut self.instanced.vertices },
        })?;

        Self::triangulate_body(points, meta, TriangulationState {
            removed: &mut self.removed,
            ears: &mut self.ears,
            out: if shape.is_singular() { &mut self.singular.vertices }
                 else { &mut self.instanced.vertices },
        })?;

        if !shape.is_singular() {
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

    /// Calculate patch-on triangles that should be rendered as curves.
    ///
    /// Accepts window- and returns normalized device coordinates.
    fn triangulate_curves(points: &[CurvePoint], meta: ShapeMetadata, mut state: CurvesState) -> Result<(), ()> {

        let len = points.len();
        debug_assert!(len >= 3);

        // make curve triangles out of quadratic curves (B-C-B)

        let mut idx = 0;
        loop {

            // get the point at idx and the four points "after" it in the polygon
            let [ia, ib, ic, id] = [
                (idx + 0) % len,
                (idx + 1) % len,
                (idx + 2) % len,
                (idx + 3) % len,
            ];

            let [a, b, c, d] = [
                points[ia], points[ib],
                points[ic], points[id]
            ];

            let increment; // how many points to skip for the next iteration

            if a.is_base() && !b.is_base() && c.is_base() {

                increment = 2;

                // quadratic curve case

                // render a the curve triangle. the range of the "uvs" decides
                // which side of the curve get's filled by the shader

                let convex = Self::convex([a.into(), b.into(), c.into()]);

                // mark all convex curved triangles as removed,
                // so they are not triangulated later
                state.removed.set(ib as u64, convex);

                Self::triangle(
                    a.into(), b.into(), c.into(), Self::uvs_for_convexity(convex),
                    meta, &mut state.out
                );

            } else if a.is_base() && !b.is_base() && !c.is_base() && d.is_base() {

                /*

                increment = 3;

                // the cubic curve case is a little more complicated
                //
                // A. split the cubic curve at it's infection- and min-/max- points
                // B. degree reduce the split out parts and generate a quadratic
                //    curve triangle for all of them
                // C. mark all of the original control points that are inside the
                //    shape as removed

                // println!("{:?}", critical_points);

                let b_inside_approx = Self::left_of_line(b.point(), a.point(), d.point());
                let c_inside_approx = Self::left_of_line(c.point(), a.point(), d.point());

                if !b_inside_approx { self.removed.set(ib as u64, true) };
                if !c_inside_approx { self.removed.set(ic as u64, true) };

                dbg!(b_inside_approx, c_inside_approx);

                let mut curve = [a.point(), b.point(), c.point(), d.point()];
                let mut previous_t = 0.0;

                let mut tr_removed = BitVec::<usize>::new();
                let mut tr_ears = BitVec::<usize>::new();
                let mut tr_points: Vec<CurvePoint> = Vec::new();

                let critical_points: Vec<f32> = (0..4).map(|it| it as f32 / 4.0).collect();

                // push first point
                // tr_removed.push(false);
                // tr_ears.push(false);
                // tr_points.push(CurvePoint::fuckery(curve[0]));

                for t in critical_points.iter().copied().chain([1.0]) { // chain 1.0 so we also get the final segment

                    // calculate where the original t value would lie on the new segment
                    let normalized_t = (t - previous_t) / (1.0 - previous_t);

                    // split off one segment of the curve
                    let [sub, rest] = Self::split_cubic(curve, normalized_t);

                    // quadratic control point (degree reduction by averaging)
                    let p2 = Point::new(
                        -0.25*sub[0].x + 0.75*sub[1].x + 0.75*sub[2].x -0.25*sub[3].x,
                        -0.25*sub[0].y + 0.75*sub[1].y + 0.75*sub[2].y -0.25*sub[3].y
                    );

                    let [p1, p3] = [sub[0], sub[3]];

                    let convex = Self::convex([p1, p2, p3]);

                    tr_removed.push(false);
                    tr_ears.push(false);
                    tr_points.push(CurvePoint::fuckery(p1));

                    if !convex {
                        tr_removed.push(false);
                        tr_ears.push(false);
                        tr_points.push(CurvePoint::fuckery(p2));
                    }

                    Self::triangle(
                        p1, p2, p3, Self::uvs(convex), meta,
                        &mut VerticesOut {
                            singular: &mut self.singular.vertices,
                            instanced: &mut self.instanced.vertices
                        }
                    );

                    curve = rest;
                    previous_t = t;

                }

                // push the end point
                tr_removed.push(false);
                tr_ears.push(false);
                tr_points.push(CurvePoint::fuckery(curve[3]));

                if b_inside_approx {
                    tr_removed.push(false);
                    tr_ears.push(false);
                    tr_points.push(b);
                };
                if c_inside_approx {
                    tr_removed.push(false);
                    tr_ears.push(false);
                    tr_points.push(c);
                };

                dbg!(&tr_points);

                Self::triangulate(&tr_points, meta, TriangulationState { removed: &mut tr_removed, ears: &mut tr_ears, out: VerticesOut { singular: &mut self.singular.vertices, instanced: &mut self.instanced.vertices } }).ok();

                */

                // TODO: this is now handeled in the lowering pass
                panic!("TODO: the rendering of cubic bézier curves is not implemented yet");

            } else {
                // not a bézier curve, maybe just a line, so just continue
                increment = 1; // TODO: check for three control points in a row, which would be invalid. right now just causes the control points to be treated as base points
            }

            idx += increment;
            if idx >= len { break };

        }

        Ok(())

    }

    /// Ear-clipping triangulation for a single polygon.
    ///
    /// Accepts window- and returns normalized device coordinates.
    ///
    /// The algorithms is purposely written in a way that is similar to the
    /// compute shader implementation.
    // TODO(DOC): link to docs on the triangulation compute shader
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

                Self::triangle(
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

    const CONVEX:  [f32; 6] = [0.5, 0.5, 0.75, 0.5, 1.0, 1.0]; // curve coords
    const CONCAVE: [f32; 6] = [0.0, 0.0, 0.25, 0.0, 0.5, 0.5]; // curve coords
    const FILLED:  [f32; 6] = [1.0; 6]; // curve coords

    fn triangle_size(kind: ShapeKind) -> usize {
        match kind {
            ShapeKind::Singular(..) => 8*3,
            ShapeKind::Instanced    => 4*3,
        }
    }

    fn triangle(a: Point, b: Point, c: Point, uvs: [f32; 6], meta: ShapeMetadata, out: &mut Vec<f32>) {

        let [ga, gb, gc] = [GlPoint::from(a, meta.size), GlPoint::from(b, meta.size), GlPoint::from(c, meta.size)];

        match meta.kind {
            ShapeKind::Singular(i) => {
                out.extend([
                    ga.x + i.pos[0], ga.y + i.pos[1], i.pos[2], uvs[0], uvs[1], i.texture[0], i.texture[1], i.texture[2],
                    gb.x + i.pos[0], gb.y + i.pos[1], i.pos[2], uvs[2], uvs[3], i.texture[0], i.texture[1], i.texture[2],
                    gc.x + i.pos[0], gc.y + i.pos[1], i.pos[2], uvs[4], uvs[5], i.texture[0], i.texture[1], i.texture[2]
                ]);
            },
            ShapeKind::Instanced => {
                out.extend([
                    ga.x, ga.y, uvs[0], uvs[1],
                    gb.x, gb.y, uvs[2], uvs[3],
                    gc.x, gc.y, uvs[4], uvs[5]
                ]);
            }
        }

    }

    /// Behaviour is unspecified if all indices are marked as removed.
    pub(self) fn neighbours(removed: &BitVec, idx: usize) -> [usize; 3] {

        let len = removed.len(); // removed.len() == polygon.len()

        #[cfg(debug_assertions)]
        {
            let mut count = 0;
            for idx in 0..len { if removed[idx] { count += 1 } }
            assert!(len > 2, "`neighbours` called with < elements");
            assert!(count <= len - 2, "`neighbtbours` called with < 2 elements alive, just {} out of {:?}", len - count, removed);
        }

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
    // TODO: make all these functions free floating and not TrigPass::XXYY
    fn triangle_intersects_point(trig: [CurvePoint; 3], point: CurvePoint) -> bool { // TODO: use Point not CurvePoint

        let abc = Self::triangle_area(trig[0], trig[1], trig[2]);

        let pab = Self::triangle_area(point, trig[0], trig[1]);
        let pbc = Self::triangle_area(point, trig[1], trig[2]);
        let pca = Self::triangle_area(point, trig[2], trig[0]);

        let total = pab + pbc + pca;

        // using a small epsilon to account for floating-point precision errors
        (total - abc).abs() < 1e-6

    }

    // function to find the critical t values (inflection points and local extremes)
    // of a cubic bézier curve
    fn critical_points(a: CurvePoint, b: CurvePoint, c: CurvePoint, d: CurvePoint) -> Vec<f32> {

        let mut t_values = Vec::new();

        // find the t values where local extremes occur (solving the first derivative)
        let ax = 3.0 * (-a.x() as f32 + 3.0 * (b.x() as f32 - c.x() as f32) + d.x() as f32);
        let bx = 6.0 * ( a.x() as f32 - 2.0 *  b.x() as f32 + c.x() as f32);
        let cx = 3.0 * ( b.x() as f32 - a.x() as f32);

        let ay = 3.0 * (-a.y() as f32 + 3.0 * (b.y() as f32 - c.y() as f32) + d.y() as f32);
        let by = 6.0 * ( a.y() as f32 - 2.0 *  b.y() as f32 + c.y() as f32);
        let cy = 3.0 * ( b.y() as f32 - a.y() as f32);

        // calculate discriminants for both x and y
        let disc_x = bx * bx - 4.0 * ax * cx;
        let disc_y = by * by - 4.0 * ay * cy;

        // solve for t in the x direction
        if disc_x >= 0.0 && ax != 0.0 {
            let sqrt_disc = disc_x.sqrt();
            let t1 = (-bx + sqrt_disc) / (2.0 * ax);
            let t2 = (-bx - sqrt_disc) / (2.0 * ax);

            if t1 >= 0.0 && t1 <= 1.0 {
                t_values.push(t1);
            }
            if t2 >= 0.0 && t2 <= 1.0 {
                t_values.push(t2);
            }
        }

        // solve for t in the y direction
        if disc_y >= 0.0 && ay != 0.0 {
            let sqrt_disc = disc_y.sqrt();
            let t1 = (-by + sqrt_disc) / (2.0 * ay);
            let t2 = (-by - sqrt_disc) / (2.0 * ay);

            if t1 >= 0.0 && t1 <= 1.0 {
                t_values.push(t1);
            }
            if t2 >= 0.0 && t2 <= 1.0 {
                t_values.push(t2);
            }
        }

        // memove duplicates and sort the t values in ascending order
        t_values.dedup();
        t_values.sort_unstable_by(
            |a, b| a.partial_cmp(b).unwrap()
        ); // scuffed f32 sort

        t_values

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
        // -- curve1 --   --- curve2  ---
    }

    fn uvs_for_convexity(convex: bool) -> [f32; 6] {
        match convex {
            true => Self::CONVEX,
            false => Self::CONCAVE,
        }
    }

    // y-flipped version
    fn left_of_line(p: Point, line_start: Point, line_end: Point) -> bool {
        let vector1_x = line_end.x - line_start.x;
        let vector1_y = -(line_end.y - line_start.y);
        let vector2_x = p.x - line_start.x;
        let vector2_y = -(p.y - line_start.y);
        let cross_product = vector1_x * vector2_y - vector1_y * vector2_x;
        cross_product > 0.0
    }

    fn bezier_point([a, b, c]: [Point; 3], t: f32) -> Point {
        let x = (1.0 - t) * (1.0 - t) * a.x + 2.0 * (1.0 - t) * t * b.x + t * t * c.x;
        let y = (1.0 - t) * (1.0 - t) * a.y + 2.0 * (1.0 - t) * t * b.y + t * t * c.y;
        Point { x, y }
    }

    fn approx_split_point(curve: [Point; 3], p: Point) -> f32 {

        let mut how = f32::INFINITY;
        let mut result = 0.0;
        let [a, b, c] = curve;

        for idx in 0..1000 {
            let t = idx as f32 / 1000.0;
            let sx = (1.0-t).powi(2) * a.x + 2.0*(1.0-t) + b.x + t.powi(2) * c.x;
            let sy = (1.0-t).powi(2) * a.y + 2.0*(1.0-t) + b.y + t.powi(2) * c.y;
            let dot = (p.x - a.x)*(sy - a.y) - (p.y - a.y) * (sx - a.x);
            if dot >= 0.0 && dot < how {
                how = dot;
                result = t;
            }
        }

        result - 0.4

    }

    /// We split the curve so that the intersecting point p now lies at the edge of the triangle
    /// enclosing the curve from 0 to the split point.
    fn split_point(curve: [Point; 3], p: Point) -> f32 {

        #![allow(non_snake_case)]

        // conv y-flip
        let mut curve = curve;
        curve.iter_mut().for_each(|it| it.y = (it.y - 500.0).abs());

        let mut p = p;
        p.y = (p.y - 500.0).abs();

        // calculate the relevant coefficients
        let [a, b, c] = curve;
        let A = p.x * (a.y - 2.0*b.y + c.y - a.x*a.y + 2.0*a.x*b.y - a.x*c.y) + p.y * (-a.x + 2.0*b.x - c.x + a.y*a.x + 2.0*a.y*b.x + a.y*c.x);
        let B = p.x * (-2.0*a.y + 2.0*b.y - 2.0*a.x*b.y) + p.y * (2.0*a.x - 2.0*b.x + 2.0*a.y*b.x);
        let C = p.x*a.y - p.y*a.x;

        // broken chatgpt version
        // let A = (p.x-a.x)*(a.y+c.y)-(p.y-a.y)*(a.x+c.x);
        // let B = -2.0*(p.x-a.x)*(a.y+b.y)+2.0*(p.y-a.y)*(a.x+b.x);
        // let C = 2.0*(p.x-a.x)*b.y-2.0*(p.y-a.y)*b.x;

        let discr = B.powi(2) - 4.0*A*C;
        (-B + (discr).sqrt()) / (2.0*A)


    }

    fn was_it_this_easy_all_along(curve: [Point; 3], p: Point) -> f32 {

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
        let root2 = (-b - inner) / (2.0*a);

        root1

    }

    /// The point must lie within the concave section of the curve. The closest point must not
    /// be an end of the curve.
    fn closest_point_on_curve(curve: [Point; 3], p: Point) -> f32 {

        // get coefficients for this curve
        let [a, b, c, d] = Self::distance_derivative_coefficients(curve, p);

        // solve the cubic equation for t values
        let roots = Self::solve_distance_derivative(a, b, c, d);

        // NOTE: for our case, where the point lies inside the curves "belly", there will
        //       be only one root and the closest point will not be at the ends

        // roots[0].expect("get relevant root")
        // TODO: clean up. roots[0] seems to fail sometimes?

        // NOTE: this would be for the general case

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
fn test_one_real_root() {

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

struct CurvesState<'a> {
    removed: &'a mut BitVec,
    out: &'a mut Vec<f32>,
}

struct TriangulationState<'a> {
    removed: &'a mut BitVec,
    ears: &'a mut BitVec,
    out: &'a mut Vec<f32>,
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

// #[test]
// fn convex() {

//     let points: &mut [CurvePoint] = &mut [
//         CurvePoint::base(0, 0),
//         CurvePoint::base(10, 0),
//         CurvePoint::base(15, 5),
//         CurvePoint::base(20, 10),
//         CurvePoint::base(15, 15),
//         CurvePoint::base(10, 20),
//         CurvePoint::base(0, 20),
//     ];

//     points.reverse();

//     let instances = &[
//         Instance { pos: [0.0, 0.0, 0.0], texture: [1.0, 1.0, 1.0] },
//     ];

//     let mut state = Triangulator::new();
//     state.size = Size { w: 20, h: 20 };
//     state.triangulate(points, instances, true).unwrap();

//     dbg!(&state.singular.vertices);

// }

// #[test]
// fn curves() {

//     let points: &mut [CurvePoint] = &mut [
//         CurvePoint::base(10, 10),
//         CurvePoint::control(20, 20),
//         CurvePoint::control(20, 20),
//         CurvePoint::control(20, 20),
//         CurvePoint::base(10, 10),
//     ];

//     let instances = &[
//         Instance { pos: [0.0, 0.0, 0.0], texture: [1.0, 1.0, 1.0] },
//     ];

//     let mut state = Triangulator::new();
//     state.size = Size { w: 20, h: 20 };
//     let result = state.curves(points, instances, true);

//     assert_eq!(
//         result,
//         Err("invalid configuration of base/control points")
//     );


// }
