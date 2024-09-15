
//! Ear-clipping triangulation on the cpu.

use std::f32::consts::PI;
use bv::BitVec;
use common::Size;

use crate::{CurveGeometry, CurvePoint, Instance, Point, Shape};

// TODO: make all u16 be i16 and enforce it to pe positive or enforce all u16's to be in range 0..32K (i16::MAX) or use another type

/// Output after processing a polygon.
pub struct OutputGeometry<'a> {
    pub singular: SingularOutputGeometry<'a>,
    pub instanced: InstancedOutputGeometry<'a>,
    /// contains a message for the last error that occured in a shape.
    /// shapes with an error are discarded
    pub error: &'static str,
}

impl<'a> OutputGeometry<'a> {
    pub fn check(&self) -> Result<(), &'static str> {
        match self.error {
            "" => Ok(()),
            msg => Err(msg),
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
    /// window size
    pub size: Size,
}

pub struct Triangulator { // TODO: make pub(crate)
    size: Size,
    // state during triangulation
    ears: BitVec<usize>,
    removed: BitVec<usize>,
    // outputs
    singular: SingularData,
    instanced: InstancedData,
    error: &'static str,
}

impl Triangulator { // TODO: rename to Preprocessor (maybe) since it does more then triangulating

    pub fn new() -> Self {
        Self {
            size: Size { w: 0, h: 0 },
            ears: BitVec::new(),
            removed: BitVec::new(),
            singular: SingularData { vertices: Vec::new() },
            instanced: InstancedData { vertices: Vec::new(), instances: Vec::new(), commands: Vec::new() },
            error: "",
        }
    }

    /// Prepares the polygons for rendering.
    ///
    /// # Processing Steps
    /// - Convert coordinates to OpenGl screen space.
    /// - Triangulate the polygon using ear-clipping, with some restrictions.
    /// - Output extra triangles for the curved parts.
    pub fn process<'s>(&'s mut self, size: Size, geometry: &CurveGeometry) -> OutputGeometry<'s> {

        self.size = size;

        // reset our state
        self.singular.vertices.clear();
        self.instanced.vertices.clear();
        self.instanced.instances.clear();
        self.instanced.commands.clear();
        self.error = "";

        // append basic triangles
        for shape in &geometry.shapes {

            // geometry is valid up to this index
            let start = [
                self.singular.vertices.len(),
                self.instanced.vertices.len(),
            ];

            // generate the mesh now
            match self.shape(shape, size, &geometry) {
                Ok(..) => (),
                // restore the valid geometry on error
                Err(msg) => {
                    self.singular.vertices.truncate(start[0]);
                    self.instanced.vertices.truncate(start[1]);
                    self.error = msg;
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
            error: self.error,
        }
        
    }

    fn shape(&mut self, shape: &Shape, size: Size, geometry: &CurveGeometry) -> Result<(), &'static str> {

        let range = shape.polygon();
        let points = geometry.points.get(range.start as usize .. range.end as usize)
            .unwrap_or_default();

        if points.len() < 3 {
            return Err("shape with less then three points")
        }

        let range = shape.instances();
        let instances = geometry.instances.get(range.start as usize .. range.end as usize)
            .unwrap_or_default();

        if instances.len() == 0 {
            return Err("shape without any instances")
        }
        
        // save at what position we were in the instances list
        let start = self.instanced.vertices.len();

        // this determines which kind of vertices are generated
        let kind = match shape.kind() {
            true => ShapeKind::Singular(&instances[0]),
            false => ShapeKind::Instanced,
        };

        let meta = ShapeMetadata { kind, size };

        // reset our state
        self.ears.clear();
        self.removed.clear();
        self.ears.resize(points.len() as u64, false);
        self.removed.resize(points.len() as u64, false);

        // generate the triangles
        self.curves(points, meta)?;
        Self::triangulate(points, meta, TriangulationState { removed: &mut self.removed, ears: &mut self.ears, out: VerticesOut { singular: &mut self.singular.vertices, instanced: &mut self.instanced.vertices } })?;

        if let ShapeKind::Instanced = kind {
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
    fn curves(&mut self, points: &[CurvePoint], meta: ShapeMetadata) -> Result<(), &'static str> {

        let len = points.len();
        debug_assert!(len >= 3);

        // 1. make curve triangles out of quadratic curves (B-C-B)
        // 2. split up cubic curves (B-C-C-B) into multiple quadratic ones and do the same

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

            if a.kind() && !b.kind() && c.kind() {

                increment = 2;

                // quadratic curve case
                //
                // A. render a single "curve triangle". the range of the "uvs" decides
                //    which side of the curve get's filled by the shader
                // B. mark all control points inside the shape as removed

                let convex = Self::convex([a.point(), b.point(), c.point()]);

                // mark all convex curved triangles as removed,
                // so they are not triangulated later
                self.removed.set(ib as u64, convex);

                Self::triangle(
                    a.point(), b.point(), c.point(), Self::uvs(convex), meta,
                    &mut VerticesOut {
                        singular: &mut self.singular.vertices,
                        instanced: &mut self.instanced.vertices
                    }
                );

            } else if a.kind() && !b.kind() && !c.kind() && d.kind() {

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

                panic!("TODO: the rendering of cubic beziér curves is not implemented yet");

            } else {
                // not a beziér curve, maybe just a line, so just continue
                increment = 1; // TODO: check for three control points in a row, which would be invalid and rn just causes the control points to be ignored
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
    fn triangulate(points: &[CurvePoint], meta: ShapeMetadata, mut state: TriangulationState) -> Result<(), &'static str> {

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
                if !changes { return Err("polygon not ccw or intersecting lines") };
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
                    points[ia].point(), points[ib].point(), points[ic].point(),
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
    const FILLED:  [f32; 6] = [1.0; 6];

    fn triangle(a: Point, b: Point, c: Point, uvs: [f32; 6], meta: ShapeMetadata, out: &mut VerticesOut) {

        let [ga, gb, gc] = [a.gl(meta.size), b.gl(meta.size), c.gl(meta.size)];

        match meta.kind {
            ShapeKind::Singular(i) => {
                out.singular.extend([
                    ga.x + i.pos[0], ga.y + i.pos[1], i.pos[2], uvs[0], uvs[1], i.texture[0], i.texture[1], i.texture[2],
                    gb.x + i.pos[0], gb.y + i.pos[1], i.pos[2], uvs[2], uvs[3], i.texture[0], i.texture[1], i.texture[2],
                    gc.x + i.pos[0], gc.y + i.pos[1], i.pos[2], uvs[4], uvs[5], i.texture[0], i.texture[1], i.texture[2]
                ]);
            },
            ShapeKind::Instanced => {
                out.instanced.extend([
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
        let convex = Self::convex([a.point(), b.point(), c.point()]);
        if !convex {
            return false
        }

        let mut intersects = false;
        for point in polygon.iter() {
            if [a, b, c].contains(point) { continue };
            intersects |= Self::triangle_intersects_point([a, b, c], *point)
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
    fn triangle_intersects_point(trig: [CurvePoint; 3], point: CurvePoint) -> bool {

        let abc = Self::triangle_area(trig[0], trig[1], trig[2]);

        let pab = Self::triangle_area(point, trig[0], trig[1]);
        let pbc = Self::triangle_area(point, trig[1], trig[2]);
        let pca = Self::triangle_area(point, trig[2], trig[0]);
    
        let total = pab + pbc + pca;

        // using a small epsilon to account for floating-point precision errors
        (total - abc).abs() < 1e-6

    }

    // function to find the critical t values (inflection points and local extremes)
    // of a cubic beziér curve
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

    fn lerp(p1: Point, p2: Point, t: f32) -> Point {
        Point::new(
            p1.x as f32 + (p2.x as f32 - p1.x as f32) * t,
            p1.y as f32 + (p2.y as f32 - p1.y as f32) * t
        )
    }

    fn uvs(convex: bool) -> [f32; 6] {
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

}

struct VerticesOut<'a> {
    singular: &'a mut Vec<f32>,
    instanced: &'a mut Vec<f32>,
}

struct TriangulationState<'a> {
    removed: &'a mut BitVec,
    ears: &'a mut BitVec,
    out: VerticesOut<'a>,
}

#[test]
fn neighbours() {

    let mut bits = BitVec::<usize>::new();
    bits.resize(10, false);

    assert_eq!(
        Triangulator::neighbours(&bits, 4),
        [3, 4, 5]
    );

    assert_eq!(
        Triangulator::neighbours(&bits, 0),
        [9, 0, 1]
    );

    assert_eq!(
        Triangulator::neighbours(&bits, 9),
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

