
//! Widget and utilities that can render curved paths efficiently.
//!
//! The paths are triangulated using a simple ear-clipping algorithm
//! and are then forewarded to the renderer. This can be used to render
//! text and other SVG-like geometry.
//!
//! # Background
//!
//! Historically, rendering this curved geometry was part of the base renderer,
//! but since it is very possible and efficient to only include already processed
//! vertices in your application by converting fonts and icons ahead of time, this
//! functionallity is actually not needed in many cases.
//!
//! Also, only having vertices as the base geometry format simplifies all other
//! algorithms that the widgets use to manipulate geometry, for example clipping.

pub mod shaper {

    use std::iter;
    use common::*;

    /// This auxillary struct is used to convert a shape described by a list
    /// of curve points into a shape described by a list of triangles.
    pub struct GeometryShaper {
        lower: LoweringPass,
        trig: TriangulationPass,
    }

    impl GeometryShaper {

        pub fn new() -> Self {
            Self {
                lower: LoweringPass::new(),
                trig: TriangulationPass::new(),
            }
        }

        /// Process a single curved shape.
        pub fn process<'s>(&'s mut self, points: &[CurvePoint]) -> Result<&'s [PartialVertex], ()> {

            let lowered = self.lower.process(points)?;
            let result  = self.trig.process(lowered)?;

            Ok(result)

        }

    }

    /// This pass performs lowering of curve data so the later algorithms can be simpler.
    struct LoweringPass {
        buf: Vec<CurvePoint>,
    }

    impl LoweringPass {

        fn new() -> Self {
            Self {
                buf: Vec::with_capacity(24)
            }
        }

        pub fn process<'s>(&'s mut self, points: &[CurvePoint]) -> Result<&'s [CurvePoint], ()> {

            self.buf.clear();

            for section in sections(points) {

                match section {

                    CurveSection::Line([(.., a), ..]) => self.buf.push(
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
                                self.buf.extend_from_slice(&[
                                    CurvePoint::fromp(a, PointKind::Base),
                                    CurvePoint::fromp(b, PointKind::Ctrl),
                                ]);
                            }
                        } else {
                            self.buf.extend_from_slice(&[
                                CurvePoint::base(a.x, a.y),
                                CurvePoint::ctrl(b.x, b.y)
                            ])
                        }

                    },

                    CurveSection::Cubic(it) => {

                        // Lower the cubic curve into quadratic curves.
                        let cubic = it.map(|(.., it)| MathPoint::from(it));

                        for [a, b, ..] in lowercubic(cubic) {
                            self.buf.extend_from_slice(&[
                                CurvePoint::fromp(a, PointKind::Base),
                                CurvePoint::fromp(b, PointKind::Ctrl),
                            ]);
                        }

                    },

                    CurveSection::Invalid => return Err(())

                }
            }

            Ok(&self.buf)

        }

    }

    enum CurveSection {
        Line      ([(u16, PhysicalPoint); 2]),
        Quadratic ([(u16, PhysicalPoint); 3]),
        Cubic     ([(u16, PhysicalPoint); 4]),
        /// Three or more control points in a row.
        Invalid
    }

    fn sections(points: &[CurvePoint]) -> impl Iterator<Item = CurveSection> {

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
                // INVALID (3+ CTRL POINTS IN A ROW):
                [..] => {
                    incr = 1;
                    result = CurveSection::Invalid;
                }
            }

            idx += incr;
            return Some(result);

        })

    }

    struct TriangulationPass {
        /// During ear-clipping, stores which vertices are valid ears.
        ears: Vec<bool>,
        /// During ear-clipping, stores which vertices have been removed already.
        removed: Vec<bool>,
        /// The output of the triangulation.
        buf: Vec<PartialVertex>,
    }

    impl TriangulationPass {

        pub fn new() -> Self {
            Self {
                ears: Vec::with_capacity(24),
                removed: Vec::with_capacity(24),
                buf: Vec::with_capacity(24),
            }
        }

        fn process<'s>(&'s mut self, points: &[CurvePoint]) -> Result<&'s [PartialVertex], ()> {

            if points.len() < 3 {
                return Err(())
            }

            // reset our state
            self.ears.clear();
            self.removed.clear();
            self.ears.resize(points.len() , false);
            self.removed.resize(points.len(), false);

            self.tcurves(points);
            self.tbody(points)?;

            Ok(&self.buf)

        }

        /// Generate all triangles that will be rendered as curves.
        fn tcurves(&mut self, points: &[CurvePoint]) {

            for section in sections(points) {

                if let CurveSection::Quadratic([(.., p1), (ictrl, ctrl), (.., p2)]) = section {

                    let curve = [p1, ctrl, p2].map(|it| MathPoint::from(it));

                    let convex = triangle_is_convex(curve);
                    let fill = match convex {
                        true => FillKind::Convex,
                        false => FillKind::Concave,
                    };

                    // Mark all CONVEX curved triangles as removed,
                    // since they are OUTSIDE the solid triangle mesh.
                    self.removed[ictrl as usize] = convex;

                    // Generate the curve triangle...
                    gen_triangle(
                        curve,      // The three vertices.
                        [false; 3], // No edge anti-aliasing information.
                        fill,       // Convex VS. Concave
                        &mut self.buf
                    );

                } else {
                    continue
                }

            }

        }

        /// Ear-clipping triangulation for a single possibly concave polygon.
        fn tbody(&mut self, points: &[CurvePoint]) -> Result<(), ()> {

            let len = points.len();
            debug_assert!(len >= 3);

            // calculate initial ear state for every point

            for idx in 0..len {
                let indices = neightbours_unremoved(&self.removed, idx);
                let isear = triangle_is_ear(indices, points);
                self.ears[idx] = isear;
            }

            // Remove ears and recalculate neighbours to
            // incrementally generate the triangle mesh.

            let mut changes = false;
            let mut counter = 0;

            loop {

                if counter < len {
                    // Increment the counter:
                    counter += 1;
                } else {
                    // To avoid looping infinitely on bad input, we emit an
                    // error if after a full cycle there were no changes.
                    if !changes { return Err(()) }
                    // Reset the counter:
                    changes = false;
                    counter = 1;
                }

                let idx = counter - 1;

                // Skip all removed points.
                if self.removed[idx] {
                    continue
                }

                if self.ears[idx] {

                    let indices @ [ia, ib, ic] = neightbours_unremoved(&self.removed, idx);
                    let abc = indices.map(|it| MathPoint::from(points[it]));

                    // If only two points are left
                    // we have finished successfully.
                    if ia == ic {
                        break
                    };

                    // we do not generate verticies for zero-area triangles
                    if triangle_area(abc).abs() > 0.0 {

                        // Compute which edges are on the outside of the shape.
                        // Only these are analytically anti-aliased in the shader.
                        let mut outers = [false; 3];
                        let edges = [[ia, ib], [ib, ic], [ic, ia]];
                        for (idx, edge) in edges.iter().enumerate() {
                            outers[idx] = edge_is_outer(*edge, points);
                        }

                        // Generate the filled inner triangle.
                        gen_triangle(
                            abc, // The three vertices.
                            outers, // Edge anti-aliasing information.
                            FillKind::Filled,
                            &mut self.buf
                        );

                    }

                    // Mark the middle point as removed.
                    self.removed[ib] = true;

                    // Recalculate left neightbour.
                    let indices = neightbours_unremoved(&self.removed, ia);
                    let ear = triangle_is_ear(indices, points);
                    self.ears[ia] = ear;

                    // Recalculate right neightbour.
                    let indices = neightbours_unremoved(&self.removed, ic);
                    let ear = triangle_is_ear(indices, points);
                    self.ears[ic] = ear;

                    changes = true;

                }

            }

            Ok(())

        }

    }

    /// Generate three vertices which make up either a normal or curve triangle.
    ///
    /// Param `outers` specifies edges in this order: [AB, BC, CA].
    fn gen_triangle(points: [MathPoint; 3], outers: [bool; 3], fill: FillKind, out: &mut Vec<PartialVertex>) {

        let edges = ((outers[0] as u8) << 2) |
                    ((outers[1] as u8) << 1) |
                    ((outers[2] as u8) << 0);

        for point in points {
            out.push(PartialVertex::new(PhysicalPoint::new(point.x as i16, point.y as i16), fill, edges));
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
    fn neightbours_unremoved(removed: &Vec<bool>, idx: usize) -> [usize; 3] {

        // TODO: Very inefficient right now...
        // TODO: To optimize this we could store a neightbour mapping for every index.

        let len = removed.len(); // removed.len() == polygon.len()

        #[cfg(debug_assertions)]
        {
            let mut count = 0;
            for idx in 0..len { if removed[idx] { count += 1 } }
            assert!(len > 2, "`neighbours` called with < 3 elements");
            assert!(count <= len - 2, "`neighbtbours` called with < 2 elements alive (just {} out of {:?})", len - count, removed);
        }

        let mut indices: [usize; 3] = [0; 3];

        // the point we are concerned about
        indices[1] = idx;

        // the right neighbour
        let mut counter = idx + 1;
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
            if !removed[counter as usize] { break }
            counter -= 1;
        }

        indices[0] = counter as usize;

        indices

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

    pub fn splitquadratic([a, b, c]: [MathPoint; 3], t: f32) -> [[MathPoint; 3]; 2] {
        let q1 = lerp(a, b, t);
        let q2 = lerp(b, c, t);
        let r0 = lerp(q1, q2, t);
        [[a, q1, r0], [r0, q2, c]]
        //  curve1       curve2
    }

    pub fn splitquadratic3(abc: [MathPoint; 3], t1: f32, t2: f32) -> [[MathPoint; 3]; 3] {
        let [x, y] = splitquadratic(abc, t1);
        let [y, z] = splitquadratic(y, t2);
        [x, y, z]
    }

    fn splitquadratic4(abc: [MathPoint; 3]) -> [[MathPoint; 3]; 4] {
        let [x, y] = splitquadratic(abc, 0.5);
        let [p, q] = splitquadratic(x, 0.5);
        let [r, s] = splitquadratic(y, 0.5);
        [p, q, r, s]
    }

    fn triangle_is_ear(indices: [usize; 3], polygon: &[CurvePoint]) -> bool {

        let abc = indices.map(|it| MathPoint::from(polygon[it]));

        // A. short curcuit if the triangle has zero area.
        //
        // We treat weird geometry with zero sized triangles as valid.
        let area = triangle_area(abc);
        if area < 1e-6 { // Account for precision errors.
            return true
        }

        // B. short curcuit if it is concave.
        let convex = triangle_is_convex(abc);
        if !convex {
            return false
        }

        // C. otherwise test for any intersections.
        for (pidx, point) in polygon.iter().enumerate() {

            if !indices.contains(&pidx) && // dont include the points that make up the ear
                triangle_intersects_point(abc, MathPoint::from(*point))
            {
                return false
            }

        };

        true

    }


    /// Check if the three points are convex, assuming counter clockwise orientation.
    fn triangle_is_convex(neighbours: [MathPoint; 3]) -> bool {

        let [a, b, c] = neighbours;

        let ba = [a.x as i32 - b.x as i32, a.y as i32 - b.y as i32];
        let bc = [c.x as i32 - b.x as i32, c.y as i32 - b.y as i32];

        let cross = bc[0] * ba[1] - bc[1] * ba[0];

        cross > 0

    }

    /// Area of the triangle ABC.
    // TODO: could use signed area (remove "abs") to also get the convexity from this
    fn triangle_area([a, b, c]: [MathPoint; 3]) -> f32 {
        (((b.x - a.x) as f32 * (c.y - a.y) as f32 -
        (c.x - a.x) as f32 * (b.y - a.y) as f32).abs()) * 0.5
    }

    fn edge_is_outer([x, y]: [usize; 2], points: &[CurvePoint]) -> bool {
        // Check if the points are direct neightbours and NOT part of a curve triangle.
        let diff = x.abs_diff(y);
        (diff == 1 || diff == points.len() - 1) &&
        points[x].kind() != PointKind::Ctrl &&
        points[y].kind() != PointKind::Ctrl

    }


    #[test]
    fn neighbours() {

        let mut bits = Vec::<bool>::new();
        bits.resize(10, false);

        assert_eq!(neightbours_unremoved(&bits, 4), [3, 4, 5]);
        assert_eq!(neightbours_unremoved(&bits, 0), [9, 0, 1]);
        assert_eq!(neightbours_unremoved(&bits, 9), [8, 9, 0]);

    }

}
