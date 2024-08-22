
//! Ear-clipping triangulation on the cpu.

use std::{f32::consts::PI, fmt, error::Error as StdError};
use bv::BitVec;
use common::Size;

use crate::{CurveGeometry, CurvePoint, GlPoint};

#[derive(Debug, PartialEq, Eq)]
pub enum TriagError {
    TooShort,
    TwoControl,
    Invalid,
}

impl fmt::Display for TriagError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooShort => write!(f, "input too short, need at least 3 points to triangulate"),
            Self::TwoControl=> write!(f, "two control points next to each other are not allowed"),
            Self::Invalid => write!(f, "invalid input, only non self-intersecting, counter-clockwise ordered polygons are accepted"),
        }
    }
}

impl StdError for TriagError {}

/// Output after processing a polygon.
pub struct OutputGeometry<'a> {
    /// triangles are fully filled
    pub basic: &'a [Triangle],
    /// triangles should be rendered as a beziér curve
    pub curved: &'a [Triangle], // TODO: easy: optimize this and return indices + vertices
}

/// The position is in normalized device coordinates.
#[derive(Debug, Clone)]
#[repr(packed)]
pub struct Triangle {
    pub a: GlPoint,
    pub b: GlPoint,
    pub c: GlPoint,
}

pub struct Triangulator { // TODO: make pub(crate)
    // window size
    size: Size,
    // state during triangulation
    ears: BitVec<usize>,
    removed: BitVec<usize>,
    // outputs
    basic: Vec<Triangle>,
    curved: Vec<Triangle>,
}

impl Triangulator { // TODO: rename (maybe) since it does more then triangulating

    pub fn new(size: Size) -> Self {
        Self {
            size,
            ears: BitVec::new(),
            removed: BitVec::new(),
            basic: Vec::new(),
            curved: Vec::new(),
        }
    }

    pub fn resize(&mut self, size: Size) {
        self.size = size;
    }

    /// Prepares the polygons for rendering.
    ///
    /// # Processing Steps
    /// - Convert coordinates to OpenGl screen space.
    /// - Triangulate the polygon using ear-clipping, with some restrictions.
    /// - Output extra triangles for the curved parts.
    pub fn process<'s>(&'s mut self, geometry: &CurveGeometry) -> Result<OutputGeometry, TriagError> {

        // reset our state
        self.basic.clear();
        self.curved.clear();

        for shape in &geometry.shapes {
            let points = &geometry.points[shape.idx as usize..shape.len as usize];
            if points.len() < 3 { return Err(TriagError::TooShort) }
            self.triangulate(points)?; // writes to self.basic
            self.curves(points)?; // writes to self.curved
        }

        Ok(OutputGeometry {
            basic: &self.basic,
            curved: &self.curved,
        })
        
    }

    /// Calculate patch-on triangles that should be rendered as curves.
    ///
    /// Appends to `self.curved`.
    /// Accepts window- and returns normalized device coordinates.
    pub fn curves(&mut self, points: &[CurvePoint]) -> Result<(), TriagError> {

        let len = points.len();
        debug_assert!(len >= 3);

        // find all pairs which look like (basic - control - basic)
        // and make triangles out of then

        let mut idx = 0;
        loop {

            let [ia, ib, ic] = if idx == 0 {
                [len - 1, idx, 1]
            } else if idx == len - 1 {
                [len - 2, idx, 0]
            } else {
                [idx - 1, idx, idx + 1]
            };

            // println!("{}, {}, {}", points[ia].basic(), points[ib].basic(), points[ic].basic());

            // can't have two control points next to each other
            if (!points[ia].basic() && !points[ib].basic()) ||
               (!points[ib].basic() && !points[ic].basic()) {
                return Err(TriagError::TwoControl)
            }

            // find the relevant pairs
            else if  points[ia].basic() &&
                    !points[ib].basic() &&
                     points[ic].basic() {

                self.curved.push(Triangle {
                    a: points[ia].gl(self.size),
                    b: points[ib].gl(self.size),
                    c: points[ic].gl(self.size),
                });

            }
            
            idx += 1;
            if idx == len { break };
            
        }
        
        Ok(())
        
    }

    /// Ear-clipping triangulation for a single polygon.
    ///
    /// Appends to `self.basic`.
    /// Accepts window- and returns normalized device coordinates.
    ///
    /// The algorithms is purposely written in a way that is similar to the
    /// compute shader implementation.
    // TODO(DOC): link to docs on the triangulation compute shader
    fn triangulate(&mut self, points: &[CurvePoint]) -> Result<(), TriagError> {
        
        // TODO: split polygons with > 1000 vertices into sub-polygons for the gpu

        let len = points.len();
        debug_assert!(len >= 3);

        // reset our state

        self.ears.clear();
        self.removed.clear();

        self.ears.resize(len as u64, false);
        self.removed.resize(len as u64, false);

        // calculate initial ear state for every point

        for idx in 0..len {
            let ear = Self::ear(points, &self.removed, idx);
            self.ears.set(idx as u64, ear);
        }

        // mark all convex triangles that include a control
        // point as removed, as we render their triangles as `curved`

        let mut idx = 0;
        loop {

            let [ia, ib, ic] = if idx == 0 {
                [len - 1, idx, 1]
            } else if idx == len - 1 {
                [len - 2, idx, 0]
            } else {
                [idx - 1, idx, idx + 1]
            };

            // can't have two control points next to each other
            if (!points[ia].basic() && !points[ib].basic()) ||
               (!points[ib].basic() && !points[ic].basic()) {
                return Err(TriagError::TwoControl)
            }

            // find the relevant pairs
            else if  points[ia].basic() &&
                    !points[ib].basic() &&
                     points[ic].basic() {

                self.removed.set(ib as u64, true);
                
            }
            
            idx += 1;
            if idx == len { break };
            
        }
        
        // remove ears and recalculate neightbours
        
        let mut changes = false; // used to check for errors
        let mut counter = 0;
        loop {

            if counter < len {
                counter += 1;
            } else {
                if !changes { return Err(TriagError::Invalid) };
                changes = false;
                counter = 1;
            }

            let idx = counter - 1;
            
            // skip all removed points
            // triangles should include control points though
            if self.removed[idx as u64] {
                continue
            }

            if self.ears[idx as u64] {

                let [ia, ib, ic] = Self::neightbours(&self.removed, idx);
                if ia == ic { // only two points were left
                    break
                };

                self.basic.push(Triangle {
                    a: points[ia].gl(self.size),
                    b: points[ib].gl(self.size),
                    c: points[ic].gl(self.size),
                });

                // mark the point as self.removed
                self.removed.set(ib as u64, true);

                // recalculate the neighbors
                let ear = Self::ear(points, &self.removed, ia);
                self.ears.set(ia as u64, ear);
                let ear = Self::ear(points, &self.removed, ic);
                self.ears.set(ic as u64, ear);

                changes = true;

            }

        }

        Ok(())

    }

    /// Behaviour is unspecified if all indices are marked as removed.
    pub(self) fn neightbours(removed: &BitVec, idx: usize) -> [usize; 3] {
        
        let len = removed.len(); // removed.len() == polygon.len()

        #[cfg(debug_assertions)]
        {
            let mut count = 0;
            for idx in 0..len { if removed[idx] { count += 1 } }
            assert!(len > 2, "`neightbours` called with < elements");
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

    /// Check if the point at `idx` is an ear.
    /// Y-flipped version.
    fn ear(polygon: &[CurvePoint], removed: &BitVec, idx: usize) -> bool {

        let [ia, ib, ic] = Self::neightbours(removed, idx);
        let [a, b, c] = [polygon[ia], polygon[ib], polygon[ic]];

        let ba = [a.x() as i32 - b.x() as i32, -(a.y() as i32 - b.y() as i32)];
        let bc = [c.x() as i32 - b.x() as i32, -(c.y() as i32 - b.y() as i32)];
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

        let mut intersects = false;
        for point in polygon.iter() {
            if [a, b, c].contains(point) { continue };
            intersects |= Self::intersects([a, b, c], *point)
        }

        !intersects && angle < 180.0 // TODO: can precicion errors mess this up so that we go into an infinite loop later? prevent infinite looping!

    }

    /// Area of the triangle ABC.
    /// Y-flipped version.
    fn area(a: CurvePoint, b: CurvePoint, c: CurvePoint) -> f32 {
        ((a.x() as f32 * (c.y() as f32 - b.y() as f32) +
          b.x() as f32 * (a.y() as f32 - c.y() as f32) +
          c.x() as f32 * (b.y() as f32 - a.y() as f32)) / 2.0).abs()
    //                  ^^^ the subtracion is fipped to account for the y-flip
    }

    /// If `point` lies within the triangle `trig`.
    /// Y-flipped version.
    fn intersects(trig: [CurvePoint; 3], point: CurvePoint) -> bool {

        let abc = Self::area(trig[0], trig[1], trig[2]);

        let pab = Self::area(point, trig[0], trig[1]);
        let pbc = Self::area(point, trig[1], trig[2]);
        let pca = Self::area(point, trig[2], trig[0]);
    
        let total = pab + pbc + pca;

        // using a small epsilon to account for floating-point precision errors
        (total - abc).abs() < 1e-6

    }

}

#[test]
fn neightbours() {

    let mut bits = BitVec::<usize>::new();
    bits.resize(10, false);

    assert_eq!(
        Triangulator::neightbours(&bits, 4),
        [3, 4, 5]
    );

    assert_eq!(
        Triangulator::neightbours(&bits, 0),
        [9, 0, 1]
    );

    assert_eq!(
        Triangulator::neightbours(&bits, 9),
        [8, 9, 0]
    );

}

#[test]
fn convex() {

    let points: &mut [CurvePoint] = &mut [
        CurvePoint::base(0, 0),
        CurvePoint::base(10, 0),
        CurvePoint::base(15, 5),
        CurvePoint::base(20, 10),
        CurvePoint::base(15, 15),
        CurvePoint::base(10, 20),
        CurvePoint::base(0, 20),
    ];

    points.reverse();

    let mut state = Triangulator::new(Size { width: 20, height: 20 });
    state.triangulate(points).unwrap();

    dbg!(&state.basic);

}

#[test]
fn curves() {

    let points: &mut [CurvePoint] = &mut [
        CurvePoint::base(10, 10),
        CurvePoint::control(20, 20),
        CurvePoint::control(20, 20),
        CurvePoint::base(10, 10),
    ];

    let mut state = Triangulator::new(Size { width: 20, height: 20 });
    let result = state.curves(points);

    assert_eq!(
        result,
        Err(TriagError::TwoControl)
    );


}

