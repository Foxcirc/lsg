
//! Ear-clipping triangulation on the cpu.

use std::{f32::consts::PI, fmt, error::Error as StdError};
use bv::BitVec;

use crate::Vertex;

#[derive(Debug, PartialEq, Eq)]
pub enum TriagError {
    TooShort,
    Invalid,
}

impl fmt::Display for TriagError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooShort => write!(f, "input too short, need at least 3 points to triangulate"),
            Self::Invalid => write!(f, "invalid input state, only non self-intersecting, counter-clockwise ordered polygons are accepted"),
        }
    }
}

impl StdError for TriagError {}

pub struct Triangulator { // TODO: make pub(crate)
    /// stores if a vertex is an ear
    ears: BitVec<usize>,
    /// stores if a vertex was removed
    removed: BitVec<usize>,
    /// stores the output triangles
    trigs: Vec<Vertex>,
}

impl Triangulator {

    pub fn new() -> Self {
        Self {
            ears: BitVec::new(),
            removed: BitVec::new(),
            trigs: Vec::new(),
        }
    }

    pub fn triangulate<'s>(&'s mut self, polygon: &[Vertex]) -> Result<&'s [Vertex], TriagError> {
        
        // TODO: split polygons with > 1000 vertices into sub-polygons for the gpu

        let len = polygon.len();

        if len < 3 {
            return Err(TriagError::TooShort)
        }

        // reset our state

        self.ears.clear();
        self.removed.clear();
        self.trigs.clear();

        self.ears.resize(len as u64, false);
        self.removed.resize(len as u64, false);

        // calculate initial ear state for every point

        for idx in 0..len {
            let ear = Self::ear(polygon, &self.removed, idx);
            self.ears.set(idx as u64, ear);
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
            if self.removed[idx as u64] {
                continue
            }

            if self.ears[idx as u64] {

                let [ia, ib, ic] = Self::neightbours(&self.removed, idx);
                if ia == ic { // only two points were left
                    break
                };

                self.trigs.extend_from_slice(&[
                    polygon[ia],
                    polygon[ib],
                    polygon[ic],
                ]);

                // mark the point as self.removed
                self.removed.set(ib as u64, true);

                // recalculate the neighbors
                let ear = Self::ear(polygon, &self.removed, ia);
                self.ears.set(ia as u64, ear);
                let ear = Self::ear(polygon, &self.removed, ic);
                self.ears.set(ic as u64, ear);

                changes = true;

            }

        }

        Ok(&self.trigs)

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
    fn ear(polygon: &[Vertex], removed: &BitVec, idx: usize) -> bool {

        let [ia, ib, ic] = Self::neightbours(removed, idx);
        let [a, b, c] = [polygon[ia], polygon[ib], polygon[ic]];

        let ba = [a.x as i32 - b.x as i32, a.y as i32 - b.y as i32];
        let bc = [c.x as i32 - b.x as i32, c.y as i32 - b.y as i32];
    
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
    fn area(a: Vertex, b: Vertex, c: Vertex) -> f32 {
        ((a.x as i32 * (b.y as i32 - c.y as i32) +
          b.x as i32 * (c.y as i32 - a.y as i32) +
          c.x as i32 * (a.y as i32 - b.y as i32)) as f32 / 2.0).abs()
    }

    /// If `point` lies within the triangle `trig`.
    fn intersects(trig: [Vertex; 3], point: Vertex) -> bool {

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

    let points: &mut [Vertex] = &mut [
        Vertex { x: 0, y: 0 },
        Vertex { x: 10, y: 0 },
        Vertex { x: 15, y: 5 },
        Vertex { x: 20, y: 10 },
        Vertex { x: 15, y: 15 },
        Vertex { x: 10, y: 20 },
        Vertex { x: 0, y: 20 },
    ];

    let mut state = Triangulator::new();
    let result = state.triangulate(points);

    assert_eq!(
        result,
        Ok(&[Vertex { x: 0 , y: 20 }, Vertex { x: 0 , y: 0  }, Vertex { x: 10, y: 0  },
          Vertex { x: 0 , y: 20 }, Vertex { x: 10, y: 0  }, Vertex { x: 15, y: 5  },
          Vertex { x: 0 , y: 20 }, Vertex { x: 15, y: 5  }, Vertex { x: 20, y: 10 },
          Vertex { x: 0 , y: 20 }, Vertex { x: 20, y: 10 }, Vertex { x: 15, y: 15 },
          Vertex { x: 0 , y: 20 }, Vertex { x: 15, y: 15 }, Vertex { x: 10, y: 20 }] as &[Vertex])
    )

}

#[test]
fn misc() {

    let points: &mut [Vertex] = &mut [
        Vertex { x: 183, y: 66 },
        Vertex { x: 502, y: 126},
        Vertex { x: 167, y: 214 },
    ];

    let mut state = Triangulator::new();
    let result = state.triangulate(points).unwrap();

    dbg!(result);

}

