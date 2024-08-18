
//! Ear-clipping triangulation on the cpu.

use std::f32::consts::PI;
use bv::BitVec;

pub struct Triangulator { // TODO: make pub(crate)
}

impl Triangulator {

    pub fn new() -> Self {
        Self { // TODO: reuse buffers
        }
    }

    pub fn triangulate(&mut self, polygon: &[[i32; 2]]) -> Vec<[i32; 2]> {

        // TODO: split polygons with > 100 vertices into sub-polygons on the gpu

        let len = polygon.len();

        // 1. calculate the angle for every edge

        let mut ears = BitVec::<usize>::new(); // stores if a vertex is an ear
        let mut removed = BitVec::<usize>::new(); // stores if a vertex was removed
        ears.resize(len as u64, false);
        removed.resize(len as u64, false);

        for idx in 0..len {
            let ear = Self::ear(polygon, &removed, idx);
            ears.set(idx as u64, ear);
        }

        let mut trigs: Vec<[i32; 2]> = Vec::new();
        
        let mut counter = 0;
        loop {

            if counter < len {
                counter += 1;
            } else {
                counter = 1;
            }

            let idx = counter - 1;
            
            // skip all removed vertices
            if removed[idx as u64] {
                continue
            }

            // std::thread::sleep_ms(100);

            if ears[idx as u64] {

                let [ia, ib, ic] = Self::neightbours(&removed, idx);
                if ia == ic { // only two points were left
                    break
                };

                trigs.extend_from_slice(&[
                    polygon[ia],
                    polygon[ib],
                    polygon[ic],
                ]);

                // mark the point as removed
                removed.set(ib as u64, true);

                // recalculate the neighbors
                let ear = Self::ear(polygon, &removed, ia);
                ears.set(ia as u64, ear);
                let ear = Self::ear(polygon, &removed, ic);
                ears.set(ic as u64, ear);

            }

        }

        trigs

    }

    /// Behaviour is unspecified if all indices are marked as removed.
    pub(self) fn neightbours(removed: &BitVec, idx: usize) -> [usize; 3] {
        
        let len = removed.len(); // removed.len() == polygon.len()

        #[cfg(debug_assertions)]
        {
            let mut count = 0;
            for idx in 0..len {
                if removed[idx] { count += 1 }
            }
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
    fn ear(polygon: &[[i32; 2]], removed: &BitVec, idx: usize) -> bool {

        let [ia, ib, ic] = Self::neightbours(removed, idx);
        let [a, b, c] = [polygon[ia], polygon[ib], polygon[ic]];

        let ba = [a[0] - b[0], a[1] - b[1]];
        let bc = [c[0] - b[0], c[1] - b[1]];
    
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
            intersects |= Self::intersects([a, b, c], [point[0], point[1]])
        }

        !intersects && angle < 180.0

    }

    /// Area of the triangle ABC.
    fn area(a: [i32; 2], b: [i32; 2], c: [i32; 2]) -> f32 {
        ((a[0] * (b[1] - c[1]) +
          b[0] * (c[1] - a[1]) +
          c[0] * (a[1] - b[1])) as f32 / 2.0).abs()
    }

    /// If `point` lies within the triangle `trig`.
    fn intersects(trig: [[i32; 2]; 3], point: [i32; 2]) -> bool {

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

    let points: &mut [[i32; 2]] = &mut [
        [0, 0],
        [10, 0],
        [15, 5],
        [20, 10],
        [15, 15],
        [10, 20],
        [0, 20],
    ];

    let mut state = Triangulator::new();
    let result = state.triangulate(points);

    assert_eq!(
        &result,
        &[[0 , 20], [0 , 0 ], [10, 0 ],
          [0 , 20], [10, 0 ], [15, 5 ],
          [0 , 20], [15, 5 ], [20, 10],
          [0 , 20], [20, 10], [15, 15],
          [0 , 20], [15, 15], [10, 20]]
    )

}

