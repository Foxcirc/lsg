
//! Ear-clipping triangulation on the cpu.

use std::f32::consts::PI;
use bv::BitVec;
use common::Size;

use crate::{CurveGeometry, CurvePoint, Instance};

// TODO: make all u16 be i16 and enforce it to pe positive or enforce all u16's to be in range 0..32K (i16::MAX) or use another type

/// Output after processing a polygon.
pub struct OutputGeometry<'a> {
    pub singular: SingularOutputGeometry<'a>,
    pub instanced: InstancedOutputGeometry<'a>,
    /// if there was an error in any of the input shapes.
    /// shapes with an error are discarded, but all others are still
    /// processed normally
    pub errornous: bool,
}

impl<'a> OutputGeometry<'a> {
    pub fn check(&self) -> Result<(), &'static str> {
        match self.errornous {
            false => Ok(()),
            true => Err("prepocesor input contained invalid shapes"),
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

pub struct Triangulator { // TODO: make pub(crate)
    size: Size,
    // state during triangulation
    ears: BitVec<usize>,
    removed: BitVec<usize>,
    // outputs
    singular: SingularData,
    instanced: InstancedData,
    errornous: bool,
}

impl Triangulator { // TODO: rename to Preprocessor (maybe) since it does more then triangulating

    pub fn new() -> Self {
        Self {
            size: Size { w: 0, h: 0 },
            ears: BitVec::new(),
            removed: BitVec::new(),
            singular: SingularData { vertices: Vec::new() },
            instanced: InstancedData { vertices: Vec::new(), instances: Vec::new(), commands: Vec::new() },
            errornous: false,
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
        self.errornous = false;

        // append basic triangles
        for shape in &geometry.shapes {

            // geometry is valid up to this index
            let start = [
                self.singular.vertices.len(),
                self.instanced.vertices.len(),
            ];

            let range = shape.polygon();
            let points = geometry.points.get(range.start as usize .. range.end as usize)
                .unwrap_or_default();

            let range = shape.instances();
            let instances = geometry.instances.get(range.start as usize .. range.end as usize)
                .unwrap_or_default();

            let singular = shape.kind();
            if let Ok(..) = self.shape(points, instances, singular) {
                if !singular {
                    for it in instances {
                        self.instanced.instances.extend([
                            it.pos[0], it.pos[1], it.pos[2], // offsetX, offsetY, z
                            it.texture[0], it.texture[1], it.texture[2],
                        ]);
                        self.instanced.commands.push(gl::DrawArraysIndirectCommand::new(
                            (self.instanced.vertices.len() - start[1]) / 4, // vertex count
                            instances.len(), // instance count
                            start[1] / 4, // start index
                        ))
                    }
                }
            } else {
                // restore the valid geometry
                self.singular.vertices.truncate(start[0]);
                self.instanced.vertices.truncate(start[1]);
                // set the flag
                self.errornous = true;
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
            errornous: self.errornous,
        }
        
    }

    pub fn shape(&mut self, points: &[CurvePoint], instances: &[Instance], singular: bool) -> Result<(), ()> {
        if points.len() < 3 || instances.len() == 0 {
            Err(())
        } else {
            self.triangulate(points, instances, singular)?;
            self.curves(points, instances, singular)?;
            Ok(())
        }
    }

    /// Calculate patch-on triangles that should be rendered as curves.
    ///
    /// Accepts window- and returns normalized device coordinates.
    pub fn curves(&mut self, points: &[CurvePoint], instances: &[Instance], singular: bool) -> Result<(), ()> {

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

            // can't have two control points next to each other
            if (!points[ia].kind() && !points[ib].kind()) ||
               (!points[ib].kind() && !points[ic].kind()) {
                return Err(())
            }

            // find the relevant pairs
            else if  points[ia].kind() &&
                    !points[ib].kind() &&
                     points[ic].kind() {

                let [a, b, c] = [points[ia], points[ib], points[ic]];
                let [ga, gb, gc] = [a.gl(self.size), b.gl(self.size), c.gl(self.size)];

                let convex = Self::convex([a, b, c]);
                let uvs = if convex { // range 0.5 to 1.0 means convex (for the shader)
                    [0.5, 0.5, 0.75, 0.5, 1.0, 1.0]
                } else { // range 0.0 to 0.5 means concave (for the shader)
                    [0.0, 0.0, 0.25, 0.0, 0.5, 0.5]
                };

                if singular {
                    let i = &instances[0];
                    self.singular.vertices.extend([
                        ga.x + i.pos[0], ga.y + i.pos[1], i.pos[2], uvs[0], uvs[1], i.texture[0], i.texture[1], i.texture[2],
                        gb.x + i.pos[0], gb.y + i.pos[1], i.pos[2], uvs[2], uvs[3], i.texture[0], i.texture[1], i.texture[2],
                        gc.x + i.pos[0], gc.y + i.pos[1], i.pos[2], uvs[4], uvs[5], i.texture[0], i.texture[1], i.texture[2]
                    ]);
                } else {
                    self.instanced.vertices.extend([
                        ga.x, ga.y, uvs[0], uvs[1],
                        gb.x, gb.y, uvs[2], uvs[3],
                        gc.x, gc.y, uvs[4], uvs[5]
                    ]);
                }

            }
            
            idx += 1;
            if idx == len { break };
            
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
    fn triangulate(&mut self, points: &[CurvePoint], instances: &[Instance], singular: bool) -> Result<(), ()> {
        
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

            let [a, b, c] = [points[ia], points[ib], points[ic]];

            // can't have two control points next to each other
            if (!a.kind() && !b.kind()) ||
               (!b.kind() && !c.kind()) {
                return Err(())
            }

            // find the relevant pairs
            else if  points[ia].kind() &&
                    !points[ib].kind() &&
                     points[ic].kind() && 
                     Self::convex([a, b, c]) {

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
                if !changes { return Err(()) };
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

                let [ga, gb, gc] = [
                    points[ia].gl(self.size),
                    points[ib].gl(self.size),
                    points[ic].gl(self.size),
                ];

                if singular {
                    let i = &instances[0];
                    self.singular.vertices.extend([
                        /* pos */ ga.x + i.pos[0], ga.y + i.pos[1], i.pos[2], /* curve */ 1.0, 1.0, /* texture */ i.texture[0], i.texture[1], i.texture[2],
                        /* pos */ gb.x + i.pos[0], gb.y + i.pos[1], i.pos[2], /* curve */ 1.0, 1.0, /* texture */ i.texture[0], i.texture[1], i.texture[2],
                        /* pos */ gc.x + i.pos[0], gc.y + i.pos[1], i.pos[2], /* curve */ 1.0, 1.0, /* texture */ i.texture[0], i.texture[1], i.texture[2]
                    ]);
                } else {
                    self.instanced.vertices.extend([
                        /* pos */ ga.x, ga.y, /* curve */ 1.0, 1.0,
                        /* pos */ gb.x, gb.y, /* curve */ 1.0, 1.0,
                        /* pos */ gc.x, gc.y, /* curve */ 1.0, 1.0,
                    ]);
                }

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

    /// Check if the point at `idx` is an ear, accounting for removed neightbours.
    /// Y-flipped version.
    fn ear(polygon: &[CurvePoint], removed: &BitVec, idx: usize) -> bool {

        let [ia, ib, ic] = Self::neightbours(removed, idx);
        let neightbours = [polygon[ia], polygon[ib], polygon[ic]];

        // short curcuit if it is concave
        let convex = Self::convex(neightbours);
        if !convex {
            return false
        }

        let [a, b, c] = neightbours;

        let mut intersects = false;
        for point in polygon.iter() {
            if [a, b, c].contains(point) { continue };
            intersects |= Self::intersects([a, b, c], *point)
        }

        !intersects

    }


    /// Check if the three points are convex, assuming counter clockwise orientation.
    /// Y-flipped version.
    fn convex(neightbours: [CurvePoint; 3]) -> bool {

        let [a, b, c] = neightbours;

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

        angle < 180.0
        
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

    let instances = &[
        Instance { pos: [0.0, 0.0, 0.0], texture: [1.0, 1.0, 1.0] },
    ];

    let mut state = Triangulator::new();
    state.size = Size { w: 20, h: 20 };
    state.triangulate(points, instances, true).unwrap();

    dbg!(&state.singular.vertices);

}

#[test]
fn curves() {

    let points: &mut [CurvePoint] = &mut [
        CurvePoint::base(10, 10),
        CurvePoint::control(20, 20),
        CurvePoint::control(20, 20),
        CurvePoint::base(10, 10),
    ];

    let instances = &[
        Instance { pos: [0.0, 0.0, 0.0], texture: [1.0, 1.0, 1.0] },
    ];

    let mut state = Triangulator::new();
    state.size = Size { w: 20, h: 20 };
    let result = state.curves(points, instances, true);

    assert_eq!(
        result,
        Err(())
    );


}

