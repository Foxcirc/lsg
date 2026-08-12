
use crate::*;
use std::ops;

/// A point, specified in physical coordinates.
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PhysicalPair {
    pub x: i16,
    pub y: i16,
}

impl PhysicalPair {
    pub const ZERO: Self = Self::new(0, 0);
    pub const MAX: Self = Self::new(i16::MAX, i16::MAX);
    pub const MIN: Self = Self::new(-i16::MAX, -i16::MAX);
    pub const fn new(x: i16, y: i16) -> Self { Self { x, y } }
    pub const fn new2([x, y]: [i16; 2]) -> Self { Self { x, y } }
    pub const fn scale(&self, factor: f64) -> PhysicalPair {
        PhysicalPair {
            x: (self.x as f64 * factor).round() as i16,
            y: (self.y as f64 * factor).round() as i16,
        }
    }
}

impl From<MeasuredPair> for PhysicalPair {
    fn from(it: MeasuredPair) -> Self {
        Self::new(it.x, it.y)
    }
}

impl From<CurvePoint> for PhysicalPair {
    fn from(it: CurvePoint) -> Self {
        Self::new(it.x(), it.y())
    }
}

pub type PhysicalPoint = PhysicalPair;
pub type PhysicalSize  = PhysicalPair;

#[derive(Default, Debug, Clone, Copy)]
pub enum Measure {
    #[default]
    Absolute,
    Relative,
}

#[derive(Default, Debug, Clone, Copy)]
pub struct MeasuredNumber {
    pub v: i16,
    pub mv: Measure
}

#[derive(Default, Debug, Clone, Copy)]
pub struct MeasuredPair {
    pub x: i16,
    pub y: i16,
    pub mx: Measure,
    pub my: Measure
}

impl MeasuredPair {
    pub const ZERO: Self = Self::new(abs(0), abs(0));
    pub const fn new(x: MeasuredNumber, y: MeasuredNumber) -> Self {
        Self { x: x.v, mx: x.mv, y: y.v, my: y.mv }
    }
}

pub type MeasuredPoint = MeasuredPair;
pub type MeasuredSize  = MeasuredPair;

/// A mathematical point.
#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct MathPoint {
    pub x: f32,
    pub y: f32,
}

impl MathPoint {
    pub const ZERO: Self = Self::new(0.0, 0.0);
    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    pub fn xy(&self) -> [f32; 2] {
        [self.x, self.y]
    }

}

impl From<PhysicalPair> for MathPoint {
    fn from(value: PhysicalPair) -> Self {
        Self::new(value.x as f32, value.y as f32)
    }
}

/// Convert discarding curve information.
impl From<CurvePoint> for MathPoint {
    fn from(value: CurvePoint) -> Self {
        Self::new(value.x() as f32, value.y() as f32)
    }
}

impl ops::Mul<f32> for MathPoint {
    type Output = MathPoint;
    fn mul(self, rhs: f32) -> Self::Output {
        Self::new(self.x * rhs, self.y * rhs)
    }
}

impl ops::Add<MathPoint> for MathPoint {
    type Output = MathPoint;
    fn add(self, rhs: MathPoint) -> Self::Output {
        Self::new(self.x + rhs.x, self.y + rhs.y)
    }
}

/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct PhysicalRect {
    pub point: PhysicalPair,
    pub size: PhysicalPair,
}

impl PhysicalRect {
    pub const MAX: Self = Self::new(PhysicalPair::MIN, PhysicalPair::MAX);
    pub const ZERO: Self = Self::new(PhysicalPair::ZERO, PhysicalPair::ZERO);
    pub const fn new(point: PhysicalPair, size: PhysicalPair) -> Self {
        Self { point, size }
    }
    pub const fn new2(x: i16, y: i16, w: i16, h: i16) -> Self {
        Self { point: PhysicalPair::new(x, y), size: PhysicalPair::new(w, h) }
    }
    pub fn xmin(&self) -> i16 { self.point.x }
    pub fn xmax(&self) -> i16 { self.point.x + self.size.x }
    pub fn ymin(&self) -> i16 { self.point.y }
    pub fn ymax(&self) -> i16 { self.point.y + self.size.y }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct MeasuredRect {
    pub point: MeasuredPoint,
    pub size: MeasuredSize
}

impl MeasuredRect {
    pub const ZERO: Self = Self {
        point: MeasuredPoint::ZERO,
        size: MeasuredSize::ZERO
    };
}

impl From<PhysicalRect> for MeasuredRect {
    fn from(it: PhysicalRect) -> Self {
        Self {
            point: MeasuredPoint::new(abs(it.point.x), abs(it.point.y)),
            size:  MeasuredSize::new(abs(it.size.x), abs(it.size.y)),
        }
    }
}

/// Utility to return a number with Measure::Absoulte.
pub const fn abs(v: i16) -> MeasuredNumber {
    MeasuredNumber { v, mv: Measure::Absolute }
}

/// Utility to return a number with Measure::Relative.
///
/// Also see: [`percent`].
pub const fn rel(v: i16) -> MeasuredNumber {
    MeasuredNumber { v, mv: Measure::Relative }
}

/// Utility to return a number with Measure::Relative.
///
/// This takes a percent-scale number as f32, which it
/// will convert to the permyriad-scale used by the api.
///
/// Also see: [`rel`].
pub const fn percent(val: f32) -> MeasuredNumber {
    rel((val * 100.0) as i16)
}

/// Computes value * scale, but using units per 10,000.
///
/// # Example Conversion
/// +-----------+-----------+-----------+-----------+
/// | Regular % | 25%       | 50%       | 12,5%     |
/// +-----------+-----------+-----------+-----------+
/// | Our Units | 2,500     | 5,000     | 1,250     |
/// +-----------+-----------+-----------+-----------+
pub const fn rescale(value: i16, scale: i16) -> i16 {
    ((value as isize * scale as isize) / 10000isize) as i16
}
