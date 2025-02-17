
/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Rect {
    pub pos: TodoFuckMePoint,
    pub size: Size,
}

impl Rect {
    pub const INFINITE: Self = Self::new(TodoFuckMePoint::ORIGIN, Size::INFINITE);
    pub const fn new(pos: TodoFuckMePoint, size: Size) -> Self {
        Self { pos, size }
    }
    pub const fn new2(x: i32, y: i32, w: u32, h: u32) -> Self {
        Self { pos: TodoFuckMePoint::new(x, y), size: Size::new(w, h) }
    }
}

#[derive(Debug, Default, Clone, Copy,PartialEq, Eq)]
pub struct Size {
    pub w: u32,
    pub h: u32
}

impl Size {
    pub const INFINITE: Self = Self::new(u32::MAX, u32::MAX);
    pub const fn new(w: u32, h: u32) -> Self { Self { w, h } }
}

/// A point in physical window coordinates.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct TodoFuckMePoint { // TODO: remove Point and make this the default and clean up the mess in egl/shaper (sometimes using float sometimes curvePoint sometimes I think why am I doing this)
    pub x: i32,
    pub y: i32,
}

impl TodoFuckMePoint {
    pub const ORIGIN: Self = Self::new(0, 0);
    pub const fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

/// A point in physical window coordinates.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Point {
    pub x: f32, // TODO: use i16?
    pub y: f32,
}

impl Point {
    pub const ORIGIN: Self = Self::new(0.0, 0.0);
    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
}


/// Area of a window that has to be redrawn.
pub struct Damage<'s> { // TODO: move to desktop?
    /// Empty means full damage.
    pub rects: &'s [Rect],
}

impl<'s> Damage<'s> {
    /// Everything will be redrawn.
    pub fn all() -> Self {
        Self { rects: &[] }
    }
    /// Only the marked rects should be redrawn.
    /// This is only an optimization and the system may choose
    /// to redraw more parts of the window.
    pub fn partial(rects: &'s [Rect]) -> Self {
        Self { rects }
    }
}
