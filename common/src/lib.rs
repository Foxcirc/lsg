
/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Rect {
    pub pos: Pos,
    pub size: Size,
}

impl Rect {
    pub const INFINITE: Self = Self::new(Pos::ORIGIN, Size::INFINITE);
    pub const fn new(pos: Pos, size: Size) -> Self {
        Self { pos, size }
    }
}

#[derive(Debug, Default, Clone, Copy,PartialEq, Eq)]
pub struct Size {
    pub w: u32,
    pub h: u32
}

impl Size {
    pub const INFINITE: Size = Size { w: u32::MAX, h: u32::MAX };
    pub fn new(w: u32, h: u32) -> Self { Self { w, h } }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Pos {
    pub x: i32,
    pub y: i32
}

impl Pos {
    pub const ORIGIN: Pos = Pos { x: 0, y: 0 };
}

/// A point in physical window coordinates.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Point {
    pub x: f32, // TODO: use i16?
    pub y: f32,
}

impl Point {
    pub fn new(x: f32, y: f32) -> Self {
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
