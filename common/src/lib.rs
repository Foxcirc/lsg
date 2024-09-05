
/// A rectangular region on a surface.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub w: i32,
    pub h: i32,
}

impl Rect {
    pub const INFINITE: Self = Self::new(0, 0, i32::MAX, i32::MAX);
    pub const fn new(x: i32, y: i32, w: i32, h: i32) -> Self {
        Self { x, y, w, h}
    }
}

#[derive(Debug, Default, Clone, Copy)]
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
    pub x: u32,
    pub y: u32
}

impl Pos {
    pub const ORIGIN: Pos = Pos { x: 0, y: 0 };
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

