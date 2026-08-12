
//! This crate contains all the builtin widgets that come with lsg,
//! aswell as their required backend logic to e.g. parse input data.

pub mod catalogue;
pub use catalogue::*;

use std::{fmt, sync::Arc};

pub trait Widget {
    fn action(&self, cx: Context) -> Response;
    // fn query(&self, query: Query);
}

// NOTE: To make it possible to use `&dyn Widget` as a `W: Widget`,
//       this blanked implementation for references would need to be
//       provided but I cannot see a use case for this right now.
//
// impl<'a, T: ?Sized + Widget> Widget for &'a T {
//     fn action(&self, cx: Context) -> Response {
//         (**self).action(cx)
//     }
// }

// pub enum Query<'a> {
//     PreferredSize { out: &'a mut common::PhysicalSize },
//     WasDeleted { out: &'a mut bool },
// }

/// Capapilities that are accessible to widgets.
pub trait Caps {
    /// Redraw the window the widget is inside of.
    fn redraw(&self);
}

#[derive(Clone, Copy)]
pub struct Context<'a> {
    /// The action to handle.
    pub action: Action<'a>,
     /// A widgets location and dimensions in absolute coordinates.
     ///
     /// This is basically a widgets "bounding box". It is used to transform
     /// relative into absolute points and offset/scale child widgets.
    pub layout: common::PhysicalRect,
    /// Capapilities that are accessible to the widget.
    pub caps: &'a dyn Caps,
}

impl<'a> Context<'a> {

    pub fn child(self, rect: common::MeasuredRect, inner: &impl Widget) -> Response {

        let layout = self.transform(rect);

        if let Some(action) = self.action.child(layout) {
            inner.action(Self { layout, action, caps: self.caps })
        } else {
            Response::Bubble
        }
    }

    pub const fn width(&self) -> i16 {
        self.layout.size.x
    }

    pub const fn height(&self) -> i16 {
        self.layout.size.y
    }

    /// Transforms a possibly relative rect into an absolute one using the `layout`.
    pub fn transform(&self, input: common::MeasuredRect) -> common::PhysicalRect {

        let common::PhysicalRect { point: offset, size: scale } = self.layout;

        let point = common::PhysicalPoint {
            x: offset.x + match input.point.mx {
                common::Measure::Absolute => input.point.x,
                common::Measure::Relative => common::rescale(input.point.x, scale.x)
            },
            y: offset.y + match input.point.my {
                common::Measure::Absolute => input.point.y,
                common::Measure::Relative => common::rescale(input.point.y, scale.y)
            }
        };

        let size = common::PhysicalSize {
            x: match input.size.mx {
                common::Measure::Absolute => input.size.x,
                common::Measure::Relative => common::rescale(input.size.x, scale.x)
            },
            y: match input.size.my {
                common::Measure::Absolute => input.size.y,
                common::Measure::Relative => common::rescale(input.size.y, scale.y)
            }
        };

        common::PhysicalRect { point, size }

    }

}

#[derive(Debug, Clone, Copy)]
pub enum Response {
    Handeled,
    Bubble
}

impl Response {
    pub fn and(&mut self, other: Response) {
        if let Self::Handeled = other {
            *self = Response::Handeled
        }
    }
}

/*

Some cool ideas:

let rect = ForceResize::build().size((5000, 5000))
    .inner(Rect::build().color(Color::Red));

let rect = ForceResize::new((5000, 5000), "rect-red-round25")

let rect = ForceResize((5000, 5000), Rect().red().round25());

let rect = widget("ForceResize(5000x5000, Rect(red, round25))");

 */

 #[derive(Debug, Clone, Copy)]
pub enum Action<'a> {

    /// Render this widget.
    ///
    /// # Why is `out` inside a mutex?
    /// This could be a regular mutable reference, since there is no actual concurrency
    /// going on, but having a regular reference here makes live much easier.
    Render { out: &'a RenderOutput },

    MouseMotion { point: common::PhysicalPoint },
    MouseDown { point: common::PhysicalPoint, button: desktop::MouseButton },
    MouseUp { point: common::PhysicalPoint, button: desktop::MouseButton },
    MouseScroll { point: common::PhysicalPoint, delta: common::PhysicalPair },

    Unhover,
    Unfocus,

    KeyDown { key: desktop::Key, repeat: bool },
    KeyUp { key: desktop::Key },

    TextInput { chr: char },
    TextCompose { chr: char },
    TextComposeCancel,

}

impl<'a> Action<'a> {

    /// Determines if and how an action should propagate to a given child.
    ///
    /// The `layout` is the layout of the child. To be efficient, some actions
    /// may be determined to not cascade, in which case `None` is returned.
    pub fn child(self, layout: common::PhysicalRect) -> Option<Action<'a>> {

        match self {
            Self::Render { out } => Some(Action::Render { out }),
            Self::MouseMotion { point }     => if point_inside_rect(point, layout) { Some(self) } else { Some(Self::Unhover) },
            Self::MouseDown   { point, .. } => if point_inside_rect(point, layout) { Some(self) } else { Some(Self::Unfocus) },
            Self::MouseUp     { point, .. } => if point_inside_rect(point, layout) { Some(self) } else { Some(Self::Unfocus) },
            Self::MouseScroll { point, .. } => if point_inside_rect(point, layout) { Some(self) } else { Some(Self::Unhover) },
            Self::Unhover => Some(self),
            Self::Unfocus => Some(self),
            Self::KeyDown { .. }     => Some(self),
            Self::KeyUp   { .. }     => Some(self),
            Self::TextInput   { .. } => Some(self),
            Self::TextCompose { .. } => Some(self),
            Self::TextComposeCancel  => Some(self)
        }

    }

}

pub fn point_inside_rect(pt: common::PhysicalPoint, rect: common::PhysicalRect) -> bool {
    pt.x >= rect.point.x && pt.x <= rect.point.x + rect.size.x &&
    pt.y >= rect.point.y && pt.y <= rect.point.y + rect.size.y
}

#[derive(Default)]
pub struct VertexGeometries {
    pub inner: Vec<Arc<common::VertexGeometry>>
}

impl VertexGeometries {

    pub fn clear(&mut self) {
        self.inner.clear();
    }

    /// Add a geometry to the store, which can be referenced by instances later.
    pub fn add(&mut self, geometry: Arc<common::VertexGeometry>) -> u16 {
        self.inner.push(geometry);
        return (self.inner.len() - 1 + 1) as u16
        //                       ^^^^ because the first geometry is ours
    }

}

#[derive(Default)]
pub struct RenderOutput {
    pub inner: common::SmartMutex<RenderOutputInner>
}

#[derive(Default)]
pub struct RenderOutputInner {
    /// Widget-added geometries.
    pub geometries: VertexGeometries,
    /// Widget-added vertices.
    pub geometry: common::VertexGeometry,
    /// Ordered shape instances, which index
    /// into the various stored geometries.
    pub instances: Vec<common::Instance>,
}

impl RenderOutput {

    pub fn clear(&mut self) {
        let mut inner = self.inner.lock();
        inner.geometries.clear();
        inner.geometry.clear();
        inner.instances.clear();
    }

    pub fn addshape(&self, shape: &[common::PartialVertex]) -> u16 {
        let mut this = self.inner.lock();
        this.geometry.add(shape)
    }

    pub fn addgeometry(&self, geometry: Arc<common::VertexGeometry>) -> u16 {
        let mut this = self.inner.lock();
        this.geometries.add(geometry)
    }

    pub fn instance(&self, cx: Context, target: common::GeometryTarget, instance: Instance) {

        let mut this = self.inner.lock();

        let new = cx.transform(common::MeasuredRect {
            point: instance.pos,
            size: instance.size
        });

        let rawinstance = common::Instance {
            target,
            pos: new.point,
            size: new.size,
            texture: instance.texture,
        };

        this.instances.push(rawinstance);

    }

}

impl fmt::Debug for RenderOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let this = self.inner.lock();
        f.debug_struct("RenderOutput")
            .field("#instances", &this.instances.len())
            .finish_non_exhaustive()
    }
}

pub struct Instance {
    /// offsetX, offsetY
    pub pos: common::MeasuredPair,
    /// Size of the shape in logical pixels.
    pub size: common::MeasuredPair,
    // Texture information.
    pub texture: common::TextureKind,
}
