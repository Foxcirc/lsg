
//! This crate contains all the builtin widgets that come with lsg,
//! aswell as their required backend logic to e.g. parse input data.

pub mod catalogue;
pub use catalogue::*;

use common::SmartMutex;
use std::sync::Arc;

pub trait Widget {
    fn action(&self, layout: Layout, action: Action);
    // fn query(&self, query: Query);
}

// pub enum Query<'a> {
//     PreferredSize { out: &'a mut common::PhysicalSize },
//     WasDeleted { out: &'a mut bool },
// }

// #[derive(Clone, Copy)]
// pub struct ActionContext<'a> {
//     pub layout: Layout,
//     pub action: Action<'a>,
// }

// impl<'a> ActionContext<'a> {
//     pub fn child(self, rect: common::MeasuredRect) -> Option<Self> {
//         let layout = self.layout.child(rect);
//         let action = self.action.cascade(layout);
//         Some(Self { layout, ation })
//     }
// }

/*

Some cool ideas:

let rect = ForceResize::build().size((5000, 5000))
    .inner(Rect::build().color(Color::Red));

let rect = ForceResize::new((5000, 5000), "rect-red-round25")

let rect = ForceResize((5000, 5000), Rect().red().round25());

let rect = widget("ForceResize(5000x5000, Rect(red, round25))");

 */

 /// Stores a widgets location and dimensions in absolute coordinates.
 ///
 /// This is basically a widgets "bounding box". It is used to transform
 /// relative into absolute points and position and constrain child widgets.
 #[derive(Default, Clone, Copy)]
 pub struct Layout {
     pub bounds: common::PhysicalRect,
 }

 impl Layout {

     pub const ZERO: Self = Self { bounds: common::PhysicalRect::ZERO };

     pub const fn new(bounds: common::PhysicalRect) -> Self {
         Self { bounds }
     }

     pub const fn width(&self) -> i16 {
         self.bounds.size.x
     }

     pub const fn height(&self) -> i16 {
         self.bounds.size.y
     }

     /// Transforms a possibly relative rect into an absolute one.
     pub fn transform(&self, input: common::MeasuredRect) -> common::PhysicalRect {
         let common::PhysicalRect { point: offset, size: scale } = self.bounds;
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

     pub fn child(&self, rect: common::MeasuredRect) -> Self {
         Self { bounds: self.transform(rect) }
     }

 }

pub enum Action<'a> {

    Render { out: &'a mut RenderOutput },
    // RenderCached { space: Space<'a> }, // This would be used to render a singular widget using app.redraw(&mywidget), so the widgets would have to store their last space-parameters (rect) and then restore and render themselves with the same offset+size. If we would use widgets ID's this could be automatic if space.child stored the new widget id somehow, but I dont like mandatory widget ID's.

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

    /// Determines if and how an action should cascade to a given child.
    ///
    /// The `layout` is the layout of the child. To be efficient, some actions
    /// may be determined to not cascade, in which case `None` is returned.
    pub fn cascade<'x>(&'x mut self, layout: Layout) -> Option<Action<'x>> {

        let bounds = layout.bounds;

        match self {
            Self::Render { out } => Some(Action::Render { out }),
            &mut Self::MouseMotion { point }     => if point_inside_rect(point, bounds) { Some(self.same()) } else { Some(Self::Unhover) },
            &mut Self::MouseDown   { point, .. } => if point_inside_rect(point, bounds) { Some(self.same()) } else { Some(Self::Unfocus) },
            &mut Self::MouseUp     { point, .. } => if point_inside_rect(point, bounds) { Some(self.same()) } else { Some(Self::Unfocus) },
            &mut Self::MouseScroll { point, .. } => if point_inside_rect(point, bounds) { Some(self.same()) } else { Some(Self::Unhover) },
            &mut Self::Unhover => Some(self.same()),
            &mut Self::Unfocus => Some(self.same()),
            &mut Self::KeyDown { .. }     => Some(self.same()),
            &mut Self::KeyUp   { .. }     => Some(self.same()),
            &mut Self::TextInput   { .. } => Some(self.same()),
            &mut Self::TextCompose { .. } => Some(self.same()),
            &mut Self::TextComposeCancel  => Some(self.same())
        }

    }

    pub fn same<'x>(&'x mut self) -> Action<'x> {

        match self {
            Self::Render { out } => Action::Render { out },
            &mut Self::MouseMotion { point }         => Action::MouseMotion { point },
            &mut Self::MouseDown   { point, button } => Action::MouseDown   { point, button },
            &mut Self::MouseUp     { point, button } => Action::MouseUp     { point, button },
            &mut Self::MouseScroll { point, delta }  => Action::MouseScroll { point, delta },
            &mut Self::Unhover => Action::Unhover,
            &mut Self::Unfocus => Action::Unfocus,
            &mut Self::KeyDown { key, repeat } => Action::KeyDown { key, repeat },
            &mut Self::KeyUp   { key }         => Action::KeyUp   { key },
            &mut Self::TextInput   { chr }     => Action::TextInput   { chr },
            &mut Self::TextCompose { chr }     => Action::TextCompose { chr },
            &mut Self::TextComposeCancel       => Action::TextComposeCancel
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
        self.geometries.clear();
        self.geometry.clear();
        self.instances.clear();
    }

    pub fn instance(&mut self, layout: Layout, target: common::GeometryTarget, instance: Instance) {

        let new = layout.transform(common::MeasuredRect {
            point: instance.pos,
            size: instance.size
        });

        let inner = common::Instance {
            target,
            pos: new.point,
            size: new.size,
            texture: instance.texture,
        };

        self.instances.push(inner);

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
