
use crate::*;

pub struct Rect {
    pub texture: common::TextureKind,
}

impl Rect {
    pub fn colored((r, g, b, a): (u8, u8, u8, u8)) -> Self {
       Self { texture: common::TextureKind::Color(r, g, b, a) }
    }
}

impl Widget for Rect {
    fn action(&self, cx: Context) -> Response {
        match cx.action {
            Action::Render { out } => {
                let shape = out.addshape(&[
                    // 1. triangle:
                    common::PartialVertex::new(common::PhysicalPoint::new(0,     0    ), common::FillKind::Filled, 0),
                    common::PartialVertex::new(common::PhysicalPoint::new(10000, 0    ), common::FillKind::Filled, 0),
                    common::PartialVertex::new(common::PhysicalPoint::new(10000, 10000), common::FillKind::Filled, 0),
                    // 2. triangle:
                    common::PartialVertex::new(common::PhysicalPoint::new(0,     0    ), common::FillKind::Filled, 0),
                    common::PartialVertex::new(common::PhysicalPoint::new(10000, 10000), common::FillKind::Filled, 0),
                    common::PartialVertex::new(common::PhysicalPoint::new(0,     10000), common::FillKind::Filled, 0),
                ]);
                out.instance(cx, common::GeometryTarget { geometry: 0, shape }, Instance {
                    pos: common::MeasuredPoint::new(common::abs(0),     common::abs(0)),
                    size: common::MeasuredSize::new(common::rel(10000), common::rel(10000)),
                    texture: self.texture,
                });
            },
            _ => (),
        };
        Response::Bubble
    }
}
