
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
    fn action(&self, layout: Layout, action: Action) {
        match action {
            Action::Render { space } => {
                let key = space.data(Data::Curves(&[
                    common::CurvePoint::base(0,     0),
                    common::CurvePoint::base(10000, 0),
                    common::CurvePoint::base(10000, 10000),
                    common::CurvePoint::base(0,     10000)
                ]));
                space.instance(layout, key, Instance {
                    pos: common::MeasuredPoint::new(common::abs(0),     common::abs(0)),
                    size: common::MeasuredSize::new(common::rel(10000), common::rel(10000)),
                    texture: self.texture,
                });
            },
            _ => (),
        }
    }
}
