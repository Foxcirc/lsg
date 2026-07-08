
use common::SmartMutex;
use crate::*;

pub struct Placement<W: Widget> {
    pub rect: common::MeasuredRect,
    pub inner: W,
}

impl<W: Widget> Widget for Placement<W> {
    fn action(&self, layout: Layout, action: Action) {
        let clayout = layout.child(self.rect);
        if let Some(it) = action.cascade(clayout) {
            self.inner.action(clayout, it);
        }
    }
}

pub struct Many<W: Widget> {
    pub inner: SmartMutex<Vec<W>>,
}

impl<W: Widget> Many<W> {
    pub fn add(&self, w: W) {
        self.inner.lock().push(w);
    }
}

impl<W: Widget> Widget for Many<W> {
    fn action(&self, layout: Layout, action: Action) {
        for entry in &*self.inner.lock() {
            entry.action(layout, action);
        }
    }
}

pub struct Cols<W: Widget> {
    pub inner: SmartMutex<Vec<(common::MeasuredNumber, W)>>
}

impl<W: Widget> Cols<W> {
    pub fn new(inner: Vec<(common::MeasuredNumber, W)>) -> Self {
        Self { inner: SmartMutex::new(inner) }
    }
}

impl<W: Widget> Widget for Cols<W> {
    fn action(&self, layout: Layout, action: Action) {

        let mut offset: i16 = 0;

        for (width, widget) in &*self.inner.lock() {

            let clayout = layout.child(common::MeasuredRect {
                point: common::MeasuredPoint::new(common::abs(offset), common::abs(0)),
                size: common::MeasuredSize::new(*width, layout.height())
            });

            offset += clayout.width().v;

            if let Some(it) = action.cascade(layout) {
                widget.action(clayout, it);
            }

        }

    }
}

pub struct Rows<W: Widget> {
    pub inner: SmartMutex<Vec<(common::MeasuredNumber, W)>>
}

impl<W: Widget> Rows<W> {
    pub fn new(inner: Vec<(common::MeasuredNumber, W)>) -> Self {
        Self { inner: SmartMutex::new(inner) }
    }
}

impl<W: Widget> Widget for Rows<W> {
    fn action(&self, layout: Layout, action: Action) {

        let mut offset: i16 = 0;

        for (height, widget) in &*self.inner.lock() {

            let clayout = layout.child(common::MeasuredRect {
                point: common::MeasuredPoint::new(common::abs(0), common::abs(offset)),
                size: common::MeasuredSize::new(layout.width(), *height)
            });

            offset += clayout.height().v;

            if let Some(it) = action.cascade(layout) {
                widget.action(clayout, it);
            }

        }

    }
}
