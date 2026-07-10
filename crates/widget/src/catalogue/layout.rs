
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

pub struct Cols2<W1: Widget, W2: Widget> {
    pub w1: SmartMutex<(common::MeasuredNumber, W1)>,
    pub w2: SmartMutex<(common::MeasuredNumber, W2)>,
}

impl<W1: Widget, W2: Widget> Cols2<W1, W2> {
    pub fn new(
        w1: (common::MeasuredNumber, W1),
        w2: (common::MeasuredNumber, W2)
    ) -> Self {
        Self {
            w1: SmartMutex::new(w1),
            w2: SmartMutex::new(w2)
        }
    }
}

impl<W1: Widget, W2: Widget> Widget for Cols2<W1, W2> {
    fn action(&self, layout: Layout, action: Action) {
        let mut offset: i16 = 0;
        implcol(layout, action, &*self.w1.lock(), &mut offset);
        implcol(layout, action, &*self.w2.lock(), &mut offset);
    }
}

pub struct Cols3<W1: Widget, W2: Widget, W3: Widget> {
    pub w1: SmartMutex<(common::MeasuredNumber, W1)>,
    pub w2: SmartMutex<(common::MeasuredNumber, W2)>,
    pub w3: SmartMutex<(common::MeasuredNumber, W3)>,
}

impl <W1: Widget, W2: Widget, W3: Widget> Cols3<W1, W2, W3> {
    pub fn new(
        w1: (common::MeasuredNumber, W1),
        w2: (common::MeasuredNumber, W2),
        w3: (common::MeasuredNumber, W3)
    ) -> Self {
        Self {
            w1: SmartMutex::new(w1),
            w2: SmartMutex::new(w2),
            w3: SmartMutex::new(w3)
        }
    }
}

impl<W1: Widget, W2: Widget, W3: Widget> Widget for Cols3<W1, W2, W3> {
    fn action(&self, layout: Layout, action: Action) {
        let mut offset: i16 = 0;
        implcol(layout, action, &*self.w1.lock(), &mut offset);
        implcol(layout, action, &*self.w2.lock(), &mut offset);
        implcol(layout, action, &*self.w3.lock(), &mut offset);
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

        for slot in &*self.inner.lock() {
            implcol(layout, action, slot, &mut offset);
        }

    }
}

fn implcol<W: Widget>(layout: Layout, action: Action, slot: &(common::MeasuredNumber, W), offset: &mut i16) {

    let (cwidth, cwidget) = slot;

    let clayout = layout.child(common::MeasuredRect {
        point: common::MeasuredPoint::new(common::abs(*offset), common::abs(0)),
        size: common::MeasuredSize::new(*cwidth, common::abs(layout.height()))
    });

    *offset += clayout.width();

    if let Some(it) = action.cascade(layout) {
        cwidget.action(clayout, it);
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

        for (cheight, cwidget) in &*self.inner.lock() {

            let clayout = layout.child(common::MeasuredRect {
                point: common::MeasuredPoint::new(common::abs(0), common::abs(offset)),
                size: common::MeasuredSize::new(common::abs(layout.width()), *cheight)
            });

            offset += clayout.height();

            if let Some(it) = action.cascade(layout) {
                cwidget.action(clayout, it);
            }

        }

    }
}

/// Clips its children to avoid their geometry escaping bounds.
///
/// A post processing is run on all added geometry and out of bounds
/// points are adjusted so that the geometry stops at the layout edges.
///
/// This widget should only be used if you know the child is going to
/// draw out of bounds on purpose. See [`Scrollable`].
pub struct Clip<W: Widget> {
    pub inner: W,
    /// We need our child to render into our own render state, since
    /// the needed adjustments to the geometry require adding/removing points.
    pub cache: SmartMutex<SpaceRenderState>,
}

impl<W: Widget> Clip<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner,
            cache: SmartMutex::new(SpaceRenderState::default()),
        }
    }
}

impl<W: Widget> Widget for Clip<W> {
    fn action(&self, layout: Layout, action: Action) {

        // The `Clip` widget is purely for visual cleanliness and doesn't need
        // to care about generally affecting the widget layouting process.

        if let Action::Render { space } = action {

            // Redirect the child's rendering into our cache.
            let cspace = Space { state: &self.cache };
            let caction = Action::Render { space: cspace };

            // Render the child that might draw out of bounds geometry.
            self.inner.action(layout, caction);

            // Now clip everything that was added by the child.

            let cache = self.cache.lock();


            // for section in cache.curves.sections() {

                // let points = cache.curves.points.get();


            // }


        } else {
            self.inner.action(layout, action);
        }

    }
}

// pub struct Scrollable<W: Widget> {
//     pub inner: W,
//     pub rect: SmartMutex<common::PhysicalPoint>,
// }

// impl<W: Widget> Scrollable<W> {
//     pub fn new(inner: W) -> Self {
//         Self { inner, point: SmartMutex::new(common::PhysicalPoint::ZERO) }
//     }
// }

// impl<W: Widget> Widget for Scrollable<W> {
//     fn action(&self, layout: Layout, action: Action) {

//         let point = self.point.lock();

//         // layout.clip = true;

//         let clayout =

//     }
// }
