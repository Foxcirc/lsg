
use crate::*;
use std::sync::Arc;

impl Widget for () {
    fn action(&self, _: Layout, _: Action) {}
}

pub struct DynWidget {
    pub inner: Arc<dyn Widget>,
}

impl DynWidget {
    pub fn new<W: Widget + 'static>(inner: Arc<W>) -> Self {
        Self { inner }
    }
}

impl Default for DynWidget {
    fn default() -> Self {
        Self { inner: Arc::new(()) }
    }
}

impl Widget for DynWidget {
    fn action(&self, layout: Layout, action: Action) {
        self.inner.action(layout, action);
    }
}
