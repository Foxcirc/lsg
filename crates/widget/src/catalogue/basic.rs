
use crate::{Widget, Action};
use std::sync::Arc;

impl Widget for () {
    fn action(&self, _: Action) {}
}

pub struct DynamicWidget {
    pub inner: Arc<dyn Widget>,
}

impl DynamicWidget {
    pub fn new<W: Widget + 'static>(inner: Arc<W>) -> Self {
        Self { inner }
    }
}

impl Default for DynamicWidget {
    fn default() -> Self {
        Self { inner: Arc::new(()) }
    }
}

impl Widget for DynamicWidget {
    fn action(&self, action: Action) {
        self.inner.action(action);
    }
}
