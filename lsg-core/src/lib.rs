
use std::{sync::Arc, ops::Deref};

pub trait Widget {
    
}

// pub trait GroupWidget {
    
// }

pub trait FullWidget {
    
}

pub struct BaseWidget<W> {
    inner: Arc<W>,
}

impl<W> Deref for BaseWidget<W> {
    type Target = W;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<W: Widget> FullWidget for BaseWidget<W> {
    
}

// pub struct BaseGroupWidget<W> {
//     inner: Arc<W>,
// }

// impl<W> Deref for BaseGroupWidget<W> {
//     type Target = W;
//     fn deref(&self) -> &Self::Target {
//         &self.inner
//     }
// }

// impl<W: GroupWidget> FullWidget for BaseGroupWidget<W> {
    
// }

#[cfg(test)]
mod tests {

    // use super::*;

}

