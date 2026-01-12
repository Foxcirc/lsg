
use core::task;
use std::{io, task::ready};

use common::SmartMutex;
use futures_lite::StreamExt;
use nix::sys::signal::Signal;

use crate::*;

/// Listens to SIGTERM and SIGINT to emit the apropriate events
pub(crate) struct SignalListener {
    signals: SmartMutex<async_signals::Signals>,
}

impl SignalListener {

    pub fn new() -> io::Result<Self> {

        let signals = async_signals::Signals::new([
            Signal::SIGTERM as i32,
            Signal::SIGINT as i32
        ]).map_err(io::Error::from)?;

        Ok(Self {
            signals: SmartMutex::new(signals)
        })

    }

    pub fn poll(&self, cx: &mut task::Context<'_>) -> task::Poll<Result<Event, EvlError>> {

        loop {

            let mut guard = self.signals.lock();
            let signal = ready!(guard.poll_next(cx)).unwrap_or_default();

            if signal == Signal::SIGTERM as i32 {
                return task::Poll::Ready(Ok(Event::Quit { reason: QuitReason::System }))
            } else if signal == Signal::SIGINT as i32 {
                return task::Poll::Ready(Ok(Event::Quit { reason: QuitReason::CtrlC }))
            }

        }

    }

}
