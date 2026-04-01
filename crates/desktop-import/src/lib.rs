
use std::{ffi::{self, c_void as void}, future, mem, ptr, sync::Arc, task, time::Instant};

use common::SmartMutex;

#[derive(Clone)]
pub struct EventLoopConfig {
    pub appid: String,
    /// If `true` relevant signals will be intercepted and
    /// turned into `Quit` events. Otherwise signals
    /// will never be intercepted.
    pub intercept: bool,
}

impl Default for EventLoopConfig {
    fn default() -> Self {
        Self {
            appid: format!("lsg-{:?}", Instant::now()),
            intercept: true,
        }
    }
}

pub struct EvlError;

pub struct EventLoopState {
    events: Vec<desktop::Event>,
}

pub struct EventLoop {
    inner: *const export::SharedEventLoop,
    state: SmartMutex<EventLoopState>
}

impl EventLoop {

    #[track_caller]
    pub fn run<R, H>(config: EventLoopConfig, handler: H) -> Result<R, EvlError>
        where H: FnOnce(Arc<Self>) -> R {

        let appid0 = ffi::CString::new(config.appid)
            .expect("`appid` contains nul byte").into_raw();

        let config0 = export::EventLoopConfig {
            appid: appid0,
            intercept: config.intercept,
        };

        let mut state = RunState::Pre(handler);

        let status = unsafe { export::event_loop_run(
            config0,
            RunState::<R, H>::handler0,
            ptr::from_mut(&mut state).cast()
        ) };

        if status == 0 {
            if let RunState::Post(value) = state.take() { Ok(value) }
            else { unreachable!() }
        } else {
            Err(EvlError)
        }

    }

    pub async fn next(&self) -> Result<(), EvlError> {
        future::poll_fn(|cx| self.poll(cx)).await
    }

    pub fn poll(&self, cx: &mut task::Context<'_>) -> task::Poll<Result<(), EvlError>> {

        let waker = cx.waker();

        let rawcx = export::EvlPollContextRust {
            waker: export::EvlPollWakerRust {
                state: waker.data().cast(),
                vtable: ptr::from_ref(waker.vtable()).cast(),
            }
        };

        let poll = unsafe { export::event_loop_poll_rust(self.inner, rawcx, ptr::null_mut()) };

        match poll {
            export::Poll::Ready => task::Poll::Ready(Ok(())),
            export::Poll::Pending => task::Poll::Pending
        }

    }

    pub fn suspend(&self) {
        unsafe { export::event_loop_suspend(self.inner) }
    }

    pub fn resume(&self) {
        unsafe { export::event_loop_resume(self.inner) }
    }

    pub fn quit(&self) {
        unsafe { export::event_loop_quit(self.inner) }
    }

}

unsafe impl common::IsDisplay for EventLoop {
    fn ptr(&self) -> *const void {
        unsafe { export::event_loop_display_ptr(self.inner) }
    }
}

enum RunState<R, H> {
    Pre(H),
    Post(R),
    Unreachable,
}

impl<R, H> RunState<R, H>
    where H: FnOnce(Arc<EventLoop>) -> R {

    pub extern "C" fn handler0 (evl0: *const export::SharedEventLoop, this0: *mut void) {

        let this: &mut Self = unsafe {
            &mut *this0.cast()
        };

        let Self::Pre(handler) = this.take() else { unreachable!() };

        let evl = EventLoop {
            inner: evl0,
            state: SmartMutex::new(EventLoopState {
                events: Vec::new()
            }),
        };

        let result = handler(Arc::new(evl));

        *this = Self::Post(result);

    }

    pub fn take(&mut self) -> Self {
        mem::replace(self, Self::Unreachable)
    }

}
