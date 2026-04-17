
//! This module reconstructs the original rust API based on the C-ABI compatible API,
//! so the crate can be used by simply linking to a library without including the implementation.

pub use implementation::*;

pub mod definitions {

    use std::ffi::c_void as void;
    use crate::ffi::types::*;

    #[allow(improper_ctypes)]
    unsafe extern "C" {

        // pub fn evl_handlers_default() -> EvlHandlers;

        pub fn event_loop_run(
            config0: EventLoopConfig,
            handler0: EventLoopHandler,
            state: *mut void,
        ) -> EvlResult;

        pub fn event_loop_poll_rust(
            this0: *const EventLoop,
            rawcx: EvlPollContextRust,
            handlers0: *const EvlHandlers,
            state: *mut void,
        ) -> Poll;

        pub fn event_loop_suspend(this0: *const EventLoop);
        pub fn event_loop_resume(this0: *const EventLoop);
        pub fn event_loop_quit(this0: *const EventLoop);
        pub fn event_loop_display_ptr(this0: *const EventLoop) -> *const void;

        pub fn monitor_info_drop(this: MonitorInfo);
        pub fn monitor_drop(this0: *mut Monitor);

        pub fn custom_icon_new(
            evl0: *const EventLoop,
            size: common::LogicalSize,
            format: crate::IconFormat,
            data0: WriteSlice,
        ) -> *mut CustomIcon;

        pub fn hovered_item_drop(this0: *mut HoveredItem);
        pub fn hovered_item_advertise(this0: *mut HoveredItem, kinds0: DataKindsSlice);

        pub fn data_readable_drop(this0: *mut DataReadable);
        pub fn data_readable_kinds(this0: *mut DataReadable) -> DataKindsSlice;
        pub fn data_readable_receive(this0: *mut DataReadable, evl0: *const EventLoop, kind: crate::DataKind) -> *mut DataReader;

        pub fn data_reader_drop(this0: *mut DataReader);
        // pub fn data_reader_as_fd(this0: *mut DataReader) -> i32;
        pub fn data_reader_read(this0: *mut DataReader, out0: ReadSlice) -> usize;

        pub fn data_writable_drop(this0: *mut DataWritable);
        pub fn data_writable_id(this0: *mut DataWritable) -> crate::Id;
        pub fn data_writable_selection(evl0: *const EventLoop, offers0: DataKindsSlice) -> *mut DataWritable;
        pub fn data_writable_dnd(wnd0: *mut Window, offers0: DataKindsSlice, icon0: *mut CustomIcon) -> *mut DataWritable;

        pub fn data_writer_drop(this0: *mut DataWriter);
        // pub fn data_writer_as_fd(this0: *mut DataWriter) -> i32;
        pub fn data_writer_write(this0: *mut DataWriter, src0: WriteSlice) -> usize;
        // pub fn data_writer_flush(this0: *mut DataWriter);

        pub fn window_drop(this0: *mut Window);
        pub fn window_new(this0: *const EventLoop) -> *mut Window;
        pub fn window_id(this0: *mut Window) -> crate::Id;
        pub fn window_present(this0: *mut Window);
        pub fn window_redraw(this0: *mut Window);
        pub fn window_transparency(this0: *mut Window, value: bool);
        pub fn window_decorations(this0: *mut Window, value: bool);
        pub fn window_title(this0: *mut Window, text0: *const i8);
        pub fn window_maximize(this0: *mut Window, value: bool);
        pub fn window_fullscreen(this0: *mut Window, value: bool, monitor0: *mut Monitor);
        pub fn window_sizehint(this0: *mut Window, size: common::PhysicalSize);
        pub fn window_minsize(this0: *mut Window, size: common::LogicalSize);
        pub fn window_minsize_unset(this0: *mut Window);
        pub fn window_maxsize(this0: *mut Window, size: common::LogicalSize);
        pub fn window_maxsize_unset(this0: *mut Window);
        pub fn window_alert(this0: *mut Window, urgency: crate::Urgency);
        pub fn window_ptr(this0: *mut Window) -> *mut void;
        pub fn window_size(this0: *mut Window) -> common::PhysicalSize;
    }

}

pub mod implementation {

    use core::{
        slice, task, mem,
        ffi::{CStr, c_void as void},
        ptr::{self, NonNull, null_mut},
    };

    use std::{sync::Arc, ffi::CString};

    use common::SmartMutex;

    use crate::{
        ffi::{types, import::definitions::*},
        DndEvent, Event, Key, MonitorEvent, WindowEvent, DataSourceEvent
    };

    pub struct EventLoopState {
        events: Vec<Event>,
    }

    pub struct EventLoopBackend {
        inner: *const types::EventLoop,
        state: SmartMutex<EventLoopState>
    }

    impl EventLoopBackend {

        #[track_caller]
        pub fn run<R, H>(config: crate::EvlConfig, handler: H) -> Result<R, crate::EvlError>
            where H: FnOnce(Arc<crate::EventLoop>) -> R {

            let appid0 = CString::new(config.appid)
                .expect("`appid` contains nul byte").into_raw();

            let config0 = types::EventLoopConfig {
                appid: appid0,
                intercept: config.intercept,
            };

            let mut state = RunState::Pre(handler);

            let status = unsafe { event_loop_run(
                config0,
                RunState::<R, H>::handler0,
                ptr::from_mut(&mut state).cast()
            ) };

            if status as u32 == types::EvlResult::Ok as u32 {
                if let RunState::Post(value) = state.take() { Ok(value) }
                else { unreachable!() }
            } else {
                Err(crate::EvlError::fatal("TODO".into()))
            }

        }

        pub fn poll(&self, cx: &mut task::Context<'_>) -> task::Poll<Result<Event, crate::EvlError>> {

            let mut guard = self.state.lock();

            if let Some(event) = guard.events.pop() {
                return task::Poll::Ready(Ok(event))
            }

            // If we don't have any events stored, we need
            // to actually poll for more.

            let waker = cx.waker();

            let rawcx = types::EvlPollContextRust {
                waker: types::EvlPollWakerRust {
                    state: waker.data().cast(),
                    vtable: ptr::from_ref(waker.vtable()).cast(),
                }
            };

            let state = &mut *guard as *mut EventLoopState;

            let poll = unsafe { event_loop_poll_rust(self.inner, rawcx, &HANDLERS, state.cast()) };

            match poll {

                types::Poll::Err => {
                    task::Poll::Ready(Err(crate::EvlError::fatal("unknown error")))
                },

                types::Poll::Ready => {
                    waker.wake_by_ref();
                    task::Poll::Pending
                },

                types::Poll::Pending => task::Poll::Pending,

            }

        }

        pub fn suspend(&self) {
            unsafe { event_loop_suspend(self.inner) }
        }

        pub fn resume(&self) {
            unsafe { event_loop_resume(self.inner) }
        }

        pub fn quit(&self) {
            unsafe { event_loop_quit(self.inner) }
        }

    }

    unsafe impl common::IsDisplay for EventLoopBackend {
        fn ptr(&self) -> *const void {
            unsafe { event_loop_display_ptr(self.inner) }
        }
    }

    enum RunState<R, H> {
        Pre(H),
        Post(R),
        Unreachable,
    }

    impl<R, H> RunState<R, H>
        where H: FnOnce(Arc<crate::EventLoop>) -> R {

        pub extern "C" fn handler0(evl0: *const types::EventLoop, this0: *mut void) {

            let this: &mut Self = unsafe {
                &mut *this0.cast()
            };

            let Self::Pre(handler) = this.take() else { unreachable!() };

            let backend = EventLoopBackend {
                inner: evl0,
                state: SmartMutex::new(EventLoopState {
                    events: Vec::new()
                }),
            };

            let evl = crate::EventLoop { backend };

            let result = handler(Arc::new(evl));

            *this = Self::Post(result);

        }

        pub fn take(&mut self) -> Self {
            mem::replace(self, Self::Unreachable)
        }

    }

    pub struct WindowBackend {
        inner: *mut types::Window,
    }

    impl Drop for WindowBackend {
        fn drop(&mut self) {
            unsafe { window_drop(self.inner) }
        }
    }

    impl WindowBackend {
        pub fn new(evl: &Arc<crate::EventLoop>) -> Self {
            let inner = unsafe { window_new(evl.backend.inner) };
            Self { inner }
        }
        pub fn id(&self) -> crate::Id {
            unsafe { window_id(self.inner) }
        }
        pub fn present(&self) {
            unsafe { window_present(self.inner) };
        }
        #[track_caller]
        pub fn redraw(&self) {
            unsafe { window_redraw(self.inner) };
        }
        pub fn transparency(&self, value: bool) {
            unsafe { window_transparency(self.inner, value) };
        }
        pub fn decorations(&self, value: bool) {
            unsafe { window_decorations(self.inner, value) };
        }
        pub fn title(&self, text: &str) {
            let text0 = CString::new(text).expect("contains nul");
            unsafe { window_title(self.inner, text0.as_ptr()) };
        }
        pub fn maximize(&self, value: bool) {
            unsafe { window_maximize(self.inner, value) };
        }
        pub fn fullscreen(&self, value: bool, monitor: Option<&crate::Monitor>) {
            let monitor0 = monitor.map(|it| it.backend.inner).unwrap_or(null_mut());
            unsafe { window_fullscreen(self.inner, value, monitor0) };
        }
        pub fn sizehint(&self, size: common::PhysicalSize) {
            unsafe { window_sizehint(self.inner, size) };
        }
        pub fn minsize(&self, size: Option<common::LogicalSize>) {
            match size {
                Some(it) => unsafe { window_minsize(self.inner, it) },
                None     => unsafe { window_minsize_unset(self.inner) }
            }
        }
        pub fn maxsize(&self, size: Option<common::LogicalSize>) {
            match size {
                Some(it) => unsafe { window_maxsize(self.inner, it) },
                None     => unsafe { window_maxsize_unset(self.inner) }
            }
        }
        pub fn alert(&self, urgency: crate::Urgency) {
            unsafe { window_alert(self.inner, urgency) };
        }
        pub fn ptr(&self) -> *mut std::ffi::c_void {
            unsafe { window_ptr(self.inner) }
        }
        pub fn size(&self) -> common::PhysicalSize {
            unsafe { window_size(self.inner) }
        }
    }

    pub struct MonitorBackend {
        inner: *mut types::Monitor,
    }

    impl Drop for MonitorBackend {
        fn drop(&mut self) {
            unsafe { monitor_drop(self.inner) };
        }
    }

    pub struct CustomIconBackend {
        inner: *mut types::CustomIcon,
    }

    impl Drop for CustomIconBackend {
        fn drop(&mut self) {
            // unsafe { types::custom_icon_drop(self.inner) };
            // TODO: figure out what to do here :/
        }
    }

    impl CustomIconBackend {
        pub fn new(evl: &crate::EventLoop, size: common::LogicalSize, format: crate::IconFormat, data: &[u8]) -> Self {
            let data0 = types::WriteSlice { ptr: data.as_ptr(), len: data.len() };
            let inner = unsafe { custom_icon_new(evl.backend.inner, size, format, data0) };
            Self { inner }
        }
    }

    pub struct HoveredItemBackend {
        inner: *mut types::HoveredItem,
    }

    impl Drop for HoveredItemBackend {
        fn drop(&mut self) {
            unsafe { hovered_item_drop(self.inner) };
        }
    }

    impl HoveredItemBackend {
        pub fn advertise(&self, kinds: &[crate::DataKind]) {
            let kinds0 = types::DataKindsSlice { ptr: kinds.as_ptr(), len: kinds.len() };
            unsafe { hovered_item_advertise(self.inner, kinds0) };
        }
    }

    pub struct DataReadableBackend {
        inner: *mut types::DataReadable,
    }

    impl Drop for DataReadableBackend {
        fn drop(&mut self) {
            unsafe { data_readable_drop(self.inner) };
        }
    }

    impl DataReadableBackend {
        pub fn kinds(&self) -> &[crate::DataKind] {
            let kinds0 = unsafe { data_readable_kinds(self.inner) };
            unsafe { slice::from_raw_parts(kinds0.ptr, kinds0.len) }
        }
        pub fn receive(&self, evl: &crate::EventLoop, kind: crate::DataKind) -> DataReaderBackend {
            let inner = unsafe { data_readable_receive(self.inner, evl.backend.inner, kind) };
            DataReaderBackend { inner } // TODO: make it directly return crate::DataReader / unify these places
        }
    }

    pub struct DataReaderBackend {
        inner: *mut types::DataReader,
    }

    impl Drop for DataReaderBackend {
        fn drop(&mut self) {
            unsafe { data_reader_drop(self.inner) };
        }
    }

    impl DataReaderBackend {
        // pub fn as_fd(&self) -> i32 {
        //     unsafe { data_reader_as_fd(self.inner) }
        // }
        pub fn read(&mut self, buf: &mut [u8]) -> Result<usize, ()> {
            let buf0 = types::ReadSlice { ptr: buf.as_mut_ptr(), len: buf.len() };
            let num = unsafe { data_reader_read(self.inner, buf0) };
            Ok(num)
        }
    }

    pub struct DataWritableBackend {
        inner: *mut types::DataWritable,
    }

    impl Drop for DataWritableBackend {
        fn drop(&mut self) {
            unsafe { data_writable_drop(self.inner) };
        }
    }

    impl DataWritableBackend {
        pub fn id(&self) -> crate::Id {
            unsafe { data_writable_id(self.inner) }
        }
        pub fn selection(evl: &crate::EventLoop, offers: &[crate::DataKind]) -> Self {
            let offers0 = types::DataKindsSlice { ptr: offers.as_ptr(), len: offers.len() };
            let inner = unsafe { data_writable_selection(evl.backend.inner, offers0) };
            Self { inner }
        }
        #[track_caller]
        pub fn dnd(handle: &crate::Window, offers: &[crate::DataKind], icon: crate::CustomIcon) -> Self {
            let offers0 = types::DataKindsSlice { ptr: offers.as_ptr(), len: offers.len() };
            let inner = unsafe { data_writable_dnd(handle.backend.inner, offers0, icon.backend.inner) };
            mem::forget(icon); // this is needed, because `data_writable_dnd` takes ownership of the value
            // TODO: ^^^ it should not take ownership (?) possible?
            Self { inner }
        }
    }

    pub struct DataWriterBackend {
        inner: *mut types::DataWriter,
    }

    impl Drop for DataWriterBackend {
        fn drop(&mut self) {
            unsafe { data_writer_drop(self.inner) };
        }
    }

    impl DataWriterBackend {
        // pub fn as_fd(&self) -> i32 {
        //     unsafe { data_writer_as_fd(self.inner) }
        // }
        pub fn write(&mut self, buf: &[u8]) -> Result<usize, ()> {
            let buf0 = types::WriteSlice { ptr: buf.as_ptr(), len: buf.len() };
            let num = unsafe { data_writer_write(self.inner, buf0) };
            Ok(num)
        }
        // pub fn flush(&mut self) -> Result<(), ()> {
        //     unsafe { data_writer_flush(self.inner) };
        //     Ok(())
        // }
    }

    macro_rules! handler {
        (($($arg:ident: $ty:ty),*) => $body:expr) => {{
            extern "C" fn f(state0: *mut void, $($arg: $ty),*) {
                let state: &mut EventLoopState = unsafe { &mut *state0.cast() };
                state.events.push($body);
            }
            f
        }};
    }

    const HANDLERS: types::EvlHandlers = types::EvlHandlers {

        resume:  handler!(() => Event::Resume),
        suspend: handler!(() => Event::Suspend),
        quit:    handler!((reason: crate::QuitReason) => Event::Quit { reason }),

        monitor_update: handler!((id: crate::Id, info0: types::MonitorInfo, monitor0: *mut types::Monitor) => {
            let info = crate::MonitorInfo {
                name:        unsafe { CStr::from_ptr(info0.name)        }.to_str().expect("invalid utf8").to_string(),
                description: unsafe { CStr::from_ptr(info0.description) }.to_str().expect("invalid utf8").to_string(),
                size: info0.size,
                refresh: info0.refresh
            };
            let backend = MonitorBackend { inner: monitor0 };
            let monitor = crate::Monitor { backend };
            Event::Monitor { id, event: crate::MonitorEvent::Update { info, monitor } }
        }),

        monitor_remove: handler!((id: crate::Id) => {
            Event::Monitor { id, event: MonitorEvent::Remove }
        }),

        window_enter:        handler!((id: crate::Id) => Event::Window { id, event: WindowEvent::Enter {} }),
        window_leave:        handler!((id: crate::Id) => Event::Window { id, event: WindowEvent::Leave {} }),
        window_should_close: handler!((id: crate::Id) => Event::Window { id, event: WindowEvent::ShouldClose {} }),
        window_redraw:       handler!((id: crate::Id) => Event::Window { id, event: WindowEvent::Redraw {} }),

        window_resize:       handler!((id: crate::Id, size: common::PhysicalSize, fullscreen: bool) => Event::Window { id, event: WindowEvent::Resize { size, fullscreen } }),
        window_rescale:      handler!((id: crate::Id, scale: f64)                                   => Event::Window { id, event: WindowEvent::Rescale { scale } }),
        window_decorations:  handler!((id: crate::Id, active: bool)                                 => Event::Window { id, event: WindowEvent::Decorations { active } }),

        window_mouse_enter:  handler!((id: crate::Id)                              => Event::Window { id, event: WindowEvent::MouseEnter }),
        window_mouse_leave:  handler!((id: crate::Id)                              => Event::Window { id, event: WindowEvent::MouseLeave }),
        window_mouse_motion: handler!((id: crate::Id, point: common::LogicalPoint) => Event::Window { id, event: WindowEvent::MouseMotion { point } }),

        window_mouse_down:   handler!((id: crate::Id, point: common::LogicalPoint, button: crate::MouseButton) => Event::Window { id, event: WindowEvent::MouseDown { point, button } }),
        window_mouse_up:     handler!((id: crate::Id, point: common::LogicalPoint, button: crate::MouseButton) => Event::Window { id, event: WindowEvent::MouseUp { point, button } }),

        window_mouse_scroll: handler!((id: crate::Id, axis: crate::ScrollAxis, value: i16) => Event::Window { id, event: WindowEvent::MouseScroll { axis, value } }),

        window_key_down_unknown: handler!((id: crate::Id, key: u32, repeat: bool)               => Event::Window { id, event: WindowEvent::KeyDown { key: Key::Unknown(key), repeat } }),
        window_key_down_special: handler!((id: crate::Id, key: crate::SpecialKey, repeat: bool) => Event::Window { id, event: WindowEvent::KeyDown { key: Key::Special(key), repeat } }),
        window_key_down_char:    handler!((id: crate::Id, chr: u32, dead: bool, repeat: bool) => match dead {
            false => Event::Window { id, event: WindowEvent::KeyDown { key: Key::Char     (char::from_u32(chr).expect("invalid charcode")), repeat } },
            true  => Event::Window { id, event: WindowEvent::KeyDown { key: Key::DeadChar (char::from_u32(chr).expect("invalid charcode")), repeat } }
        }),

        window_key_up_unknown: handler!((id: crate::Id, key: u32)               => Event::Window { id, event: WindowEvent::KeyUp { key: Key::Unknown(key) } }),
        window_key_up_special: handler!((id: crate::Id, key: crate::SpecialKey) => Event::Window { id, event: WindowEvent::KeyUp { key: Key::Special(key) } }),
        window_key_up_char:    handler!((id: crate::Id, chr: u32, dead: bool) => match dead {
            false => Event::Window { id, event: WindowEvent::KeyUp { key: Key::Char     (char::from_u32(chr).expect("invalid charcode")) } },
            true  => Event::Window { id, event: WindowEvent::KeyUp { key: Key::DeadChar (char::from_u32(chr).expect("invalid charcode")) } }
        }),

        window_text_input:          handler!((id: crate::Id, chr: u32) => Event::Window { id, event: WindowEvent::TextInput   { chr: char::from_u32(chr).expect("invalid charcode") } }),
        window_text_compose:        handler!((id: crate::Id, chr: u32) => Event::Window { id, event: WindowEvent::TextCompose { chr: char::from_u32(chr).expect("invalid charcode") } }),
        window_text_compose_cancel: handler!((id: crate::Id)           => Event::Window { id, event: WindowEvent::TextComposeCancel }),

        window_dnd_motion: handler!((id: crate::Id, sameapp: bool, x: f64, y: f64, item0: *mut types::HoveredItem) => {
            let backend = HoveredItemBackend { inner: item0 };
            let item = crate::HoveredItem { backend };
            Event::Window { id, event: WindowEvent::Dnd { event: DndEvent::Motion { x, y, item }, sameapp } }
        }),

        window_dnd_drop:   handler!((id: crate::Id, sameapp: bool, x: f64, y: f64, readable: *mut types::DataReadable) => {
            let backend = DataReadableBackend { inner: readable };
            let readable = crate::DataReadable { backend };
            Event::Window { id, event: WindowEvent::Dnd { event: DndEvent::Drop { x, y, readable }, sameapp } }
        }),

        window_dnd_cancel: handler!((id: crate::Id, sameapp: bool) => {
            Event::Window { id, event: WindowEvent::Dnd { event: DndEvent::Cancel, sameapp } }
        }),

        data_source_send:    handler!((id: crate::Id, kind: crate::DataKind, writer0: *mut types::DataWriter) => {
            let backend = DataWriterBackend { inner: writer0 };
            let writer = crate::DataWriter { backend };
            Event::DataSource { id, event: DataSourceEvent::Send { kind, writer } }
        }),

        data_source_success: handler!((id: crate::Id) => Event::DataSource { id, event: DataSourceEvent::Success }),
        data_source_close:   handler!((id: crate::Id) => Event::DataSource { id, event: DataSourceEvent::Close }),

        selection_update: handler!((readable0: *mut types::DataReadable) => {
            match NonNull::new(readable0) {
                Some(ptr) => {
                    let backend = DataReadableBackend { inner: ptr.as_ptr() };
                    let readable = crate::DataReadable { backend };
                    Event::SelectionUpdate { readable: Some(readable) }
                },
                None => {
                    Event::SelectionUpdate { readable: None }
                }
            }
        }),

    };

}
