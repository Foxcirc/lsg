
use crate::*;

use std::sync::Arc;
use std::task::{Context, Poll};

pub struct EventLoopBackend;

impl EventLoopBackend {
    pub fn run<R, H>(_config: EvlConfig, _handler: H) -> Result<R, EvlError>
    where
        H: FnOnce(Arc<EventLoop>) -> R,
    {
        unimplemented!()
    }

    pub fn poll(&self, _cx: &mut Context<'_>) -> Poll<Result<Event, EvlError>> {
        unimplemented!()
    }

    pub fn suspend(&self) {
        unimplemented!()
    }

    pub fn resume(&self) {
        unimplemented!()
    }

    pub fn quit(&self) {
        unimplemented!()
    }

    pub fn ptr(&self) -> *const std::ffi::c_void {
        unimplemented!()
    }
}

pub struct WindowBackend;

impl WindowBackend {
    pub fn new(_evl: &Arc<EventLoop>) -> Self {
        unimplemented!()
    }

    pub fn id(&self) -> Id {
        unimplemented!()
    }

    pub fn present(&self) {
        unimplemented!()
    }

    pub fn redraw(&self) {
        unimplemented!()
    }

    pub fn transparency(&self, _value: bool) {
        unimplemented!()
    }

    pub fn decorations(&self, _value: bool) {
        unimplemented!()
    }

    pub fn title(&self, _text: &str) {
        unimplemented!()
    }

    pub fn maximize(&self, _value: bool) {
        unimplemented!()
    }

    pub fn fullscreen(&self, _value: bool, _monitor: Option<&Monitor>) {
        unimplemented!()
    }

    pub fn sizehint(&self, _size: common::PhysicalSize) {
        unimplemented!()
    }

    pub fn minsize(&self, _size: Option<common::LogicalSize>) {
        unimplemented!()
    }

    pub fn maxsize(&self, _size: Option<common::LogicalSize>) {
        unimplemented!()
    }

    pub fn alert(&self, _urgency: Urgency) {
        unimplemented!()
    }

    #[cfg(target_family = "wasm")]
    pub fn bind(&self, _id: &str) {
        unimplemented!()
    }

    pub fn ptr(&self) -> *mut std::ffi::c_void {
        unimplemented!()
    }

    pub fn size(&self) -> common::PhysicalSize {
        unimplemented!()
    }
}

pub struct MonitorBackend;

pub struct DataReadableBackend;

impl DataReadableBackend {
    pub fn kinds(&self) -> &[DataKind] {
        unimplemented!()
    }

    pub fn receive(&self, _evl: &EventLoop, _kind: DataKind) -> DataReaderBackend {
        unimplemented!()
    }
}

pub struct DataWritableBackend;

impl DataWritableBackend {
    pub fn id(&self) -> Id {
        unimplemented!()
    }

    pub fn selection(_evl: &EventLoop, _offers: &[DataKind]) -> Self {
        unimplemented!()
    }

    pub fn dnd(_handle: &Window, _offers: &[DataKind], _icon: CustomIcon) -> Self {
        unimplemented!()
    }
}

pub struct DataReaderBackend;

impl DataReaderBackend {
    pub fn read(&mut self, _buf: &mut [u8]) -> Result<usize, ()> {
        unimplemented!()
    }
}

pub struct DataWriterBackend;

impl DataWriterBackend {
    pub fn write(&mut self, _buf: &[u8]) -> Result<usize, ()> {
        unimplemented!()
    }
}

pub struct HoveredItemBackend;

impl HoveredItemBackend {
    pub fn advertise(&self, _kinds: &[DataKind]) {
        unimplemented!()
    }
}

pub struct CustomIconBackend;

impl CustomIconBackend {
    pub fn new(_evl: &EventLoop, _size: common::LogicalSize, _format: IconFormat, _data: &[u8]) -> Self {
        unimplemented!()
    }
}
