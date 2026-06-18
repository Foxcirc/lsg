
use common::*;
use crate::*;

use std::rc::Rc;

pub struct GraphicsBackend;

impl GraphicsBackend {
    pub fn new<D: IsDisplay>(_display: &D) -> Result<Self, GraphicsError> {
        unimplemented!()
    }
}

pub struct ProgramBackend;

impl ProgramBackend {
    pub fn new(_gp: &Graphics, _shaders: &[Source]) -> Self {
        unimplemented!()
    }

    pub fn uniformloc(&mut self, _name: &str) -> crate::Location {
        unimplemented!()
    }
}

pub struct SurfaceBackend;

impl SurfaceBackend {
    pub fn new<S: IsSurface>(_gp: &Rc<Graphics>, _window: &S) -> Self {
        unimplemented!()
    }

    pub fn resize(&mut self, _size: PhysicalSize) {
        unimplemented!()
    }

    pub fn draw<'a>(&mut self, _cmd: crate::DrawCommand<'a>) {
        unimplemented!()
    }

    pub fn blit(&mut self, _texture: &super::Texture) {
        unimplemented!()
    }

    pub fn swap(&mut self) {
        unimplemented!()
    }
}

pub struct TextureBackend;

impl TextureBackend {
    pub fn maxsize(_gp: &Graphics) -> usize {
        unimplemented!()
    }

    pub fn new(_gp: &Rc<Graphics>, _size: PhysicalSize, _data: Option<&[u8]>) -> Self {
        unimplemented!()
    }

    pub fn size(&self) -> PhysicalSize {
        unimplemented!()
    }

    pub fn resize(&mut self, _size: PhysicalSize, _data: Option<&[u8]>) {
        unimplemented!()
    }

    pub fn clear(&mut self, _values: [f32; 4]) {
        unimplemented!()
    }

    pub fn inspect(&mut self) -> Vec<u8> {
        unimplemented!()
    }

    pub fn frombuf(&mut self, _src: &[u8], _dstrect: PhysicalRect) {
        unimplemented!()
    }

    // Accepts &Self since frontend passes &src.backend
    pub fn fromtex(&mut self, _src: &Texture, _srcrect: PhysicalRect, _destrect: PhysicalRect) {
        unimplemented!()
    }

    pub fn draw(&mut self, _cmd: DrawCommand) {
        unimplemented!()
    }
}

pub struct VertexBufferBackend;

impl VertexBufferBackend {
    pub fn new(_gp: &Graphics, _layout: &[VertexAttrib]) -> Self {
        unimplemented!()
    }

    pub fn frombuf(&mut self, _src: &[u8]) {
        unimplemented!()
    }
}
