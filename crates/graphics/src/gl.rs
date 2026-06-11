
use common::*;
use std::sync::Arc;

pub struct GraphicsBackend {
    egl: Arc<egl::Instance>,
    cfg: egl::Config,
    ctx: egl::Context,
    /// Sometimes for texture operations we temporarily need an FBO.
    scratch: gl::FrameBuffer,
}

impl GraphicsBackend {
    pub fn new<D: IsDisplay>(display: &D) -> Result<Self, crate::GraphicsError> {

        // Initialize an EGL context.
        let egl = egl::Instance::new(display)?;
        let cfg = egl::Config::build().api(egl::Api::Es3).version(3, 0).finish(&egl)?;
        let ctx = egl::Context::new(&egl, &cfg)?;

        gl::load_with(|name| egl.get_proc_address(name)).ok();
        //                 We take whatever is available ^^^^

        ctx.bind(None); // This is important!

        let scratch = gl::gen_frame_buffer();

        Ok(Self { egl, cfg, ctx, scratch })
    }
}

pub struct ProgramBackend {
    program: gl::LinkedProgram
}

impl ProgramBackend {
    pub fn new(gp: &crate::Graphics, sources: &[crate::Source]) -> Result<Self, crate::GraphicsError> {

        gp.backend.ctx.bind(None);

        // In the GL implementation, this corresponds to
        // linking all the shaders into a program.

        let mut builder = gl::create_program();

        for crate::Source { kind, data } in sources {
            let kind1 = match kind {
                crate::SourceKind::Vertex   => gl::ShaderType::Vertex,
                crate::SourceKind::Fragment => gl::ShaderType::Fragment
            };
            let shader = gl::create_shader(kind1, data)?;
            gl::attach_shader(&mut builder, shader);
        }

        let program = gl::link_program(builder)?;

        Ok(Self { program })

    }
    #[cfg(target_os = "linux")]
    #[track_caller]
    pub fn uniformloc(&self, gp: &crate::Graphics, name: &str) -> crate::Location {

        gp.backend.ctx.bind(None);

        let gl::UniformLocation { index } = gl::uniform_location(&self.program, name)
            .expect("uniform not present in shader (unused uniforms are optimized away)");

        crate::Location(index as usize)

    }
}

pub struct SurfaceBackend {
    surface: egl::Surface,
}

impl SurfaceBackend {

    pub fn new<S: IsSurface>(gp: &crate::Graphics, window: &S) -> Self {

        let surface = egl::Surface::new(
            &gp.backend.egl, &gp.backend.cfg, window, window.size(),
        ).expect("cannot create egl surface");

        Self { surface }

    }

    pub fn resize(&mut self, _gp: &crate::Graphics, size: PhysicalSize) {
        //                   ^^^ we keep this for consistency with the `resize` methods
        self.surface.resize(size);
    }


    pub fn draw<'a>(&self, gp: &crate::Graphics, cmd: crate::DrawCommand<'a>) {

        gp.backend.ctx.bind(Some(&self.surface));

        gl::resize_viewport(self.surface.size());
        draw(&gl::FrameBuffer::default(), cmd);

    }

    pub fn blit(&mut self, gp: &crate::Graphics, texture: &crate::Texture) {

        gp.backend.ctx.bind(Some(&self.surface));

        assert!(self.surface.size() == texture.size());

        gl::frame_buffer_texture_2d(
            &gp.backend.scratch,
            gl::AttachmentPoint::Color0,
            &texture.backend.texture
        );

        let target = (&gl::FrameBuffer::default(), PhysicalRect::MAX);
        let source = (&gp.backend.scratch,         PhysicalRect::MAX);

        gl::blit_frame_buffer(target, source, gl::TexValue::Linear);

    }

    pub fn swap(&mut self, gp: &crate::Graphics) {
        gp.backend.ctx.swap(&self.surface, egl::Damage::all());
    }

}

pub struct TextureBackend {
    texture: gl::Texture,
    size: PhysicalSize,
}

impl TextureBackend {

    pub fn maxsize(gp: &crate::Graphics) -> usize {
        gp.backend.ctx.bind(None);
        gl::get_integer_v(gl::Property::MaxTextureSize) as usize
    }

    pub fn new(gp: &crate::Graphics, size: PhysicalSize, data: Option<&[u8]>) -> Self {

        gp.backend.ctx.bind(None);

        let texture = gl::gen_texture(gl::TextureType::Basic2D);

        gl::tex_sensible_defaults(&texture);
        gl::tex_image_2d(
            &texture,
            size,
            gl::GpuColorFormat::Rgba8,
            gl::ColorFormat::Rgba,
            gl::DataType::U8,
            data
        );

        Self { texture, size }

    }

    pub fn size(&self) -> PhysicalSize {
        self.size
    }

    pub fn resize(&mut self, gp: &crate::Graphics, size: PhysicalSize, data: Option<&[u8]>) {

        gp.backend.ctx.bind(None);

        self.size = size;

        gl::tex_image_2d(
            &self.texture, self.size,
            gl::GpuColorFormat::Rgba8,
            gl::ColorFormat::Rgba,
            gl::DataType::U8,
            data
        );

    }

    pub fn clear(&mut self, gp: &crate::Graphics, values: [f32; 4]) {

        gp.backend.ctx.bind(None);

        // Write to `self.texture`.
        gl::frame_buffer_texture_2d(&gp.backend.scratch, gl::AttachmentPoint::Color0, &self.texture);

        gl::clear(&gp.backend.scratch, values);

    }

    pub fn inspect(&mut self, gp: &crate::Graphics) -> Vec<u8> {

        gp.backend.ctx.bind(None);

        // Read from `self.texture`.
        gl::frame_buffer_texture_2d(&gp.backend.scratch, gl::AttachmentPoint::Color0, &self.texture);

        unsafe { gl::read_pixels(
            &gp.backend.scratch, PhysicalRect::new(PhysicalPoint::ZERO, self.size),
            gl::ColorFormat::Rgba, gl::DataType::U8
        ) }

    }

    pub fn frombuf(&mut self, gp: &crate::Graphics, src: &[u8], dstrect: PhysicalRect) {

        gp.backend.ctx.bind(None);

        // Read into `self.texture`.
        gl::tex_sub_image_2d(&self.texture, dstrect, gl::ColorFormat::Rgba, gl::DataType::U8, src);

    }

    /// Copy a region into `self` from another texture.
    ///
    /// # Panics
    /// The regions have to be the same size.
    #[track_caller]
    pub fn fromtex(&mut self, gp: &crate::Graphics, src: &TextureBackend, srcrect: PhysicalRect, dstrect: PhysicalRect) {

        gp.backend.ctx.bind(None);

        assert!(srcrect.size == dstrect.size, "src and dest size must be equal");

        // Read from `src.texture`.
        gl::frame_buffer_texture_2d(&gp.backend.scratch, gl::AttachmentPoint::Color0, &src.texture);
        gl::copy_tex_sub_image_2d((&gp.backend.scratch, srcrect.pos), (&self.texture, dstrect.pos), srcrect.size);

    }

    pub fn draw<'a>(&self, gp: &crate::Graphics, cmd: crate::DrawCommand<'a>) {

        gp.backend.ctx.bind(None);

        gl::frame_buffer_texture_2d(&gp.backend.scratch, gl::AttachmentPoint::Color0, &self.texture);
        gl::resize_viewport(self.size);
        draw(&gp.backend.scratch, cmd);

    }

}

/// Used to draw on textures and surfaces.
fn draw<'a>(target: &gl::FrameBuffer, cmd: crate::DrawCommand<'a>) {

    match cmd.options.blend {
        crate::BlendMode::None => {
            gl::disable(gl::Capability::Blend)
        },
        crate::BlendMode::OrderedTransparency => {
            gl::enable(gl::Capability::Blend);
            gl::blend_func(gl::BlendFunc::SrcAlpha, gl::BlendFunc::OneMinusSrcAlpha);
        }
    }

    for (idx, texture) in cmd.textures.iter().enumerate() {
        gl::active_texture(&texture.src.backend.texture, idx);
        gl::uniform_1i(&cmd.program.backend.program, texture.sampler.togl(), idx as i32);
    }

    gl::draw_arrays(
        &target,
        &cmd.program.backend.program,
        &cmd.src.backend.vao,
        cmd.options.primitive.togl(),
        0, // start
        cmd.src.vertsize() // count
    );

}

impl crate::Location {
    pub fn togl(self) -> gl::UniformLocation {
        gl::UniformLocation { index: self.0 as u32 }
    }
}

impl crate::Primitive {
    pub fn togl(&self) -> gl::Primitive {
        match self { Self::Triangles => gl::Primitive::Triangles }
    }
}

pub struct VertexBufferBackend {
    vbo: gl::Buffer,
    vao: gl::VertexArray,
    /// Size of a vertex.
    vertsize: usize
}

impl VertexBufferBackend {
    pub fn new(gp: &crate::Graphics, layout: &[crate::Attrib]) -> Self {

        gp.backend.ctx.bind(None);

        let vbo = gl::gen_buffer(gl::BufferType::Array);
        let vao = gl::gen_vertex_array();

        let vertsize = layout.iter()
            .map(|it| it.size())
            .sum();

        let mut loc = 0;
        let mut offset = 0;

        for it in layout {

            gl::vertex_attrib_pointer2(&vao, &vbo, gl::VertexAttribs {
                loc: gl::AttribLocation::new(loc),
                count: it.count,
                kind: it.kind.togl(),
                normalize: false,
                stride: vertsize,
                start: offset
            });

            loc += 1;
            offset += it.size();

        }



        Self { vbo, vao, vertsize }

    }

    pub fn frombuf(&self, gp: &crate::Graphics, src: &[u8]) {
        gp.backend.ctx.bind(None);
        gl::buffer_data(&self.vbo, src, gl::DrawHint::Dynamic);
    }

    pub fn vertsize(&self) -> usize {
        self.vertsize
    }
}

impl crate::AttribKind {
    fn togl(&self) -> gl::DataType {
        match self {
            Self::F32 => gl::DataType::F32,
            Self::U32 => gl::DataType::U32,
            Self::I32 => gl::DataType::I32,
            Self::U16 => gl::DataType::U16,
            Self::I16 => gl::DataType::I16,
            Self::U8  => gl::DataType::U8,
            Self::I8  => gl::DataType::I8
        }
    }
}

impl crate::Attrib {
    pub fn size(&self) -> usize {
        self.kind.togl().size() * self.count
    }
}
