
/// Simple wrapper around `gl` that improves ergonomics.
///
/// # Why use this
///
/// This crate aims to take the pain out of the old and clumbersome api, while
/// being only a small wrapper without huge runtime cost.
///
/// 1. All functions are safe
/// 2. Strong typing for all kinds of handles
/// 3. Automatic binding (the bind functions are only provided for special use cases)
/// 4. Resource cleanup on drop
///
/// This library does not deal with context creation, for that you should
/// use platform specific libraries like `wgl` or `egl`.
///
/// # Initialization
/// 1. bind a context
/// 2. use [`load_with`] to load all functions that are present
///
/// # Debug messages
/// If you are using `tracing` the easiest way to receive debug messages is
/// `gl::debug_message_callback(gl::debug_message_tracing_handler)` Otherwise,
/// you can write your own handler. Note that debug messages are only supported
/// in gl version 4.0 and onwards.
///
/// # Sharing objects
/// Most structs simply contain an `id: u32`. However because the resource is
/// cleaned up on drop, you still can't just clone it. To share e.g. a [`Buffer`] between
/// mutliple places in your code you should use an [`Rc`](std::rc::Rc).
///
/// # Completeness
/// Right now, not all functions are implemented and many enums are incomplete,
/// but adding more is really easy and often a matter of minutes.

use std::{ffi::{c_void as void, CStr, CString}, fmt, mem::size_of, ptr::{null, null_mut}, slice, sync::Mutex, error::Error as StdError};
use common::{Size, Rect};

#[derive(Debug)]
pub struct FnsUnknown;

pub fn load_with<F: FnMut(&'static str) -> Option<extern "system" fn()>>(mut f: F) -> Result<(), FnsUnknown> {

    let mut result = Ok(());

    gl::load_with(|name| match f(name) {
        Some(ptr) => ptr as *const void,
        None => {
            result = Err(FnsUnknown);
            null()
        },
    });

    result

}

pub trait DebugCallback: Fn(DebugSource, DebugType, DebugSeverity, u32, &str) {}
impl<F: Fn(DebugSource, DebugType, DebugSeverity, u32, &str)> DebugCallback for F {}

static USERDATA: Mutex<Option<Box<dyn DebugCallback + Send>>> = Mutex::new(None);

extern "system" fn debug_callback(
    source: u32,
    kind: u32,
    id: u32,
    severity: u32,
    message_len: i32,
    message_ptr: *const i8,
    _userdata: *mut void
) {

    let mut userdata = USERDATA.lock().unwrap();
    let f = userdata.as_mut().unwrap();

    let message = std::str::from_utf8(
        unsafe { slice::from_raw_parts(message_ptr.cast(), message_len as usize) }
    ).unwrap();

    let source = DebugSource::try_from_u32(source).unwrap();
    let kind = DebugType::try_from_u32(kind).unwrap();
    let severity = DebugSeverity::try_from_u32(severity).unwrap();

    f(source, kind, severity, id, message);

}

#[track_caller]
pub fn debug_message_callback<F: DebugCallback + Send + 'static>(f: F) {

    let version = get_version();
    if version.major < 4 {
        let msg = format!("debug messages are unsupported this gl version ({}.{})", version.major, version.minor);
        f(DebugSource::Application, DebugType::Other, DebugSeverity::Notification, 0, &msg);
    }

    let userdata = &mut *USERDATA.lock().unwrap();
    *userdata = Some(Box::new(f));

    unsafe { gl::DebugMessageCallback(Some(debug_callback), null_mut()) };

}

pub fn debug_message_control(severity: Option<DebugSeverity>, source: Option<DebugSource>, kind: Option<DebugType>, enabled: bool) {
    unsafe { gl::DebugMessageControl(
        source   .map(|val| val as u32).unwrap_or(gl::DONT_CARE),
        kind     .map(|val| val as u32).unwrap_or(gl::DONT_CARE),
        severity .map(|val| val as u32).unwrap_or(gl::DONT_CARE),
        0, // ids.len
        null(), // ids (array)
        enabled as u8
    ) }
}

/// # Panics
/// `source` must be `Application` or `ThirdParty`.
#[track_caller]
pub fn debug_message_insert(severity: DebugSeverity, source: DebugSource, id: u32, msg: &str) {

    assert!(
        matches!(source, DebugSource::Application | DebugSource::ThirdParty),
        "`source` must be `Application` or `ThirdParty`"
    );

    unsafe { gl::DebugMessageInsert(
        source as u32,
        gl::DEBUG_TYPE_OTHER,
        id,
        severity as u32,
        msg.len() as i32,
        msg.as_ptr() as *const i8,
    ) }

}

pub fn debug_message_tracing_handler(source: DebugSource, _kind: DebugType, severity: DebugSeverity, _id: u32, msg: &str) {

    use DebugSeverity::*;

    let message = format!("from {:?}: {}", source, msg.trim_end_matches("\n"));

    if severity == Notification || severity == Low {
        tracing::debug!("{}", message);
    } else if severity == Medium {
        tracing::warn!("{}", message);
    } else {
        tracing::error!("{}", message);
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum DebugSeverity {
    // BREAKING: remember to change `try_from_u32` when updating variants
    High         = gl::DEBUG_SEVERITY_HIGH,
    Medium       = gl::DEBUG_SEVERITY_MEDIUM,
    Low          = gl::DEBUG_SEVERITY_LOW,
    Notification = gl::DEBUG_SEVERITY_NOTIFICATION
}

impl DebugSeverity {
    pub fn try_from_u32(v: u32) -> Option<Self> {
        match v {
            gl::DEBUG_SEVERITY_HIGH         => Some(Self::High),
            gl::DEBUG_SEVERITY_MEDIUM       => Some(Self::Medium),
            gl::DEBUG_SEVERITY_LOW          => Some(Self::Low),
            gl::DEBUG_SEVERITY_NOTIFICATION => Some(Self::Notification),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum DebugType {
    // BREAKING: remember to change `try_from_u32` when updating variants
    UndefinedBehaviour = gl::DEBUG_TYPE_UNDEFINED_BEHAVIOR,
    DeprecatedBehavior = gl::DEBUG_TYPE_DEPRECATED_BEHAVIOR,
    Error              = gl::DEBUG_TYPE_ERROR,
    Other              = gl::DEBUG_TYPE_OTHER,
    Performance        = gl::DEBUG_TYPE_PERFORMANCE,
    Portability        = gl::DEBUG_TYPE_PORTABILITY,
    PopGroup           = gl::DEBUG_TYPE_POP_GROUP,
    PushGroup          = gl::DEBUG_TYPE_PUSH_GROUP,
    Marker             = gl::DEBUG_TYPE_MARKER
}

impl DebugType {
    pub fn try_from_u32(v: u32) -> Option<Self> {
        match v {
            gl::DEBUG_TYPE_UNDEFINED_BEHAVIOR => Some(Self::UndefinedBehaviour),
            gl::DEBUG_TYPE_DEPRECATED_BEHAVIOR => Some(Self::DeprecatedBehavior),
            gl::DEBUG_TYPE_ERROR => Some(Self::Error),
            gl::DEBUG_TYPE_OTHER => Some(Self::Other),
            gl::DEBUG_TYPE_PERFORMANCE => Some(Self::Performance),
            gl::DEBUG_TYPE_PORTABILITY => Some(Self::Portability),
            gl::DEBUG_TYPE_POP_GROUP => Some(Self::PopGroup),
            gl::DEBUG_TYPE_PUSH_GROUP => Some(Self::PushGroup),
            gl::DEBUG_TYPE_MARKER => Some(Self::Marker),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum DebugSource {
    // BREAKING: remember to change `try_from_u32` when updating variants
    Api            = gl::DEBUG_SOURCE_API,
    Application    = gl::DEBUG_SOURCE_APPLICATION,
    Other          = gl::DEBUG_SOURCE_OTHER,
    ShaderCompiler = gl::DEBUG_SOURCE_SHADER_COMPILER,
    ThirdParty     = gl::DEBUG_SOURCE_THIRD_PARTY,
    WindowSystem   = gl::DEBUG_SOURCE_WINDOW_SYSTEM
}

impl DebugSource {
    pub fn try_from_u32(v: u32) -> Option<Self> {
        match v {
            gl::DEBUG_SOURCE_API => Some(Self::Api),
            gl::DEBUG_SOURCE_APPLICATION => Some(Self::Application),
            gl::DEBUG_SOURCE_OTHER => Some(Self::Other),
            gl::DEBUG_SOURCE_SHADER_COMPILER => Some(Self::ShaderCompiler),
            gl::DEBUG_SOURCE_THIRD_PARTY => Some(Self::ThirdParty),
            gl::DEBUG_SOURCE_WINDOW_SYSTEM => Some(Self::WindowSystem),
            _ => None
        }
    }
}

pub fn get_integer_v(property: Property) -> Result<usize, PropertyUnknown> {
    let mut out = -1;
    unsafe { gl::GetIntegerv(property as u32, &mut out) };
    match out {
        -1 => Err(PropertyUnknown),
        val => Ok(val as usize),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Version {
    major: usize,
    minor: usize,
}

/// The version of the currently bound context.
#[track_caller]
pub fn get_version() -> Version {
    let minor = get_integer_v(Property::MinorVersion);
    let major = get_integer_v(Property::MajorVersion);
    minor.and_then(move |minor| Ok(Version { minor, major: major? } ))
        .expect("cannot get version")
}

#[derive(Debug)]
pub struct PropertyUnknown;

#[repr(u32)]
pub enum Property {

    MajorVersion = gl::MAJOR_VERSION,
    MinorVersion = gl::MINOR_VERSION,

    FrameBufferBinding         = gl::FRAMEBUFFER_BINDING,
    ReadFramebufferBinding     = gl::READ_FRAMEBUFFER_BINDING,
    ArrayBufferBinding         = gl::ARRAY_BUFFER_BINDING,
    ElementBufferBinding       = gl::ELEMENT_ARRAY_BUFFER_BINDING,
    VertexArrayBinding         = gl::VERTEX_ARRAY_BINDING,

    Texture1DBinding           = gl::TEXTURE_BINDING_1D,
    Texture2DBinding           = gl::TEXTURE_BINDING_2D,
    Texture3DBinding           = gl::TEXTURE_BINDING_3D,
    Texture1DArrayBinding      = gl::TEXTURE_BINDING_1D_ARRAY,
    Texture2DArrayBinding      = gl::TEXTURE_BINDING_2D_ARRAY,
    Texture2DMultisampleBinding      = gl::TEXTURE_BINDING_2D_MULTISAMPLE,
    Texture2DMultisampleArrayBinding = gl::TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY,

    CurrentProgram             = gl::CURRENT_PROGRAM,
    RenderBufferBinding        = gl::RENDERBUFFER_BINDING,
    UniformBufferBinding       = gl::UNIFORM_BUFFER_BINDING,
    SamplerBinding             = gl::SAMPLER_BINDING,
    ShaderStorageBufferBinding = gl::SHADER_STORAGE_BUFFER_BINDING,
    AtomicCounterBufferBinding = gl::ATOMIC_COUNTER_BUFFER_BINDING,
    DrawIndirectBufferBinding  = gl::DRAW_INDIRECT_BUFFER_BINDING,
    QueryBufferBinding         = gl::QUERY_BUFFER_BINDING,

}

#[derive(Debug)]
pub struct Shader {
    id: u32
}

impl Drop for Shader {
    fn drop(&mut self) {
        if self.id != 0 {
            unsafe { gl::DeleteShader(self.id) }
        }
    }
}

impl Shader {
    pub const fn invalid() -> Self {
        Self { id: 0 }
    }
}

pub fn create_shader(kind: ShaderType, source: &str) -> Result<Shader, ShaderError> {

    let source_ptr = source.as_ptr().cast();
    let source_len = source.len();

    let id = unsafe { gl::CreateShader(kind as u32) };
    unsafe { gl::ShaderSource(id, 1, &source_ptr, (&source_len as *const usize).cast()) };
    unsafe { gl::CompileShader(id) };

    if !shader_compile_status(id) {

        let len = shader_info_log_length(id);

        if len == 0 {
            let msg = CString::new("[no error log provided]").unwrap();
            return Err(ShaderError { msg, kind });
        }

        let mut buf: Vec<u8> = Vec::new();
        buf.resize(len, 0);

        let mut written = 0; // not actually used
        unsafe { gl::GetShaderInfoLog(id, len as i32, &mut written, buf.as_mut_ptr().cast()) };

        let msg = CString::from_vec_with_nul(buf).unwrap();
        Err(ShaderError { msg, kind })

    } else {
        Ok(Shader { id })
    }

}

fn shader_compile_status(shader: u32) -> bool {
    let mut out = 0;
    unsafe { gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut out) };
    out as u8 == gl::TRUE
}

fn shader_info_log_length(shader: u32) -> usize {
    let mut out = 0;
    unsafe { gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut out) };
    out as usize
}

pub struct ShaderError {
    pub msg: CString,
    pub kind: ShaderType
}

impl fmt::Debug for ShaderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,
            "ShaderError {{ kind: {:?}, tmsg: {}}}",
            self.kind, self.msg.to_string_lossy().trim_end_matches("\n")
        )
    }
}

impl fmt::Display for ShaderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,
            "shader compilation error in {:?}, {}", self.kind, self.msg.to_string_lossy().trim_end_matches("\n")
        )
    }
}

impl StdError for ShaderError {}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum ShaderType {
    Vertex = gl::VERTEX_SHADER,
    Fragment = gl::FRAGMENT_SHADER,
    Compute = gl::COMPUTE_SHADER
}

#[derive(Debug)]
pub struct Program {
    id: u32,
    shaders: Vec<Shader>,
    keepalive: bool,
}

impl Drop for Program {
    fn drop(&mut self) {
        if self.id != 0 && !self.keepalive {
            unsafe { gl::DeleteProgram(self.id) }
        }
    }
}

impl Program {
    pub const fn invalid() -> Self {
        Self { id: 0, shaders: Vec::new(), keepalive: false }
    }
}

pub fn create_program() -> Program {
    Program {
        id: unsafe { gl::CreateProgram() },
        shaders: Vec::with_capacity(2),
        keepalive: false,
    }
}

pub fn attach_shader(program: &mut Program, shader: Shader) {
    unsafe { gl::AttachShader(program.id, shader.id) }
    program.shaders.push(shader);
}

pub fn link_program(mut program: Program) -> Result<LinkedProgram, LinkError> {

    unsafe { gl::LinkProgram(program.id) }

    if !program_compile_status(program.id) {

        let len = program_info_log_length(program.id);
        let mut written = 0; // not actually used
        let mut buf: Vec<u8> = Vec::new();
        buf.resize(len, 0);

        unsafe { gl::GetProgramInfoLog(program.id, len as i32, &mut written, buf.as_mut_ptr().cast()) };

        let msg = CString::from_vec_with_nul(buf).unwrap();

        Err(LinkError { msg })

    } else {
        program.keepalive = true; // should not be deleted in `drop`
        Ok(LinkedProgram { id: program.id })
    }

}

fn program_compile_status(program: u32) -> bool {
    let mut out = 0;
    unsafe { gl::GetProgramiv(program, gl::LINK_STATUS, &mut out) };
    out as u8 == gl::TRUE
}

fn program_info_log_length(program: u32) -> usize {
    let mut out = 0;
    unsafe { gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut out) };
    out as usize
}

pub struct LinkError {
    msg: CString,
}

impl fmt::Debug for LinkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,
            "LinkErrorError {{\n\tmsg: {}}}",
            self.msg.to_string_lossy().trim_end_matches("\n")
        )
    }
}

impl fmt::Display for LinkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg.to_string_lossy().trim_end_matches("\n"))
    }
}

impl StdError for LinkError {}

#[derive(Debug)]
pub struct LinkedProgram {
    pub(crate) id: u32,
}

impl Drop for LinkedProgram {
    fn drop(&mut self) {
        if self.id != 0 {
            unsafe { gl::DeleteProgram(self.id) }
        }
    }
}

impl LinkedProgram {
    pub const fn invalid() -> Self { Self { id: 0 } }
    pub fn current() -> Self {
        Self { id: get_integer_v(Property::CurrentProgram).unwrap_or(0) as u32 }
    }
}

pub fn bind_program(program: &LinkedProgram) {
    unsafe { gl::UseProgram(program.id) }
}

pub fn attrib_location(program: &LinkedProgram, name: &str) -> Result<AttribLocation, AttribUnknown> {
    let mut buf = [0; 1024];
    let cname = to_small_cstr(&mut buf, name);
    let result = unsafe { gl::GetAttribLocation(program.id, cname.as_ptr()) };
    if result == -1 {
        Err(AttribUnknown)
    } else {
        Ok(AttribLocation { index: result as u32 })
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct AttribLocation {
    pub index: u32,
}

impl AttribLocation {
    pub fn new(index: u32) -> Self {
        Self { index }
    }
}

impl From<u32> for AttribLocation {
    fn from(value: u32) -> Self {
        Self::new(value)
    }
}

#[derive(Debug)]
pub struct AttribUnknown;

#[derive(Debug)]
pub struct VertexArray {
    id: u32
}

impl Drop for VertexArray {
    fn drop(&mut self) {
        if self.id != 0 {
            unsafe { gl::DeleteVertexArrays(1, &self.id); }
        }
    }
}

impl VertexArray {
    pub const fn invalid() -> Self { Self { id: 0 } }
    pub fn current() -> Self {
        Self { id: get_integer_v(Property::VertexArrayBinding).unwrap_or(0) as u32 }
    }
}

pub fn gen_vertex_array() -> VertexArray {
    let mut id = 0;
    unsafe { gl::GenVertexArrays(1, &mut id) };
    VertexArray { id }
}

pub fn bind_vertex_array(this: &VertexArray) {
    unsafe { gl::BindVertexArray(this.id) }
}

pub fn gen_buffer(kind: BufferType) -> Buffer {
    let mut id = 0;
    unsafe { gl::GenBuffers(1, &mut id) };
    Buffer { id, kind }
}

/// This function doesn't perform type-checking, it accepts a slice of any type and hands it out as bytes.
/// This is not unsafe, since all bytes are valid numbers for OpenGL, but can still be unintuitive.
/// In order to correctly handle vertex layout and avoid bugs use [`AttribStorage`].
pub fn buffer_data<T>(buffer: &Buffer, data: &[T], usage: DrawHint) {

    bind_buffer(buffer);

    unsafe { gl::BufferData(
        buffer.kind as u32,
        (data.len() * size_of::<T>()) as isize,
        data.as_ptr().cast(),
        usage as u32
    ) }

}

pub fn bind_buffer(this: &Buffer) {
    unsafe { gl::BindBuffer(this.kind as u32, this.id) }
}

#[derive(Debug)]
pub struct Buffer {
    id: u32,
    kind: BufferType,
}

impl Drop for Buffer {
    fn drop(&mut self) {
        if self.id != 0 {
            unsafe { gl::DeleteBuffers(1, &self.id); }
        }
    }
}

impl Buffer {
    pub const fn invalid() -> Self { Self { id: 0, kind: BufferType::Array } }
    pub fn current(kind: BufferType) -> Self {
        match kind {
            BufferType::Array        => Self { id: get_integer_v(Property::ArrayBufferBinding).unwrap_or(0) as u32, kind },
            BufferType::Element      => Self { id: get_integer_v(Property::ElementBufferBinding).unwrap_or(0) as u32, kind },
            BufferType::DrawIndirect => Self { id: get_integer_v(Property::DrawIndirectBufferBinding).unwrap_or(0) as u32, kind },
        }
    }
}

// TODO: add a "vertex layout specification" mechanism that allows:
// - auto-generating the aproapriate vertex-attrib_... calls from the spec
// - verifying that the correct data is passed when using VertexStorage
// in that way a "renderer" can assure that the vertex data it gets is in the correct layout, a clean cooperation between shaper and renderer

/// This utility type acts as a buffer to store your vertex data. This should
/// make appending vertex attributes with multiple different types easier.
#[derive(Debug, Default)]
pub struct AttribStorage {
    pub inner: Vec<u8>,
}

impl AttribStorage {

    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn extend_f<const N: usize>(&mut self, vals: [f32; N]) {
        self.inner.extend(vals.map(|it| it.to_ne_bytes()).as_flattened());
    }

    pub fn extend_i<const N: usize>(&mut self, vals: [i32; N]) {
        self.inner.extend(vals.map(|it| it.to_ne_bytes()).as_flattened());
    }

    pub fn extend_u<const N: usize>(&mut self, vals: [u32; N]) {
        self.inner.extend(vals.map(|it| it.to_ne_bytes()).as_flattened());
    }

}

pub fn vertex_attrib_1f(vao: &VertexArray, loc: impl Into<AttribLocation>, x: f32) {
    bind_vertex_array(vao);
    unsafe { gl::VertexAttrib1f(loc.into().index, x) };
}

pub fn vertex_attrib_2f(vao: &VertexArray, loc: impl Into<AttribLocation>, x: f32, y: f32) {
    bind_vertex_array(vao);
    unsafe { gl::VertexAttrib2f(loc.into().index, x, y) };
}

pub fn vertex_attrib_3f(vao: &VertexArray, loc: impl Into<AttribLocation>, x: f32, y: f32, z: f32) {
    bind_vertex_array(vao);
    unsafe { gl::VertexAttrib3f(loc.into().index, x, y, z) };
}

pub fn vertex_attrib_1i(vao: &VertexArray, loc: impl Into<AttribLocation>, x: i32) {
    bind_vertex_array(vao);
    unsafe { gl::VertexAttribI1i(loc.into().index, x) };
}

pub fn vertex_attrib_1u(vao: &VertexArray, loc: impl Into<AttribLocation>, x: u32) {
    bind_vertex_array(vao);
    unsafe { gl::VertexAttribI1ui(loc.into().index, x) };
}

#[derive(Debug, Default)]
pub struct VertexAttribs {
    pub loc: AttribLocation,
    pub count: usize,
    pub kind: DataType,
    pub normalize: bool,
    pub stride: usize,
    pub start: usize,
}

/// Specify a vertex attribute.
///
/// # Caveats
/// - The attribute will be immediatly `enabled`.
/// - Remember `stride` is in bytes!
/// - There is no special variant for integer types.
#[track_caller]
pub fn vertex_attrib_pointer(vao: &VertexArray, vbo: &Buffer, loc: impl Into<AttribLocation>, count: usize, kind: DataType, normalize: bool, stride: usize, start: usize) {

    assert_eq!(vbo.kind, BufferType::Array);

    bind_vertex_array(vao);
    bind_buffer(vbo);

    let index = loc.into().index;

    if kind == DataType::F32 {

        unsafe { gl::VertexAttribPointer(
            index,
            count as i32,
            kind as u32,
            normalize as u8,
            stride as i32,
            start as *const _,
        ) };

    } else {

        // why **the FUCK** is there another function for this and there is no warning
        // when using the other function with an integer data type
        unsafe { gl::VertexAttribIPointer(
            index,
            count as i32,
            kind as u32,
            stride as i32,
            start as *const _,
        ) };

    };

    unsafe { gl::EnableVertexAttribArray(index) };

}

/// Same as the other one, but accepts named arguments as a struct.
#[track_caller]
pub fn vertex_attrib_pointer2(vao: &VertexArray, vbo: &Buffer, attribs: VertexAttribs) {
    vertex_attrib_pointer(
        vao, vbo,
        attribs.loc, attribs.count, attribs.kind,
        attribs.normalize, attribs.stride, attribs.start
    );
}

#[derive(Debug, Clone, Copy)]
pub enum Divisor {
    PerVertex,
    PerInstances(u32),
}

#[track_caller]
pub fn vertex_attrib_divisor(vao: &VertexArray, location: u32, divisor: Divisor) {
    bind_vertex_array(vao);
    let value = match divisor {
        Divisor::PerVertex => 0,
        Divisor::PerInstances(0) => panic!("Divisor::PerInstances can't be 0"),
        Divisor::PerInstances(n) => n,
    };
    unsafe { gl::VertexAttribDivisor(location, value) };
}

#[derive(Debug)]
pub struct FrameBuffer {
    id: u32,
}

impl Drop for FrameBuffer {
    fn drop(&mut self) {
        if self.id != 0 {
            unsafe { gl::DeleteFramebuffers(1, &self.id) }
        }
    }
}

impl Default for FrameBuffer {
    fn default() -> Self {
        Self { id: 0 }
    }
}

impl FrameBuffer {
    pub fn current() -> Self {
        Self { id: get_integer_v(Property::FrameBufferBinding).unwrap_or(0) as u32 }
    }
}

pub fn gen_frame_buffer() -> FrameBuffer {
    let mut id = 0;
    unsafe { gl::GenFramebuffers(1, &mut id) };
    FrameBuffer { id }
}

pub fn bind_frame_buffer(fbo: &FrameBuffer) {
    unsafe { gl::BindFramebuffer(gl::FRAMEBUFFER, fbo.id) };
}

pub fn bind_read_frame_buffer(fbo: &FrameBuffer) {
    unsafe { gl::BindFramebuffer(gl::READ_FRAMEBUFFER, fbo.id) };
}

pub fn bind_draw_frame_buffer(fbo: &FrameBuffer) {
    unsafe { gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, fbo.id) };
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum AttachmentPoint {
    Color0 = gl::COLOR_ATTACHMENT0,
    Color1 = gl::COLOR_ATTACHMENT1,
}

pub fn frame_buffer_texture_2d(fbo: &FrameBuffer, attachment: AttachmentPoint, texture: &Texture, lod: usize) {
    bind_frame_buffer(fbo);
    unsafe { gl::FramebufferTexture2D(
        gl::FRAMEBUFFER,
        attachment as u32,
        texture.kind as u32,
        texture.id as u32,
        lod as i32
    ) }
}

pub fn frame_buffer_render_buffer(fbo: &FrameBuffer, attachment: AttachmentPoint, rbo: &RenderBuffer) {
    bind_frame_buffer(fbo);
    unsafe { gl::FramebufferRenderbuffer(
        gl::FRAMEBUFFER,
        attachment as u32,
        gl::RENDERBUFFER,
        rbo.id,
    ) }
}

pub fn draw_buffers(fbo: &FrameBuffer, buffers: &[AttachmentPoint]) {
    bind_frame_buffer(fbo);
    unsafe { gl::DrawBuffers(buffers.len() as i32, buffers.as_ptr().cast()) }
}

#[track_caller]
pub fn clear_buffer_v(fbo: &FrameBuffer, attachment: AttachmentPoint, value: &[f32]) {
    bind_frame_buffer(fbo);
    // somebody has to be seriously punished for creating this horrible api...
    // why is this completely different compared to ALL other functions that deal with fbo's!?
    let (param1, param2) = match attachment {
        AttachmentPoint::Color0 => (gl::COLOR, 0),
        AttachmentPoint::Color1 => (gl::COLOR, 1),
    };
    let mut safe = [0.0; 4];
    safe[..value.len()].copy_from_slice(value);
    unsafe { gl::ClearBufferfv(param1, param2, safe.as_ptr()) }
}

/// Copy a region from `source` to `target`. Currently only copies the **color buffer**.
/// You don't need to bind anything yourself!
pub fn blit_frame_buffer(target: (&FrameBuffer, Rect), source: (&FrameBuffer, Rect), filter: FilterValue) {
    bind_draw_frame_buffer(target.0);
    bind_read_frame_buffer(source.0);
    unsafe { gl::BlitFramebuffer(
        source.1.pos.x as i32, source.1.pos.y as i32, source.1.size.w as i32, source.1.size.h as i32, // source rect
        target.1.pos.x as i32, target.1.pos.y as i32, target.1.size.w as i32, target.1.size.h as i32, // target rect
        gl::COLOR_BUFFER_BIT,
        filter as u32,
    ); }
}

#[derive(Debug)]
pub struct RenderBuffer {
    id: u32,
}

impl Drop for RenderBuffer {
    fn drop(&mut self) {
        if self.id != 0 {
            unsafe { gl::DeleteRenderbuffers(1, &self.id) }
        }
    }
}

impl RenderBuffer {
    pub const fn invalid() -> Self { Self { id: 0 } }
    pub fn current() -> Self { // TODO: Make all the Option<Self> and remove unwrap_or(0), rn silently failing is pretty bad design
        Self { id: get_integer_v(Property::RenderBufferBinding).unwrap_or(0) as u32 }
    }
}

pub fn gen_render_buffer() -> RenderBuffer {
    let mut id = 0;
    unsafe { gl::GenRenderbuffers(1, &mut id) };
    RenderBuffer { id }
}

pub fn bind_render_buffer(this: &RenderBuffer) {
    unsafe { gl::BindRenderbuffer(gl::RENDERBUFFER, this.id) }
}

pub fn render_buffer_storage(this: &RenderBuffer, format: PreciseColorFormat, size: Size) {
    bind_render_buffer(this);
    unsafe { gl::RenderbufferStorage(gl::RENDERBUFFER, format as u32, size.w as i32, size.h as i32); }
}

pub fn render_buffer_storage_multisample(this: &RenderBuffer, samples: usize, format: PreciseColorFormat, size: Size) {
    bind_render_buffer(this);
    unsafe { gl::RenderbufferStorageMultisample(gl::RENDERBUFFER, samples as i32, format as u32, size.w as i32, size.h as i32); }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum PreciseColorFormat {
    R8      = gl::R8,
    Rgba8   = gl::RGBA8,
    Rgba16F = gl::RGBA16F,
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum BaseColorFormat { // why the fuck does this even exist...
    Rgba = gl::RGBA,
    Red  = gl::RED,
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum TextureType {
    Basic1D             = gl::TEXTURE_1D,
    Basic2D             = gl::TEXTURE_2D,
    Basic3D             = gl::TEXTURE_3D,
    Array1D             = gl::TEXTURE_1D_ARRAY,
    Array2D             = gl::TEXTURE_2D_ARRAY,
    Multisample2D       = gl::TEXTURE_2D_MULTISAMPLE,
    MultisampleArray2D  = gl::TEXTURE_2D_MULTISAMPLE_ARRAY,
}

#[derive(Debug)]
pub struct Texture {
    id: u32,
    kind: TextureType,
}

impl Drop for Texture {
    fn drop(&mut self) {
        if self.id != 0 {
            unsafe { gl::DeleteTextures(1, &self.id) }
        }
    }
}

impl Texture {
    pub const fn invalid() -> Self { Self { id: 0, kind: TextureType::Basic1D } }
    /// Returns the texture which is currently bound to that type.
    pub fn current(kind: TextureType) -> Self {
        match kind {
            TextureType::Basic1D => Self { id: get_integer_v(Property::Texture1DBinding).unwrap_or(0) as u32, kind },
            TextureType::Basic2D => Self { id: get_integer_v(Property::Texture2DBinding).unwrap_or(0) as u32, kind },
            TextureType::Basic3D => Self { id: get_integer_v(Property::Texture3DBinding).unwrap_or(0) as u32, kind },
            TextureType::Array1D => Self { id: get_integer_v(Property::Texture1DArrayBinding).unwrap_or(0) as u32, kind },
            TextureType::Array2D => Self { id: get_integer_v(Property::Texture2DArrayBinding).unwrap_or(0) as u32, kind },
            TextureType::Multisample2D      => Self { id: get_integer_v(Property::Texture2DMultisampleBinding).unwrap_or(0) as u32, kind },
            TextureType::MultisampleArray2D => Self { id: get_integer_v(Property::Texture2DMultisampleArrayBinding).unwrap_or(0) as u32, kind },
        }
    }
}

pub fn gen_texture(kind: TextureType) -> Texture {
    let mut id = 0;
    unsafe { gl::GenTextures(1, &mut id) };
    Texture { id, kind }
}

pub fn bind_texture(texture: &Texture) {
    unsafe { gl::BindTexture(texture.kind as u32, texture.id) };
}

pub fn tex_image_2d(texture: &Texture, lod: usize, fcolor: PreciseColorFormat, size: Size, fpixel: BaseColorFormat, kind: DataType) {
    bind_texture(texture);
    unsafe { gl::TexImage2D(
        texture.kind as u32,
        lod as i32,
        fcolor as i32,
        size.w as i32, size.h as i32,
        0, // "must always be 0"
        fpixel as u32,
        kind as u32,
        null()
    ) }
}

/// Not available in ES versions.
pub fn tex_image_2d_multisample(texture: &Texture, samples: usize, fcolor: PreciseColorFormat, size: Size) {
    bind_texture(texture);
    unsafe { gl::TexImage2DMultisample(
        texture.kind as u32,
        samples as i32,
        fcolor as u32,
        size.w as i32,
        size.h as i32,
        1,
    ); }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum FilterKind {
    MagFilter = gl::TEXTURE_MAG_FILTER,
    MinFilter = gl::TEXTURE_MIN_FILTER,
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum FilterValue {
    Linear = gl::LINEAR,
    Nearest = gl::NEAREST,
}

pub fn tex_parameter_i(texture: &Texture, property: FilterKind, value: FilterValue) {
    bind_texture(texture);
    unsafe { gl::TexParameteri(
        texture.kind as u32,
        property as u32,
        value as i32
    ) }
}

/// Will also bind the texture!
/// A location of `0` corresponds to `TEXTURE0`.
pub fn active_texture(location: usize, texture: &Texture) {
    let param = gl::TEXTURE0 + location as u32;
    unsafe { gl::ActiveTexture(param) };
    bind_texture(texture);
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum DataType {
    #[default]
    F32 = gl::FLOAT,
    I8 = gl::BYTE,
    U8 = gl::UNSIGNED_BYTE,
    I32 = gl::INT,
    U32 = gl::UNSIGNED_INT,
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum DrawHint {
    Static  = gl::STATIC_DRAW, // set once, used often
    Dynamic = gl::DYNAMIC_DRAW, // modified often, used often
    Stream  = gl::STREAM_DRAW // set once, used at most a few times
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum BufferType {
    Array        = gl::ARRAY_BUFFER,
    Element      = gl::ELEMENT_ARRAY_BUFFER,
    DrawIndirect = gl::DRAW_INDIRECT_BUFFER,
}


pub fn uniform_location(program: &LinkedProgram, name: &str) -> Result<UniformLocation, UniformUnknown> {
    let mut buf = [0; 1024];
    let cname = to_small_cstr(&mut buf, name);
    let index = unsafe { gl::GetUniformLocation(program.id, cname.as_ptr()) };
    if index == -1 {
        Err(UniformUnknown)
    } else {
        Ok(UniformLocation { index: index as u32 })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UniformLocation {
    pub index: u32,
}

impl UniformLocation {
    pub fn new(index: u32) -> Self {
        Self { index }
    }
}

impl From<u32> for UniformLocation {
    fn from(value: u32) -> Self {
        Self::new(value)
    }
}

#[derive(Debug)]
pub struct UniformUnknown;

pub fn uniform_1i(program: &LinkedProgram, uniform: UniformLocation, val: i32) {
    bind_program(program);
    unsafe { gl::Uniform1i(uniform.index as i32, val) };
}

pub fn uniform_1ui(program: &LinkedProgram, uniform: UniformLocation, val: u32) {
    bind_program(program);
    unsafe { gl::Uniform1ui(uniform.index as i32, val) };
}

pub fn uniform_3f(program: &LinkedProgram, uniform: UniformLocation, x: f32, y: f32, z: f32) {
    bind_program(program);
    unsafe { gl::Uniform3f(uniform.index as i32, x, y, z) };
}

pub fn uniform_4f(program: &LinkedProgram, uniform: UniformLocation, x: f32, y: f32, z: f32, w: f32) {
    bind_program(program);
    unsafe { gl::Uniform4f(uniform.index as i32, x, y, z, w) };
}

#[repr(u32)]
pub enum Capability {
    Blend = gl::BLEND,
    AlphaToCoverage = gl::SAMPLE_ALPHA_TO_COVERAGE,
}

pub fn enable(capability: Capability) {
    unsafe { gl::Enable(capability as u32) }
}

pub fn disable(capability: Capability) {
    unsafe { gl::Disable(capability as u32) }
}

#[repr(u32)]
pub enum BlendFunc {
    One              = gl::ONE,
    Zero             = gl::ZERO,
    SrcAlpha         = gl::SRC_ALPHA,
    OneMinusSrcAlpha = gl::ONE_MINUS_SRC_ALPHA,
}

pub fn blend_func(src: BlendFunc, dst: BlendFunc) {
    unsafe { gl::BlendFunc(src as u32, dst as u32) }
}

pub fn blend_func_i(attachment: AttachmentPoint, func1: BlendFunc, func2: BlendFunc) {
    let idx = match attachment {
        AttachmentPoint::Color0 => 0,
        AttachmentPoint::Color1 => 1,
    };
    unsafe { gl::BlendFunci(idx, func1 as u32, func2 as u32) }
}

pub fn blend_func_seperate(src1: BlendFunc, dst1: BlendFunc, src2: BlendFunc, dst2: BlendFunc) {
    unsafe { gl::BlendFuncSeparate(src1 as u32, dst1 as u32, src2 as u32, dst2 as u32) }
}

pub fn depth_mask(enabled: bool) {
    unsafe { gl::DepthMask(enabled as u8) }
}

pub fn resize_viewport(size: Size) {
    unsafe { gl::Viewport(0, 0, size.w as i32, size.h as i32) }
}

/// `count` is the number of vertices (not primitives).
pub fn draw_arrays(fbo: &FrameBuffer, program: &LinkedProgram, vao: &VertexArray, primitive: Primitive, start: usize, count: usize) {
    bind_frame_buffer(fbo);
    bind_program(program);
    bind_vertex_array(vao);
    unsafe { gl::DrawArrays(primitive as u32, start as i32, count as i32) }
}

/// Expects u32 as indices right now.
/// `start` is an index in bytes * sizeof(u32), not in bytes.
/// `count` is the number of indices (not primitives).
pub fn draw_elements(fbo: &FrameBuffer, program: &LinkedProgram, vao: &VertexArray, primitive: Primitive, start: usize, count: usize) {
    bind_frame_buffer(fbo);
    bind_program(program);
    bind_vertex_array(vao);
    unsafe { gl::DrawElements(primitive as u32, count as i32, gl::UNSIGNED_INT, (start * size_of::<u32>()) as *const void) }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct DrawArraysIndirectCommand {
    pub vertices: u32,
    pub instances: u32,
    pub first: u32,
    // SAFETY: reserved must be initialized to 0
    reserved: u32,
}

impl DrawArraysIndirectCommand {
    pub fn new(vertices: usize, instances: usize, first: usize) -> Self {
        Self {
            vertices: vertices as u32,
            instances: instances as u32,
            first: first as u32,
            reserved: 0
        }
    }
}

#[track_caller]
pub fn draw_arrays_indirect(fbo: &FrameBuffer, program: &LinkedProgram, vao: &VertexArray, commands: &Buffer, primitive: Primitive, start: usize) {
    assert_eq!(commands.kind, BufferType::DrawIndirect);
    bind_frame_buffer(fbo);
    bind_program(program);
    bind_vertex_array(vao);
    bind_buffer(commands);
    unsafe { gl::DrawArraysIndirect(primitive as u32, start as *const void) }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum Primitive {
    Triangles = gl::TRIANGLES,
    Lines = gl::LINES,
    LineStrip = gl::LINE_STRIP,
}

pub fn clear(fbo: &FrameBuffer, r: f32, g: f32, b: f32, alpha: f32) {
    bind_frame_buffer(fbo);
    unsafe { gl::ClearColor(r, g, b, alpha) };
    unsafe { gl::Clear(gl::COLOR_BUFFER_BIT) };
}

/// Sets both front and back buffer.
pub fn polygon_mode(mode: PolygonMode) {
    unsafe { gl::PolygonMode(gl::FRONT_AND_BACK, mode as u32) }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum PolygonMode {
    Fill = gl::FILL,
    Line = gl::LINE
}

#[track_caller]
pub fn to_small_cstr<'d>(buf: &'d mut [u8], text: &str) -> &'d CStr {

    let len = text.len();

    buf[..len].copy_from_slice(text.as_bytes()); // copy the name
    buf[len] = 0u8; // add the null byte

   CStr::from_bytes_with_nul(&buf[..len + 1]).unwrap()

}
