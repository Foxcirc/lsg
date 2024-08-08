
// simple wrapper for common opengl functions

use std::{ffi::{c_void as void, CStr, CString}, fmt, mem::size_of, ptr::{null, null_mut}, slice, sync::Mutex};
use num_enum::TryFromPrimitive;

pub fn load_with<F: FnMut(&'static str) -> *const void>(f: F) {
    gl::load_with(f)
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

    let source = DebugSource::try_from_primitive(source).unwrap();
    let kind = DebugType::try_from_primitive(kind).unwrap();
    let severity = DebugSeverity::try_from_primitive(severity).unwrap();

    f(source, kind, severity, id, message);

}

pub fn debug_message_callback<F: DebugCallback + Send + 'static>(f: F) {

    let userdata = &mut *USERDATA.lock().unwrap();
    *userdata = Some(Box::new(f));

    unsafe { gl::DebugMessageCallback(Some(debug_callback), null_mut()) };

}

pub fn debug_message_control(severity: Option<DebugSeverity>, source: Option<DebugSource>, kind: Option<DebugType>, enabled: bool) {
    unsafe { gl::DebugMessageControl(
        source.map(|val| val as u32).unwrap_or(gl::DONT_CARE),
        kind.map(|val| val as u32).unwrap_or(gl::DONT_CARE),
        severity.map(|val| val as u32).unwrap_or(gl::DONT_CARE),
        0, // ids.len
        null(), // ids (array)
        enabled as u8
    ) }
}

pub fn debug_message_tracing_handler(source: DebugSource, kind: DebugType, severity: DebugSeverity, id: u32, msg: &str) {

    use DebugSeverity::*;

    let _span = tracing::span!(tracing::Level::INFO, "GlMessage").entered();

    let message = format!("gl {}, from {:?}, of kind {:?}, '{:?}'", id, source, kind, msg.trim_end_matches("\n"));

    if severity == Notification || severity == Low {
        tracing::debug!("{}", message);
    } else if severity == Medium {
        tracing::warn!("{}", message);
    } else {
        tracing::error!("{}", message);
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u32)]
pub enum DebugSeverity {
    High         = gl::DEBUG_SEVERITY_HIGH,
    Medium       = gl::DEBUG_SEVERITY_MEDIUM,
    Low          = gl::DEBUG_SEVERITY_LOW,
    Notification = gl::DEBUG_SEVERITY_NOTIFICATION
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u32)]
pub enum DebugType {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u32)]
pub enum DebugSource {
    Api            = gl::DEBUG_SOURCE_API,
    Application    = gl::DEBUG_SOURCE_APPLICATION,
    Other          = gl::DEBUG_SOURCE_OTHER,
    ShaderCompiler = gl::DEBUG_SOURCE_SHADER_COMPILER,
    ThirdParty     = gl::DEBUG_SOURCE_THIRD_PARTY,
    WindowSystem   = gl::DEBUG_SOURCE_WINDOW_SYSTEM
}

pub struct Shader {
    id: u32
}

pub fn create_shader(kind: ShaderType, source: &str) -> Result<Shader, ShaderError> {

    let source_ptr = source.as_ptr().cast();
    let source_len = source.len();

    let id = unsafe { gl::CreateShader(kind as u32) };
    unsafe { gl::ShaderSource(id, 1, &source_ptr, (&source_len as *const usize).cast()) };
    unsafe { gl::CompileShader(id) };

    if !shader_compile_status(id) {

        let len = shader_info_log_length(id);
        let mut written = 0; // not actually used
        let mut buf: Vec<u8> = Vec::new();
        buf.resize(len, 0);

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
    msg: CString,
    kind: ShaderType
}

impl fmt::Debug for ShaderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,
            "ShaderError {{\x1b[0;31m\n    {:?}Shader:\n    {}\n\x1b[0;39m}}",
            self.kind, self.msg.to_string_lossy().trim_end_matches("\n")
        )
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum ShaderType {
    Vertex = gl::VERTEX_SHADER,
    Fragment = gl::FRAGMENT_SHADER,
    Compute = gl::COMPUTE_SHADER
}

pub struct Program {
    id: u32,
    shaders: Vec<Shader>,
}

pub fn create_program() -> Program {
    Program {
        id: unsafe { gl::CreateProgram() },
        shaders: Vec::with_capacity(2)
    }
}

pub fn attach_shader(program: &mut Program, shader: Shader) {
    unsafe { gl::AttachShader(program.id, shader.id) }
    program.shaders.push(shader);
}

pub fn link_program(mut program: Program) -> Result<LinkedProgram, LinkError> {

    unsafe { gl::LinkProgram(program.id) }

    for shader in program.shaders.drain(..) {
        unsafe { gl::DeleteShader(shader.id) }
    }

    if !program_compile_status(program.id) {

        let len = program_info_log_length(program.id);
        let mut written = 0; // not actually used
        let mut buf: Vec<u8> = Vec::new();
        buf.resize(len, 0);

        unsafe { gl::GetProgramInfoLog(program.id, len as i32, &mut written, buf.as_mut_ptr().cast()) };

        let msg = CString::from_vec_with_nul(buf).unwrap();

        Err(LinkError { msg })

    } else {
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
            "LinkErrorError {{\x1b[0;31m\n    {}\n\x1b[0;39m}}",
            self.msg.to_string_lossy().trim_end_matches("\n")
        )
    }
}

pub struct LinkedProgram {
    pub(crate) id: u32,
}

pub fn bind_program(program: &LinkedProgram) {
    unsafe { gl::UseProgram(program.id) }
}

pub fn attrib_location(program: &LinkedProgram, name: &str) -> Result<usize, AttribUnknown> {
    let mut buf = [0; 1024];
    let cname = to_small_cstr(&mut buf, name);
    let result = unsafe { gl::GetAttribLocation(program.id, cname.as_ptr()) };
    if result == -1 {
        Err(AttribUnknown)
    } else {
        Ok(result as usize)
    }
}

#[derive(Debug)]
pub struct AttribUnknown;

#[repr(transparent)]
pub struct VertexArrayObject {
    id: u32
}

pub fn gen_vertex_array() -> VertexArrayObject {
    let mut id = 0;
    unsafe { gl::GenVertexArrays(1, &mut id) };
    VertexArrayObject { id }
}

pub fn bind_vertex_array(this: &VertexArrayObject) {
    unsafe { gl::BindVertexArray(this.id) }
}

pub fn gen_buffer(kind: BufferType) -> Buffer {

    let mut id = 0;
    unsafe { gl::GenBuffers(1, &mut id) };

    Buffer { id, kind }
    
}

pub fn buffer_data(this: &Buffer, data: &[f32], usage: DrawHint) {
    
    bind_buffer(this);

    unsafe { gl::BufferData(
        this.kind as u32,
        (data.len() * size_of::<f32>()) as isize,
        data.as_ptr().cast(),
        usage as u32
    ) }

}

pub fn bind_buffer(this: &Buffer) {
    unsafe { gl::BindBuffer(this.kind as u32, this.id) }
}

pub struct Buffer {
    id: u32,
    kind: BufferType,
}

/// The array will also be enabled.
pub fn vertex_attribs(vao: &VertexArrayObject,vbo: &Buffer, location: usize, count: usize, kind: DataType, normalize: bool, stride: usize, start: usize) {

    assert_eq!(vbo.kind, BufferType::ArrayBuffer);

    bind_vertex_array(vao);
    bind_buffer(vbo);

    unsafe { gl::VertexAttribPointer(
        location as u32,
        count as i32,
        kind as u32,
        normalize as u8,
        stride as i32,
        start as *const _,
    ) };

    unsafe { gl::EnableVertexAttribArray(location as u32) };

}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum DataType {
    Float = gl::FLOAT,
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
    ArrayBuffer   = gl::ARRAY_BUFFER,
    ElementBuffer = gl::ELEMENT_ARRAY_BUFFER,
}


pub fn uniform_location(program: &LinkedProgram, name: &str) -> Result<usize, UniformUnknown> {
    let mut buf = [0; 1024];
    let cname = to_small_cstr(&mut buf, name);
    let result = unsafe { gl::GetUniformLocation(program.id, cname.as_ptr()) };
    if result == -1 {
        Err(UniformUnknown)
    } else {
        Ok(result as usize)
    }
}

#[derive(Debug)]
pub struct UniformUnknown;

pub fn uniform_4f(program: &LinkedProgram, uniform: usize, x: f32, y: f32, z: f32, w: f32) {
    bind_program(program);
    unsafe { gl::Uniform4f(uniform as i32, x, y, z, w) };
}

pub fn uniform_3f(program: &LinkedProgram, uniform: usize, x: f32, y: f32, z: f32) {
    bind_program(program);
    unsafe { gl::Uniform3f(uniform as i32, x, y, z) };
}

pub fn resize_viewport(width: u32, height: u32) {
    unsafe { gl::Viewport(0, 0, width as i32, height as i32) }
}

pub fn draw_arrays(program: &LinkedProgram, vao: &VertexArrayObject, primitive: Primitive, start: usize, count: usize) {
    bind_program(program);
    bind_vertex_array(vao);
    unsafe { gl::DrawArrays(primitive as u32, start as i32, count as i32) }
}

pub fn draw_elements(program: &LinkedProgram, vao: &VertexArrayObject, primitive: Primitive, count: usize) {
    bind_program(program);
    bind_vertex_array(vao);
    unsafe { gl::DrawElements(primitive as u32, count as i32, gl::UNSIGNED_INT, null()) }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum Primitive {
    Triangles = gl::TRIANGLES,
    Lines = gl::LINES,
    LineStrip = gl::LINE_STRIP,
}

pub fn clear(r: f32, g: f32, b: f32, alpha: f32) {
    unsafe { gl::ClearColor(r, g, b, alpha) };
    unsafe { gl::Clear(gl::COLOR_BUFFER_BIT) };
}

pub fn polygon_mode(mode: PolygonMode) {
    unsafe { gl::PolygonMode(gl::FRONT_AND_BACK, mode as u32) }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum PolygonMode {
    Fill = gl::FILL,
    Line = gl::LINE
}

pub fn to_small_cstr<'d>(buf: &'d mut [u8; 1024], text: &str) -> &'d CStr {

    let len = text.len();

    buf[..len].copy_from_slice(text.as_bytes()); // copy the name
    buf[len] = 0u8; // add the null byte

   CStr::from_bytes_with_nul(&buf[..=len]).unwrap()

}

