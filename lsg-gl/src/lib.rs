
use std::ffi::c_void as void;

pub use debug::*;
pub use shader::*;
pub use vao::*;
pub use buffer::*;
pub use draw::*;

// simple wrapper for common opengl functions

pub fn load_with<F: FnMut(&'static str) -> *const void>(f: F) {
    gl::load_with(f)
}

pub mod debug {
    
    use std::{ffi::c_void as void, sync::Mutex, ptr::{null_mut, null}, slice};

    use num_enum::TryFromPrimitive;

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

    pub fn colored_print(source: DebugSource, _kind: DebugType, severity: DebugSeverity, _id: u32, message: &str) {
        let color = if severity == DebugSeverity::High { "31" } else { "34" };
        println!(
            "gl/{:?} (Severity: {:?}): \x1b[{}m{}\x1b[39m",
            source, severity, color, message.trim_end_matches("\n")
        );
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

}

pub mod shader {

    use std::{ffi::CString, fmt};

    pub struct Shader {
        id: u32
    }

    impl Shader {

        pub fn new(kind: ShaderType, source: &str) -> Result<Self, ShaderError> {

            let source_ptr = source.as_ptr().cast();
            let source_len = source.len();

            let id = unsafe { gl::CreateShader(kind as u32) };
            unsafe { gl::ShaderSource(id, 1, &source_ptr, (&source_len as *const usize).cast()) };
            unsafe { gl::CompileShader(id) };

            if !compile_status(id) {

                let len = info_log_length(id);
                let mut written = 0; // not actually used
                let mut buf: Vec<u8> = Vec::new();
                buf.resize(len, 0);

                unsafe { gl::GetShaderInfoLog(id, len as i32, &mut written, buf.as_mut_ptr().cast()) };

                let msg = CString::from_vec_with_nul(buf).unwrap();
                Err(ShaderError { msg, kind })

            } else {
                Ok(Self { id })
            }

        }

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

    fn compile_status(shader: u32) -> bool {
        let mut out = 0;
        unsafe { gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut out) };
        out as u8 == gl::TRUE
    }

    fn info_log_length(shader: u32) -> usize {
        let mut out = 0;
        unsafe { gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut out) };
        out as usize
    }

    pub struct Program {
        id: u32,
        shaders: Vec<Shader>,
    }

    impl Program {

        pub fn new() -> Self {
            Self {
                id: unsafe { gl::CreateProgram() },
                shaders: Vec::with_capacity(2)
            }
        }

        pub fn attach(&mut self, shader: Shader) {
            unsafe { gl::AttachShader(self.id, shader.id) }
            self.shaders.push(shader);
        }
        
        pub fn link(mut self) -> LinkedProgram {
            unsafe { gl::LinkProgram(self.id) }
            for shader in self.shaders.drain(..) {
                unsafe { gl::DeleteShader(shader.id) }
            }
            LinkedProgram { id: self.id }
        }

    }

    pub struct LinkedProgram {
        id: u32,
    }

    pub fn bind_program(program: &LinkedProgram) {
        unsafe { gl::UseProgram(program.id) }
    }

}

pub mod vao {

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
    
}

pub mod buffer {

    use std::{mem::size_of, ptr::null};

    pub fn gen_buffer(kind: BufferType) -> Buffer {

        let mut id = 0;
        unsafe { gl::GenBuffers(1, &mut id) };

        Buffer { id, kind }
        
    }

    pub fn buffer_data<T>(this: &Buffer, data: &[T], usage: DrawHint) {
        
        bind_buffer(this);
        unsafe { gl::BufferData(
            this.kind as u32,
            (data.len() * size_of::<T>()) as isize,
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

    pub fn vertex_attribs(this: &Buffer, location: usize, count: usize, kind: DataType, normalize: bool, stride: usize) {

        assert_eq!(this.kind, BufferType::ArrayBuffer);

        bind_buffer(this);
        unsafe { gl::VertexAttribPointer(
            location as u32,
            count as i32,
            kind as u32,
            normalize as u8,
            (stride * size_of::<f32>()) as i32,
            null(), // unsupported
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


}

pub mod draw {

    use std::ptr::null;

    use crate::{VertexArrayObject, LinkedProgram, bind_program, bind_vertex_array};

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
        Triangles = gl::TRIANGLES
    }

    pub fn clear(r: f32, g: f32, b: f32, alpha: f32) {
        unsafe { gl::ClearColor(r, g, b, alpha) };
        unsafe { gl::Clear(gl::COLOR_BUFFER_BIT) };
    }
    
}

