
use std::{ffi::c_void as void, fmt, mem, sync::Arc, error::Error as StdError};
use common::*;

// trait Widget<R: Renderer>
// fn draw(&mut self, renderer: &mut R);

pub struct Renderer {
    // used for converting from screen-space to opengl view-space
    size: Size,
}

impl Renderer {

    pub fn new(size: Size) -> Self {

        Self {
            size
        }

    }
    
    pub fn resize(&mut self, size: Size) {
        self.size = size;
        gl::resize_viewport(size);
    }
    
    pub fn render<C>(&self, target: &C, geometry: &Geometry)
      where C: OpenGlContext {

        target.bind();

        // for now, just render all edges
        
    }

}

pub trait OpenGlContext {
    /// Make the context current.
    fn bind(&self);
}

pub struct Geometry {
    /// similar to ArrayBuffer, stores the vertices in a rather un-structured way
    vertices: Vec<Vertex>,
    /// similar to ElementBuffer, polygons reference the `vertices` to avoid duplication
    polygons: Vec<Polygon>,
    // TODO: implement custom geometry, with custom shaders and more
}

/// Vertex position in screen-space coordinates.
#[derive(Debug, Clone, Copy)]
pub struct Vertex {
    pub x: u16,
    pub y: u16,
}

#[derive(Debug, Clone, Copy)]
pub struct Polygon {
    /// index into the `vertices` buffer
    pub idx: u16,
    /// how many vertices to read
    pub len: u16,
}

/* 
the egl library is made accessible for future use
outside of this crate
 */

pub trait GlDisplay {
    /// ### Platforms
    /// **On Wayland,**
    /// should return a pointer to the `wl-display` proxy object.
    // TODO: add link to example in the desktop crate
    fn ptr(&self) -> *mut void;
}

pub trait GlSurface {
    /// ### Platforms
    /// **On Wayland,**
    /// should return a pointer to a `wl-surface` proxy object.
    // TODO: add link to example in the desktop crate
    fn ptr(&self) -> *mut void;
}

pub struct EglError {
    msg: String
}

impl fmt::Debug for EglError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.msg)
    }
}

impl fmt::Display for EglError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl StdError for EglError {}

impl<'a> From<&'a str> for EglError {
    fn from(value: &'a str) -> Self {
        Self { msg: value.to_string() }
    }
}

impl From<egl::Error> for EglError {
    fn from(value: egl::Error) -> Self {
        Self { msg: value.to_string() }
    }
}

type FnSwapBuffersWithDamage = fn(
    egl::EGLDisplay,
    egl::EGLSurface,
    *const void /* damage rect array */,
    egl::Int
) -> egl::Int;

pub struct EglInstance {
    lib: Arc<egl::DynamicInstance<egl::EGL1_0>>,
    swap_buffers_with_damage: Option<FnSwapBuffersWithDamage>,
    display: egl::Display,
}

impl EglInstance {

    /// Should be only be called once.
    pub fn new<D: GlDisplay>(display: &D) -> Result<Arc<Self>, EglError> {
        
        let loaded = unsafe {
            egl::DynamicInstance::<egl::EGL1_0>::load_required()
                .map_err(|_| "failed to load egl 1.0")? // NOTE: don't forget to update egl version in error message
        };

        let lib = Arc::new(loaded);

        let egl_display = unsafe {
            lib.get_display(display.ptr())
        }.ok_or("no display")?;

        lib.initialize(egl_display)?;

        let func = lib.get_proc_address("eglSwapBuffersWithDamageKHR");
        let swap_buffers_with_damage: Option<FnSwapBuffersWithDamage> =
            unsafe { mem::transmute(func) };

        {
            let _span = tracing::span!(tracing::Level::TRACE, "EglNewInstance").entered();
            tracing::trace!(
                "swap_buffers_with_damage extension present: {}",
                swap_buffers_with_damage.is_some()
            );
        }

        Ok(Arc::new(Self {
            lib,
            swap_buffers_with_damage,
            display: egl_display,
        }))
        
    }

    pub fn get_proc_address(&self, name: &str) -> Option<extern "system" fn()> {
        self.lib.get_proc_address(name)
    }
    
}

struct EglBase {
    instance: Arc<EglInstance>,
    egl_surface: egl::Surface,
    egl_context: egl::Context,
    damage_rects: Vec<Rect>, // only here to save some allocations
    size: Size, // updated in resize
}

impl Drop for EglBase {
    fn drop(&mut self) {
        self.instance.lib.destroy_surface(self.instance.display, self.egl_surface).unwrap();
        self.instance.lib.destroy_context(self.instance.display, self.egl_context).unwrap();
    }
}

impl EglBase {

    /// Create a new egl context that will draw onto the given surface.
    pub(crate) fn new(instance: &Arc<EglInstance>, surface: egl::Surface, config: egl::Config, size: Size) -> Result<Self, EglError> {

        let context = {
            let attribs = [
                egl::CONTEXT_MAJOR_VERSION, 4,
                egl::CONTEXT_MINOR_VERSION, 0,
                egl::CONTEXT_CLIENT_VERSION, 3,
                egl::CONTEXT_OPENGL_DEBUG, if cfg!(debug) { 1 } else { 0 },
                egl::NONE,
            ];
            instance.lib.create_context(instance.display, config, None, &attribs).unwrap()
        };

        Ok(Self {
            instance: Arc::clone(&instance),
            egl_surface: surface,
            egl_context: context,
            damage_rects: Vec::with_capacity(2),
            size,
        })
        
    }

    /// Make this context current.
    pub(crate) fn bind(&self) -> Result<(), egl::Error> {

        self.instance.lib.make_current(
            self.instance.display,
            Some(self.egl_surface), // note: it is an error to only specify one of the two (read/draw) surfaces
            Some(self.egl_surface),
            Some(self.egl_context)
        )
        
    }

    /// Unbind this context.
    pub(crate) fn unbind(&self) -> Result<(), egl::Error> {

        self.instance.lib.make_current(
            self.instance.display,
            None, None, None
        )
        
    }

    /// Returns an error if this context is not the current one.
    ///
    /// # Damage
    /// The origin is in the top left of the surface.
    /// Normally EGL specifies the origin in the bottom left of the surface but this is **NOT**
    /// what this library does. We recalculate the origin for consistency with windowing systems.
    pub(crate) fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EglError> {

        // recalculate the origin of the rects to be in the top left

        let damage = damage.unwrap_or(&[]);

        self.damage_rects.clear();
        self.damage_rects.extend_from_slice(damage);

        for rect in self.damage_rects.iter_mut() {
            rect.y = self.size.height as i32 - rect.y - rect.h;
        }

        if let Some(func) = self.instance.swap_buffers_with_damage {
            // swap with damage, if the fn could be found
            (func)(self.instance.display.as_ptr(), self.egl_surface.as_ptr(), self.damage_rects.as_ptr().cast(), damage.len() as egl::Int);
        } else {
            // normal swap (if the extension is unsupported)
            self.instance.lib.swap_buffers(self.instance.display, self.egl_surface)?;
        }

        Ok(())

    }
   
}

/// Basic context to draw onto a window.
pub struct EglContext {
    inner: EglBase,
    wl_egl_surface: wayland_egl::WlEglSurface, // needs to be kept alive
}

impl EglContext {

    pub fn new<S: GlSurface>(instance: &Arc<EglInstance>, window: &S, size: Size) -> Result<Self, EglError> {

        let config = {
            let attribs = [
                egl::SURFACE_TYPE, egl::WINDOW_BIT,
                egl::RENDERABLE_TYPE, egl::OPENGL_BIT,
                egl::RED_SIZE, 8,
                egl::GREEN_SIZE, 8,
                egl::BLUE_SIZE, 8,
                egl::ALPHA_SIZE, 8,
                egl::NONE
            ];
            instance.lib.choose_first_config(instance.display, &attribs)?
                .ok_or("failed to choose an egl config (normal context)")?
        };

        let wl_egl_surface = unsafe {
            wayland_egl::WlEglSurface::new_from_raw(
                window.ptr().cast(),
                size.width as i32,
                size.height as i32
            ).map_err(|_| "cannot create WlEglSurface")?
        };

        let attrs = [
            egl::RENDER_BUFFER, egl::BACK_BUFFER,
            egl::NONE,
        ];

        let surface = unsafe {
            instance.lib.create_window_surface(
                instance.display,
                config,
                wl_egl_surface.ptr().cast_mut(),
                Some(&attrs),
            )?
        };

        let inner = EglBase::new(instance, surface, config, size)?;

        Ok(Self {
            inner,
            wl_egl_surface,
        })
        
    }

    /// Make this context current.
    pub fn bind(&self) -> Result<(), egl::Error> {
        self.inner.bind()
    }

    /// Unbind this context.
    pub fn unbind(&self) -> Result<(), egl::Error> {
        self.inner.unbind()
    }

    /// Returns an error if this context is not the current one.
    #[track_caller]
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EglError> {
        self.inner.swap_buffers(damage)
    }

    /// Don't forget to also resize your opengl viewport!
    pub fn resize(&mut self, size: Size) {
        self.inner.size = size;
        self.wl_egl_surface.resize(size.width as i32, size.height as i32, 0, 0);
    }

}

pub struct EglPixelBuffer {
    inner: EglBase,
}

impl EglPixelBuffer {

    /// Create a new egl context that will draw onto the given window.
    pub fn new(instance: &Arc<EglInstance>, size: Size) -> Result<Self, EglError> {

        let config = {
            let attribs = [
                egl::SURFACE_TYPE, egl::PBUFFER_BIT,
                // egl::RENDERABLE_TYPE, egl::OPENGL_ES3_BIT,
                egl::RED_SIZE, 8,
                egl::GREEN_SIZE, 8,
                egl::BLUE_SIZE, 8,
                egl::ALPHA_SIZE, 8,
                egl::NONE
            ];
            instance.lib.choose_first_config(instance.display, &attribs)?
                .ok_or("failed to choose an egl config (pbuffer)")?
        };

        let surface = instance.lib.create_pbuffer_surface(
            instance.display,
            config,
            &[]
        )?;

        let inner = EglBase::new(instance, surface, config, size)?;

        Ok(Self {
            inner,
        })
        
    }

    /// Make this context current.
    pub fn bind(&self) -> Result<(), egl::Error> {
        self.inner.bind()
    }

    /// Unbind this context.
    pub fn unbind(&self) -> Result<(), egl::Error> {
        self.inner.unbind()
    }

    /// Returns an error if this context is not the current one.
    #[track_caller]
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EglError> {
        self.inner.swap_buffers(damage)
    }

}

