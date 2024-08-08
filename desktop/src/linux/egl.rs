
use std::{
    ffi::c_void as void, mem, sync::Arc
};

use khronos_egl as egl;
use wayland_client::Proxy;

use crate::*;

type FnSwapBuffersWithDamage = fn(
    khronos_egl::EGLDisplay,
    khronos_egl::EGLSurface,
    *const void /* damage rect array */,
    khronos_egl::Int
) -> khronos_egl::Int;

pub struct EglInstance {
    lib: Arc<egl::DynamicInstance<egl::EGL1_0>>,
    swap_buffers_with_damage: Option<FnSwapBuffersWithDamage>,
    display: egl::Display,
}

impl EglInstance {

    /// Should be only be called once. Although initializing multiple instances is not a hard error.
    pub fn new<T: 'static + Send>(evh: &mut EventLoop<T>) -> Result<Arc<Self>, EvlError> {
        
        let loaded = unsafe {
            egl::DynamicInstance::<egl::EGL1_0>::load_required()
                .map_err(|_| "failed to load egl 1.0")? // NOTE: don't forget to update egl version in error message
        };

        let lib = Arc::new(loaded);

        let wl_display = evh.wayland.state.con.get_ref().display().id().as_ptr();
        let egl_display = unsafe {
            lib.get_display(wl_display.cast())
        }.ok_or("no display")?;

        lib.initialize(egl_display)?;

    	// side note: const EGL_EXTENSIONS = 0x3055

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
    pub(crate) fn new(instance: &Arc<EglInstance>, surface: egl::Surface, config: egl::Config, size: Size) -> Result<Self, EvlError> {

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
    pub(crate) fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EvlError> {

        // recalculate the origin of the rects to be in the top left

        let damage = damage.unwrap_or(&[]);

        self.damage_rects.clear();
        self.damage_rects.extend_from_slice(damage);

        for rect in self.damage_rects.iter_mut() {
            rect.y = self.size.height as i32 - rect.y - rect.h;
        }

        if let Some(func) = self.instance.swap_buffers_with_damage {
            // swap with damage, if the fn could be found
            (func)(self.instance.display.as_ptr(), self.egl_surface.as_ptr(), self.damage_rects.as_ptr().cast(), damage.len() as khronos_egl::Int);
        } else {
            // normal swap (if the extension is unsupported)
            self.instance.lib.swap_buffers(self.instance.display, self.egl_surface)?;
        }

        Ok(())

    }
   
}

pub struct EglContext {
    inner: EglBase,
    id: WindowId,
    wl_egl_surface: wayland_egl::WlEglSurface, // note: needs to be kept alive
}

impl EglContext {

    /// Create a new egl context that will draw onto the given window.
    pub fn new<T: 'static + Send>(instance: &Arc<EglInstance>, window: &BaseWindow<T>, size: Size) -> Result<Self, EvlError> {

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

        let wl_egl_surface = wayland_egl::WlEglSurface::new(
            window.wl_surface.id(),
            size.width as i32,
            size.height as i32
        )?;

        let attrs = [
            egl::RENDER_BUFFER, egl::BACK_BUFFER,
            egl::NONE,
        ];

        let surface = unsafe {
            instance.lib.create_window_surface(
                instance.display,
                config,
                wl_egl_surface.ptr() as *mut void,
                Some(&attrs),
            )?
        };

        let inner = EglBase::new(instance, surface, config, size)?;

        Ok(Self {
            inner,
            id: window.id,
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
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>, token: PresentToken) -> Result<(), EvlError> {
        debug_assert!(self.id == token.id, "present token for another window");
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
    pub fn new(instance: &Arc<EglInstance>, size: Size) -> Result<Self, EvlError> {

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
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EvlError> {
        self.inner.swap_buffers(damage)
    }

}

