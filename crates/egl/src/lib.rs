
use common::{Size, Rect, Damage};
use std::{ffi::c_void as void, fmt, mem, sync::Arc, error::Error as StdError};

/*
TODO: verify thread safety and add more constraints
     - right now you can share an instance between threads and concurrently create contexts
     - eglCreateContext may not be thread-safe (more research needed)
     - in egl, it is safe to bind two different contexts, to two different surfaces on two different threads
     - some other functions also seem to be safe, like get-proc-addrs
     - how is this in lsg, can you render from multiple threads using this in theory rn, at least if you implement your own renderer
 */

/// ### Safety
/// You must always return a valid pointer.
pub unsafe trait IsDisplay {
    /// ### Platforms
    /// **On Wayland,**
    /// should return a pointer to the `wl-display` proxy object.
    // TODO: add link to example in the desktop crate
    fn ptr(&self) -> *mut void;
}

/// ### Safety
/// You must always return a valid pointer.
// TODO: when can the surface wayland object be dropped?
pub unsafe trait IsSurface {
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
        Self { msg: format!("{}", value) }
    }
}

impl From<egl::Error> for EglError {
    fn from(value: egl::Error) -> Self {
        Self { msg: format!("{}", value) }
    }
}

type FnSwapBuffersWithDamage = fn(
    egl::EGLDisplay,
    egl::EGLSurface,
    *const void /* damage rect array */,
    egl::Int
) -> egl::Int;

#[derive(Clone)]
pub struct Instance {
    lib: Arc<egl::DynamicInstance<egl::EGL1_5>>,
    swap_buffers_with_damage: Option<FnSwapBuffersWithDamage>,
    display: egl::Display,
}

impl Instance {

    /// Should be only be called once.
    pub fn new<D: IsDisplay>(display: &D) -> Result<Self, EglError> {

        let lib = unsafe {
            let loaded = egl::DynamicInstance::<egl::EGL1_5>::load_required()
                .map_err(|_| "failed to load egl 1.5")?; // NOTE: don't forget to update egl version in error message
            Arc::new(loaded)
        };

        let egl_display = unsafe {
            lib.get_display(display.ptr())
        }.ok_or("no display")?;

        lib.initialize(egl_display)?;

        // load the eglSwapBufferWithDamage extension function
        let func = lib.get_proc_address("eglSwapBuffersWithDamageKHR");
        let swap_buffers_with_damage: Option<FnSwapBuffersWithDamage> =
            unsafe { mem::transmute(func) };

        Ok(Self {
            lib,
            swap_buffers_with_damage,
            display: egl_display,
        })

    }

    pub fn get_proc_address(&self, name: &str) -> Option<extern "system" fn()> {
        self.lib.get_proc_address(name)
    }

}

#[derive(Default)]
pub enum Api {
    #[default]
    OpenGl,
    Es3,
}

#[derive(Default)]
pub enum Profile {
    #[default]
    Core,
    Compat,
}

pub struct Sizes {
    pub rgba: (usize, usize, usize, usize),
    pub depth: usize,
    pub stencil: usize,
}

impl Default for Sizes {
    fn default() -> Self {
        Self {
            rgba: (8, 8, 8, 8),
            depth: 0,
            stencil: 0,
        }
    }
}

#[derive(Default)]
pub struct ConfigBuilder {
    pub api: Api,
    pub profile: Profile,
    pub version: [usize; 2],
    pub debug: bool,
    pub sizes: Sizes,
}

impl ConfigBuilder {

    pub fn api(mut self, api: Api) -> Self {
        self.api = api;
        self
    }

    pub fn profile(mut self, profile: Profile) -> Self {
        self.profile = profile;
        self
    }

    pub fn version(mut self, major: usize, minor: usize) -> Self {
        self.version = [major, minor];
        self
    }

    pub fn debug(mut self, value: bool) -> Self {
        self.debug = value;
        self
    }

    pub fn sizes(mut self, value: Sizes) -> Self {
        self.sizes = value;
        self
    }

    pub fn finish(self, instance: &Instance) -> Result<Config, EglError> {

        let attribs = [

            // surface attribs

            egl::SURFACE_TYPE, egl::WINDOW_BIT, // only window surfaces are supported for now

            egl::RENDERABLE_TYPE, match self.api {
                Api::OpenGl => egl::OPENGL_BIT,
                Api::Es3    => egl::OPENGL_ES3_BIT,
            },

            egl::RED_SIZE,     self.sizes.rgba.0  as i32,
            egl::GREEN_SIZE,   self.sizes.rgba.1  as i32,
            egl::BLUE_SIZE,    self.sizes.rgba.2  as i32,
            egl::ALPHA_SIZE,   self.sizes.rgba.3  as i32,
            egl::DEPTH_SIZE,   self.sizes.depth   as i32,
            egl::STENCIL_SIZE, self.sizes.stencil as i32,

            egl::NONE

        ];

        let config = instance.lib.choose_first_config(instance.display, &attribs)?
            .ok_or("failed to choose an egl config (normal context)")?;

        let context_attrs = [
            egl::CONTEXT_CLIENT_VERSION, self.version[0] as i32,
            egl::CONTEXT_MAJOR_VERSION, self.version[0] as i32,
            egl::CONTEXT_MINOR_VERSION, self.version[1] as i32,
            egl::CONTEXT_OPENGL_DEBUG, self.debug as i32,
            egl::CONTEXT_OPENGL_PROFILE_MASK, match self.profile {
                Profile::Core => egl::CONTEXT_OPENGL_CORE_PROFILE_BIT,
                Profile::Compat => egl::CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT,
            },
            egl::NONE,
        ];

        let surface_attrs = [
            egl::RENDER_BUFFER, egl::BACK_BUFFER,
            egl::NONE,
        ];

        Ok(Config {
            inner: config,
            api: self.api,
            context_attrs,
            surface_attrs,
        })

    }

}

pub struct Config {
    inner: egl::Config,
    context_attrs: [i32; 11],
    surface_attrs: [i32; 3],
    api: Api,
}

impl Config {

    pub fn build() -> ConfigBuilder {
        ConfigBuilder::default()
    }

}

pub struct Context {
    instance: Instance,
    inner: egl::Context,
    damage_rects: Vec<Rect>, // only here to amortize some allocations, used in `resize`
}

impl Drop for Context {
    fn drop(&mut self) {
        self.instance.lib.destroy_context(
            self.instance.display,
            self.inner
        ).expect("failed to destroy egl context");
    }
}

impl Context {

    pub fn new(instance: &Instance, config: &Config) -> Result<Self, EglError> {

        // opengl (and related) has the by far worst api i've seen... ever
        // like what the fuck is this, why is this not part of the attributes?!
        match config.api {
            Api::OpenGl => instance.lib.bind_api(egl::OPENGL_API)?,
            Api::Es3    => instance.lib.bind_api(egl::OPENGL_ES_API)?,
        }

        let context = instance.lib.create_context(
            instance.display,
            config.inner,
            None,
            &config.context_attrs
        )?;

        Ok(Self {
            instance: instance.clone(),
            inner: context,
            damage_rects: Vec::new(),
        })

    }

    /// Make this context current, and set the target surface.
    pub fn bind(&self, instance: &Instance, surface: Option<&Surface>) -> Result<(), EglError> {

        self.instance.lib.make_current(
            self.instance.display,
            surface.map(|it| it.inner), // NOTE: it is an error to only specify one of the two (read/draw) surfaces
            surface.map(|it| it.inner),
            Some(self.inner)
        )?;

        if surface.is_some() {
            // set swap-interval to 0, because we never want to block waiting
            // for a frame to be vsync-ed
            // TODO: does doing this potentially every frame represent a significant performance penalty? sadly a context + surface has to be bound for this
            instance.lib.swap_interval(instance.display, 0)?;
        }

        Ok(())

    }

    /// Clear the current context.
    pub fn unbind(&self) -> Result<(), EglError> {
        self.instance.lib.make_current(
            self.instance.display,
            None, None, None
        )?;
        Ok(())
    }

    /// Swap the back and front buffers.
    ///
    /// `surface` must be the same surface that was specifified in `bind`.
    /// If `damage` is an empty slice, everything will be redrawn.
    pub fn swap(&mut self, surface: &Surface, damage: Damage) -> Result<(), EglError> {

        // recalculate the origin of the rects to be in the top left
        self.damage_rects.clear();
        self.damage_rects.extend_from_slice(damage.rects);
        for rect in self.damage_rects.iter_mut() {
            rect.pos.y = surface.size.h as isize - rect.pos.y - rect.size.h as isize;
        }

        if let Some(func) = self.instance.swap_buffers_with_damage {
            // swap with damage, if the fn could be found
            (func)(self.instance.display.as_ptr(), surface.inner.as_ptr(), self.damage_rects.as_ptr().cast(), damage.rects.len() as i32);
        } else {
            // normal swap (if the extension is unsupported)
            self.instance.lib.swap_buffers(self.instance.display, surface.inner)?;
        }

        Ok(())

    }

}

/// A double-buffered window surface.
pub struct Surface {
    inner: egl::Surface,
    size: Size,
    #[cfg(unix)]
    wl_egl_surface: wayland_egl::WlEglSurface,
}

impl Surface {

    pub fn new<I: IsSurface>(instance: &Instance, config: &Config, inner: &I, size: Size) -> Result<Self, EglError> {

        #[cfg(unix)] // unix means wayland, as far as we are concerned
        let wl_egl_surface = unsafe {
            wayland_egl::WlEglSurface::new_from_raw(
                inner.ptr().cast(),
                size.w as i32,
                size.h as i32
            ).map_err(|_| "cannot create WlEglSurface")?
        };

        #[cfg(unix)]
        let target = wl_egl_surface.ptr().cast_mut();

        #[cfg(not(unix))]
        let target = inner.ptr();

        let surface = unsafe {
            instance.lib.create_window_surface(
                instance.display,
                config.inner,
                target,
                Some(&config.surface_attrs),
            )?
        };

        Ok(Self {
            inner: surface,
            size,
            #[cfg(unix)]
            wl_egl_surface
        })

    }

    pub fn resize(&mut self, size: Size) {
        self.size = size;
        let (w, h) = (size.w as i32, size.h as i32);
        self.wl_egl_surface.resize(w, h, 0, 0);
    }

    pub fn size(&self) -> Size {
        self.size
    }

}
