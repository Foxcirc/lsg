
use common::*;
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

pub mod v2 {

    // TODO: make this the default

    use super::*;

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
        pub fn bind<'d, S>(&self, instance: &Instance, surface: S) -> Result<(), EglError>
          where S: Into<Option<&'d Surface>> {

            let it = surface.into();

            self.instance.lib.make_current(
                self.instance.display,
                it.map(|it| it.inner), // NOTE: it is an error to only specify one of the two (read/draw) surfaces
                it.map(|it| it.inner),
                Some(self.inner)
            )?;

            if it.is_some() {
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

}

/*

struct BaseContext {
    instance: Instance,
    egl_surface: egl::Surface,
    egl_context: egl::Context,
    damage_rects: Vec<Rect>, // only here to save some allocations, used in `resize`
    size: Size, // updated in `resize`
}

impl Drop for BaseContext {
    fn drop(&mut self) {
        self.instance.lib.destroy_surface(self.instance.display, self.egl_surface).unwrap();
        self.instance.lib.destroy_context(self.instance.display, self.egl_context).unwrap();
    }
}

impl BaseContext {

    /// Create a new egl context that will draw onto the given surface.
    pub(crate) fn new(instance: &Instance, surface: egl::Surface, config: egl::Config, size: Size, share: Option<&ShareContext>) -> Result<Self, EglError> {

        // type BindApi = fn(u32);
        // let func = instance.get_proc_address("eglBindAPI").expect("cannot load fn eglBindAPI");
        // let bind_api: BindApi = unsafe { mem::transmute(func) };

        // opengl has the by far worst api i've seen... ever
        // like what the fuck is this, why is this not an attrib?!
        // bind_api(egl::OPENGL_API);

        let context = {
            let attribs = [

                egl::CONTEXT_MAJOR_VERSION, 4,
                egl::CONTEXT_MINOR_VERSION, 0,
                egl::CONTEXT_CLIENT_VERSION, 3,

                egl::CONTEXT_OPENGL_DEBUG, if cfg!(debug_assertions) { 1 } else { 0 },

                egl::NONE,

            ];
            instance.lib.create_context(
                instance.display,
                config,
                // share.map(|it| it.inner.egl_context),
                None,
                &attribs
            )?
        };

        Ok(Self {
            instance: instance.clone(),
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
            Some(self.egl_surface), // NOTE: it is an error to only specify one of the two (read/draw) surfaces
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

    pub(crate) fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EglError> {

        // recalculate the origin of the rects to be in the top left

        let damage = damage.unwrap_or(&[]);

        self.damage_rects.clear();
        self.damage_rects.extend_from_slice(damage);

        for rect in self.damage_rects.iter_mut() {
            rect.y = self.size.h as i32 - rect.y - rect.h;
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

struct SurfacelessContext {
    instance: Instance,
    egl_context: egl::Context,
}

impl Drop for SurfacelessContext {
    fn drop(&mut self) {
        self.instance.lib.destroy_context(self.instance.display, self.egl_context).unwrap();
    }
}

impl SurfacelessContext {

    /// Create a new egl context that will draw onto the given surface.
    pub(crate) fn new(instance: &Instance, config: egl::Config) -> Result<Self, EglError> {

        todo!("surfaceless context");
        let context = {
            let attribs = [
                egl::CONTEXT_MAJOR_VERSION, 4,
                egl::CONTEXT_MINOR_VERSION, 3,
                egl::CONTEXT_CLIENT_VERSION, 4, // TODO: was 3 once idk what it means what am i doing get me out
                egl::CONTEXT_OPENGL_PROFILE_MASK, egl::CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT,
                egl::CONTEXT_OPENGL_DEBUG, if cfg!(debug) { 1 } else { 0 },
                egl::NONE,
            ];
            instance.lib.create_context(instance.display, config, None, &attribs).unwrap()
        };

        Ok(Self {
            instance: instance.clone(),
            egl_context: context,
        })

    }

    /// Make this context current.
    pub(crate) fn bind(&self) -> Result<(), egl::Error> {

        self.instance.lib.make_current(
            self.instance.display,
            None,
            None,
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

}

/// Context that is used to share properties between other context.
/// For example, if you have multiple windows, with their own context each,
/// you can share shaders between them.
pub struct ShareContext {
    inner: SurfacelessContext,
}

impl ShareContext {

    pub fn new(instance: &Instance) -> Result<Self, EglError> {

        let config = {
            let attribs = [
                // egl::SURFACE_TYPE, egl::WINDOW_BIT,
                // egl::RENDERABLE_TYPE, egl::OPENGL_ES3_BIT,
                // egl::RED_SIZE, 8,
                // egl::GREEN_SIZE, 8,
                // egl::BLUE_SIZE, 8,
                // egl::ALPHA_SIZE, 8,
                // TODO: what attribs to pass here
                egl::NONE
            ];
            instance.lib.choose_first_config(instance.display, &attribs)?
                .ok_or("failed to choose an egl config (normal context)")?
        }; // TODO: move "choosing a config" into the SurfacelessContext / BaseContext, or out of there

        let inner = SurfacelessContext::new(instance, config)?;

        Ok(Self { inner })

    }

    /// Make this context current.
    pub fn bind(&self) -> Result<(), egl::Error> {
        self.inner.bind()
    }

    /// Unbind this context.
    pub fn unbind(&self) -> Result<(), egl::Error> {
        self.inner.unbind()
    }

}

/// Context that is used to draw onto a window.
pub struct Context {
    inner: BaseContext,
    wl_egl_surface: wayland_egl::WlEglSurface, // needs to be kept alive
}

impl Context {

    pub fn new<S: IsSurface>(instance: &Instance, window: &S, size: Size, share: Option<&ShareContext>) -> Result<Self, EglError> {

        // TODO: add the ability to use not opengl ES
        // TODO: add the ability to use a custom config
        let config = {
            let attribs = [ // surface attribs

                egl::SURFACE_TYPE, egl::WINDOW_BIT,
                // egl::RENDERABLE_TYPE, egl::OPENGL_BIT,
                egl::RENDERABLE_TYPE, egl::OPENGL_ES3_BIT,

                egl::RED_SIZE, 8,
                egl::GREEN_SIZE, 8,
                egl::BLUE_SIZE, 8,
                egl::ALPHA_SIZE, 8,
                // egl::DEPTH_SIZE, 24,
                // egl::STENCIL_SIZE, 8,
                egl::NONE
            ];
            instance.lib.choose_first_config(instance.display, &attribs)?
                .ok_or("failed to choose an egl config (normal context)")?
        };

        let wl_egl_surface = unsafe {
            wayland_egl::WlEglSurface::new_from_raw(
                window.ptr().cast(),
                size.w as i32,
                size.h as i32
            ).map_err(|_| "cannot create WlEglSurface")?
        };

        let attrs = [ // TODO: attrs or attribs, choose a name man
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

        let inner = BaseContext::new(instance, surface, config, size, share)?;

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


    /// Swap the back and front buffer, which applies changes to the window.
    ///
    /// Returns an error if this context is not the current one.
    ///
    /// # Damage
    /// The origin is in the top left of the surface.
    /// Normally EGL specifies the origin in the bottom left of the surface but this is **not**
    /// what this library does. We recalculate the origin for consistency with windowing systems.
    pub fn swap_buffers(&mut self, damage: Option<&[Rect]>) -> Result<(), EglError> {
        // TODO: i would like to harden the interface so that this can't fail
        self.inner.swap_buffers(damage)
    }

    /// Don't forget to also resize your opengl viewport!
    pub fn resize(&mut self, size: Size) {
        self.inner.size = size;
        self.wl_egl_surface.resize(size.w as i32, size.h as i32, 0, 0);
    }

}

pub struct PixelBuffer {
    inner: BaseContext,
}

impl PixelBuffer {

    /// Create a new egl context that will draw onto the given window.
    pub fn new(instance: &Arc<Instance>, size: Size, share: Option<&ShareContext>) -> Result<Self, EglError> {

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

        let inner = BaseContext::new(instance, surface, config, size, share)?;

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

*/
