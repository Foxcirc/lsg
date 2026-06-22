
 //! This file implements functionality for creating an OpenGL context
 //! and loading OpenGL functions on native platforms. It uses EGL as
 //! a backend, which works on Linux, Windows and Android.
 //!
 //! # Concurrency
 //! Objects can generally be accessed from multiple threads, however
 //! they need to be behind a lock (as their methods require `&mut self`)
 //! and it is to keep in mind that **a `Context` or `Surface` can only ever
 //! be bound on one thread at a time.**

 use common::{PhysicalRect, PhysicalSize, SmartMutex};
 use std::{error::Error as StdError, ffi::c_void as void, fmt, mem, sync::Arc};

 pub struct LoadError {
     msg: String
 }

 impl fmt::Debug for LoadError {
     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
         write!(f, "{:?}", self.msg)
     }
 }

 impl fmt::Display for LoadError {
     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
         write!(f, "{}", self.msg)
     }
 }

 impl StdError for LoadError {}

 impl<'a> From<&'a str> for LoadError {
     fn from(value: &'a str) -> Self {
         Self { msg: format!("{}", value) }
     }
 }

 impl From<egl::Error> for LoadError {
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

 /// This type should be treated as a singleton.
 pub struct Instance {
     inner: SmartMutex<InstanceInner>
 }

struct InstanceInner {
     lib: egl::DynamicInstance<egl::EGL1_5>,
     swap_buffers_with_damage: Option<FnSwapBuffersWithDamage>,
     display: egl::Display,
 }

 // SAFETY: Internal `Mutex` is used.
 unsafe impl Send for Instance {}
 unsafe impl Sync for Instance {}

 impl Instance {

     pub fn new<D: common::IsDisplay>(display: &D) -> Result<Arc<Self>, LoadError> {

         let lib = unsafe {
             egl::DynamicInstance::<egl::EGL1_5>::load_required()
                 .map_err(|_| "failed to load egl 1.5")? // NOTE: don't forget to update egl version in error message
         };

         let egl_display = unsafe {
             lib.get_display(display.ptr().cast_mut())
         }.ok_or("no display")?;

         lib.initialize(egl_display)?;

         // load the eglSwapBufferWithDamage extension function
         let func = lib.get_proc_address("eglSwapBuffersWithDamageKHR");
         let swap_buffers_with_damage: Option<FnSwapBuffersWithDamage> =
             unsafe { mem::transmute(func) };

         Ok(Arc::new(Self {
             inner: SmartMutex::new(InstanceInner {
                 lib,
                 swap_buffers_with_damage,
                 display: egl_display,
             })
         }))

     }

     pub fn get_proc_address(&self, name: &str) -> Option<extern "system" fn()> {
         let instance = self.inner.lock();
         instance.lib.get_proc_address(name)
     }

 }

 #[derive(Default)]
 pub enum Api {
     #[default]
     OpenGl,
     Es3, // NOTE: Yes, this is supposed to be Es3.
 }

 #[derive(Default)]
 pub enum AlphaFormat {
     #[default]
     Premultiplied,
     Unmultiplied,
 }

 /// The sizes of the buffers for this
 /// context and surfaces used with it.
 ///
 /// Only applied on desktop OpenGL!
 pub struct BufferDesc {
     pub rgba: (usize, usize, usize, usize),
     pub depth: usize,
     pub stencil: usize,
 }

 impl Default for BufferDesc {
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
     pub version: [usize; 2],
     pub debug: bool,
     pub sizes: BufferDesc,
     pub alpha: AlphaFormat,
 }

 impl ConfigBuilder {

     pub fn api(mut self, api: Api) -> Self {
         self.api = api;
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

     /// Only applied on desktop OpenGL!
     pub fn bufferdesc(mut self, value: BufferDesc) -> Self {
         self.sizes = value;
         self
     }

     pub fn alpha(mut self, value: AlphaFormat) -> Self {
         self.alpha = value;
         self
     }

     pub fn finish(self, locked: &Instance) -> Result<Config, LoadError> {

         let instance = locked.inner.lock();

         let attribs = match self.api {
             Api::OpenGl => vec![
                 egl::SURFACE_TYPE, egl::WINDOW_BIT,
                 egl::RENDERABLE_TYPE, egl::OPENGL_BIT,
                 egl::RED_SIZE,     self.sizes.rgba.0  as i32,
                 egl::GREEN_SIZE,   self.sizes.rgba.1  as i32,
                 egl::BLUE_SIZE,    self.sizes.rgba.2  as i32,
                 egl::ALPHA_SIZE,   self.sizes.rgba.3  as i32,
                 egl::DEPTH_SIZE,   self.sizes.depth   as i32,
                 egl::STENCIL_SIZE, self.sizes.stencil as i32,
                 egl::NONE
             ],
             // Es3 is weird.
             Api::Es3 => vec![
                 egl::SURFACE_TYPE, egl::WINDOW_BIT,
                 egl::RENDERABLE_TYPE, egl::OPENGL_ES3_BIT,
                 egl::NONE
             ]
         };

         let config = instance.lib.choose_first_config(instance.display, &attribs)?
             .ok_or("failed to choose an egl config")?;

         let context_attrs = match self.api {
             Api::OpenGl => vec![
                 egl::CONTEXT_CLIENT_VERSION, self.version[0] as i32,
                 egl::CONTEXT_MAJOR_VERSION, self.version[0] as i32,
                 egl::CONTEXT_MINOR_VERSION, self.version[1] as i32,
                 egl::CONTEXT_OPENGL_DEBUG, self.debug as i32,
                 egl::CONTEXT_OPENGL_PROFILE_MASK, egl::CONTEXT_OPENGL_CORE_PROFILE_BIT,
                 egl::NONE,
             ],
             // Es3 is weird.
             Api::Es3 => vec![
                 egl::CONTEXT_CLIENT_VERSION, self.version[0] as i32,
                 egl::NONE,
             ]
         };

         let surface_attrs = vec![
             egl::RENDER_BUFFER, egl::BACK_BUFFER,
             egl::ALPHA_FORMAT, match self.alpha {
                 AlphaFormat::Premultiplied => egl::ALPHA_FORMAT_PRE,
                 AlphaFormat::Unmultiplied => egl::ALPHA_FORMAT_NONPRE,
             },
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
     inner: egl::Config, // NOTE: seems it does not need `drop`
     context_attrs: Vec<i32>,
     surface_attrs: Vec<i32>,
     api: Api,
 }

  // SAFETY: EGL generally does not mutate this after creation.
  unsafe impl Send for Config {}
  unsafe impl Sync for Config {}

 impl Config {
     pub fn build() -> ConfigBuilder {
         ConfigBuilder::default()
     }
 }

 pub struct Context {
     inner: SmartMutex<ContextInner>
 }

struct ContextInner {
     instance: Arc<Instance>,
     inner: egl::Context,
     // Here to amortize some allocations. Used when swapping buffers.
     damage_rects: Vec<PhysicalRect>,
 }

 // SAFETY: Internal `Mutex` is used.
 unsafe impl Send for Context {}
 unsafe impl Sync for Context {}

 impl Drop for ContextInner {
     fn drop(&mut self) {
         let instance = self.instance.inner.lock();
         instance.lib.destroy_context(
             instance.display,
             self.inner
         ).expect("failed to destroy egl context");
     }
 }

 impl Context {

     pub fn new(locked: &Arc<Instance>, config: &Config) -> Result<Self, LoadError> {

         let instance = locked.inner.lock();

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
             inner: SmartMutex::new(ContextInner {
                 instance: Arc::clone(locked),
                 inner: context,
                 damage_rects: Vec::new(),
             })
         })

     }

     /// Make this context current, and set the target surface.
     ///
     /// # Panic
     /// This may panic if binding fails which could be, for example because
     /// - The GPU ran out of memory for allocating auxillary buffers
     /// - Bound on two different threads at the same time
     /// - Context was lost due to driver crash or hardware failure
     #[track_caller]
     pub fn bind(&self, surface: Option<&Surface>) {

         let context = self.inner.lock();
         let instance = context.instance.inner.lock();

         instance.lib.make_current(
             instance.display,
             // NOTE: it is an error to only specify one of the two (read/draw) surfaces
             surface.map(|it| it.inner.lock().inner),
             surface.map(|it| it.inner.lock().inner),
             Some(context.inner)
         ).unwrap();

         if surface.is_some() {
             // Set swap-interval to 0, because we never want to
             // block waiting for a frame to be vsync-ed.
             // TODO: does doing this potentially every frame represent a significant performance penalty?
             //       sadly a context + surface has to be bound for this
             instance.lib.swap_interval(instance.display, 0)
                 .unwrap();
         }

     }

     /// Clear the current context.
     #[track_caller]
     pub fn unbind(&self) {

         let context = self.inner.lock();
         let instance = context.instance.inner.lock();

         instance.lib.make_current(
             instance.display,
             None, None, None
         ).unwrap();

     }

     /// Swap the back and front buffers.
     ///
     /// `surface` must be the same surface that was specifified in `bind`.
     /// If `damage` is an empty slice, everything will be redrawn.
     #[track_caller]
     pub fn swap(&self, locked: &Surface, damage: Damage) {

         let ContextInner { ref instance, ref mut damage_rects, .. } = *self.inner.lock();
         // ^^^^ I destructure here cause of borrow checker issues.

         let surface = locked.inner.lock();
         let instance = instance.inner.lock();

         // recalculate the origin of the rects to be in the top left
         damage_rects.clear();
         damage_rects.extend_from_slice(damage.rects);
         for rect in damage_rects.iter_mut() {
             rect.pos.y = (surface.size.h as isize - rect.pos.y as isize - rect.size.h as isize) as i16;
         }

         if let Some(func) = instance.swap_buffers_with_damage {
             // swap with damage, if the fn could be found
             (func)(
                 instance.display.as_ptr(),
                 surface.inner.as_ptr(),
                 damage_rects.as_ptr().cast(),
                 damage.rects.len() as i32
             );
         } else {
             // normal swap (if the extension is unsupported)
             instance.lib.swap_buffers(instance.display, surface.inner)
                 .unwrap();
         }

     }

 }

pub struct Surface {
    inner: SmartMutex<SurfaceInner>
}

 /// A double-buffered window surface.
 struct SurfaceInner {
     inner: egl::Surface,
     size: PhysicalSize,
     #[cfg(target_os = "linux")]
     wl_egl_surface: wayland_egl::WlEglSurface,
 }

  // SAFETY: Internal `Mutex` is used.
  unsafe impl Send for Surface {}
  unsafe impl Sync for Surface {}

 impl Surface {

     pub fn new<I: common::IsSurface>(locked: &Instance, config: &Config, window: &I) -> Result<Self, LoadError> {

         let instance = locked.inner.lock();

         // Get the physical window buffer size.
         let size = window.size();

         #[cfg(target_os = "linux")] // Linux means wayland, as far as we are concerned.
         let wl_egl_surface = unsafe {
             wayland_egl::WlEglSurface::new_from_raw(
                 window.ptr().cast(),
                 size.w as i32,
                 size.h as i32
             ).map_err(|_| "cannot create WlEglSurface")?
         };

         #[cfg(target_os = "linux")]
         let target = wl_egl_surface.ptr().cast_mut();

         #[cfg(not(target_os = "linux"))]
         let target = window.ptr();

         let surface = unsafe {
             instance.lib.create_window_surface(
                 instance.display,
                 config.inner,
                 target,
                 Some(&config.surface_attrs),
             )?
         };

         Ok(Self {
             inner: SmartMutex::new(SurfaceInner {
                 inner: surface,
                 size,
                 #[cfg(target_os = "linux")]
                 wl_egl_surface
             })
         })

     }

     pub fn resize(&self, size: PhysicalSize) {

         let mut surface = self.inner.lock();

         surface.size = size;

         #[cfg(target_os = "linux")]
         surface.wl_egl_surface.resize(size.w as i32, size.h as i32, 0, 0);

     }

     pub fn size(&self) -> PhysicalSize {
         let surface = self.inner.lock();
         surface.size
     }

 }

 /// Area of a window that has to be redrawn.
 pub struct Damage<'s> {
     /// Empty means full damage.
     pub rects: &'s [PhysicalRect],
 }

 impl<'s> Damage<'s> {
     /// Everything will be redrawn.
     pub fn all() -> Self {
         Self { rects: &[] }
     }
     /// Only the marked rects should be redrawn.
     /// This is only an optimization and the system may choose
     /// to redraw more parts of the window.
     pub fn partial(rects: &'s [PhysicalRect]) -> Self {
         Self { rects }
     }
 }
