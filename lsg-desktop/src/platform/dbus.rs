
#[test]
fn dbus() {

    use futures_lite::future::block_on;

    block_on(async {

    
        let con = DbusConnection::new().unwrap();

        let msg = DbusMessage::call(
             "org.freedesktop.DBus",
             "/org/freedesktop/DBus",
             "org.freedesktop.DBus.Introspectable",
             "Introspect",
        ).build();

        let resp = con.send(msg).await.unwrap();

        println!("{:?}", resp.args);

        let text: &str = resp.arg(0).unwrap();
        println!("{}", text);
        
    })
    
}

// #### dbus implementation ####

use async_lock::Mutex as AsyncMutex;
use futures_lite::{FutureExt, pin, ready};
use libdbus_sys as dbus;
use std::{mem, ffi, fmt, error::Error as StdError, ptr::{self, NonNull}, os::fd::{BorrowedFd, RawFd, AsFd}, io, future::poll_fn, task::Poll, collections::HashMap, sync::Arc};

pub struct DbusConnection {
    con: *mut dbus::DBusConnection,
    shared: Arc<SharedData>,
}

unsafe impl Send for DbusConnection {}
unsafe impl Sync for DbusConnection {}

/// This will call dbus_shutdown which means you can't use any libdbus library after this anymore. // TODO: only keep if below TODO is implemented
impl Drop for DbusConnection {
    fn drop(&mut self) {
        unsafe { dbus::dbus_connection_close(self.con) }
        unsafe { dbus::dbus_connection_unref(self.con) }
        // unsafe { dbus::dbus_shutdown() }; // TODO: add support for this in libdbus-sys crate
    }
}

impl DbusConnection {

    pub fn new() -> Result<Self, DbusError> {

        // init the error checking stuff
        let mut error = unsafe { mem::zeroed() };
        unsafe { dbus::dbus_error_init(&mut error) };

        // create a new connection
        let con = unsafe { dbus::dbus_bus_get_private(dbus::DBusBusType::Session, &mut error) };
        if con.is_null() { return Err(error.into()) }

        unsafe { dbus::dbus_connection_set_exit_on_disconnect(con, 0) };

        extern fn add_fd(watch: *mut dbus::DBusWatch, data: *mut ffi::c_void) -> u32 {

            let data: &mut FdData = unsafe { &mut *data.cast() };

            let fd = unsafe { dbus::dbus_watch_get_unix_fd(watch) };

            if !data.active.contains_key(&fd) {
                match async_io::Async::new(DbusFd { inner: fd }) {
                    Ok(async_fd) => {
                        let result = DbusWatch { fd: async_fd, watch };
                            data.active.insert(fd, result);
                    },
                    Err(err) => data.err = Some(err),
                };
            }

            1

        }

        extern fn remove_fd(watch: *mut dbus::DBusWatch, data: *mut ffi::c_void) {

            let data: &mut FdData = unsafe { &mut *data.cast() };

            let fd = unsafe { dbus::dbus_watch_get_unix_fd(watch) };

            data.active.remove(&fd);

        }

        extern fn toggle_fd(watch: *mut dbus::DBusWatch, data: *mut ffi::c_void) {

            let enabled = unsafe { dbus::dbus_watch_get_enabled(watch) };

            if enabled == 1 {
                add_fd(watch, data);
            } else {
                remove_fd(watch, data);
            }

        }

        extern fn free_data(data: *mut ffi::c_void) {
            // data is NOT the whole `shared` but the variable `raw`
            unsafe { drop(Arc::from_raw(data as *const FdData)) }
        }

        // we can't use a mutex to store these, since this would lead to recursive locking in the dbus cb's
        let fds = Arc::new(FdData {
            lock: AsyncMutex::new(()),
            active: HashMap::with_capacity(1),
            err: None
        });

        let raw = Arc::into_raw(Arc::clone(&fds));

        let shared = Arc::new(SharedData {
            fds,
            replies: AsyncMutex::new(ReplyData {
                events: HashMap::with_capacity(1)
            })
        });

        unsafe {
            dbus::dbus_connection_set_watch_functions(
                con, Some(add_fd), Some(remove_fd), Some(toggle_fd),
                raw as *mut _, Some(free_data)
            );
        }

        Ok(Self {
            con,
            shared
        })

    }

    pub async fn send(&self, message: DbusMessage) -> Result<DbusResponse, DbusError> {

        let mut my_serial = 0;

        let result = unsafe { dbus::dbus_connection_send( // add to send queue
            self.con, message.inner, &mut my_serial,
        ) };

        let (sender, receiver) = flume::bounded(1);

        let mut guard = self.shared.replies.lock().await;
        guard.events.insert(my_serial, sender);
        drop(guard);

        assert!(result != 0); // TODO: more robust errors

        enum Either<'lock> {
            Dispatch(async_lock::MutexGuard<'lock, ()>),  // todo: use rc instead of box
            Reply,
        }

        loop {

            let message = async {
                receiver.recv_async().await.unwrap();
                Either::Reply
            };
            
            let dispatch = async {
                let guard = self.shared.fds.lock.lock().await;
                // we have to keep the guard alive so we can safely access `shared` later
                Either::Dispatch(guard) 
            };

            // prefer an arriving `message` so it get's seen by all tasks
            match message.or(dispatch).await {

                // this thread is now responsible for pumping the dbus event loop
                Either::Dispatch(_guard) => {

                    // SAFETY: since we fdds.lock, we can mutably access the active fd's
                    let fds = unsafe { &mut *Arc::as_ptr(&self.shared.fds).cast_mut() };

                    loop {
                        
                        // check if any fd is ready
                        // TODO: does this loop actually block until readability? cause with dbus_con_dispatch() it is bugged
                        poll_fn(|ctx| {

                            let mut ready = false;

                            for it in fds.active.values() {
                                ready |= it.fd.poll_readable(ctx).is_ready()
                                      |  it.fd.poll_writable(ctx).is_ready()
                            }

                            if ready { Poll::Ready(()) }
                            else { Poll::Pending }

                        }).await;

                        unsafe { dbus::dbus_connection_read_write_dispatch(self.con, 0) };

                        let result = unsafe { dbus::dbus_connection_pop_message(self.con) };

                        if let Some(val) = NonNull::new(result) {

                            let result_serial = unsafe { dbus::dbus_message_get_reply_serial(result) };

                            let guard = self.shared.replies.lock().await;
                            let sender = guard.events.get(&my_serial).unwrap();

                            let resp = DbusResponse::consume(val);

                            if my_serial == result_serial {
                                return Ok(resp);
                            } else {
                                sender.try_send(resp).unwrap(); // can't block
                            }

                        }
                        
                    }

                },

                // we've got our reply!
                Either::Reply => {
                    // let response = data.response.take().unwrap();
                    // let reply = DbusResponse::consume(response);
                    // return Ok(reply)
                    unreachable!();
                }

            };

        }

    }

}

struct MessageData {
    ready: event_listener::Event,
    response: Option<NonNull<dbus::DBusMessage>>,
}

struct DbusFd {
    inner: RawFd,
}

impl AsFd for DbusFd {
    fn as_fd(&self) -> BorrowedFd<'_> {
        // will be valid when this struct exists
        unsafe { BorrowedFd::borrow_raw(self.inner) }
    }
}

struct SharedData {
    fds: Arc<FdData>,
    replies: AsyncMutex<ReplyData>
}

struct FdData {
    lock: AsyncMutex<()>, // lock that the current dispatcher task holds
    active: HashMap<RawFd, DbusWatch>,
    err: Option<io::Error>,
}

struct ReplyData {
    events: HashMap<u32 /* message serial */, flume::Sender<DbusResponse>>,
}

struct DbusWatch {
    fd: async_io::Async<DbusFd>,
    watch: *mut dbus::DBusWatch,
}

pub struct DbusMessageBuilder {
    arena: TempArena, // this stores c strings
    message: *mut dbus::DBusMessage,
    iter: dbus::DBusMessageIter,
}

impl DbusMessageBuilder {

    pub fn build(self) -> DbusMessage {
        DbusMessage {
            arena: self.arena,
            inner: self.message
        }
    }

    pub fn arg<'a, A: Into<DbusArg<'a>>>(&mut self, input: A) {
        let arg: DbusArg = input.into();
        match arg {
            DbusArg::I32(num) => {
                let ptr = self.arena.push_i32(num as i32);
                unsafe { dbus::dbus_message_iter_append_basic(
                    &mut self.iter,
                    dbus::DBUS_TYPE_INT32,
                    ptr.cast()
                ) };
            },
            DbusArg::OwnedStr(text) => {
                let ptr = self.arena.push_string(text);
                unsafe { dbus::dbus_message_iter_append_basic(
                    &mut self.iter,
                    dbus::DBUS_TYPE_STRING,
                    ptr.cast()
                ) };
            },
            DbusArg::Str(text) => {
                let ptr = self.arena.push_str(text);
                unsafe { dbus::dbus_message_iter_append_basic(
                    &mut self.iter,
                    dbus::DBUS_TYPE_STRING,
                    ptr.cast()
                ) };
            }
        }
    }
    
}

pub struct DbusMessage {
    arena: TempArena,
    inner: *mut dbus::DBusMessage,
}

impl Drop for DbusMessage {
    fn drop(&mut self) {
        unsafe { dbus::dbus_message_unref(self.inner) }
    }
}

impl DbusMessage {

    pub fn call(dest: &str, path: &str, iface: &str, method: &str) -> DbusMessageBuilder {

        let mut buffer = TempArena::new();

        let dest   = buffer.push_str(dest);
        let path   = buffer.push_str(path);
        let iface  = buffer.push_str(iface);
        let method = buffer.push_str(method);

        let message = unsafe { dbus::dbus_message_new_method_call(
            dest, path, iface, method
        ) };

        assert!(!message.is_null());

        let mut iter = unsafe { mem::zeroed() };
        unsafe { dbus::dbus_message_iter_init_append(message, &mut iter) };

        DbusMessageBuilder {
            arena: buffer,
            message,
            iter
        }
    }
    
}

pub struct DbusResponse {
    message: NonNull<dbus::DBusMessage>,
    args: Vec<RawArg>,
}

impl Drop for DbusResponse {
    fn drop(&mut self) {
        unsafe { dbus::dbus_message_unref(self.message.as_ptr()) }
    }
}

impl DbusResponse {

    pub(crate) fn consume(raw: NonNull<dbus::DBusMessage>) -> Self {

        let mut args = Vec::new();

        let mut iter = unsafe { mem::zeroed() };
        unsafe { dbus::dbus_message_iter_init(raw.as_ptr(), &mut iter) }; // TODO: what does this retval mean?

        loop {
            let kind = unsafe { dbus::dbus_message_iter_get_arg_type(&mut iter) };
            match kind {
                dbus::DBUS_TYPE_INVALID => break,
                dbus::DBUS_TYPE_STRING => {
                    let mut out = ptr::null_mut();
                    unsafe { dbus::dbus_message_iter_get_basic(&mut iter, ((&mut out) as *mut *mut i8).cast()) };
                    args.push(RawArg::Str(out));
                },
                dbus::DBUS_TYPE_INT32 => {
                    let mut out = 0i32;
                    unsafe { dbus::dbus_message_iter_get_basic(&mut iter, ((&mut out) as *mut i32).cast()) };
                    args.push(RawArg::I32(out));
                },
                _ => println!("unknown return type..."),
            }
            unsafe { dbus::dbus_message_iter_next(&mut iter) };
        }

        Self {
            args,
            message: raw,
        }
        
    }

    pub fn arg<'a, T: FromDbus<'a>>(&'a self, idx: usize) -> Option<T> {

        let arg = self.args.get(idx)?;
        T::from_dbus(arg)
        
    }

    pub fn args(&self) -> &[RawArg] {
        &self.args
    }
    
}

pub enum DbusArg<'a> {
    I32(i32),
    Str(&'a str),
    OwnedStr(String),
}

impl<'a> From<i32> for DbusArg<'a>   { fn from(value: i32)       -> Self { Self::I32(value) } }
impl<'a> From<&'a str> for DbusArg<'a> { fn from(value: &'a str) -> Self { Self::Str(value) } }
impl<'a> From<String> for DbusArg<'a>  { fn from(value: String)  -> Self { Self::OwnedStr(value) } }

pub trait FromDbus<'a> {
    fn from_dbus(arg: &RawArg) -> Option<Self> where Self: Sized;
}

impl<'a> FromDbus<'a> for i32 {
    fn from_dbus(arg: &RawArg) -> Option<Self> {
        if let RawArg::I32(val) = arg { Some(*val) }
        else { None }
    }
}

impl<'a> FromDbus<'a> for &'a str {
    fn from_dbus(arg: &RawArg) -> Option<Self> {
        if let RawArg::Str(val) = arg {
            let cstr = unsafe { ffi::CStr::from_ptr(*val) };
            Some(cstr.to_str().unwrap())
        }
        else { None }
    }
}

#[derive(Debug)]
enum RawArg {
    I32(i32),
    Str(*mut i8),
}

struct TempArena {
    inner: Box<[u8]>,
    idx: usize,
    allocs: Vec<ffi::CString>,
}

impl TempArena { // TODO: unit test this mf

    pub fn new() -> Self {
        Self {
            inner: vec![0; 2048].into_boxed_slice(),
            idx: 0,
            allocs: Vec::new(),
        }
    }
    
    pub fn push_string(&mut self, string: String) -> *const i8 {
        let new = ffi::CString::new(string).unwrap();
        let result = new.as_c_str().as_ptr();
        self.allocs.push(new);
        result
    }

    /// This extends the lifetime to the lifetime of &self.
    /// &'x self -> &'x [u8].
    /// Moving the DbusMesssage doesn't move the buffer.
    /// Will fall back to one-to-one heap allocation if the string is to large.
    pub fn push_str(&mut self, string: &str) -> *const i8 {
        let start = self.idx;
        if start + string.len() < self.inner.len() || string.len() > 512 {
            self.inner[self.idx..self.idx + string.len()]
                .copy_from_slice(string.as_bytes()); // copy the string
            self.idx += string.len();
            self.inner[self.idx + 1] = 0; // append the nul-byte
            self.idx += 1;
            self.inner[start..self.idx].as_ptr().cast()
        } else {
            let new = ffi::CString::new(string).unwrap();
            let result = new.as_c_str().as_ptr();
            self.allocs.push(new);
            result
        }
    }
    
    pub fn push_i32(&mut self, num: i32) -> *const i32 {
        let start = self.idx;
        if start + 4 < self.inner.len() {
            self.inner[self.idx..4]
                .copy_from_slice(&num.to_ne_bytes()); // copy the number
            self.idx += 4;
            self.inner[start..self.idx].as_ptr().cast()
        } else {
            panic!("arena full");
        }
    }
    
}

pub struct DbusError {
    inner: dbus::DBusError,
}

impl Drop for DbusError {
    fn drop(&mut self) {
        unsafe { dbus::dbus_error_free(&mut self.inner) };
    }
}

impl fmt::Debug for DbusError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = if self.inner.name.is_null() {
            unsafe { ffi::CStr::from_bytes_with_nul_unchecked(b"unknown\0") }
        } else {
            unsafe { ffi::CStr::from_ptr(self.inner.name) }
        };
        let message = if self.inner.message.is_null() {
            unsafe { ffi::CStr::from_bytes_with_nul_unchecked(b"unknown\0") }
        } else {
            unsafe { ffi::CStr::from_ptr(self.inner.message) }
        };
        write!(f, "Dbus Error {:?} {:?}", name, message)
    }
}

impl fmt::Display for DbusError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl StdError for DbusError {}

// impl DbusError {
//     fn empty() -> Self {
//         let mut inner = unsafe { mem::zeroed() };
//         unsafe { dbus::dbus_error_init(&mut inner) };
//         Self { inner }
//     }
// }

impl From<dbus::DBusError> for DbusError {
    fn from(value: dbus::DBusError) -> Self {
        Self { inner: value }
    }
}
