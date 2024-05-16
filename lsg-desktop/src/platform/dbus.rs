
mod sys {

    // TODO: don't use io::Error everywhere as the values and messages don't make sense at all

    use std::{ffi, io};

    #[repr(transparent)]
    pub struct SdBus {
        inner: ffi::c_void
    }

    #[repr(transparent)]
    pub struct SdSlot {
        inner: ffi::c_void
    }

    #[repr(transparent)]
    pub struct SdMessage {
        inner: ffi::c_void
    }

    #[repr(C)]
    pub struct SdError {
        /// Can be NULL.
        pub name: *mut ffi::c_char,
        /// Can be NULL.
        pub msg: *mut ffi::c_char
    }

    #[derive(Clone, Copy)]
    #[repr(transparent)]
    pub struct SdResult {
        inner: ffi::c_int
    }

    impl SdResult {

        pub fn ok(&self) -> Result<u32, io::Error> {

            if self.inner < 0 {
                Err(io::Error::from_raw_os_error(-self.inner))
            } else {
                Ok(self.inner as u32)
            }

        }

    }

    #[derive(Clone, Copy)]
    #[repr(u64)]
    pub enum SdDumpKind { // this should've been a bool honestly
        WithHeader  = 1 << 0,
        SubtreeOnly = 1 << 1,
    }

    #[derive(Clone, Copy)]
    #[repr(u8)]
    pub enum SdMessageKind { // i do not fear UB
        Null = 0,
        MethodCall = 1,
        Response = 2,
        Error = 3,
        Signal = 4,
    }

    #[derive(Clone, Copy)]
    #[repr(i32)]
    pub enum SdHandlerStatus { // i do not fear UB
        /// Use the `out` argument to return a more precise error.
        Error = -1,
        /// Also call other handlers.
        Foreward = 0,
        /// Don't call other handlers.
        Consume = 1,
    }

    pub type SdMessageHandler = extern fn(
        msg: *mut SdMessage,
        data: *mut ffi::c_void,
        out: *mut SdError
    ) -> SdHandlerStatus;
    
    #[derive(Clone, Copy, Debug)]
    #[repr(i8)] // ffi::c_char, i do not fear UB
    pub enum SdBasicKind { // i do not fear UB
        // invalid
        Null  = 0,
        // simple types
        Byte      = 'y' as u32 as i8, // uint8_t *
        Bool      = 'b' as u32 as i8, // int * (not bool *)
        I16       = 'n' as u32 as i8, // int16_t *
        U16       = 'q' as u32 as i8, // uint16_t *
        I32       = 'i' as u32 as i8, // int32_t *
        U32       = 'u' as u32 as i8, // uint32_t *
        I64       = 'x' as u32 as i8, // int64_t *
        U64       = 't' as u32 as i8, // uint64_t *
        Double    = 'd' as u32 as i8, // double *
        String    = 's' as u32 as i8, // const char **
        ObjPath   = 'o' as u32 as i8, // const char **
        Signature = 'g' as u32 as i8, // const char **
        UnixFd    = 'h' as u32 as i8, // int *
        // compound types
        Array       = 'a' as u32 as i8, // int (len), ... (items)
        Variant     = 'v' as u32 as i8, // const char* (signature), any
        StructBegin = '(' as u32 as i8, // ... (items)
        StructEnd   = ')' as u32 as i8, // 
        PairBegin   = '{' as u32 as i8, // any, any (a pair)
        PairEnd     = '}' as u32 as i8, // 
        Pair        = 'e' as u32 as i8, // dict entry phantom type (undocumented)
        Struct      = 'r' as u32 as i8, // dict entry phantom type (undocumented)
    }

    extern {

        // bus

        pub fn sd_bus_open_user(out: &mut *mut SdBus) -> SdResult;

        pub fn sd_bus_unref(bus: *mut SdBus) -> *mut SdBus;

        pub fn sd_bus_flush(bus: *mut SdBus) -> SdResult;
        pub fn sd_bus_close(bus: *mut SdBus) -> SdResult;

        pub fn sd_bus_get_fd(bus: *mut SdBus) -> SdResult;
        pub fn sd_bus_process(bus: *mut SdBus, out: &mut *mut SdMessage) -> SdResult;

        pub fn sd_bus_send(bus: *mut SdBus, msg: *mut SdMessage, cookie: &mut u64) -> SdResult;

        // signals

        pub fn sd_bus_match_signal(
            // obj
            bus: *mut SdBus, out: &mut *mut SdSlot,
            // dest
            sender: *const ffi::c_char, path: *const ffi::c_char,
            interface: *const ffi::c_char, member: *const ffi::c_char,
            // cb
            callback: SdMessageHandler, data: *mut ffi::c_void
        ) -> SdResult;

        // message

        pub fn sd_bus_message_new_method_call(
            // obj
            bus: *mut SdBus, out: &mut *mut SdMessage,
            // dest
            dest: *const ffi::c_char, path: *const ffi::c_char,
            iface: *const ffi::c_char, method: *const ffi::c_char
        ) -> SdResult;

        pub fn sd_bus_message_unref(msg: *mut SdMessage) -> *mut SdMessage;

        pub fn sd_bus_message_dump(msg: *mut SdMessage, out: *mut ffi::c_void /* libc::FILE, pass NULL for stdout */, kind: SdDumpKind) -> SdResult;

        pub fn sd_bus_message_get_type(msg: *mut SdMessage, out: &mut SdMessageKind) -> SdResult;
        pub fn sd_bus_message_get_reply_cookie(msg: *mut SdMessage, out: &mut u64) -> SdResult;
        pub fn sd_bus_message_get_error(msg: *mut SdMessage) -> *mut SdError;

        pub fn sd_bus_message_get_sender(msg: *mut SdMessage) -> *const ffi::c_char;
        pub fn sd_bus_message_get_path(msg: *mut SdMessage) -> *const ffi::c_char;
        pub fn sd_bus_message_get_interface(msg: *mut SdMessage) -> *const ffi::c_char;
        pub fn sd_bus_message_get_member(msg: *mut SdMessage) -> *const ffi::c_char;

        pub fn sd_bus_message_peek_type(msg: *mut SdMessage, kind: &mut SdBasicKind, contents: &mut *mut SdBasicKind) -> SdResult;
        pub fn sd_bus_message_skip(msg: *mut SdMessage, kinds: *const SdBasicKind /* null-terminated array */) -> SdResult;
        pub fn sd_bus_message_read_basic(msg: *mut SdMessage, kind: SdBasicKind, out: *mut ffi::c_void) -> SdResult;
        pub fn sd_bus_message_enter_container(msg: *mut SdMessage, kind: SdBasicKind, contents: *const SdBasicKind) -> SdResult;
        pub fn sd_bus_message_exit_container(msg: *mut SdMessage) -> SdResult;

        pub fn sd_bus_message_append_basic(msg: *mut SdMessage, kind: SdBasicKind, out: *const ffi::c_void) -> SdResult;
        pub fn sd_bus_message_open_container(msg: *mut SdMessage, kind: SdBasicKind, contents: *const SdBasicKind) -> SdResult;
        pub fn sd_bus_message_close_container(msg: *mut SdMessage) -> SdResult;

        // error

        pub fn sd_bus_error_free(err: *mut SdError);

        // slot

        pub fn sd_bus_slot_unref(slot: *mut SdSlot) -> *mut SdSlot;
    
    }
    
}

use std::{collections::{hash_map::DefaultHasher, HashMap}, error, ffi::{self, CStr, CString}, future::poll_fn, hash::{Hash, Hasher}, io::{self, Write}, mem::zeroed, os::fd::{AsFd, AsRawFd, BorrowedFd, RawFd}, pin::Pin, ptr::{self, NonNull}, sync::Arc, task::{self, Poll, Waker}};
use async_lock::Mutex as AsyncMutex;
use futures_lite::{stream::unfold, FutureExt};

#[test]
fn dbus() {

    use futures_lite::future::block_on;

    block_on(async {

    
        let mut con = Connection::new().unwrap();

        let call = DbusCall::new(
             "org.freedesktop.DBus",
             "/org/freedesktop/DBus",
             "org.freedesktop.DBus.Debug.Stats",
             "GetStats",
        );
        
        let resp = con.send(call).await.unwrap();

        // println!("{:?}", resp.args);

        // let text: &str = resp.arg(0);
        // let text: &str = resp.arg(1);
        // println!("{}", text);

        dbg!(&resp.args);
        
    })
    
}

// #### dbus implementation ####

pub struct Connection {
    bus: *mut sys::SdBus,
    buslock: AsyncMutex<()>,
    busfd: async_io::Async<BusFd>,
    pending: AsyncMutex<PendingData>,
}

struct PendingData {
    /// list of channels to send responses to
    channels: HashMap<Id, flume::Sender<DbusResponse>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Id {
    Cookie(u64),
    SignalHash(u64)
}

struct BusFd {
    inner: RawFd,
}

impl AsFd for BusFd {
    fn as_fd(&self) -> BorrowedFd<'_> {
        unsafe { BorrowedFd::borrow_raw(self.inner) }
    }
}

// unsafe impl Send for DbusConnection {}
// unsafe impl Sync for DbusConnection {} TODO:

impl Drop for Connection {
    fn drop(&mut self) {
        unsafe { sys::sd_bus_flush(self.bus) };
        unsafe { sys::sd_bus_close(self.bus) };
        unsafe { sys::sd_bus_unref(self.bus) };
    }
}

impl Connection {

    pub fn new() -> io::Result<Self> {

        let mut bus = ptr::null_mut();
        unsafe { sys::sd_bus_open_user(&mut bus) }.ok()?;
        
        let raw = unsafe { sys::sd_bus_get_fd(bus) }.ok()?;
        let fd = BusFd { inner: raw as RawFd };

        assert!(!bus.is_null());

        Ok(Self {
            bus,
            buslock: AsyncMutex::new(()),
            busfd: async_io::Async::new(fd)?,
            pending: AsyncMutex::new(PendingData {
                channels: HashMap::new(),
            }),
        })
        
    }

    pub async fn send<'b>(&mut self, call: DbusCall<'b>) -> RequestResult<DbusResponse> {

        let dest   = CString::new(call.dest).map_err(io_error_other)?; // TODO: use an Arena
        let path   = CString::new(call.path).map_err(io_error_other)?;
        let iface  = CString::new(call.iface).map_err(io_error_other)?;
        let method = CString::new(call.method).map_err(io_error_other)?;

        let mut msg = ptr::null_mut();
        unsafe { sys::sd_bus_message_new_method_call(
            self.bus, &mut msg,
            dest.as_ptr(), path.as_ptr(), iface.as_ptr(), method.as_ptr()
        ) }.ok()?;

        assert!(!msg.is_null());
        for arg in call.args {
            push_arg(msg, arg);
        }

        let mut cookie = 0;
        unsafe { sys::sd_bus_send(self.bus, msg, &mut cookie) }.ok()?;

        let id = Id::Cookie(cookie);

        let mut guard = self.pending.lock_blocking(); // should never really block
        let (sender, receiver) = flume::bounded(1);
        guard.channels.insert(id, sender);
        drop(guard);

        self.drive(&receiver).await

    }

    pub async fn listen<'a, 's>(&'s self, signal: DbusSignal<'a>) -> io::Result<impl futures_lite::Stream<Item = RequestResult<DbusResponse>> + 's> {

        let path   = CString::new(signal.path).map_err(io_error_other)?; // TODO: use an Arena
        let iface  = CString::new(signal.iface).map_err(io_error_other)?;
        let method = CString::new(signal.method).map_err(io_error_other)?;

        extern fn ignore(_msg: *mut sys::SdMessage, _data: *mut ffi::c_void, _err: *mut sys::SdError) -> sys::SdHandlerStatus {
            sys::SdHandlerStatus::Consume
        }

        let mut slot = ptr::null_mut();
        unsafe {
            sys::sd_bus_match_signal(
                self.bus, &mut slot,
                ptr::null(), path.as_ptr(), iface.as_ptr(), method.as_ptr(),
                ignore, ptr::null_mut())
        }.ok()?;

        assert!(!slot.is_null());

        let hash = hash_dbus_signal(&path, &iface, &method);
        let id = Id::SignalHash(hash);

        let mut guard = self.pending.lock_blocking(); // should never really block
        let (sender, receiver) = flume::bounded(1);
        guard.channels.insert(id, sender);
        drop(guard);

        struct State {
            pub slot: *mut sys::SdSlot,
            pub receiver: flume::Receiver<DbusResponse>,
        }

        impl Drop for State {
            fn drop(&mut self) {
                unsafe { sys::sd_bus_slot_unref(self.slot) };
            }
        }

        let state = State {
            slot, // we move it in here so it get's dropped correctly
            receiver,
        };

        Ok(unfold(state, |it| async {
            Some((self.drive(&it.receiver).await, it))
        }))
        
    }

    /// This will try to wait for dbus messages and process them until
    /// there is a response abailable on the `receiver`.
    /// Only one task will drive the dbus at a time. (get it)
    async fn drive(&self, receiver: &flume::Receiver<DbusResponse>) -> RequestResult<DbusResponse> {
        
        loop {

            enum Either<'g> {
                /// A response arrived on the `receiver`.
                Response(DbusResponse),
                /// The dispatch lock could be arquired. We are now responsible for
                /// driving dbus.
                Dispatch(async_lock::MutexGuard<'g, ()>),
            }

            let response = async {
                let msg = receiver.recv_async().await.unwrap();
                Either::Response(msg)
            };

            let dispatch = async {
                let guard = self.buslock.lock().await;
                Either::Dispatch(guard)
            };

            match response.or(dispatch).await {
                Either::Response(resp) => {
                    return Ok(resp)
                },
                Either::Dispatch(_guard) => {

                    // this task will now run a single iteration
                    // of the dispatch loop

                    poll_fn(|ctx| {
                        let ready = self.busfd.poll_readable(ctx).is_ready()
                                  | self.busfd.poll_writable(ctx).is_ready();
                        if ready { Poll::Ready(()) } else { Poll::Pending }
                    }).await;

                    loop {

                        let mut msg = ptr::null_mut();
                        unsafe { sys::sd_bus_process(self.bus, &mut msg) };

                        if !msg.is_null() {

                            let mut kind = sys::SdMessageKind::Null;
                            unsafe { sys::sd_bus_message_get_type(msg, &mut kind).ok()? };

                            match kind {

                                sys::SdMessageKind::Null=> unreachable!(),
                                sys::SdMessageKind::MethodCall => unreachable!(),

                                sys::SdMessageKind::Error => {

                                    let raw = unsafe { sys::sd_bus_message_get_error(msg) };
                                    assert!(!raw.is_null());

                                    let name = unsafe { (&*raw).name };
                                    let cstr = unsafe { CStr::from_ptr(name) };

                                    let err = io::Error::other(cstr.to_string_lossy());

                                    unsafe { sys::sd_bus_message_unref(msg) };
                                    unsafe { sys::sd_bus_error_free(raw) };

                                    return Err(RequestError::Dbus(err));

                                },

                                sys::SdMessageKind::Response => {

                                    // unsafe { sys::sd_bus_message_dump(msg, ptr::null_mut(), sys::SdDumpKind::WithHeader).ok()? };

                                    let mut cookie = 0;
                                    let ret = unsafe { sys::sd_bus_message_get_reply_cookie(msg, &mut cookie) };
                                    if ret.ok().is_ok() {
                                        
                                        let resp = DbusResponse::consume(msg)?; // BUG: if this is an error, the message is leaked

                                        let mut guard = self.pending.lock_blocking();

                                        let sender = guard.channels.remove(&Id::Cookie(cookie)).unwrap();
                                        sender.try_send(resp).unwrap(); // can't block

                                    }

                                },

                                sys::SdMessageKind::Signal => {

                                    let resp = DbusResponse::consume(msg)?;

                                    let path = unsafe { CStr::from_ptr(resp._path) };
                                    let iface = unsafe { CStr::from_ptr(resp._iface) };
                                    let method = unsafe { CStr::from_ptr(resp._method) };

                                    let hash = hash_dbus_signal(path, iface, method);

                                    let mut guard = self.pending.lock_blocking();
                                
                                    let val = guard.channels.remove(&Id::SignalHash(hash));
                                    if let Some(sender) = val {
                                        // we are actually expecting this signal
                                        sender.try_send(resp).unwrap(); // can't block
                                    }

                                }

                            }

                        } else {
                            break // no more data to process
                        }
                    
                    }

                }
            }
        
        }

    }

}

fn push_arg(msg: *mut sys::SdMessage, arg: Arg<'_>) {

    match arg {
        
        Arg::Byte(val)     => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::Byte,   (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::Bool(val)     => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::Bool,   (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::I16(val)      => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::I16,    (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::U16(val)      => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::U16,    (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::I32(val)      => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::I32,    (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::U32(val)      => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::U32,    (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::I64(val)      => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::I64,    (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::U64(val)      => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::U64,    (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::Double(val)   => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::Double, (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::UnixFd(val)   => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::UnixFd, (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::Str(val)      => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::String, val    as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::OwnedStr(val) => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::String, (&val) as *const _ as *const ffi::c_void).ok().unwrap(); },
        Arg::Fd(val)       => unsafe { sys::sd_bus_message_append_basic(msg, sys::SdBasicKind::UnixFd, &val.as_raw_fd() as *const _ as *const ffi::c_void).ok().unwrap(); },

    }
    
}

fn io_error_other<E: Into<Box<dyn error::Error + Send + Sync>>>(err: E) -> io::Error {
    io::Error::other(err)
}

fn hash_dbus_signal(path: &CStr, iface: &CStr, method: &CStr) -> u64 {
    let mut hasher = DefaultHasher::new();
    path.hash(&mut hasher);
    iface.hash(&mut hasher);
    method.hash(&mut hasher);
    hasher.finish()
}

pub struct DbusProxy {
    dest: String,
    path: String,
}

impl DbusProxy {
    
}

pub struct DbusCall<'a> {
    dest: &'a str,
    path: &'a str,
    iface: &'a str,
    method: &'a str,
    args: Vec<Arg<'a>>,
}

impl<'a> DbusCall<'a> {

    pub fn new(dest: &'a str, path: &'a str, iface: &'a str, method: &'a str) -> Self {

        Self {
            dest,
            path,
            iface,
            method,
            args: Vec::new()
        }

    }
    
    pub fn arg<A: ValidArg<'a>>(&mut self, input: A) {
        self.args.push(input.into_arg());
    }
    
}

pub struct DbusSignal<'a> {
    path: &'a str,
    iface: &'a str,
    method: &'a str,
}

impl<'a> DbusSignal<'a> {

    pub fn new(path: &'a str, iface: &'a str, method: &'a str) -> Self {
        Self {
            path,
            iface,
            method
        }
    }

}

pub struct DbusResponse {
    inner: *mut sys::SdMessage,
    _sender: *const ffi::c_char,
    _path: *const ffi::c_char,
    _iface: *const ffi::c_char,
    _method: *const ffi::c_char,
    pub args: Vec<RawArg>,
}

impl Drop for DbusResponse {
    fn drop(&mut self) {
        unsafe { sys::sd_bus_message_unref(self.inner) }; // ignore the result
    }
}

impl DbusResponse {

    pub(crate) fn consume(msg: *mut sys::SdMessage) -> ParseResult<Self> {

        assert!(!msg.is_null());

        let sender     = unsafe { sys::sd_bus_message_get_sender(msg) };
        let path       = unsafe { sys::sd_bus_message_get_path(msg) };
        let interface  = unsafe { sys::sd_bus_message_get_interface(msg) };
        let method     = unsafe { sys::sd_bus_message_get_member(msg) };

        let mut args = Vec::new();

        loop {

            let mut kind = sys::SdBasicKind::Null;
            let mut contents = ptr::null_mut();
            let more = unsafe { sys::sd_bus_message_peek_type(msg, &mut kind, &mut contents) }.ok().unwrap();
            if more == 0 { break }

            let Some(arg) = read_arg(msg, kind, contents)? else { break };
            args.push(arg);

        };

        Ok(Self {
            inner: msg,
            _sender: sender,
            _path: path,
            _iface: interface,
            _method: method,
            args
        })

    }

    #[track_caller]
    pub fn arg<'a, T: ValidArg<'a>>(&'a self, idx: usize) -> T {
        match self.get(idx) {
            Ok(val) => val,
            Err(err) => panic!("cannot read arg #{idx}: {err:?}")
        }
    }

    pub fn get<'a, T: ValidArg<'a>>(&'a self, idx: usize) -> Result<T, ArgError> {
        let arg = self.args.get(idx).ok_or(ArgError::DoesntExist)?;
        T::from_raw_arg(arg).ok_or(ArgError::InvalidType)
    }
    
}

fn read_arg(msg: *mut sys::SdMessage, kind: sys::SdBasicKind, contents: *const sys::SdBasicKind) -> ParseResult<Option<RawArg>> {
    
    match kind {

        // basic kinds

        sys::SdBasicKind::Null => {
            unreachable!();
        },
        sys::SdBasicKind::Byte => {
            let mut val: u8 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u8 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::Byte(val))))
        },
        sys::SdBasicKind::Bool => {
            let mut val: ffi::c_int = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i32 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::Bool(val == 1))))
        },
        sys::SdBasicKind::I16 => {
            let mut val: i16 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i16 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::I16(val))))
        },
        sys::SdBasicKind::U16 => {
            let mut val: u16 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u16 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::U16(val))))
        },
        sys::SdBasicKind::I32 => {
            let mut val: i32 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i32 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::I32(val))))
        },
        sys::SdBasicKind::U32 => {
            let mut val: u32 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u32 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::U32(val))))
        },
        sys::SdBasicKind::I64 => {
            let mut val: i64 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i64 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::I64(val))))
        },
        sys::SdBasicKind::U64 => {
            let mut val: u64 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u64 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::U64(val))))
        },
        sys::SdBasicKind::Double => {
            let mut val: f64 = 0.0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut f64 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::Double(val.to_bits()))))
        },
        sys::SdBasicKind::String |
        sys::SdBasicKind::Signature |
        sys::SdBasicKind::ObjPath => {
            let mut val: *const ffi::c_char = ptr::null();
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut *const ffi::c_char as _) }.ok()?;
            assert!(!val.is_null());
            let cstr = unsafe { CStr::from_ptr(val) };
            Ok(Some(RawArg::Simple(SimpleArg::Str(cstr))))
        },
        sys::SdBasicKind::UnixFd => {
            let mut val: RawFd = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut RawFd as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleArg::UnixFd(val))))
        },

        // compound kinds

        sys::SdBasicKind::Array => {

            if let sys::SdBasicKind::PairBegin = unsafe { *contents } {

                // this is a map

                let mut map: HashMap<SimpleArg, RawArg> = HashMap::new();

                // enter the array
                unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok()?;
            
                // TODO: remove all the debug unwraps and cascade the error

                loop {

                    // unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok().unwrap();

                    let Some((kind, contents)) = peek_kind(msg) else { break };
                    assert!(matches!(kind, sys::SdBasicKind::Pair));

                    // enter the pair
                    unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok()?;

                    let (kind, contents) = peek_kind(msg).ok_or(ParseError::eof())?;
                    let key = read_arg(msg, kind, ptr::null())?.ok_or(ParseError::eof())?;
                    assert!(contents.is_null()); // cannot have contents since this must be a basic value

                    let (kind, contents) = peek_kind(msg).ok_or(ParseError::eof())?;
                    let val = read_arg(msg, kind, contents)?.ok_or(ParseError::eof())?;

                    let RawArg::Simple(key) = key else { return Err(ParseError::protocol()) };
                    map.insert(key, val);

                    unsafe { sys::sd_bus_message_exit_container(msg) }.ok()?;
                    unsafe { sys::sd_bus_message_skip(msg, ptr::null()) }; // consume the "pair"

                };

                unsafe { sys::sd_bus_message_exit_container(msg) }.ok()?;
                // TODO: do we need to skip the map here?

                Ok(Some(RawArg::Compound(CompoundArg::Map(map))))
                
            } else {

                // this is an array

                let mut args = Vec::new();
                
                unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok()?;
            
                loop {

                    let Some((kind, contents)) = peek_kind(msg) else { break };
                    let arg = read_arg(msg, kind, contents)?.ok_or(ParseError::eof())?;
                    args.push(arg);

                };

                unsafe { sys::sd_bus_message_exit_container(msg) }.ok()?;

                Ok(Some(RawArg::Compound(CompoundArg::Array(args))))
                
            }

        },
        sys::SdBasicKind::Struct |
        sys::SdBasicKind::Variant => {

            unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok()?;

            let mut args = Vec::new();

            loop {

                // TODO: there seems to be a case where read_arg expects contents to be non-null without asserting it

                let Some((kind, contents)) = peek_kind(msg) else { break };
                let arg = read_arg(msg, kind, contents)?.ok_or(ParseError::eof())?;
                args.push(arg);
                
            }

            unsafe { sys::sd_bus_message_exit_container(msg) }.ok()?;
            unsafe { sys::sd_bus_message_skip(msg, ptr::null()) }; // consume the "variant"

            Ok(Some(RawArg::Compound(CompoundArg::Struct(args))))

        },
        sys::SdBasicKind::Pair        => { unreachable!() },
        sys::SdBasicKind::StructBegin => { unreachable!(); },
        sys::SdBasicKind::StructEnd   => { unreachable!(); },
        sys::SdBasicKind::PairBegin   => { Ok(Some(RawArg::Ignore)) },
        sys::SdBasicKind::PairEnd     => { Ok(Some(RawArg::Ignore)) },

    }

}

fn peek_kind(msg: *mut sys::SdMessage) -> Option<(sys::SdBasicKind, *const sys::SdBasicKind)> {
    let mut kind = sys::SdBasicKind::Null;
    let mut contents = ptr::null_mut();
    let more = unsafe { sys::sd_bus_message_peek_type(msg, &mut kind, &mut contents) }.ok().unwrap();
    if more == 0 {
        None
    } else {
        Some((kind, contents))
    }
}

#[derive(Debug)]
pub enum ArgError {
    DoesntExist,
    InvalidType,
}

pub enum Arg<'a> {
    Byte(u8),
    Bool(bool),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Double(f64),
    UnixFd(RawFd),
    Str(&'a str),
    OwnedStr(String),
    Fd(BorrowedFd<'a>),
}

impl<'a> From<String>  for Arg<'a> { fn from(value: String)  -> Self { Self::OwnedStr(value) } }

pub trait ValidArg<'a> {
    fn into_arg(self) -> Arg<'a> where Self: Sized;
    fn from_raw_arg(arg: &RawArg) -> Option<Self> where Self: Sized;
}

impl<'a> ValidArg<'a> for &'a str {
    fn into_arg(self) -> Arg<'a> where Self: Sized + 'a {
        Arg::Str(self)
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleArg::Str(val)) = arg { Some(unsafe { &**val }.to_str().unwrap()) }
        else { None }
    }
}

impl<'a> ValidArg<'a> for String {
    fn into_arg(self) -> Arg<'a> where Self: Sized + 'a {
        Arg::OwnedStr(self)
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleArg::Str(val)) = arg { Some(unsafe { &**val }.to_str().ok()?.to_string()) }
        else { None }
    }
}

impl<'a> ValidArg<'a> for f64 {
    fn into_arg(self) -> Arg<'a> where Self: Sized + 'a {
        Arg::Double(self)
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleArg::Double(val)) = arg { Some(f64::from_bits(*val)) }
        else { None }
    }
}

impl<'a> ValidArg<'a> for BorrowedFd<'a> {
    fn into_arg(self) -> Arg<'a> where Self: Sized + 'a {
        Arg::Fd(self)
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleArg::UnixFd(val)) = arg { Some(unsafe { BorrowedFd::borrow_raw(*val) }) }
        else { None }
    }
}

macro_rules! impl_valid_arg {
    ($(($name: ident: $t: ident)),*,) => {
        $(
            impl<'a> ValidArg<'a> for $t {
                fn into_arg(self) -> Arg<'a> where Self: Sized + 'a {
                    Arg::$name(self)
                }
                fn from_raw_arg(arg: &RawArg) -> Option<Self> {
                    if let RawArg::Simple(SimpleArg::$name(val)) = arg { Some(*val) }
                    else { None }
                }
            }
        )*
    };
}

impl_valid_arg!(
    (Byte: u8),
    (Bool: bool),
    (I16: i16),
    (U16: u16),
    (I32: i32),
    (U32: u32),
    (I64: i64),
    (U64: u64),
);

#[derive(Debug)]
pub enum RawArg {
    Simple(SimpleArg),
    Compound(CompoundArg),
    Ignore,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum SimpleArg {
    Byte(u8),
    Bool(bool),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Double(u64), // so I don't have to write a custom Eq impl
    UnixFd(RawFd),
    Str(*const CStr),
}

#[derive(Debug)]
pub enum CompoundArg {
    Array(Vec<RawArg>),
    Struct(Vec<RawArg>),
    Map(HashMap<SimpleArg, RawArg>),
}

#[derive(Debug)]
pub struct ParseError {
    inner: io::Error,
}

pub type ParseResult<T> = Result<T, ParseError>;

impl From<io::Error> for ParseError {
    fn from(inner: io::Error) -> Self {
        Self { inner }
    }
}

impl ParseError {
    /// No more data, but data was expected.
    fn eof() -> Self {
        Self { inner: io::Error::new(io::ErrorKind::UnexpectedEof, "not enough values") }
    }
    /// Otherwise uncovered protocol violation.
    fn protocol() -> Self {
        Self { inner: io::Error::new(io::ErrorKind::Other, "protocol violation") }
    }
}

#[derive(Debug)]
pub enum RequestError {
    Dbus(io::Error),
    Parse(ParseError)
}

pub type RequestResult<T> = Result<T, RequestError>;

impl From<io::Error> for RequestError {
    fn from(value: io::Error) -> Self {
        Self::Dbus(value)
    }
}

impl From<ParseError> for RequestError {
    fn from(value: ParseError) -> Self {
        Self::Parse(value)
    }
}
