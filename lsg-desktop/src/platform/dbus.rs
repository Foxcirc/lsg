
use core::fmt;
use std::{collections::{hash_map::DefaultHasher, HashMap}, error, ffi::{self, CStr, CString}, future::poll_fn, hash::{Hash, Hasher}, io, os::fd::{AsFd, AsRawFd, BorrowedFd, RawFd}, ptr::{self, NonNull}, sync::Arc, task::Poll, time::Duration};
use async_lock::Mutex as AsyncMutex;
use futures_lite::{pin, FutureExt};

#[test]
fn dbus() {

    use futures_lite::future::block_on;

    block_on(async {

    
        let con = Connection::new().unwrap();

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

        // dbg!(&resp.args);
        
    })
    
}

#[test]
fn notifications() {

    use futures_lite::future::block_on;

    block_on(async {

        let mut con = Arc::new(Connection::new().unwrap());
        let proxy = NotifyProxy::new(&con);

        let notif = Notif::new("lsg-test (dbus)", "show me the magic")
            .body("some text in the body")
            .urgency(Urgency::Critical)
            .action(Action::default("default selection"))
            .action(Action::new("a", "selection A"))
            .action(Action::new("b", "selection B"));
        
        let id = proxy.send(notif).await.unwrap();
        let action = proxy.invoked(id).await.unwrap();

        println!("invoked action: {}", action.id);

        proxy.close(id).await.unwrap();
        
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
    channels: HashMap<Id, (async_broadcast::Sender<DbusResponse>, async_broadcast::Receiver<DbusResponse>)>,
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

    pub fn new() -> io::Result<Arc<Self>> {

        let mut bus = ptr::null_mut();
        unsafe { sys::sd_bus_open_user(&mut bus) }.ok()?;
        
        let raw = unsafe { sys::sd_bus_get_fd(bus) }.ok()?;
        let fd = BusFd { inner: raw as RawFd };

        assert!(!bus.is_null());

        Ok(Arc::new(Self {
            bus,
            buslock: AsyncMutex::new(()),
            busfd: async_io::Async::new(fd)?,
            pending: AsyncMutex::new(PendingData {
                channels: HashMap::new(),
            }),
        }))
        
    }

    pub async fn send<'b>(&self, call: DbusCall<'b>) -> RequestResult<DbusResponse> {

        let dest   = CString::new(call.dest).map_err(io_error_other)?; // TODO: arena
        let path   = CString::new(call.path).map_err(io_error_other)?;
        let iface  = CString::new(call.iface).map_err(io_error_other)?;
        let method = CString::new(call.method).map_err(io_error_other)?;

        let mut msg = ptr::null_mut();
        unsafe { sys::sd_bus_message_new_method_call(
            self.bus, &mut msg,
            dest.as_ptr(), path.as_ptr(), iface.as_ptr(), method.as_ptr()
        ) }.ok().map_err(|_| RequestError::argfmt())?;

        assert!(!msg.is_null());
        for arg in call.args {
            push_arg(msg, arg).unwrap(); // should not fail
        }

        // unsafe { sys::sd_bus_message_seal(msg, 1234, 1234) };
        // unsafe { sys::sd_bus_message_dump(msg, ptr::null_mut(), sys::SdDumpKind::SubtreeOnly) };
        // panic!("");

        let mut cookie = 0;
        unsafe { sys::sd_bus_send(self.bus, msg, &mut cookie) }.ok()?;

        let id = Id::Cookie(cookie);

        let mut guard = self.pending.lock_blocking(); // should never really block
        let pair = async_broadcast::broadcast(1);
        let mut receiver = pair.1.clone();
        guard.channels.insert(id, pair);
        drop(guard);

        self.drive(&mut receiver).await

    }

    pub async fn listen<'a, 's>(self: &'s Arc<Self>, signal: DbusSignal<'a>) -> RequestResult<Listener> {

        let path   = CString::new(signal.path).map_err(io_error_other)?; // TODO: arena
        let iface  = CString::new(signal.iface).map_err(io_error_other)?;
        let method = CString::new(signal.method).map_err(io_error_other)?;

        extern fn ignore(_msg: *mut sys::SdMessage, _data: *mut ffi::c_void, _err: *mut sys::SdError) -> sys::SdHandlerStatus {
            // note: _msg is only borrowed here
            // the message is processed in the main event loop
            sys::SdHandlerStatus::Forward // call other handlers if registered
        }

        let mut slot = ptr::null_mut();
        unsafe {
            sys::sd_bus_match_signal(
                self.bus, &mut slot,
                ptr::null(), path.as_ptr(), iface.as_ptr(), method.as_ptr(),
                ignore, ptr::null_mut())
        }.ok().map_err(|_| RequestError::argfmt())?;

        assert!(!slot.is_null());

        let hash = hash_dbus_signal(&path, &iface, &method);
        let id = Id::SignalHash(hash);

        let mut guard = self.pending.lock_blocking(); // should never really block

        if !guard.channels.contains_key(&id) {
            let pair = async_broadcast::broadcast(1);
            guard.channels.insert(id, pair);
        }

        let pair = guard.channels.get(&id).unwrap();

        Ok(Listener {
            con: Arc::clone(&self),
            receiver: pair.1.clone(),
            slot
        })

    }

    /// This will try to wait for dbus messages and process them until
    /// there is a response abailable on the `receiver`.
    /// Only one task will drive the dbus at a time. (get it)
    async fn drive(&self, receiver: &mut async_broadcast::Receiver<DbusResponse>) -> RequestResult<DbusResponse> {
        
        loop {

            enum Either<'g> {
                /// A response arrived on the `receiver`.
                Response(DbusResponse),
                /// The dispatch lock could be arquired. We are now responsible for
                /// driving dbus.
                Dispatch(async_lock::MutexGuard<'g, ()>),
            }

            let response = async {
                let msg = receiver.recv_direct().await.unwrap();
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

                                        let pair = guard.channels.remove(&Id::Cookie(cookie)).unwrap();
                                        pair.0.broadcast(resp).await.unwrap(); // can't block

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
                                    if let Some(pair) = val { // we can get signals we aren't expecting
                                       pair.0.broadcast(resp).await.unwrap(); // can't block
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

pub struct Listener {
    con: Arc<Connection>,
    receiver: async_broadcast::Receiver<DbusResponse>,
    slot: *mut sys::SdSlot,
}

impl Drop for Listener {
    fn drop(&mut self) {
        unsafe { sys::sd_bus_slot_unref(self.slot) };
    }
}

impl Clone for Listener {
    fn clone(&self) -> Self {
        unsafe { sys::sd_bus_slot_ref(self.slot) };
        Self {
            con: Arc::clone(&self.con),
            receiver: self.receiver.clone(),
            slot: self.slot,
        }
    }
}

impl Listener {
    pub async fn next(&mut self) -> RequestResult<DbusResponse> {
        self.con.drive(&mut self.receiver).await
    }
}

fn push_arg(msg: *mut sys::SdMessage, arg: Arg<'_>) -> ParseResult<()> {

    match arg {
        Arg::Simple(simple) => {
            let vals = match simple {
                SimpleArg::Byte(val)   => Some((sys::SdBasicKind::Byte,   &val as *const _ as *const ffi::c_void)),
                SimpleArg::I16(val)    => Some((sys::SdBasicKind::I16,    &val as *const _ as *const ffi::c_void)),
                SimpleArg::U16(val)    => Some((sys::SdBasicKind::U16,    &val as *const _ as *const ffi::c_void)),
                SimpleArg::I32(val)    => Some((sys::SdBasicKind::I32,    &val as *const _ as *const ffi::c_void)),
                SimpleArg::U32(val)    => Some((sys::SdBasicKind::U32,    &val as *const _ as *const ffi::c_void)),
                SimpleArg::I64(val)    => Some((sys::SdBasicKind::I64,    &val as *const _ as *const ffi::c_void)),
                SimpleArg::U64(val)    => Some((sys::SdBasicKind::U64,    &val as *const _ as *const ffi::c_void)),
                SimpleArg::Double(val) => Some((sys::SdBasicKind::Double, &val as *const _ as *const ffi::c_void)),
                // the following values need lifetime extension because they don't reference `arg` directly
                SimpleArg::Bool(val)   => {
                    let new = val as ffi::c_int;
                    let (kind, ptr) = (sys::SdBasicKind::Bool, &new as *const _ as *const ffi::c_void); // TODO: same as below
                    unsafe { sys::sd_bus_message_append_basic(msg, kind, ptr).ok()?; }
                    None
                },
                SimpleArg::Fd(val)     => {
                    let new = val.as_raw_fd();
                    let (kind, ptr) = (sys::SdBasicKind::UnixFd, &new as *const _ as *const ffi::c_void); // TODO: is this okay or is the pointer invalid
                    unsafe { sys::sd_bus_message_append_basic(msg, kind, ptr).ok()?; }
                    None
                },
                SimpleArg::Str(val) => {
                    // todo: use an arena... but with a smaller block size
                    let cstr = CString::new(val).unwrap();
                    let (kind, ptr) = (sys::SdBasicKind::String, cstr.as_ptr() as *const ffi::c_void);
                    unsafe { sys::sd_bus_message_append_basic(msg, kind, ptr).ok()?; }
                    None
                },
                SimpleArg::OwnedStr(val) => {
                    let cstr = CString::new(val).unwrap();
                    let (kind, ptr) = (sys::SdBasicKind::String, cstr.as_ptr() as *const ffi::c_void);
                    unsafe { sys::sd_bus_message_append_basic(msg, kind, ptr).ok()?; }
                    None
                },
            };
            if let Some((kind, ptr)) = vals {
                unsafe { sys::sd_bus_message_append_basic(msg, kind, ptr).ok()?; }
            }
        },
        Arg::Compound(compound) => match compound {

            CompoundArg::Array(kind, items) => {
                let contents = [kind, sys::SdBasicKind::Null];
                unsafe { sys::sd_bus_message_open_container(msg, sys::SdBasicKind::Array, contents.as_ptr()).ok()? };
                for it in items {
                    push_arg(msg, it)?;
                }
                unsafe { sys::sd_bus_message_close_container(msg).ok()? };
            },

            CompoundArg::Map(tkey, tval, map) => {

                let contents = [
                    sys::SdBasicKind::PairBegin,
                    tkey, tval,
                    sys::SdBasicKind::PairEnd,
                    // sys::SdBasicKind::Pair,
                    sys::SdBasicKind::Null,
                ]; // {key, val}
                
                unsafe { sys::sd_bus_message_open_container(msg, sys::SdBasicKind::Array, contents.as_ptr()).ok().unwrap() };

                for (key, val) in map.into_iter() {

                    let contents = [tkey, tval, sys::SdBasicKind::Null]; // {key, val}
                    unsafe { sys::sd_bus_message_open_container(msg, sys::SdBasicKind::Pair, contents.as_ptr()).ok().unwrap() };
                    push_arg(msg, Arg::Simple(key))?;
                    push_arg(msg, val)?;
                    unsafe { sys::sd_bus_message_close_container(msg).ok().unwrap() };

                }

                unsafe { sys::sd_bus_message_close_container(msg).ok().unwrap() };
                
            },

            CompoundArg::Variant(kinds, val) => {

                unsafe { sys::sd_bus_message_open_container(msg, sys::SdBasicKind::Variant, kinds.as_ptr()).ok().unwrap() };
                push_arg(msg, *val).unwrap();
                unsafe { sys::sd_bus_message_close_container(msg).ok().unwrap() };

            }
            
        }
    };

    Ok(())
    
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
            args: Vec::new(),
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
            method,
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

impl Clone for DbusResponse {
    fn clone(&self) -> Self {
        unsafe { sys::sd_bus_message_ref(self.inner) };
        Self {
            inner: self.inner,
            _sender: self._sender,
            _path: self._path,
            _iface: self._iface,
            _method: self._method,
            args: self.args.clone(),
        }
    }
}

impl Drop for DbusResponse {
    fn drop(&mut self) {
        unsafe { sys::sd_bus_message_unref(self.inner) };
    }
}

impl DbusResponse {

    pub(crate) fn consume(msg: *mut sys::SdMessage) -> ParseResult<Self> {

        assert!(!msg.is_null());

        // these are used for identifying signals
        let sender     = unsafe { sys::sd_bus_message_get_sender(msg) };
        let path       = unsafe { sys::sd_bus_message_get_path(msg) };
        let interface  = unsafe { sys::sd_bus_message_get_interface(msg) };
        let method     = unsafe { sys::sd_bus_message_get_member(msg) };

        let mut args = Vec::new();

        loop {

            let mut kind = sys::SdBasicKind::Null;
            let mut contents = ptr::null_mut();
            let more = unsafe { sys::sd_bus_message_peek_type(msg, &mut kind, &mut contents) }.ok()?;
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

impl fmt::Debug for DbusResponse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DbusResponse {{")?;
        writeln!(f, "\tsender: {:?}", NonNull::new(self._sender.cast_mut()).map(|ptr| unsafe { CStr::from_ptr(ptr.as_ptr()) }))?;
        writeln!(f, "\tpath: {:?}",   NonNull::new(self._path.cast_mut()).  map(|ptr| unsafe { CStr::from_ptr(ptr.as_ptr()) } ))?;
        writeln!(f, "\tiface: {:?}",  NonNull::new(self._iface.cast_mut()). map(|ptr| unsafe { CStr::from_ptr(ptr.as_ptr()) }))?;
        writeln!(f, "\tmethod: {:?}", NonNull::new(self._method.cast_mut()).map(|ptr| unsafe { CStr::from_ptr(ptr.as_ptr()) }))?;
        writeln!(f, "\t### response args ###")?;
        writeln!(f, "{:#?}", self.args)?;
        writeln!(f, "}}")
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
            Ok(Some(RawArg::Simple(SimpleRawArg::Byte(val))))
        },
        sys::SdBasicKind::Bool => {
            let mut val: ffi::c_int = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i32 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::Bool(val == 1))))
        },
        sys::SdBasicKind::I16 => {
            let mut val: i16 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i16 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::I16(val))))
        },
        sys::SdBasicKind::U16 => {
            let mut val: u16 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u16 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::U16(val))))
        },
        sys::SdBasicKind::I32 => {
            let mut val: i32 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i32 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::I32(val))))
        },
        sys::SdBasicKind::U32 => {
            let mut val: u32 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u32 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::U32(val))))
        },
        sys::SdBasicKind::I64 => {
            let mut val: i64 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i64 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::I64(val))))
        },
        sys::SdBasicKind::U64 => {
            let mut val: u64 = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u64 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::U64(val))))
        },
        sys::SdBasicKind::Double => {
            let mut val: f64 = 0.0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut f64 as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::Double(val.to_bits()))))
        },
        sys::SdBasicKind::String |
        sys::SdBasicKind::Signature |
        sys::SdBasicKind::ObjPath => {
            let mut val: *const ffi::c_char = ptr::null();
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut *const ffi::c_char as _) }.ok()?;
            assert!(!val.is_null());
            let cstr = unsafe { CStr::from_ptr(val) };
            Ok(Some(RawArg::Simple(SimpleRawArg::Str(cstr))))
        },
        sys::SdBasicKind::UnixFd => {
            let mut val: RawFd = 0;
            unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut RawFd as _) }.ok()?;
            Ok(Some(RawArg::Simple(SimpleRawArg::UnixFd(val))))
        },

        // compound kinds

        sys::SdBasicKind::Array => {

            if let sys::SdBasicKind::PairBegin = unsafe { *contents } {

                // this is a map

                let mut map: HashMap<SimpleRawArg, RawArg> = HashMap::new();

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

                Ok(Some(RawArg::Compound(CompoundRawArg::Map(map))))
                
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

                Ok(Some(RawArg::Compound(CompoundRawArg::Array(args))))
                
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

            Ok(Some(RawArg::Compound(CompoundRawArg::Struct(args))))

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

#[derive(Debug)]
pub enum Arg<'a> {
    Simple(SimpleArg<'a>),
    Compound(CompoundArg<'a>),
}
impl<'a> Arg<'a> {

    fn kinds(&self) -> Vec<sys::SdBasicKind> {
        let mut out = Vec::new();
        push_kinds(&mut out, self);
        out.push(sys::SdBasicKind::Null);
        out
    }

}

fn push_kinds(out: &mut Vec<sys::SdBasicKind>, arg: &Arg<'_>) {
    match arg {
        Arg::Simple(simple) => {
            let kind = match simple {
                SimpleArg::Byte(..)     => sys::SdBasicKind::Byte,
                SimpleArg::Bool(..)     => sys::SdBasicKind::Bool,
                SimpleArg::I16(..)      => sys::SdBasicKind::I16,
                SimpleArg::U16(..)      => sys::SdBasicKind::U16,
                SimpleArg::I32(..)      => sys::SdBasicKind::I32,
                SimpleArg::U32(..)      => sys::SdBasicKind::U32,
                SimpleArg::I64(..)      => sys::SdBasicKind::I64,
                SimpleArg::U64(..)      => sys::SdBasicKind::U64,
                SimpleArg::Double(..)   => sys::SdBasicKind::Double,
                SimpleArg::Str(..)      => sys::SdBasicKind::String,
                SimpleArg::OwnedStr(..) => sys::SdBasicKind::String,
                SimpleArg::Fd(..)       => sys::SdBasicKind::UnixFd,
            };
            out.push(kind);
        },
        Arg::Compound(compound) => match compound {
            CompoundArg::Array(kind, ..) => {
                out.push(sys::SdBasicKind::Array);
                out.push(*kind);
            },
            CompoundArg::Map(tkey, tval, ..) => {
                out.push(sys::SdBasicKind::Array);
                out.push(sys::SdBasicKind::PairBegin);
                out.push(*tkey);
                out.push(*tval);
                out.push(sys::SdBasicKind::PairEnd);
            },
            CompoundArg::Variant(..) => {
                out.push(sys::SdBasicKind::Variant);
            }
        }
    };
}

#[derive(Debug)]
pub enum SimpleArg<'a> {
    Byte(u8),
    Bool(bool),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Double(f64),
    Str(&'a str),
    OwnedStr(String),
    Fd(BorrowedFd<'a>),
}

impl PartialEq for SimpleArg<'_> { // can't derive because the world is a cruel place
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Byte(val), Self::Byte(other)) => val == other,
            (Self::Bool(val), Self::Bool(other)) => val == other,
            (Self::I16(val), Self::I16(other)) => val == other,
            (Self::U16(val), Self::U16(other)) => val == other,
            (Self::I32(val), Self::I32(other)) => val == other,
            (Self::U32(val), Self::U32(other)) => val == other,
            (Self::I64(val), Self::I64(other)) => val == other,
            (Self::U64(val), Self::U64(other)) => val == other,
            (Self::Double(val), Self::Double(other)) => val == other,
            (Self::Str(val), Self::Str(other)) => val == other,
            (Self::OwnedStr(val), Self::OwnedStr(other)) => val == other,
            (Self::Fd(val), Self::Fd(other)) => val.as_raw_fd() == other.as_raw_fd(),
            (..) => false,
        }
    }
}

impl Eq for SimpleArg<'_> {} // TODO: is this okay? make a CustomEq type and wrap the not Eq types instead of this mess

impl Hash for SimpleArg<'_> { // can't derive because the world is a cruel place
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            Self::Byte(val) => Hash::hash(val, hasher),
            Self::Bool(val) => Hash::hash(val, hasher),
            Self::I16(val) => Hash::hash(val, hasher),
            Self::U16(val) => Hash::hash(val, hasher),
            Self::I32(val) => Hash::hash(val, hasher),
            Self::U32(val) => Hash::hash(val, hasher),
            Self::I64(val) => Hash::hash(val, hasher),
            Self::U64(val) => Hash::hash(val, hasher),
            Self::Double(..) => unimplemented!(),
            Self::Str(val) => Hash::hash(val, hasher),
            Self::OwnedStr(val) => Hash::hash(val, hasher),
            Self::Fd(val) => Hash::hash(&val.as_raw_fd(), hasher),
        }
    }
}

#[derive(Debug)]
pub enum CompoundArg<'a> {
    Array(sys::SdBasicKind, Vec<Arg<'a>>),
    Map(sys::SdBasicKind, sys::SdBasicKind, HashMap<SimpleArg<'a>, Arg<'a>>),
    Variant(Vec<sys::SdBasicKind>, Box<Arg<'a>>),
}

impl<'a> From<String>  for Arg<'a> { fn from(value: String)  -> Self { Self::Simple(SimpleArg::OwnedStr(value)) } }

pub trait ValidArg<'a> {
    fn into_arg(self) -> Arg<'a> where Self: Sized;
    fn from_raw_arg(arg: &RawArg) -> Option<Self> where Self: Sized;
    fn kind() -> sys::SdBasicKind;
}

impl<'a> ValidArg<'a> for &'a str {
    fn into_arg(self) -> Arg<'a> where Self: Sized {
        Arg::Simple(SimpleArg::Str(self))
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleRawArg::Str(val)) = arg { Some(unsafe { &**val }.to_str().unwrap()) }
        else { None }
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::String
    }
}

impl<'a> ValidArg<'a> for String {
    fn into_arg(self) -> Arg<'a> where Self: Sized {
        Arg::Simple(SimpleArg::OwnedStr(self))
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleRawArg::Str(val)) = arg { Some(unsafe { &**val }.to_str().ok()?.to_string()) }
        else { None }
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::String
    }
}

impl<'a> ValidArg<'a> for f64 {
    fn into_arg(self) -> Arg<'a> where Self: Sized {
        Arg::Simple(SimpleArg::Double(self))
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleRawArg::Double(val)) = arg { Some(f64::from_bits(*val)) }
        else { None }
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::Double
    }
}

impl<'a> ValidArg<'a> for BorrowedFd<'a> {
    fn into_arg(self) -> Arg<'a> where Self: Sized {
        Arg::Simple(SimpleArg::Fd(self))
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleRawArg::UnixFd(val)) = arg { Some(unsafe { BorrowedFd::borrow_raw(*val) }) }
        else { None }
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::UnixFd
    }
}

macro_rules! impl_valid_arg {
    ($(($name: ident: $t: ident)),*,) => {
        $(
            impl<'a> ValidArg<'a> for $t {
                fn into_arg(self) -> Arg<'a> where Self: Sized {
                    Arg::Simple(SimpleArg::$name(self))
                }
                fn from_raw_arg(arg: &RawArg) -> Option<Self> {
                    if let RawArg::Simple(SimpleRawArg::$name(val)) = arg { Some(*val) }
                    else { None }
                }
                fn kind() -> sys::SdBasicKind {
                    sys::SdBasicKind::$name
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

impl<'a> ValidArg<'a> for Arg<'a> { // variadic
    fn into_arg(self) -> Arg<'a> where Self: Sized {
        Arg::Compound(CompoundArg::Variant(self.kinds(), Box::new(self))) // TODO: arenaaaaa
    }
    fn from_raw_arg(_arg: &RawArg) -> Option<Self> {
        unimplemented!("parse variadic raw-arg -> arg");
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::Variant
    }
}

impl<'a, T: ValidArg<'a>> ValidArg<'a> for Vec<T> {
    fn into_arg(self) -> Arg<'a> where Self: Sized {
        let kind = T::kind();
        let contents = self.into_iter().map(T::into_arg).collect();
        Arg::Compound(CompoundArg::Array(kind, contents))
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Compound(CompoundRawArg::Array(items)) = arg {
            let mut out = Vec::new();
            for it in items.iter() { out.push(T::from_raw_arg(it)?); }
            Some(out)
        }
        else { None }
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::Array
    }
}

impl<'a, K: ValidArg<'a>, V: ValidArg<'a>> ValidArg<'a> for HashMap<K, V> {
    fn into_arg(self) -> Arg<'a> where Self: Sized {
        let tkey = K::kind();
        let tval = V::kind();
        let map = self.into_iter().map(|(k, v)| (as_simple_arg(k.into_arg()), v.into_arg())).collect();
        Arg::Compound(CompoundArg::Map(tkey, tval, map))
    }
    fn from_raw_arg(_arg: &RawArg) -> Option<Self> {
        // if let RawArg::Compound(CompoundRawArg::Map(map)) = arg {
        //     let out = map.into_iter().filter_map(|(k, v)| Some((K::from_raw_arg(&RawArg::Simple(k))?, V::from_raw_arg(v)?))).collect();
        //     Some(out)
        // }
        // else { None }
        todo!("construct hashmap");
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::Array
    }
}

fn as_simple_arg(arg: Arg<'_>) -> SimpleArg {
    if let Arg::Simple(val) = arg {
        val
    } else {
        unreachable!()
    }
}

#[derive(Debug, Clone)]
pub enum RawArg {
    Simple(SimpleRawArg),
    Compound(CompoundRawArg),
    Ignore,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SimpleRawArg {
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

#[derive(Debug, Clone)]
pub enum CompoundRawArg {
    Array(Vec<RawArg>),
    Struct(Vec<RawArg>),
    Map(HashMap<SimpleRawArg, RawArg>),
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
    Parse(ParseError),
}

impl RequestError {
    fn argfmt() -> Self {
        Self::Dbus(io::Error::new(
            io::ErrorKind::InvalidInput,
            "invalid dbus parameter (eg. object path not starting with `/`)"
        ))
    }
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

// ####### actual desktop interface implementation #######

pub struct NotifyProxy {
    con: Arc<Connection>,
}

impl NotifyProxy {

    pub fn new(con: &Arc<Connection>) -> Self {
        Self {
            con: Arc::clone(&con)
        }
    }

    pub async fn send(&self, notif: Notif<'_>) -> RequestResult<NotifId> {

        let mut call = DbusCall::new(
            "org.freedesktop.Notifications",
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
            "Notify",
        );

        call.arg(notif.app); // app name
        call.arg(notif.replaces.unwrap_or(0) as u32); // replaces id
        call.arg(notif.icon.map(Icon::name).unwrap_or("")); // icon
        call.arg(notif.summary); // summary
        call.arg(notif.body.unwrap_or("")); // body

        let actions: Vec<&str> = notif.actions
            .into_iter()
            .map(|it| [it.id, it.name])
            .flatten()
            .collect();

        call.arg(actions); // actions

        let mut hints: HashMap<&str, Arg> = HashMap::new();
        if let Some(urgency) = notif.urgency {
            hints.insert("urgency", Arg::Simple(SimpleArg::Byte(urgency.num())));
        }
        if let Some(category) = notif.category {
            hints.insert("category", Arg::Simple(SimpleArg::Str(category.name())));
        }

        call.arg(hints); // hints

        call.arg(notif.timeout.map(|dur|
            if dur == Duration::MAX { 0 }
            else { i32::try_from(dur.as_millis()).unwrap_or(i32::MAX) }
        ).unwrap_or(-1)); // expiration timeout
        
        let resp = self.con.send(call).await?;
        let id = resp.arg(0);

        Ok(id)
        
    }

    /// Forcefully close a notification.
    pub async fn close(&self, id: NotifId) -> RequestResult<()> {

        let mut call = DbusCall::new(
            "org.freedesktop.Notifications",
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
            "CloseNotification",
        );

        call.arg(id as u32);

        self.con.send(call).await?;

        Ok(())
        
    }

    /// Wait until an action is invoked.
    pub async fn invoked(&self, id: NotifId) -> RequestResult<InvokedAction> {

        let signal = DbusSignal::new(
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
            "ActionInvoked"
        );

        let stream = self.con.listen(signal).await?;
        pin!(stream);

        let mut key = String::new();

        loop {
            let resp = stream.next().await?; // wait for a dbus signal to arrive
            let event: u32 = resp.arg(0);
            key = resp.arg(1);
            if event == id { break };
        }

        Ok(InvokedAction { id: key })
        
    }

}

pub type NotifId = u32;

pub struct Notif<'a> {
    // required
    app: &'a str,
    summary: &'a str,
    // optional
    body: Option<&'a str>,
    replaces: Option<usize>,
    icon: Option<Icon>, // TODO: add support for custom Icons (the same as used in the wayland code)
    actions: Vec<Action<'a>>,
    timeout: Option<Duration>,
    // hints
    urgency: Option<Urgency>,
    category: Option<Category>,
}

impl<'a> Notif<'a> {
    pub fn new(app: &'a str, summary: &'a str) -> Self {
        Self {
            app,
            summary,
            body: None,
            replaces: None,
            icon: None,
            actions: Vec::new(),
            timeout: None,
            urgency: None,
            category: None,
        }
    }
    pub fn replaces(mut self, id: usize) -> Self {
        self.replaces = Some(id);
        self
    }
    pub fn icon(mut self, icon: Icon) -> Self {
        self.icon = Some(icon);
        self
    }
    pub fn body(mut self, text: &'a str) -> Self {
        self.body = Some(text);
        self
    }
    pub fn action(mut self, action: Action<'a>) -> Self {
        self.actions.push(action);
        self
    }
    pub fn timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }
    pub fn urgency(mut self, urgency: Urgency) -> Self {
        self.urgency = Some(urgency);
        self
    }
    #[cfg(target_os = "linux")]
    pub fn category(mut self, category: Category) -> Self {
        self.category = Some(category);
        self
    }
}

pub enum Icon {
    
}

impl Icon {
    fn name(self) -> &'static str {
        todo!()
    }
}

pub struct Action<'a> {
    pub id: &'a str,
    pub name: &'a str,
}

impl<'a> Action<'a> {
    fn default(name: &'a str) -> Self {
        Self { id: "default", name }
    }
    fn new(id: &'a str, name: &'a str) -> Self {
        Self { id, name }
    }
}

pub struct InvokedAction {
    pub id: String,
}

pub enum Category {
    Device,              // device
    DeviceAdded,         // device.added
    DeviceError,         // device.error
    DeviceRemoved,       // device.removed
    Email,               // email
    EmailArrived,        // email.arrived
    EmailBounced,        // email.bounced
    Im,                  // im
    ImError,             // im.error"
    ImReceived,          // im.received
    Network,             // network
    NetworkConnected,    // network.connected
    NetworkDisconnected, // network.disconnected
    NetworkError,        // network.error
    Presence,            // presence
    PresenceOffline,     // presence.offline"
    PresenceOnline,      // presence.online"
    Transfer,            // transfer
    TransferComplete,    // transfer.complete
    TransferError,       // transfer.error
}

impl Category {
    pub(self) fn name(&self) -> &'static str {
        match self {
            Self::Device              => "device",
            Self::DeviceAdded         => "device.added",
            Self::DeviceError         => "device.error",
            Self::DeviceRemoved       => "device.removed",
            Self::Email               => "email",
            Self::EmailArrived        => "email.arrived",
            Self::EmailBounced        => "email.bounced",
            Self::Im                  => "im",
            Self::ImError             => "im.error",
            Self::ImReceived          => "im.received",
            Self::Network             => "network",
            Self::NetworkConnected    => "network.connected",
            Self::NetworkDisconnected => "network.disconnected",
            Self::NetworkError        => "network.error",
            Self::Presence            => "presence",
            Self::PresenceOffline     => "presence.offline",
            Self::PresenceOnline      => "presence.online",
            Self::Transfer            => "transfer",
            Self::TransferComplete    => "transfer.complete",
            Self::TransferError       => "transfer.error",
        }
    }
}

pub enum Urgency {
    Low,
    Normal,
    Critical
}
impl Urgency {
    fn num(&self) -> u8 {
        match self {
            Self::Low => 0,
            Self::Normal => 1,
            Self::Critical => 2,
        }
    }
}

mod sys {

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
        Forward = 0,
        /// Don't call other handlers.
        Consume = 1,
    }

    pub type SdMessageHandler = extern fn(
        msg: *mut SdMessage,
        data: *mut ffi::c_void,
        out: *mut SdError
    ) -> SdHandlerStatus;
    
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
        Pair        = 'e' as u32 as i8, // dict entry phantom type
        Struct      = 'r' as u32 as i8, // struct phantom type
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

        pub fn sd_bus_message_ref(msg: *mut SdMessage) -> *mut SdMessage;
        pub fn sd_bus_message_unref(msg: *mut SdMessage) -> *mut SdMessage;

        pub fn sd_bus_message_dump(msg: *mut SdMessage, out: *mut ffi::c_void /* libc::FILE, pass NULL for stdout */, kind: SdDumpKind) -> SdResult;
        pub fn sd_bus_message_seal(msg: *mut SdMessage, cookie: u64, timeout_usec: u64) -> SdResult;

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

        pub fn sd_bus_slot_ref(slot: *mut SdSlot) -> *mut SdSlot;
        pub fn sd_bus_slot_unref(slot: *mut SdSlot) -> *mut SdSlot;
    
    }
    
}
