
use core::fmt;
use std::{collections::{hash_map::DefaultHasher, HashMap, HashSet}, convert::Infallible, ffi::{self, CStr, CString}, hash::{Hash, Hasher}, io::{self, Write}, mem, os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd, RawFd}, ptr::{self, NonNull}, sync::Arc, thread::sleep_ms, time::Duration};
use async_lock::Mutex as AsyncMutex;
use futures_lite::{pin, FutureExt};

#[test]
fn bug() {

    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber).unwrap();

    use nix::libc as libc;
    use std::fs::File;

    async_io::block_on(async {

        // create a new pipe
        let mut fds = [0, 0];
        let res = unsafe { libc::pipe(fds.as_mut_ptr()) };
        assert!(res == 0);

        let [read, write] = fds.map(|fd|
            unsafe { OwnedFd::from_raw_fd(fd) }
        );

        // write some data to the pipe
        let mut file = File::from(write);
        file.write_all(b"foo bar").expect("write file");
        file.flush().expect("flush file");

        // create and async adapter to the read end
        let adapter = async_io::Async::new(read)
            .expect("create async adapter");

        let mut is_first_time = true;
        
        for _ in 0..10 {

            let ready = async {
                if is_first_time {
                    is_first_time = false;
                    futures_lite::future::ready(1).await
                } else {
                    // futures_lite::future::ready(1).await
                    futures_lite::future::pending().await
                }
            };

            let io = async {
                adapter.readable().await.expect("await readablity");
                2
            };

            // BUG(1): the order of this `or` doesn't matter since async-io's readable
            //         never returns `Poll::Ready` on first poll
            let result = io.or(ready).await;
            dbg!(result);
        }
        
    });
    
}

#[test]
fn call() {

    async_io::block_on(async {
    
        let con = Connection::new().unwrap();

        let call = MethodCall::new(
             "org.freedesktop.DBus",
             "/org/freedesktop/DBus",
             "org.freedesktop.DBus.Debug.Stats",
             "GetStats",
        );
        
        let _resp = con.send(call).await.unwrap();
        //   ^^^^ parsing this is enough (maps, variants, etc. covered)

        // println!("{:?}", resp.args);

        // let text: &str = resp.arg(0);
        // let text: &str = resp.arg(1);
        // println!("{}", text);

        // dbg!(&resp.args);
        
    })
    
}

#[test]
fn notifications() {

    // let subscriber = tracing_subscriber::FmtSubscriber::builder()
    //     .with_max_level(tracing::Level::TRACE)
    //     .finish();

    // tracing::subscriber::set_global_default(subscriber).unwrap();

    async_io::block_on(async {

        let con = Connection::new().unwrap();
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

#[test]
fn service() {

    // let subscriber = tracing_subscriber::FmtSubscriber::builder()
    //     .with_max_level(tracing::Level::TRACE)
    //     .finish();

    // tracing::subscriber::set_global_default(subscriber).unwrap();

    async_io::block_on(async {

        let con = Connection::new().unwrap();

        // let mut service = Service::new("org.freedesktop.StatusNotifierItem-X-X");
        let mut service = Service::new("lsg.test");

        let mut iface = Iface::new();
        iface.property("foo", 10i32);
        service.add(
            "/StatusNotifierItem",
            "org.kde.StatusNotifierItem",
            iface
        );

        con.run(service).await.unwrap();

        // let pid = nix::unistd::getpid().as_raw();
        // let name = format!("org.freedesktop.StatusNotifierItem-{}-1", pid);

        // let mut call = MethodCall::new(
        //     "org.kde.StatusNotifierWatcher",
        //     "/StatusNotifierWatcher",
        //     "org.kde.StatusNotifierWatcher",
        //     "RegisterStatusNotifierItem"
        // );

        // call.arg(name);

        // con.send(call).await.unwrap();
        
    })
    
}

// #### dbus implementation ####

struct Reactor {
    locked: AsyncMutex<BusData>,
}

struct BusData {
    bus: *mut sys::SdBus,
    busfd: async_io::Async<BusFd>,
    /// messages to send
    reqs: async_channel::Receiver<ClientMessage>,
    /// received responses to send back to the tasks
    calls: HashMap<Id, async_channel::Sender<Message>>,
    /// received signals to send back to the tasks
    signals: async_broadcast::Sender<Arc<Message>>,
    /// signals we are already listening on
    listening: HashSet<Id>,
    /// registered services
    services: HashMap<String, Service>,
}

impl Drop for Reactor {
    fn drop(&mut self) {
        let guard = self.locked.try_lock().unwrap();
        unsafe { sys::sd_bus_flush(guard.bus) };
        unsafe { sys::sd_bus_close(guard.bus) };
        unsafe { sys::sd_bus_unref(guard.bus) };
    }
}

impl Reactor {

    /// Run the reactor and process events. This future never completes.
    pub async fn run(&self) -> DbusResult<Infallible> {

        let mut guard = self.locked.lock().await;                

        // this task will now run the the dispatch loop,
        // theoretically forever

        // the beauty of RAII really shows here, since if this task
        // is cancelled from outside interupption, the guard is dropped and
        // another task can pick up the event loop

        loop {

            // only wait for `writable` events when neceserry, otherwise we would infinitely
            // loop here, since the fd is always gonna be writable.

            let events = unsafe { sys::sd_bus_get_events(guard.bus) }.ok()?;

            if events & 0x4 /* POLLOUT */ as u32 == 1 {

                // the async-io `writable` method is very slow right now and
                // the fd should always be writable, so we don't actually wait here

                println!("processing events (writable)");
                process_events(&mut guard, Ok(()))?;

            } else {

                enum Either {
                    /// We should send this request.
                    Request(ClientMessage),
                    /// The bus fd is now readable.
                    Readable(io::Result<()>),
                }

                let request = async {
                    let val = guard.reqs.recv().await.unwrap();
                    Either::Request(val)
                };

                let readable = async {
                    let val = guard.busfd.readable().await;
                    Either::Readable(val)
                };

                // BUG: right now, on load, the `readable` future never resolves even though the connection
                // is readable. this seems to be a bug in async-io where a thread is incorrectly parked and never unparked or something
                // this is my conclusion because as soon as another system thread interacts with our program, the future resolves.
                // this bug also goes away if the `request` future is replaced by `pending().await`
                // the bug also happens when you replace the channel recv with future::ready, so it just always happens when the other future resolves (first?)

                match readable.or(request).await {
                    Either::Request(request) => process_request(&mut guard, request)?,
                    Either::Readable(result) => process_events(&mut guard, result)?
                }

            }

        }

    }
    
}

fn process_events(guard: &mut async_lock::MutexGuard<'_, BusData>, result: Result<(), io::Error>) -> DbusResult<()> {

    result?;

    'inner: loop {

        let mut msg = ptr::null_mut();
        let worked = unsafe { sys::sd_bus_process(guard.bus, &mut msg) }.ok()?;

        if !msg.is_null() {

            let mut kind = sys::SdMessageKind::Null;
            unsafe { sys::sd_bus_message_get_type(msg, &mut kind).ok()? };

            dbg!(&kind);

            match kind {

                sys::SdMessageKind::Null => unreachable!(),

                sys::SdMessageKind::Error => {

                    let raw = unsafe { sys::sd_bus_message_get_error(msg) };
                    assert!(!raw.is_null());

                    let name = unsafe { (&*raw).name };
                    let cstr = unsafe { CStr::from_ptr(name) };

                    let err = io::Error::other(cstr.to_string_lossy());

                    unsafe { sys::sd_bus_message_unref(msg) };
                    unsafe { sys::sd_bus_error_free(raw) };

                    return Err(DbusError::Dbus(err));

                },

                sys::SdMessageKind::Response => {

                    // unsafe { sys::sd_bus_message_dump(msg, ptr::null_mut(), sys::SdDumpKind::WithHeader).ok()? };

                    let mut cookie = 0;
                    let ret = unsafe { sys::sd_bus_message_get_reply_cookie(msg, &mut cookie) };

                    if ret.ok().is_ok() {

                        let resp = Message::consume(msg)?; // BUG: if this is an error, the message is leaked

                        let id = Id::Cookie(cookie);
                        let send = guard.calls.remove(&id).unwrap();
                        send.try_send(resp).unwrap();

                    }

                },

                sys::SdMessageKind::Signal => {

                    let signal = Message::consume(msg)?;
                    guard.signals.try_broadcast(Arc::new(signal)).unwrap(); // wait for other tasks if full

                }

                sys::SdMessageKind::MethodCall => {

                    let req = Message::consume(msg)?;

                    if let Some(service) = guard.services.get_mut(&req.dest) {

                        let result = service.handle(req);
                        let resp = match result {

                            MethodReply::Ok { args } => {

                                let mut resp = ptr::null_mut();
                                unsafe { sys::sd_bus_message_new_method_return(msg, &mut resp) }
                                    .ok().map_err(|_| DbusError::invalid_arguments())?;
                                assert!(!msg.is_null());

                                for arg in args {
                                    push_arg(msg, arg).unwrap(); // should not fail
                                }

                                resp

                            },

                            MethodReply::Err { name, msg: desc } => {

                                let cname = CString::new(name).unwrap();
                                let cdesc = CString::new(desc).unwrap(); // todo: fuck this

                                let mut err: sys::SdError = unsafe { mem::zeroed() };
                                unsafe { sys::sd_bus_error_set(&mut err, cname.as_ptr(), cdesc.as_ptr()) }.ok().unwrap();
                                // err.name = cname.as_ptr().cast_mut();
                                // err.msg = cdesc.as_ptr().cast_mut();
                                
                                let mut resp = ptr::null_mut();
                                dbg!(msg);
                                dbg!(&resp);
                                dbg!(cname, err.name);
                                unsafe { sys::sd_bus_message_new_method_error(msg, &mut resp, &err) }.ok().unwrap();

                                // TODO: is err leaked here?

                                resp

                            }

                        };

                        let mut _cookie = 0;
                        unsafe { sys::sd_bus_send(guard.bus, resp, &mut _cookie) }.ok()?;

                    }
                    
                },

            }

        };

        if worked == 0 {
            // no more events to process, return and release the lock
            // so this task also has a chance to wake up and check if it has received new data
            break 'inner;
        }

    }

    Ok(())
    
}

fn process_request(guard: &mut async_lock::MutexGuard<'_, BusData>, request: ClientMessage) -> DbusResult<()> {

    match request {

        ClientMessage::MethodCall(send, call) => {
   
            let dest   = CString::new(call.dest).map_err(io::Error::other)?; // TODO: arena
            let path   = CString::new(call.path).map_err(io::Error::other)?;
            let iface  = CString::new(call.iface).map_err(io::Error::other)?;
            let method = CString::new(call.method).map_err(io::Error::other)?;

            let mut msg = ptr::null_mut();
            unsafe { sys::sd_bus_message_new_method_call(
                guard.bus, &mut msg,
                dest.as_ptr(), path.as_ptr(), iface.as_ptr(), method.as_ptr()
            ) }.ok().map_err(|_| DbusError::invalid_arguments())?;

            assert!(!msg.is_null());
            for arg in call.args {
                push_arg(msg, arg).unwrap(); // should not fail
            }

            let mut cookie = 0;
            unsafe { sys::sd_bus_send(guard.bus, msg, &mut cookie) }.ok()?;

            let cookie = Id::Cookie(cookie);
            guard.calls.insert(cookie, send);

        },

        ClientMessage::SignalMatch(signal) => {

            let hash = hash_dbus_signal(&signal.path, &signal.iface, &signal.method);
            let id = Id::SignalHash(hash);

            if guard.listening.insert(id) {

                // the signal was previously unregistered
            
                let path   = CString::new(signal.path).map_err(io::Error::other)?; // TODO: arena
                let iface  = CString::new(signal.iface).map_err(io::Error::other)?;
                let method = CString::new(signal.method).map_err(io::Error::other)?;

                extern fn ignore(_msg: *mut sys::SdMessage, _data: *mut ffi::c_void, _err: *mut sys::SdError) -> sys::SdHandlerStatus {
                    // note: _msg is only borrowed here
                    // the message is processed in the main event loop
                    sys::SdHandlerStatus::Forward // call other handlers if registered
                }

                let mut slot = ptr::null_mut();
                unsafe {
                    sys::sd_bus_match_signal(
                        guard.bus, &mut slot,
                        ptr::null(), path.as_ptr(), iface.as_ptr(), method.as_ptr(),
                        ignore, ptr::null_mut())
                }.ok().map_err(|_| DbusError::invalid_arguments())?;

                assert!(!slot.is_null());

                // slot will be bound to the lifetime of the whole bus
                // i am too exhausted to implement anything more efficient here
                unsafe { sys::sd_bus_slot_set_floating(slot, true as ffi::c_int) };

            }
    
        },

        ClientMessage::AddService(mut service) => {

            println!("adding service {}", &service.name);

            extern fn ignore(_msg: *mut sys::SdMessage, _data: *mut ffi::c_void, _err: *mut sys::SdError) -> sys::SdResult {
                // this handler also ignores the message, since it is handeled in the main
                // event loop
                // note: _msg is only borrowed here
                sys::SdResult::OK
            }

            let raw = CString::new(&service.name[..]).unwrap();  // todo: temp str to cstr
            unsafe { sys::sd_bus_request_name(guard.bus, raw.as_ptr(), 0) };

            let mut paths = HashMap::new(); // unique paths

            for (info, iface) in &mut service.ifaces {

                // only add paths once but add the slot to all interfaces
                // with the same path

                paths.entry(&info.path)
                .and_modify(|slot| {

                    unsafe { sys::sd_bus_slot_ref(*slot) };
                    iface.slot = *slot;

                })
                .or_insert_with(|| {
                        
                    let path = CString::new(&info.path[..]).unwrap(); // todo: temp str to cstr
                    let mut slot = ptr::null_mut();
                    unsafe { sys::sd_bus_add_object(
                        guard.bus, &mut slot,
                        path.as_ptr(), ignore,
                        ptr::null_mut()
                    ) };
        
                    assert!(!slot.is_null());

                    iface.slot = slot; // don't need to ref it here
                    slot

                });

            }

            guard.services.insert(service.name.clone(), service);
            
        },

    }

    Ok(())
    
}

// TODO: implement Send + Sync for some types.
//     NOT IMPLEMENT FOR
// -> DbusResponse, since cloning it calls sd_bus_message_ref wich is not threadsafe
//    if we parse the response and copy the data instantly into an arena, this could be threadsafe tho

#[derive(Clone)]
pub struct Connection {
    reactor: Arc<Reactor>,
    /// handle to send requests
    reqs: async_channel::Sender<ClientMessage>,
    signals: async_broadcast::Receiver<Arc<Message>>,
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

impl Connection {

    pub fn new() -> io::Result<Self> {

        let mut bus = ptr::null_mut();
        unsafe { sys::sd_bus_open_user(&mut bus) }.ok()?;
        
        let raw = unsafe { sys::sd_bus_get_fd(bus) }.ok()?;
        let fd = BusFd { inner: raw as RawFd };

        assert!(!bus.is_null());

        let reqs = async_channel::bounded(8);
        let mut signals = async_broadcast::broadcast(16);
        signals.0.set_overflow(true); // drop old signals if not consumed

        let reactor = Reactor {
            locked: AsyncMutex::new(BusData {
                bus,
                busfd: async_io::Async::new(fd)?,
                reqs: reqs.1,
                calls: HashMap::new(),
                signals: signals.0,
                listening: HashSet::new(),
                services: HashMap::new(),
            }),
        };

        Ok(Self {
            reactor: Arc::new(reactor),
            reqs: reqs.0,
            signals: signals.1,
        })
        
    }
    
    pub async fn send(&self, call: MethodCall) -> DbusResult<Message> {
        
        let (send, recv) = async_channel::bounded(1);
        let req = ClientMessage::MethodCall(send, call);
        self.reqs.try_send(req).unwrap();

        enum Either {
            Resp(Message),
            Reactor(DbusResult<Infallible>),
        }
            
        let next = async { Either::Resp(recv.recv().await.unwrap()) };
        let reactor = async { Either::Reactor(self.reactor.run().await) };

        match next.or(reactor).await {
            Either::Resp(resp) => return Ok(resp),
            Either::Reactor(result) => result?, // will be an error
        };

        unreachable!();

    }

    pub async fn listen(&self, signal: SignalMatch) -> DbusResult<Listener> {

        let hash = hash_dbus_signal(&signal.path, &signal.iface, &signal.method);
        let id = Id::SignalHash(hash);

        let req = ClientMessage::SignalMatch(signal);
        self.reqs.try_send(req).unwrap();

        Ok(Listener {
            id,
            con: self.clone(),
            recv: self.signals.clone(),
        })

    }

    /// Run a service.
    /// Will never resolve.
    pub async fn run(&self, service: Service) -> DbusResult<()> {

        let req = ClientMessage::AddService(service);
        self.reqs.try_send(req).unwrap();

        self.reactor.run().await?;

        Ok(())
        
    }

}

#[derive(Clone)]
pub struct Listener {
    id: Id,
    con: Connection,
    recv: async_broadcast::Receiver<Arc<Message>>,
}

impl Listener {

    pub async fn next(&mut self) -> DbusResult<Arc<Message>> {

        enum Either {
            Resp(Arc<Message>),
            Reactor(DbusResult<Infallible>),
        }

        loop {

            let next = async { Either::Resp(self.recv.recv_direct().await.unwrap()) };
            let reactor = async { Either::Reactor(self.con.reactor.run().await) };

            match next.or(reactor).await {

                Either::Resp(resp) => {

                    let hash = hash_dbus_signal(&resp.path, &resp.iface, &resp.method);

                    if self.id == Id::SignalHash(hash) {
                        return Ok(resp)
                    }

                },

                Either::Reactor(result) => {
                    result?; // will return an error
                },

            };
            
        }

    }

}

fn push_arg(msg: *mut sys::SdMessage, arg: Arg) -> ParseResult<()> {

    match arg {

        // basic kind

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

                SimpleArg::String(val) => {
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

        // container kinds

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

fn hash_dbus_signal<S: AsRef<[u8]>>(path: S, iface: S, method: S) -> u64 {
    let mut hasher = DefaultHasher::new();
    path.as_ref().hash(&mut hasher);
    iface.as_ref().hash(&mut hasher);
    method.as_ref().hash(&mut hasher);
    hasher.finish()
}

pub struct Service {
    name: String,
    ifaces: HashMap<IfaceInfo, Iface>,
}

impl Service {

    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            ifaces: HashMap::new(),
        }
    }

    pub fn add(&mut self, path: &str, iface: &str, value: Iface) {
        self.ifaces.insert(
            IfaceInfo { path: path.to_string(), name: iface.to_string()  },
            value
        );
    }

    fn handle(&mut self, req: Message) -> MethodReply {

        dbg!(&req);

        let info = IfaceInfo {
            path: req.path.clone(), // TODO: not clone every time here
            name: req.iface.clone(),
        };

        if req.method == "Introspect" {
            
        }
        
        else if let Some(iface) = self.ifaces.get_mut(&info) {
            if let Some(exposed) = iface.exposed.get_mut(&req.method) {

                
            }
        };

        MethodReply::err("lsg.unimplemented", "not implemented")
        
    }
    
}

enum Exposed {
    Property(Arg),
    Method(Box<dyn Fn(Message) -> MethodReply>), // todo: arenas of dyn
}

#[derive(PartialEq, Eq, Hash)]
pub struct IfaceInfo {
    path: String,
    name: String,
}

pub struct Iface {
    exposed: HashMap<String, Exposed>,
    slot: *mut sys::SdSlot,
}

impl Iface {

    pub fn new() -> Self {
        Self {
            exposed: HashMap::new(),
            slot: ptr::null_mut(),
        }
    }

    pub fn property<T: ValidArg>(&mut self, name: &str, val: T) {
        self.exposed.insert(
            name.to_string(),
            Exposed::Property(val.into_arg())
        );
    }

    pub fn method<F: Fn(Message) -> MethodReply + 'static>(&mut self, name: &str, cb: F) {
        self.exposed.insert(
            name.to_string(),
            Exposed::Method(Box::new(cb))
        );
    }
    
}

pub struct MethodCall {
    dest: String, // todo: arena :(
    path: String,
    iface: String,
    method: String,
    args: Vec<Arg>,
}

impl MethodCall {

    pub fn new(dest: &str, path: &str, iface: &str, method: &str) -> Self {

        Self {
            dest: dest.to_string(), // Todo: arena
            path: path.to_string(),
            iface: iface.to_string(),
            method: method.to_string(),
            args: Vec::new(),
        }

    }
    
    pub fn arg<A: ValidArg>(&mut self, input: A) {
        self.args.push(input.into_arg());
    }
    
}

pub struct SignalMatch {
    path: String,
    iface: String,
    method: String,
}

impl SignalMatch {

    pub fn new(path: &str, iface: &str, method: &str) -> Self {
        Self {
            path: path.to_string(), // TODO: arena
            iface: iface.to_string(),
            method: method.to_string(),
        }
    }

}

pub enum MethodReply {
    Ok { args: Vec<Arg> },
    Err { name: String, msg: String },
}

impl MethodReply {

    pub fn ok() -> Self {
        Self::Ok {
            args: Vec::new()
        }
    }
    
    pub fn err(name: &str, msg: &str) -> Self {
        Self::Err {
            name: name.to_string(),
            msg: msg.to_string(),
        }
    }
    
    pub fn arg<A: ValidArg>(&mut self, input: A) {
        if let Self::Ok { args } = self {
            args.push(input.into_arg());
        } else {
            panic!("cannot push args onto an error reply");
        }
    }
    
}

pub struct SignalTrigger {
    path: String, // todo: arena
    iface: String,
    method: String,
    args: Vec<Arg>,
}

impl SignalTrigger {

    pub fn new(path: &str, iface: &str, method: &str) -> Self {

        Self {
            path: path.to_string(),
            iface: iface.to_string(),
            method: method.to_string(),
            args: Vec::new(),
        }

    }
    
    pub fn arg<A: ValidArg>(&mut self, input: A) {
        self.args.push(input.into_arg());
    }
    
}

enum ClientMessage {
    MethodCall(async_channel::Sender<Message>, MethodCall),
    SignalMatch(SignalMatch),
    AddService(Service),
}

enum ServerMessage {
    MethodReply(MethodReply),
    SignalTrigger(SignalTrigger),
}

pub struct Message {
    inner: *mut sys::SdMessage,
    sender: String,
    dest: String,
    path: String,
    iface: String,
    method: String,
    pub args: Vec<RawArg>,
}

impl Drop for Message {
    fn drop(&mut self) {
        unsafe { sys::sd_bus_message_unref(self.inner) };
    }
}

impl Message {

    pub(crate) fn consume(msg: *mut sys::SdMessage) -> ParseResult<Self> {

        assert!(!msg.is_null());

        let sender = NonNull::new(unsafe { sys::sd_bus_message_get_sender(msg)      } as *mut _).map(|it| unsafe { CStr::from_ptr(it.as_ptr()) }.to_str().unwrap().to_string()).unwrap_or_default();
        let dest   = NonNull::new(unsafe { sys::sd_bus_message_get_destination(msg) } as *mut _).map(|it| unsafe { CStr::from_ptr(it.as_ptr()) }.to_str().unwrap().to_string()).unwrap_or_default();
        let path   = NonNull::new(unsafe { sys::sd_bus_message_get_path(msg)        } as *mut _).map(|it| unsafe { CStr::from_ptr(it.as_ptr()) }.to_str().unwrap().to_string()).unwrap_or_default();
        let iface  = NonNull::new(unsafe { sys::sd_bus_message_get_interface(msg)   } as *mut _).map(|it| unsafe { CStr::from_ptr(it.as_ptr()) }.to_str().unwrap().to_string()).unwrap_or_default();
        let method = NonNull::new(unsafe { sys::sd_bus_message_get_member(msg)      } as *mut _).map(|it| unsafe { CStr::from_ptr(it.as_ptr()) }.to_str().unwrap().to_string()).unwrap_or_default();

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
            sender,
            dest,
            path,
            iface,
            method,
            args
        })

    }

    #[track_caller]
    pub fn arg<'a, T: ValidArg>(&'a self, idx: usize) -> T {
        match self.get(idx) {
            Ok(val) => val,
            Err(err) => panic!("cannot read arg #{idx}: {err:?}")
        }
    }

    pub fn get<'a, T: ValidArg>(&'a self, idx: usize) -> Result<T, ArgError> {
        let arg = self.args.get(idx).ok_or(ArgError::DoesntExist)?;
        T::from_raw_arg(arg).ok_or(ArgError::InvalidType)
    }
    
}

impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DbusResponse {{")?;
        writeln!(f, "    sender: {:?}", self.sender)?;
        writeln!(f, "    path: {:?}",   self.path)?;
        writeln!(f, "    iface: {:?}",  self.iface)?;
        writeln!(f, "    method: {:?}", self.method)?;
        writeln!(f, "    args: {:#?}", self.args)?;
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
            Ok(Some(RawArg::Simple(SimpleRawArg::String(cstr))))
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
pub enum Arg {
    Simple(SimpleArg),
    Compound(CompoundArg),
}
impl Arg {

    fn kinds(&self) -> Vec<sys::SdBasicKind> {
        let mut out = Vec::new();
        push_kinds(&mut out, self);
        out.push(sys::SdBasicKind::Null);
        out
    }

}

fn push_kinds(out: &mut Vec<sys::SdBasicKind>, arg: &Arg) {
    match arg {
        Arg::Simple(simple) => {
            let kind = match simple {
                SimpleArg::Byte(..)   => sys::SdBasicKind::Byte,
                SimpleArg::Bool(..)   => sys::SdBasicKind::Bool,
                SimpleArg::I16(..)    => sys::SdBasicKind::I16,
                SimpleArg::U16(..)    => sys::SdBasicKind::U16,
                SimpleArg::I32(..)    => sys::SdBasicKind::I32,
                SimpleArg::U32(..)    => sys::SdBasicKind::U32,
                SimpleArg::I64(..)    => sys::SdBasicKind::I64,
                SimpleArg::U64(..)    => sys::SdBasicKind::U64,
                SimpleArg::Double(..) => sys::SdBasicKind::Double,
                SimpleArg::String(..) => sys::SdBasicKind::String,
                SimpleArg::Fd(..)     => sys::SdBasicKind::UnixFd,
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
pub enum SimpleArg {
    Byte(u8),
    Bool(bool),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Double(f64),
    String(String),
    Fd(OwnedFd),
}

impl PartialEq for SimpleArg { // can't derive because the world is a cruel place
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
            (Self::String(val), Self::String(other)) => val == other,
            (Self::Fd(val), Self::Fd(other)) => val.as_raw_fd() == other.as_raw_fd(),
            (..) => false,
        }
    }
}

impl Eq for SimpleArg {} // TODO: is this okay? make a CustomEq type and wrap the not Eq types instead of this mess

impl Hash for SimpleArg { // can't derive because the world is a cruel place
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
            Self::String(val) => Hash::hash(val, hasher),
            Self::Fd(val) => Hash::hash(&val.as_raw_fd(), hasher),
        }
    }
}

#[derive(Debug)]
pub enum CompoundArg {
    Array(sys::SdBasicKind, Vec<Arg>),
    Map(sys::SdBasicKind, sys::SdBasicKind, HashMap<SimpleArg, Arg>),
    Variant(Vec<sys::SdBasicKind>, Box<Arg>),
}

pub trait ValidArg {
    fn into_arg(self) -> Arg where Self: Sized;
    fn from_raw_arg(arg: &RawArg) -> Option<Self> where Self: Sized;
    fn kind() -> sys::SdBasicKind;
}

impl<'a> ValidArg for &'a str {
    fn into_arg(self) -> Arg {
        Arg::Simple(SimpleArg::String(self.to_string()))
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleRawArg::String(val)) = arg { Some(unsafe { &**val }.to_str().unwrap()) }
        else { None }
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::String
    }
}

impl<'a> ValidArg for String {
    fn into_arg(self) -> Arg {
        Arg::Simple(SimpleArg::String(self))
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleRawArg::String(val)) = arg { Some(unsafe { &**val }.to_str().ok()?.to_string()) }
        else { None }
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::String
    }
}

impl<'a> ValidArg for f64 {
    fn into_arg(self) -> Arg {
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

impl<'a> ValidArg for OwnedFd {
    fn into_arg(self) -> Arg {
        Arg::Simple(SimpleArg::Fd(self))
    }
    fn from_raw_arg(arg: &RawArg) -> Option<Self> {
        if let RawArg::Simple(SimpleRawArg::UnixFd(val)) = arg { Some(unsafe { OwnedFd::from_raw_fd(*val) }) }
        else { None }
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::UnixFd
    }
}

macro_rules! impl_valid_arg {
    ($(($name: ident: $t: ident)),*,) => {
        $(
            impl<'a> ValidArg for $t {
                fn into_arg(self) -> Arg {
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

impl<'a> ValidArg for Arg { // variadic
    fn into_arg(self) -> Arg {
        Arg::Compound(CompoundArg::Variant(self.kinds(), Box::new(self))) // TODO: arenaaaaa
    }
    fn from_raw_arg(_arg: &RawArg) -> Option<Self> {
        unimplemented!("parse variadic raw-arg -> arg");
    }
    fn kind() -> sys::SdBasicKind {
        sys::SdBasicKind::Variant
    }
}

impl<'a, T: ValidArg> ValidArg for Vec<T> {
    fn into_arg(self) -> Arg {
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

impl<'a, K: ValidArg, V: ValidArg> ValidArg for HashMap<K, V> {
    fn into_arg(self) -> Arg {
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

fn as_simple_arg(arg: Arg) -> SimpleArg {
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
    String(*const CStr),
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
pub enum DbusError {
    Dbus(io::Error),
    Parse(ParseError),
}

impl DbusError {
    fn invalid_arguments() -> Self {
        Self::Dbus(io::Error::new(
            io::ErrorKind::InvalidInput,
            "invalid dbus parameter (eg. object path not starting with `/`)"
        ))
    }
}

pub type DbusResult<T> = Result<T, DbusError>;

impl From<io::Error> for DbusError {
    fn from(value: io::Error) -> Self {
        Self::Dbus(value)
    }
}

impl From<ParseError> for DbusError {
    fn from(value: ParseError) -> Self {
        Self::Parse(value)
    }
}

// ####### actual desktop interface implementation #######

pub struct NotifyProxy {
    con: Connection,
}

impl NotifyProxy {

    pub fn new(con: &Connection) -> Self {
        Self { con: con.clone() }
    }

    pub async fn send(&self, notif: Notif<'_>) -> DbusResult<NotifId> {

        let mut call = MethodCall::new(
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
            hints.insert("category", Arg::Simple(SimpleArg::String(category.name().to_string())));
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
    pub async fn close(&self, id: NotifId) -> DbusResult<()> {

        let mut call = MethodCall::new(
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
    pub async fn invoked(&self, id: NotifId) -> DbusResult<InvokedAction> {

        let signal = SignalMatch::new(
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
            "ActionInvoked"
        );

        let stream = self.con.listen(signal).await?;
        pin!(stream);

        let mut key;

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
        /// can be null
        pub name: *mut ffi::c_char,
        /// can be null
        pub msg: *mut ffi::c_char,
        /// more unspecified fields, uuhh this is unsafe
        _need_free: i32, // whatever, it's internal
    }

    #[derive(Clone, Copy)]
    #[repr(transparent)]
    pub struct SdResult {
        inner: ffi::c_int
    }

    impl SdResult {

        pub const OK: Self = Self::new(0);
        pub const ERR: Self = Self::new(-1);

        pub const fn new(code: i32) -> Self {
            Self { inner: code }
        }

        pub fn io(err: io::Error) -> Self {
            Self { inner: - err.raw_os_error().unwrap_or(1) }
        }

        pub fn ok(&self) -> Result<u32, io::Error> {

            if self.inner < 0 {
                Err(io::Error::from_raw_os_error(-self.inner))
            } else {
                Ok(self.inner as u32)
            }

        }

    }

    #[derive(Debug, Clone, Copy)]
    #[repr(u64)]
    pub enum SdDumpKind { // this should've been a bool honestly
        WithHeader  = 1 << 0,
        SubtreeOnly = 1 << 1,
    }

    #[derive(Debug, Clone, Copy)]
    #[repr(u8)]
    pub enum SdMessageKind { // i do not fear UB
        Null = 0,
        MethodCall = 1,
        Response = 2,
        Error = 3,
        Signal = 4,
    }

    #[derive(Debug, Clone, Copy)]
    #[repr(i32)]
    pub enum SdHandlerStatus { // i do not fear UB
        /// Use the `out` argument to return a more precise error.
        Error = -1,
        /// Also call other handlers.
        Forward = 0,
        /// Don't call other handlers.
        Consume = 1,
    }

    pub type SdMessageFilter = extern fn(
        msg: *mut SdMessage,
        data: *mut ffi::c_void,
        out: *mut SdError
    ) -> SdHandlerStatus;
    
    pub type SdMessageHandler = extern fn(
        msg: *mut SdMessage,
        data: *mut ffi::c_void,
        out: *mut SdError
    ) -> SdResult;
    
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
        pub fn sd_bus_get_events(bus: *mut SdBus) -> SdResult;
        pub fn sd_bus_process(bus: *mut SdBus, out: &mut *mut SdMessage) -> SdResult;
        pub fn sd_bus_wait(bus: *mut SdBus, timeout: u64) -> SdResult;

        // services

        pub fn sd_bus_request_name(bus: *mut SdBus, name: *const ffi::c_char, flags: u64) -> SdResult;
        pub fn sd_bus_release_name(bus: *mut SdBus, name: *const ffi::c_char) -> SdResult;
        
        pub fn sd_bus_add_object(
            bus: *mut SdBus,
            out: &mut *mut SdSlot,
            // path
            path: *const ffi::c_char,
            // cb
            callback: SdMessageHandler,
            data: *mut ffi::c_void,
        );

        // signals

        pub fn sd_bus_match_signal(
            // obj
            bus: *mut SdBus, out: &mut *mut SdSlot,
            // dest
            sender: *const ffi::c_char, path: *const ffi::c_char,
            interface: *const ffi::c_char, member: *const ffi::c_char,
            // cb
            callback: SdMessageFilter,
            data: *mut ffi::c_void
        ) -> SdResult;

        // message

        pub fn sd_bus_send(bus: *mut SdBus, msg: *mut SdMessage, cookie: &mut u64) -> SdResult;

        pub fn sd_bus_message_new_method_call(
            // obj
            bus: *mut SdBus, out: &mut *mut SdMessage,
            // dest
            dest: *const ffi::c_char, path: *const ffi::c_char,
            iface: *const ffi::c_char, method: *const ffi::c_char
        ) -> SdResult;

        pub fn sd_bus_message_new_method_return(
            call: *mut SdMessage, out: &mut *mut SdMessage,
        ) -> SdResult;

        pub fn sd_bus_message_new_method_error(
            call: *mut SdMessage, out: &mut *mut SdMessage, err: *const SdError,
        ) -> SdResult;

        pub fn sd_bus_message_ref(msg: *mut SdMessage) -> *mut SdMessage;
        pub fn sd_bus_message_unref(msg: *mut SdMessage) -> *mut SdMessage;

        pub fn sd_bus_message_dump(msg: *mut SdMessage, out: *mut ffi::c_void /* libc::FILE, pass NULL for stdout */, kind: SdDumpKind) -> SdResult;
        pub fn sd_bus_message_seal(msg: *mut SdMessage, cookie: u64, timeout_usec: u64) -> SdResult;

        pub fn sd_bus_message_get_type(msg: *mut SdMessage, out: &mut SdMessageKind) -> SdResult;
        pub fn sd_bus_message_get_reply_cookie(msg: *mut SdMessage, out: &mut u64) -> SdResult;
        pub fn sd_bus_message_get_error(msg: *mut SdMessage) -> *mut SdError;

        pub fn sd_bus_message_get_sender(msg: *mut SdMessage) -> *const ffi::c_char;
        pub fn sd_bus_message_get_destination(msg: *mut SdMessage) -> *const ffi::c_char;
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

        pub fn sd_bus_error_set(err: &mut SdError, name: *const ffi::c_char, msg: *const ffi::c_char) -> SdResult;
        pub fn sd_bus_error_is_set(err: &mut SdError) -> SdResult;
        pub fn sd_bus_error_free(err: *mut SdError);

        // slot

        pub fn sd_bus_slot_ref(slot: *mut SdSlot) -> *mut SdSlot;
        pub fn sd_bus_slot_unref(slot: *mut SdSlot) -> *mut SdSlot;

        pub fn sd_bus_slot_set_floating(slot: *mut SdSlot, val: ffi::c_int) -> *mut SdSlot;
    
    }
    
}
