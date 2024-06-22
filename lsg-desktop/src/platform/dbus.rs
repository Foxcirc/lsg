
use std::{
    sync::Arc,
    time::Duration,
    env, iter, mem,
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    convert::{identity, Infallible},
    io::{self, Read, Write},
    os::{fd::{AsRawFd, OwnedFd}, unix::net::UnixStream},
};

use async_lock::Mutex as AsyncMutex;
use futures_lite::FutureExt;

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
        
        let _resp = con.method_call(call).await.unwrap();
          // ^^^^ parsing this is enough (maps, variants, etc. covered)

    })
    
}

#[test]
fn notifications() {

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
        // let action = proxy.invoked(id).await.unwrap();

        // println!("invoked action: {}", action.id);

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

        let mut listener = con.run_service("lsg.test").await.unwrap();

        loop {

            let msg = listener.next_call().await.unwrap();


            println!("{:?}", msg);
            let reply = MethodError::unimplemented(&msg);
            con.reply_with(Err(reply)).await.unwrap();

        }

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
    bus: async_io::Async<UnixStream>,
    /// current serial
    serial: u32,
    /// messages to send
    reqs: async_channel::Receiver<ClientMessage>,
    /// received responses to send back to the tasks
    resps: HashMap<Id, async_channel::Sender<Result<MethodReply, MethodError>>>,
    /// received signals to send back to the tasks
    signals: async_broadcast::Sender<Arc<SignalTrigger>>,
    /// registered services
    services: HashMap<String, async_channel::Sender<MethodCall>>,
    /// current message buffer
    buf: Vec<u8>,
}

impl Reactor {

    /// Run the reactor and process events. This future never completes.
    pub async fn run_forever(&self) -> RequestResult<Infallible> {

        let mut guard = self.locked.lock().await;                

        // this task will now run the the dispatch loop,
        // theoretically forever

        // the beauty of RAII really shows here, since if this task
        // is cancelled from outside interupption, the guard is dropped and
        // another task can pick up the event loop

        loop {

            // only wait for `writable` events when neceserry, otherwise we would infinitely
            // loop here, since the fd is always gonna be writable.

            enum Either {
                /// We should execute this request
                Request(ClientMessage),
                /// We have read more data from the socket
                MoreData,
            }

            let reqs = guard.reqs.clone(); // we have to clone because we can't borrow guard in both `request` and `readable`

            let request = async {
                let val = reqs.recv().await.unwrap();
                io::Result::Ok(Either::Request(val))
            };

            let readable = async {

                let mut buf = [0; 1024]; // TODO: test mechanism by decreasing the buffer size to smth like [0; 24]

                let bytes = unsafe { guard.bus.read_with_mut(|it| it.read(&mut buf) ) }.await?;
                if bytes == 0 { return Err(io::Error::other("kicked by broker")) };
                guard.buf.extend_from_slice(&buf[..bytes]);

                Ok(Either::MoreData)

            };

            match readable.or(request).await? {
                Either::Request(request) => process_request(&mut guard, request).await?,
                Either::MoreData         => process_incoming(&mut guard)?,
            }

        }

    }

    pub async fn process_all_requests(&self) -> RequestResult<()> {

        let mut guard = self.locked.lock().await;                
        while let Ok(req) = guard.reqs.try_recv() {
            process_request(&mut guard, req).await?;
        }

        Ok(())

    }
    
}

fn process_incoming(guard: &mut async_lock::MutexGuard<'_, BusData>) -> RequestResult<()> {

    loop {

        let result = GenericMessage::deserialize(&guard.buf);

        match result {

            Ok((offset, mut msg)) => {

                // remove the data that was parsed
                guard.buf.drain(..offset);

                match msg.kind {

                    MessageKind::Invalid => todo!("got an invalid message"),

                    MessageKind::Error => {

                        let serial: u32 = msg.take_field(FieldCode::ReplySerial).expect("todo");
                        let id = Id::Serial(serial);
                        let opt = guard.resps.remove(&id);

                        if let Some(sender) = opt {
                            let err = MethodError::deserialize(msg).expect("todo");
                            sender.try_send(Err(err)).unwrap();
                        }

                    },

                    MessageKind::MethodReply => {

                        let serial: u32 = msg.take_field(FieldCode::ReplySerial).expect("todo");
                        let id = Id::Serial(serial);
                        let opt = guard.resps.remove(&id);

                        if let Some(sender) = opt {
                            let reply = MethodReply::deserialize(msg);
                            sender.try_send(Ok(reply)).unwrap();
                        }

                    },

                    MessageKind::Signal => {

                        let signal = SignalTrigger::deserialize(msg).expect("todo: parse error 3");
                        guard.signals.try_broadcast(Arc::new(signal)).unwrap(); // replaces last message if full

                    }

                    MessageKind::MethodCall => {

                        // let serial = msg.serial;
                        // let sender: String = msg.take_field(FieldCode::Sender).expect("todo");

                        let call = MethodCall::deserialize(msg).expect("todo: parse error 2");

                        if let Some(sender) = guard.services.get_mut(&call.dest) {
                            sender.try_send(call).expect("todo: cannot send, full");
                        }
        
                            // let result = service.handle(call);
                            // let mut msg = serialize_method_call_result(result);

                            // msg.serial = u32::MAX; // don't care about the serial

                            // msg.fields.push(header_field(FieldCode::ReplySerial, SimpleArg::U32(serial)   ));
                            // msg.fields.push(header_field(FieldCode::Dest ,       SimpleArg::String(sender)));

                            // todo!("send reply");

                    },

                }

                continue

            }

            Err(ParseError::Partial) => break Ok(()), // do nothing and wait for more data
            Err(other) => todo!("parse error: {other:?}"),

        }

    }
    
}

async fn process_request(guard: &mut async_lock::MutexGuard<'_, BusData>, request: ClientMessage) -> RequestResult<()> {

    match request {

        ClientMessage::Authenticate => {

            // this will block the reactor until we've authenticated

            // sasl authentication

            let stream = unsafe { guard.bus.get_mut() };

            write!(stream, "\0")?;
            write!(stream, "AUTH EXTERNAL ")?;

            // write the current uid in a hex ascii representation

            let uid = nix::unistd::Uid::current();

            let num = uid.as_raw();
            let mut divisor = 1;

            while num / divisor >= 10 {
                divisor *= 10;
            }

            while divisor > 0 {
                let digit = (num / divisor) % 10;
                write!(stream, "{:02x}", digit + b'0' as u32)?;
                divisor /= 10;
            }

            write!(stream, "\r\n")?;
            stream.flush()?;

            let mut buf = [0; 128];
            unsafe { guard.bus.read_with_mut(|it| it.read(&mut buf)) }.await?;
            if &buf[..2] != b"OK" { return Err(RequestError::Io(io::Error::other("sasl authentication failed"))) }

            // begin the session, no more sasl messages will be send after this
            
            let stream = unsafe { guard.bus.get_mut() };
            write!(stream, "BEGIN\r\n")?;
            stream.flush()?;

            // send the `Hello` message

            let hello = MethodCall::hello();

            let mut msg = hello.serialize();
            msg.serial = u32::MAX; // any valid serial works here
            let data = msg.serialize();

            stream.write_all(&data)?;
            stream.flush()?;

            // the reply will just be discarded later

        },

        ClientMessage::MethodCall(send, call) => {

            let serial = guard.serial;
            guard.serial += 1;
            guard.resps.insert(Id::Serial(serial), send);

            let mut msg = call.serialize();
            msg.serial = serial;
            let data = msg.serialize();

            let stream = unsafe { guard.bus.get_mut() };
            stream.write_all(&data)?;

        },

        ClientMessage::MethodCallVoid(call) => {

            let mut msg = call.serialize();
            msg.serial = u32::MAX;
            let data = msg.serialize();

            let stream = unsafe { guard.bus.get_mut() };
            stream.write_all(&data)?;

        },

        ClientMessage::MethodReply(val) => {

            let mut msg = val.serialize();
            msg.serial = u32::MAX;
            let data = msg.serialize();

            let stream = unsafe { guard.bus.get_mut() };
            stream.write_all(&data)?;

        },

        ClientMessage::MethodError(val) => {

            let mut msg = val.serialize();
            msg.serial = u32::MAX;
            let data = msg.serialize();

            let stream = unsafe { guard.bus.get_mut() };
            stream.write_all(&data)?;

        },

        ClientMessage::AddService(send, name) => {

            // the name was already previously registered
            // we just add the service to our list of active services here

            guard.services.insert(name, send);
            
        },

    }

    Ok(())
    
}

#[derive(Clone)]
pub struct Connection {
    reactor: Arc<Reactor>,
    /// handle to send requests
    reqs: async_channel::Sender<ClientMessage>,
    signals: async_broadcast::Receiver<Arc<SignalTrigger>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Id {
    Serial(u32),
    SignalHash(u64)
}

impl Connection {

    pub fn new() -> io::Result<Self> {

        let addr = env::var("DBUS_SESSION_BUS_ADDRESS")
            .map_err(io::Error::other)?;

        let path = addr.strip_prefix("unix:path=")
            .ok_or(io::Error::other("invalid bus address format"))?;

        let stream = UnixStream::connect(path)?;

        let reqs = async_channel::bounded(4);
        let mut signals = async_broadcast::broadcast(4);
        signals.0.set_overflow(true); // drop old signals if not consumed

        // authenticate and say hello, this is executed
        // when the reactor first runs
        reqs.0.try_send(ClientMessage::Authenticate).unwrap();

        let reactor = Reactor {
            locked: AsyncMutex::new(BusData {
                bus: async_io::Async::new(stream)?,
                reqs: reqs.1,
                resps: HashMap::new(),
                signals: signals.0,
                services: HashMap::new(),
                serial: 1, // 0 is not allowed, so start at 1
                buf: Vec::new(),
            }),
        };

        Ok(Self {
            reactor: Arc::new(reactor),
            reqs: reqs.0,
            signals: signals.1,
        })
        
    }
    
    pub async fn method_call(&self, call: MethodCall) -> RequestResult<MethodReply> {
        
        let (send, recv) = async_channel::bounded(1);
        let req = ClientMessage::MethodCall(send, call);
        self.reqs.try_send(req).unwrap();

        let next = async { recv.recv().await.unwrap().map_err(RequestError::Bus) };
        let reactor = async { Err(self.reactor.run_forever().await.unwrap_err()) };

        next.or(reactor).await

    }

    pub async fn method_call_void(&self, call: MethodCall) -> RequestResult<()> {
        
        let req = ClientMessage::MethodCallVoid(call);
        self.reqs.try_send(req).unwrap();

        self.reactor.process_all_requests().await

    }

    pub async fn reply_with(&self, result: Result<MethodReply, MethodError>) -> RequestResult<()> {
        
        match result {
            Ok(val) => {
                let req = ClientMessage::MethodReply(val);
                self.reqs.try_send(req).unwrap();
            },
            Err(val) => {
                let req = ClientMessage::MethodError(val);
                self.reqs.try_send(req).unwrap();
            }
        }

        self.reactor.process_all_requests().await

    }

    pub async fn listen_on(&self, matching: SignalMatch) -> RequestResult<SignalListener> {

        let call = MethodCall::add_match(matching.rule.clone());
        let req = ClientMessage::MethodCallVoid(call);
        self.reqs.try_send(req).unwrap();

        Ok(SignalListener {
            matching,
            con: self.clone(),
            recv: self.signals.clone(),
            reqs: self.reqs.clone(), // to remove the match on drop
            ref_test: Arc::new(()),
        })

    }

    /// Run a service.
    /// Will never resolve.
    pub async fn run_service(&self, name: &str) -> RequestResult<ServiceListener> {

        let call = MethodCall::request_name(
            name.to_string(),
            0x4 /* don't queue */
        );

        let mut resp = self.method_call(call).await?;
        let val: u32 = resp.arg(0);
        if val >= 3 { return Err(RequestError::AlreadyOwned) }

        let (send, recv) = async_channel::bounded(4);
        let req = ClientMessage::AddService(send, name.to_string());
        self.reqs.try_send(req).unwrap();

        Ok(ServiceListener {
            name: name.to_string(),
            con: self.clone(),
            recv,
            reqs: self.reqs.clone(), // to remove the match on drop
            ref_test: Arc::new(()),
        })
        
    }

}

/// # Cloning
/// When a signal arrives it will be broadcast to all currently alive `SignalListeners` that listen
/// for it. This is why the the `SignalTrigger` is returned wrapped in an Arc.
#[derive(Clone)]
pub struct SignalListener {
    matching: SignalMatch,
    con: Connection,
    recv: async_broadcast::Receiver<Arc<SignalTrigger>>,
    reqs: async_channel::Sender<ClientMessage>,
    /// used to test if this is the last listener of this kind on drop
    ref_test: Arc<()>,
}

impl Drop for SignalListener {
    fn drop(&mut self) {
        let arc = mem::take(&mut self.ref_test);
        if Arc::into_inner(arc).is_some() {
            // remove the match rule
            let call = MethodCall::remove_match(self.matching.rule.clone());
            let req = ClientMessage::MethodCallVoid(call);
            self.reqs.try_send(req).unwrap();
        }
    }
}

impl SignalListener {

    pub async fn next_signal(&mut self) -> RequestResult<Arc<SignalTrigger>> {

        loop {

            let next = async { Ok(self.recv.recv_direct().await.unwrap()) };
            let reactor = async { Err(self.con.reactor.run_forever().await.unwrap_err()) };

            let resp = next.or(reactor).await?;
            if self.matching.hash == resp.hash {
                return Ok(resp)
            }

        }

    }

}

/// # Cloning
/// A message will only be sent to one `ServiceListener`. Cloning will lead to work-strealing behaviour.
#[derive(Clone)]
pub struct ServiceListener {
    name: String,
    con: Connection,
    recv: async_channel::Receiver<MethodCall>,
    reqs: async_channel::Sender<ClientMessage>,
    /// used to test if this is the last listener of this kind on drop
    ref_test: Arc<()>,
}

impl Drop for ServiceListener {
    fn drop(&mut self) {
        let arc = mem::take(&mut self.ref_test);
        if Arc::into_inner(arc).is_some() {
            // remove the match rule
            let call = MethodCall::release_name(mem::take(&mut self.name));
            let req = ClientMessage::MethodCallVoid(call);
            self.reqs.try_send(req).unwrap();
        }
    }
}

impl ServiceListener {

    pub async fn next_call(&mut self) -> RequestResult<MethodCall> {

        let next = async { Ok(self.recv.recv().await.unwrap()) };
        let reactor = async { Err(self.con.reactor.run_forever().await.unwrap_err()) };

        next.or(reactor).await

    }

}

fn hash_signal(path: &str, iface: &str, member: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    path.hash(&mut hasher);
    iface.hash(&mut hasher);
    member.hash(&mut hasher);
    hasher.finish()
}

#[derive(Debug)] // TODO: derive debug for everything
pub struct MethodCall {
    pub dest: String, // todo: arena :(
    pub path: String,
    pub iface: String,
    pub member: String,
    pub no_reply: bool,
    pub allow_interactive_auth: bool,
    pub args: Vec<Arg>,
    caller: Option<MethodCaller>,
}

impl MethodCall {

    pub fn new(dest: &str, path: &str, iface: &str, member: &str) -> Self {

        Self {
            caller: None, // this would be ourselves
            dest: dest.to_string(), // Todo: arena
            path: path.to_string(),
            iface: iface.to_string(),
            member: member.to_string(),
            no_reply: false,
            allow_interactive_auth: true,
            args: Vec::new(),
        }

    }
    
    pub fn arg<A: ValidArg>(&mut self, input: A) {
        self.args.push(input.pack());
    }

    pub(self) fn serialize(self) -> GenericMessage {

        let mut msg = GenericMessage::default();

        msg.kind = MessageKind::MethodCall;
        msg.args = self.args;

        msg.fields.push(header_field(FieldCode::Dest,    SimpleArg::String(self.dest)));
        msg.fields.push(header_field(FieldCode::ObjPath, SimpleArg::ObjPath(self.path)));
        msg.fields.push(header_field(FieldCode::Iface,   SimpleArg::String(self.iface)));
        msg.fields.push(header_field(FieldCode::Member,  SimpleArg::String(self.member)));
        
        msg

    }

    fn deserialize(mut msg: GenericMessage) -> Result<Self, ParseError> {

        Ok(Self {
            caller: Some(MethodCaller {
                name: msg.take_field(FieldCode::Sender).ok_or(ParseError::InvalidData)?,
                serial: msg.serial,
            }),
            dest: msg.take_field(FieldCode::Dest).ok_or(ParseError::InvalidData)?,
            path: msg.take_field(FieldCode::ObjPath).ok_or(ParseError::InvalidData)?,
            iface: msg.take_field(FieldCode::Iface).ok_or(ParseError::InvalidData)?,
            member: msg.take_field(FieldCode::Member).ok_or(ParseError::InvalidData)?,
            no_reply: msg.no_reply,
            allow_interactive_auth: msg.allow_interactive_auth,
            args: msg.args
        })
        
    }

    pub fn hello() -> Self {
        Self::new(
             "org.freedesktop.DBus",
             "/org/freedesktop/DBus",
             "org.freedesktop.DBus",
             "Hello",
        )
    }

    pub fn add_match(rule: String) -> Self {
        let mut val = Self::new(
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus",
            "org.freedesktop.DBus",
            "AddMatch",
        );
        val.args.push(rule.pack());
        val
    }

    pub fn remove_match(rule: String) -> Self {
        let mut val = Self::new(
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus",
            "org.freedesktop.DBus",
            "RemoveMatch"
        );
        val.arg(rule);
        val
    }

    pub fn request_name(name: String, flags: u32) -> Self {
        let mut val = Self::new(
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus",
            "org.freedesktop.DBus",
            "RequestName"
        );
        val.arg(name);
        val.arg(flags);
        val
    }
    
    pub fn release_name(name: String) -> Self {
        let mut val = Self::new(
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus",
            "org.freedesktop.DBus",
            "ReleaseName"
        );
        val.arg(name);
        val
    }
    
}

#[derive(Debug, Clone)]
pub struct SignalMatch {
    rule: String,
    hash: u64,
}

impl SignalMatch {

    pub fn new(path: &str, iface: &str, member: &str) -> Self {

        let rule = format!(
            "path={},interface={},member={}",
            path, iface, member,
        );

        let hash = hash_signal(path, iface, member);

        Self { rule, hash } // TODO: arena
    }

}

#[derive(Debug, Clone)]
struct MethodCaller {
    name: String,
    serial: u32,
}

#[derive(Debug)]
pub struct MethodReply {
    args: Vec<Option<Arg>>,
    caller: Option<MethodCaller>,
}

impl MethodReply {

    pub fn new(to: &MethodCall) -> Self {
        Self {
            caller: to.caller.clone(),
            args: Vec::new(),
        }
    }

    pub fn push<A: ValidArg>(&mut self, input: A) {
        self.args.push(Some(input.pack()));
    }

    #[track_caller]
    pub fn arg<T: ValidArg>(&mut self, idx: usize) -> T {
        match self.get(idx) {
            Ok(val) => val,
            Err(err) => panic!("cannot read arg #{idx}: {err:?}")
        }
    }

    pub fn get<T: ValidArg>(&mut self, idx: usize) -> Result<T, ArgError> {
        let arg = self.args.get_mut(idx)
            .ok_or(ArgError::DoesntExist)?
            .take().ok_or(ArgError::Taken)?;
        T::unpack(arg).ok_or(ArgError::WrongType)
    }

    pub(self) fn serialize(self) -> GenericMessage {

        let mut msg = GenericMessage::default();

        msg.kind = MessageKind::MethodReply;
        msg.args = self.args.into_iter()
            .filter_map(identity)
            .collect();

        // add reply information

        let caller = self.caller.unwrap();

        msg.fields.push(header_field(FieldCode::ReplySerial, SimpleArg::U32(caller.serial)));
        msg.fields.push(header_field(FieldCode::Dest, SimpleArg::String(caller.name)));

        msg

    }

    fn deserialize(msg: GenericMessage) -> Self {

        Self {
            caller: None, // would be ourselves
            args: msg.args.into_iter().map(Some).collect()
        }
        
    }
    
}

#[derive(Debug)]
pub struct SignalTrigger {
    hash: u64, // used to check if we listen to this signal
    path: String, // todo: arena
    iface: String,
    member: String,
    args: Vec<Option<Arg>>,
}

impl SignalTrigger {

    pub fn new(path: &str, iface: &str, member: &str) -> Self {

        Self {
            hash: hash_signal(path, iface, member),
            path: path.to_string(),
            iface: iface.to_string(),
            member: member.to_string(),
            args: Vec::new(),
        }

    }
    
    pub fn push<A: ValidArg>(&mut self, input: A) {
        self.args.push(Some(input.pack()));
    }

    /// In contrast to other `arg` methods, this clones the value.
    #[track_caller]
    pub fn arg<T: ValidArg>(&self, idx: usize) -> T {
        match self.get(idx) {
            Ok(val) => val,
            Err(err) => panic!("cannot read arg #{idx}: {err:?}")
        }
    }

    /// In contrast to other `get` methods, this clones the value.
    pub fn get<T: ValidArg>(&self, idx: usize) -> Result<T, ArgError> {
        let arg = self.args.get(idx)
            .ok_or(ArgError::DoesntExist)?
            .clone().ok_or(ArgError::Taken)?;
        T::unpack(arg).ok_or(ArgError::WrongType)
    }

    fn deserialize(mut msg: GenericMessage) -> Result<Self, ParseError> {

        let mut this = Self {
            hash: 0,
            path: msg.take_field(FieldCode::ObjPath).ok_or(ParseError::InvalidData)?,
            iface: msg.take_field(FieldCode::Iface).ok_or(ParseError::InvalidData)?,
            member: msg.take_field(FieldCode::Member).ok_or(ParseError::InvalidData)?,
            args: msg.args.into_iter().map(Some).collect()
        };

        this.hash = hash_signal(&this.path, &this.iface, &this.member);

        Ok(this)
        
    }
    
}

enum ClientMessage {
    /// authenticate the client (send lazily on startup)
    Authenticate,
    MethodCall(async_channel::Sender<Result<MethodReply, MethodError>>, MethodCall),
    MethodCallVoid(MethodCall),
    MethodReply(MethodReply),
    MethodError(MethodError),
    AddService(async_channel::Sender<MethodCall>, String /* name */),
}

enum ServerMessage {
    MethodReply(MethodReply),
    SignalTrigger(SignalTrigger),
}

#[derive(Debug, Default)]
#[repr(u8)]
pub enum MessageKind {
    #[default]
    Invalid     = 0,
    MethodCall  = 1,
    MethodReply = 2,
    Error       = 3,
    Signal      = 4,
}
impl MessageKind {
    fn from_raw(val: u8) -> Option<Self> {
        match val {
            0 => Some(Self::Invalid),
            1 => Some(Self::MethodCall),
            2 => Some(Self::MethodReply),
            3 => Some(Self::Error),
            4 => Some(Self::Signal),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum FieldCode {
    Invalid      = 0,
    ObjPath      = 1,
    Iface        = 2,
    Member       = 3,
    ErrName      = 4,
    ReplySerial  = 5,
    Dest         = 6,
    Sender       = 7,
    Signature    = 8,
    NumFds       = 9,
}

#[derive(Debug, Default)]
pub struct GenericMessage {
    pub serial: u32,
    pub no_reply: bool,
    pub allow_interactive_auth: bool,
    pub kind: MessageKind,
    pub fields: Vec<(u8, Variant)>, // header fields
    pub args: Vec<Arg>, // message body
}

impl GenericMessage {

    pub fn serialize(mut self) -> Vec<u8> {

        let mut buf = SerializeBuf {
            data: Vec::with_capacity(1024)
        };

        // ### header ###
        
        // endianess
        let endianess = if cfg!(target_endian = "big") { 'B' } else { 'l' };
        serialize_arg((endianess as u8).pack(), &mut buf);

        // message kind
        serialize_arg((self.kind as u8).pack(), &mut buf);

        // flags, interactive authorization is enabled
        serialize_arg((0x4u8).pack(), &mut buf);

        // version 1
        serialize_arg(1u8.pack(), &mut buf);

        // body length, filled in later
        let idx = buf.data.len();
        serialize_arg(0u32.pack(), &mut buf);

        // unique serial
        serialize_arg(self.serial.pack(), &mut buf);

        // construct message signature

        let mut kinds = Vec::new();
        for it in self.args.iter() { skind(it, &mut kinds) };

        self.fields.push(header_field(FieldCode::Signature, SimpleArg::Signature(kinds)));

        // TODO: impl unix fds (also need to add a special header field!!!)

        // serialize header fields
        serialize_arg(self.fields.pack(), &mut buf);

        // ### body ###

        buf.insert_pad(8); // body must be 8-byte aligned

        let orig = buf.data.len();

        for arg in self.args {
            serialize_arg(arg, &mut buf);
        }

        // update the body len
        let size = buf.data.len() - orig;
        buf.data.splice(idx..idx + 4, (size as u32).to_ne_bytes());

        buf.data

    }

    pub fn deserialize(data: &[u8]) -> Result<(usize, GenericMessage), ParseError> {

        let mut buf = DeserializeBuf {
            data,
            offset: 0,
            endianess: 'l', // is overwritten with the first byte read
        };

        let raw: u8 = unpack_arg(&mut buf)?;
        let endianess = char::from_u32(raw as u32).ok_or(ParseError::InvalidData)?;
        buf.endianess = endianess; // correct the endianess

        let raw: u8 = unpack_arg(&mut buf)?;
        let kind = MessageKind::from_raw(raw).ok_or(ParseError::Partial)?;

        let raw: u8 = unpack_arg(&mut buf)?;
        let no_reply = raw & 0x1 == 1;
        let allow_interactive_auth = raw & 0x4 == 1;

        let proto_version: u8 = unpack_arg(&mut buf)?;
        assert!(proto_version == 1);
        if proto_version != 1 { return Err(ParseError::Protocol) };

        let body_len: u32 = unpack_arg(&mut buf)?;
        let serial: u32 = unpack_arg(&mut buf)?;

        if buf.data.len() - buf.offset < body_len as usize {
            // no need to do more parsing, we haven't got enough data anyways
            return Err(ParseError::Partial);
        }
        
        let mut header_fields: Vec<(u8, Variant)> = unpack_arg(&mut buf)?;

        let Signature(signature) = take_field(&mut header_fields, FieldCode::Signature)
            .unwrap_or_default();
        
        buf.consume_pad(8);

        let before_body = buf.offset;
        let mut args = Vec::with_capacity(2);
        for CompleteKind(kinds) in iter_complete_types(&signature) {
            let arg = deserialize_arg(&mut buf, &kinds)?;
            args.push(arg);
        }

        // assure we have read the right amount of args
        debug_assert_eq!(buf.offset - body_len as usize, before_body);

        Ok((
            buf.offset,
            Self {
                serial,
                no_reply,
                allow_interactive_auth,
                kind,
                fields: header_fields,
                args,
            }
        ))

    }

    fn take_field<T: ValidArg>(&mut self, target: FieldCode) -> Option<T> {
        take_field(&mut self.fields, target)
    }

}

fn take_field<T: ValidArg>(fields: &mut Vec<(u8, Variant)>, target: FieldCode) -> Option<T> {
    fields.iter()
        .position(|(code, ..)| *code == target as u8)
        .and_then(|idx| {
            let (.., variant) = fields.swap_remove(idx);
            T::unpack(variant.get()?)
        })
}

struct SerializeBuf {
    data: Vec<u8>,
}

impl SerializeBuf {
    pub fn insert_pad(&mut self, padding: usize) {
        let needed = (padding - self.data.len() % padding) % padding;
        self.data.resize(self.data.len() + needed, 0);
    }
}

struct DeserializeBuf<'a> {
    data: &'a [u8],
    offset: usize,
    endianess: char,
}

impl DeserializeBuf<'_> {

    fn consume_pad(&mut self, padding: usize) -> Option<()> {
        let needed = (padding - self.offset % padding) % padding;
        self.consume_bytes(needed).map(drop)
    }

    fn consume_bytes(&mut self, len: usize) -> Option<&[u8]> {
        if len <= self.data.len() {
            let (bytes, rest) = self.data.split_at(len); // MAINTENANCE: replace with split_at_checked when it becomes stabilzed
            self.data = rest;
            self.offset += len;
            Some(bytes)
        } else {
            None
        }
    }

}

#[test]
fn deserialize_buf() {

    let data = &[0, 1, 2, 3, 4];
    let mut buf = DeserializeBuf {
        data,
        offset: 0,
        endianess: 'l',
    };

    buf.consume_pad(8); // shouldn't consume anything
    assert_eq!(buf.data.len(), 5);

    let val = buf.consume_bytes(2).unwrap();
    assert_eq!(val, &[0, 1]);

    buf.consume_pad(4);
    assert_eq!(buf.data.len(), 1);

    let val = buf.consume_bytes(1).unwrap();
    assert_eq!(val, &[4]);

}

fn header_field(code: FieldCode, arg: SimpleArg) -> (u8, Variant) {
    (code as u8, Variant::new(Arg::Simple(arg)))
}

// #### type system ####

fn serialize_arg(arg: Arg, buf: &mut SerializeBuf) {

    match arg {

        // basic kind

        Arg::Simple(simple) => {

            match simple {

                // numbers
                SimpleArg::Byte(val)   => { buf.insert_pad(1); buf.data.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::I16(val)    => { buf.insert_pad(2); buf.data.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::U16(val)    => { buf.insert_pad(2); buf.data.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::I32(val)    => { buf.insert_pad(4); buf.data.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::U32(val)    => { buf.insert_pad(4); buf.data.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::I64(val)    => { buf.insert_pad(8); buf.data.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::U64(val)    => { buf.insert_pad(8); buf.data.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::Double(val) => { buf.insert_pad(8); buf.data.extend_from_slice(&val.inner.to_ne_bytes()) },

                // other
                SimpleArg::Bool(val) => { buf.insert_pad(4); buf.data.extend_from_slice(&(val as i32).to_ne_bytes()) },
                SimpleArg::Fd(val) => {
                    buf.insert_pad(4);
                    buf.data.extend_from_slice(&val.inner.as_raw_fd().to_ne_bytes());
                    mem::forget(val); // don't `drop` the OwnedFd since we transfer ownership here
                },

                SimpleArg::String(val) | SimpleArg::ObjPath(val) => {
                    // the length of the string, in bytes (u32)
                    buf.insert_pad(4);
                    buf.data.extend_from_slice(&(val.len() as u32).to_ne_bytes());
                    // the data
                    buf.data.extend_from_slice(val.as_bytes());
                    buf.data.extend_from_slice(&[0]); // zero-terminated
                },

                SimpleArg::Signature(val) => {
                    // the length of the string, in bytes (u8)
                    buf.data.extend_from_slice(&(val.len() as u8).to_ne_bytes());
                    // the data
                    buf.data.extend(val.iter().map(|it| *it as u8));
                    buf.data.extend_from_slice(&[0]); // zero-terminated
                }

            };

        },

        // container kinds

        Arg::Compound(compound) => match compound {

            CompoundArg::Array(t, items) => {

                // the length of the array, in bytes (u32)
                // this is a dummy value which is replaced later
                buf.insert_pad(4);
                let idx = buf.data.len();
                buf.data.extend_from_slice(&0u32.to_ne_bytes());

                // the items

                buf.insert_pad(align_from_signature(t)); // pad even if empty

                let orig = buf.data.len();
                for arg in items {
                    serialize_arg(arg, buf);
                }

                // update the size
                let size = buf.data.len() - orig;
                assert!(size < (2usize.pow(26)), "array length too long for dbus ({} bytes)", size);
                buf.data.splice(idx..idx + 4, (size as u32).to_ne_bytes());

            },

            CompoundArg::Map(.., map) => {

               // the length of the map, in bytes (u32)
                buf.insert_pad(4);
                let idx = buf.data.len();
                buf.data.extend_from_slice(&(map.len() as u32).to_ne_bytes());

                buf.insert_pad(8); // pad even if the map is empty
                let orig = buf.data.len();

                // the items
                for (key, val) in map {
                    buf.insert_pad(8);
                    serialize_arg(Arg::Simple(key), buf);
                    serialize_arg(val, buf);
                }
                
                let size = buf.data.len() - orig;
                assert!(size < 2^26);
                buf.data.splice(idx..idx + 4, (size as u32).to_ne_bytes());

            },

            CompoundArg::Variant(val) => {

                // the signature string
                let signature = val.kinds();
                serialize_arg(Arg::Simple(SimpleArg::Signature(signature)), buf);

                // the argument
                serialize_arg(*val, buf);

            }

            CompoundArg::EmptyVariant => panic!("EmptyVariant cannot be sent"),
            
            CompoundArg::Struct(fields) => {

                buf.insert_pad(8);
                for val in fields {
                    serialize_arg(val, buf);
                }

            }
            
        }
    };

}

fn align_from_signature(t: Vec<ArgKind>) -> usize {
    t[0].align()
}

macro_rules! deserialize_int {
    ($buf:ident, $typ:ident) => {
        {
            let size = mem::size_of::<$typ>();
            let bytes = $buf.consume_bytes(size).ok_or(ParseError::Partial)?.try_into().unwrap();
            if $buf.endianess == 'l' {
                $typ::from_le_bytes(bytes)
            } else if $buf.endianess == 'B' {
                $typ::from_be_bytes(bytes)
            } else {
                unreachable!("invalid endianess {:?}", $buf.endianess)
            }
        }
    };
}

fn deserialize_arg(buf: &mut DeserializeBuf, kinds: &[ArgKind]) -> Result<Arg, ParseError> {
    
    match kinds {

        [ArgKind::Byte] => {
            buf.consume_pad(1);
            let val = deserialize_int!(buf, u8);
            Ok(Arg::Simple(SimpleArg::Byte(val)))
        },
        
        [ArgKind::Bool] => {
            buf.consume_pad(4);
            let val = deserialize_int!(buf, u32);
            Ok(Arg::Simple(SimpleArg::Bool(val == 1)))
        },
        
        [ArgKind::I16] => {
            buf.consume_pad(2);
            let val = deserialize_int!(buf, i16);
            Ok(Arg::Simple(SimpleArg::I16(val)))
        },
        
        [ArgKind::U16] => {
            buf.consume_pad(2);
            let val = deserialize_int!(buf, u16);
            Ok(Arg::Simple(SimpleArg::U16(val)))
        },
        
        [ArgKind::I32] => {
            buf.consume_pad(4);
            let val = deserialize_int!(buf, i32);
            Ok(Arg::Simple(SimpleArg::I32(val)))
        },
        
        [ArgKind::U32] => {
            buf.consume_pad(4);
            let val = deserialize_int!(buf, u32);
            Ok(Arg::Simple(SimpleArg::U32(val)))
        },
        
        [ArgKind::I64] => {
            buf.consume_pad(8);
            let val = deserialize_int!(buf, i64);
            Ok(Arg::Simple(SimpleArg::I64(val)))
        },
        
        [ArgKind::U64] => {
            buf.consume_pad(8);
            let val = deserialize_int!(buf, u64);
            Ok(Arg::Simple(SimpleArg::U64(val)))
        },

        [ArgKind::String | ArgKind::ObjPath] => {
            let len = unpack_arg::<u32>(buf)? as usize;
            let raw = buf.consume_bytes(len + 1).ok_or(ParseError::Partial)?; // +1 for the \0
            let val = String::from_utf8(raw[..len].to_vec()).expect("verify utf8");
            Ok(Arg::Simple(SimpleArg::String(val)))
        },

        [ArgKind::Signature] => {
            let len = unpack_arg::<u8>(buf)? as usize;
            let raw = buf.consume_bytes(len + 1).ok_or(ParseError::Partial)?; // +1 for the \0
            let val = raw[..len].into_iter()
                .map(|it| ArgKind::deserialize(*it).expect("verify signature"))
                .collect();
            Ok(Arg::Simple(SimpleArg::Signature(val)))
        },

        [ArgKind::Array, ArgKind::PairBegin, contents @ .., ArgKind::PairEnd] => {

            let mut iter = iter_complete_types(contents);

            let CompleteKind(key_kinds) = iter.next().expect("get first element of dict entry");
            let CompleteKind(val_kinds) = iter.next().expect("get second element of dict entry");
            assert_eq!(key_kinds.len(), 1, "map key type was a container: {:?}", val_kinds);
            assert_eq!(iter.next(), None); // should only contain 2 complete types

            let len: u32 = unpack_arg(buf)?;

            let mut map = HashMap::new(); // TODO: arena

            let start = buf.offset;
            while buf.offset - start < len as usize {

                buf.consume_pad(8); // dict entries are always 8-byte padded

                let key = deserialize_arg(buf, &key_kinds)?;
                let val = deserialize_arg(buf, &val_kinds)?;

                let Arg::Simple(skey) = key else { panic!("a map key must not be a container") };

                map.insert(skey, val);

            }

            Ok(Arg::Compound(CompoundArg::Map(key_kinds[0], val_kinds, map)))
            
        },

        [ArgKind::Array, contents @ ..] => {

            let len: u32 = unpack_arg(buf)?;

            let mut args = Vec::new();
            let start = buf.offset;
            while buf.offset - start < len as usize {
                let arg = deserialize_arg(buf, contents)?;
                args.push(arg);
            }

            Ok(Arg::Compound(CompoundArg::Array(contents.to_vec(), args))) // TODO: arena
            
        },

        [ArgKind::StructBegin,
         contents @ ..,
         ArgKind::StructEnd] => {

            buf.consume_pad(8);

            let mut args = Vec::new();
            for CompleteKind(kinds) in iter_complete_types(contents) {
                let arg = deserialize_arg(buf, &kinds)?;
                args.push(arg);
            }

            Ok(Arg::Compound(CompoundArg::Struct(args)))
            
        },

        [ArgKind::Variant] => {

            let Signature(kinds) = unpack_arg(buf)?;

            if kinds.len() > 0 {
                let arg = deserialize_arg(buf, &kinds)?; // read the actual value
                Ok(Arg::Compound(CompoundArg::Variant(Box::new(arg))))
            } else {
                // an "empty" variant which really shouldn't be allowed
                // this variant doesn't even have a signature so we can't produce
                // a default value here
                Ok(Arg::Compound(CompoundArg::EmptyVariant))
            }
           
            
        },
        
        other => panic!("cannot deserialize signature {:?}", other),
        
    }
    
}

#[derive(Debug, PartialEq, Eq)]
struct CompleteKind(pub Vec<ArgKind>);

fn iter_complete_types<'d>(contents: &'d [ArgKind]) -> impl Iterator<Item = CompleteKind> + 'd {

    let mut outer = contents.iter();

    iter::from_fn(move || {

        enum State {
            OneMore,
            Struct(usize /* depth */),
        }

        let mut kinds = Vec::with_capacity(2);
        let mut state = State::OneMore;

        for kind in &mut outer {

            kinds.push(*kind);

            match state {
                State::OneMore => match kind {

                    // simple kinds
                    ArgKind::Byte | ArgKind::Bool |
                    ArgKind::I16 | ArgKind::U16 |
                    ArgKind::I32 | ArgKind::U32 |
                    ArgKind::I64 | ArgKind::U64 |
                    ArgKind::Double |
                    ArgKind::String | ArgKind::ObjPath | ArgKind::Signature |
                    ArgKind::UnixFd |
                    ArgKind::Variant => break, // this one kind already represents a complete type

                    // array
                    ArgKind::Array => continue, // parse one more complete type

                    // structs
                    ArgKind::StructBegin => state = State::Struct(1), // wait until StructEnd
                    ArgKind::StructEnd   => unreachable!(),

                    ArgKind::PairBegin => state = State::Struct(1), // wait until PairEnd
                    ArgKind::PairEnd   => unreachable!(),

                    ArgKind::Pair   => panic!(), // shouldn't be in a type signature
                    ArgKind::Struct => panic!(), // shouldn't be in a type signature

                },

                State::Struct(ref mut n) => {
                    if let      ArgKind::StructBegin | ArgKind::PairBegin = kind { *n += 1 }
                    else if let ArgKind::StructEnd   | ArgKind::PairEnd   = kind { *n -= 1 }
                    if *n == 0 { break }
                }

            }

        }

        if kinds.is_empty() {
            None
        } else {
            Some(CompleteKind(kinds))
        }
    })

}

fn unpack_arg<T: ValidArg>(buf: &mut DeserializeBuf) -> Result<T, ParseError> {
    T::unpack(deserialize_arg(buf, &T::kinds())?).ok_or(ParseError::InvalidData)
}


#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(i8)]
pub enum ArgKind {
    // simple types
    Byte      = 'y' as i8, // uint8_t *
    Bool      = 'b' as i8, // int * (not bool *)
    I16       = 'n' as i8, // int16_t *
    U16       = 'q' as i8, // uint16_t *
    I32       = 'i' as i8, // int32_t *
    U32       = 'u' as i8, // uint32_t *
    I64       = 'x' as i8, // int64_t *
    U64       = 't' as i8, // uint64_t *
    Double    = 'd' as i8, // double *
    String    = 's' as i8, // const char **
    ObjPath   = 'o' as i8, // const char **
    Signature = 'g' as i8, // const char **
    UnixFd    = 'h' as i8, // int *
    // compound types
    Array       = 'a' as i8, // int (len), ... (items)
    Variant     = 'v' as i8, // const char* (signature), any
    StructBegin = '(' as i8, // ... (items)
    StructEnd   = ')' as i8, // 
    PairBegin   = '{' as i8, // any, any (a pair)
    PairEnd     = '}' as i8, // 
    Pair        = 'e' as i8, // dict entry phantom type
    Struct      = 'r' as i8, // struct phantom type
}

impl ArgKind {
    pub fn align(&self) -> usize {
        match self {
            ArgKind::Byte        => 1,
            ArgKind::Bool        => 1,
            ArgKind::I16         => 2,
            ArgKind::U16         => 2,
            ArgKind::I32         => 4,
            ArgKind::U32         => 4,
            ArgKind::I64         => 8,
            ArgKind::U64         => 8,
            ArgKind::Double      => 8,
            ArgKind::String      => 4,
            ArgKind::ObjPath     => 4,
            ArgKind::Signature   => 1,
            ArgKind::UnixFd      => 4,
            ArgKind::Array       => 4,
            ArgKind::Variant     => 1,
            ArgKind::StructBegin => 8,
            ArgKind::StructEnd   => 8,
            ArgKind::PairBegin   => 8,
            ArgKind::PairEnd     => 8,
            ArgKind::Pair        => 8,
            ArgKind::Struct      => 8,           
        }
    }
}

impl ArgKind {

    fn deserialize(val: u8) -> Option<Self> {
        match char::from_u32(val as u32)? {
            'y' => Some(Self::Byte),
            'b' => Some(Self::Bool),
            'n' => Some(Self::I16),
            'q' => Some(Self::U16),
            'i' => Some(Self::I32),
            'u' => Some(Self::U32),
            'x' => Some(Self::I64),
            't' => Some(Self::U64),
            'd' => Some(Self::Double),
            's' => Some(Self::String),
            'o' => Some(Self::ObjPath),
            'g' => Some(Self::Signature),
            'h' => Some(Self::UnixFd),
            'a' => Some(Self::Array),
            'v' => Some(Self::Variant),
            '(' => Some(Self::StructBegin),
            ')' => Some(Self::StructEnd),
            '{' => Some(Self::PairBegin),
            '}' => Some(Self::PairEnd),
            'e' => Some(Self::Pair),
            'r' => Some(Self::Struct),
            _ => None,
        }
    }
    
}

#[derive(Debug)]
pub enum ArgError {
    Taken,
    DoesntExist,
    WrongType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Simple(SimpleArg),
    Compound(CompoundArg),
}

impl Arg {

    fn kinds(&self) -> Vec<ArgKind> {
        let mut out = Vec::new(); // TODO: not use Vec but an iterator or smth else
        skind(self, &mut out);
        out
    }

    fn align(&self) -> usize {
        self.kinds()[0].align() // TODO: not use vec in `kinds` so this is efficient
    }

}

fn skind(arg: &Arg, out: &mut Vec<ArgKind>) { // TODO: think about the relation of this and "ValidArg::kinds"
    match arg {
        Arg::Simple(simple) => {
            let kind = match simple {
                SimpleArg::Byte(..)   => ArgKind::Byte,
                SimpleArg::Bool(..)   => ArgKind::Bool,
                SimpleArg::I16(..)    => ArgKind::I16,
                SimpleArg::U16(..)    => ArgKind::U16,
                SimpleArg::I32(..)    => ArgKind::I32,
                SimpleArg::U32(..)    => ArgKind::U32,
                SimpleArg::I64(..)    => ArgKind::I64,
                SimpleArg::U64(..)    => ArgKind::U64,
                SimpleArg::Double(..) => ArgKind::Double,
                SimpleArg::Fd(..)     => ArgKind::UnixFd,
                SimpleArg::String(..)    => ArgKind::String,
                SimpleArg::Signature(..) => ArgKind::Signature,
                SimpleArg::ObjPath(..)      => ArgKind::ObjPath,
            };
            out.push(kind);
        },
        Arg::Compound(compound) => match compound {
            CompoundArg::Array(kinds, ..) => {
                out.push(ArgKind::Array);
                out.extend(kinds);
            },
            CompoundArg::Map(tkey, tval, ..) => {
                out.push(ArgKind::Array);
                out.push(ArgKind::PairBegin);
                out.push(*tkey);
                out.extend(tval);
                out.push(ArgKind::PairEnd);
            },
            CompoundArg::Variant(..) => {
                out.push(ArgKind::Variant);
            }
            CompoundArg::EmptyVariant => panic!("EmptyVariant cannot be sent"),
            CompoundArg::Struct(fields) => {
                out.push(ArgKind::StructBegin);
                for it in fields.iter() {
                    skind(it, out);
                }
                out.push(ArgKind::StructEnd);
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SimpleArg {
    Byte(u8),
    Bool(bool),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Double(OpsF64),
    String(String),
    ObjPath(String),
    Signature(Vec<ArgKind>),
    Fd(OpsOwnedFd),
}

#[derive(Debug, Clone)]
pub struct OpsF64 {
    pub inner: f64,
}

impl PartialEq for OpsF64 {
    fn eq(&self, _: &Self) -> bool {
        panic!("cannot compare a SimpleArg::Double since floats do not implement PartialEq in rust");
    }
}

impl Eq for OpsF64 {}

impl Hash for OpsF64 {
    fn hash<H: Hasher>(&self, _: &mut H) {
        panic!("cannot hash a SimpleArg::Double since floats do not implement Hash");
    }
}

#[derive(Debug)]
pub struct OpsOwnedFd {
    pub inner: OwnedFd,
}

impl Clone for OpsOwnedFd {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.try_clone().expect("failed to clone an OwnedFd when cloning SimpleArg::Fd"),
        }
    }
}

impl PartialEq for OpsOwnedFd {
    fn eq(&self, other: &Self) -> bool {
        self.inner.as_raw_fd() == other.inner.as_raw_fd()
    }
}

impl Eq for OpsOwnedFd {}

impl Hash for OpsOwnedFd {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.inner.as_raw_fd(), state)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompoundArg {
    Array(Vec<ArgKind>, Vec<Arg>),
    Map(ArgKind , Vec<ArgKind>, HashMap<SimpleArg, Arg>),
    Variant(Box<Arg>),
    EmptyVariant,
    Struct(Vec<Arg>),
}

pub trait ValidArg {
    fn pack(self) -> Arg where Self: Sized;
    fn unpack(arg: Arg) -> Option<Self> where Self: Sized;
    fn kinds() -> Vec<ArgKind>;
}

impl<'a> ValidArg for &'a str {
    fn pack(self) -> Arg {
        Arg::Simple(SimpleArg::String(self.to_string()))
    }
    fn unpack(_arg: Arg) -> Option<Self> {
        unimplemented!();
    }
    fn kinds() -> Vec<ArgKind> {
        vec![ArgKind::String]
    }
}

impl ValidArg for String {
    fn pack(self) -> Arg {
        Arg::Simple(SimpleArg::String(self))
    }
    fn unpack(arg: Arg) -> Option<Self> {
        if let Arg::Simple(SimpleArg::String(val)) = arg { Some(val) }
        else { None }
    }
    fn kinds() -> Vec<ArgKind> {
        vec![ArgKind::String]
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Signature(pub Vec<ArgKind>);

impl ValidArg for Signature {
    fn pack(self) -> Arg {
        let Self(val) = self;
        Arg::Simple(SimpleArg::Signature(val))
    }
    fn unpack(arg: Arg) -> Option<Self> {
        if let Arg::Simple(SimpleArg::Signature(val)) = arg { Some(Self(val)) }
        else { None }
    }
    fn kinds() -> Vec<ArgKind> {
        vec![ArgKind::Signature]
    }
}

impl ValidArg for f64 {
    fn pack(self) -> Arg {
        Arg::Simple(SimpleArg::Double(OpsF64 { inner: self }))
    }
    fn unpack(arg: Arg) -> Option<Self> {
        if let Arg::Simple(SimpleArg::Double(val)) = arg { Some(val.inner) }
        else { None }
    }
    fn kinds() -> Vec<ArgKind> {
        vec![ArgKind::Double]
    }
}

impl ValidArg for OwnedFd {
    fn pack(self) -> Arg {
        Arg::Simple(SimpleArg::Fd(OpsOwnedFd { inner: self }))
    }
    fn unpack(arg: Arg) -> Option<Self> {
        if let Arg::Simple(SimpleArg::Fd(val)) = arg { Some(val.inner) }
        else { None }
    }
    fn kinds() -> Vec<ArgKind> {
        vec![ArgKind::UnixFd]
    }
}

macro_rules! impl_valid_arg {
    ($(($name: ident: $t: ident)),*,) => {
        $(
            impl ValidArg for $t {
                fn pack(self) -> Arg {
                    Arg::Simple(SimpleArg::$name(self))
                }
                fn unpack(arg: Arg) -> Option<Self> {
                    if let Arg::Simple(SimpleArg::$name(val)) = arg { Some(val) }
                    else { None }
                }
                fn kinds() -> Vec<ArgKind> {
                    vec![ArgKind::$name]
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

#[derive(Debug, PartialEq, Eq)]
pub struct Variant {
    inner: Option<Arg>,
}

impl Variant {

    pub fn new(arg: Arg) -> Self {
        Self { inner: Some(arg) }
    }

    pub fn new_empty() -> Self {
        Self { inner: None }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_none()
    }

    #[track_caller]
    pub fn get(self) -> Option<Arg> {
        self.inner
    }

    #[track_caller]
    pub fn arg(self) -> Arg {
        self.inner.expect("variant was empty but expected a value")
    }

}

impl ValidArg for Variant {
    fn pack(self) -> Arg {
        let val = self.get().expect("cannot serialize empty variant");
        Arg::Compound(CompoundArg::Variant(Box::new(val))) // TODO: arenaaaaa
    }
    fn unpack(arg: Arg) -> Option<Self> {
        if let Arg::Compound(CompoundArg::Variant(val)) = arg { Some(Self::new(*val)) }
        else if let Arg::Compound(CompoundArg::EmptyVariant) = arg { Some(Self::new_empty()) }
        else { None }
    }
    fn kinds() -> Vec<ArgKind> {
        vec![ArgKind::Variant]
    }
}

impl<T: ValidArg> ValidArg for Vec<T> {
    fn pack(self) -> Arg {
        let kind = T::kinds();
        let contents = self.into_iter().map(T::pack).collect();
        Arg::Compound(CompoundArg::Array(kind, contents))
    }
    fn unpack(arg: Arg) -> Option<Self> {
        if let Arg::Compound(CompoundArg::Array(.., items)) = arg {
            let mut out = Vec::new();
            for it in items { out.push(T::unpack(it)?); }
            Some(out)
        }
        else { None }
    }
    fn kinds() -> Vec<ArgKind> { // TODO: SmallVec or Arena
        let mut result = Vec::with_capacity(4);
        result.push(ArgKind::Array);
        result.extend(T::kinds());
        result
    }
}

impl<K: ValidArg + Eq + Hash, V: ValidArg> ValidArg for HashMap<K, V> {
    fn pack(self) -> Arg {
        let tkey = K::kinds()[0];
        let tval = V::kinds();
        let map = self.into_iter().map(|(k, v)| (as_simple_arg(k.pack()), v.pack())).collect();
        Arg::Compound(CompoundArg::Map(tkey, tval, map))
    }
    fn unpack(_arg: Arg) -> Option<Self> {
        // if let Arg::Compound(CompoundArg::Map(.., map)) = arg {
        //     let mut out = HashMap::new();
        //     for (key, it) in map.iter() {
        //         out.insert(K::unpack(key)?, V::unpack(it)?);
        //     }
        //     Some(out)
        // }
        // else { None }
        todo!("fuck fuck fuck hashmap fuck");

// TODO: seperate SimpleArg and CompoundArg traits.... this is kinda important
    }
    fn kinds() -> Vec<ArgKind> {
        vec![ArgKind::Array]
    }
}

/// implement ValidArg for tuple types, which are packed into a dbus `struct`
macro_rules! impl_valid_arg_tuple {
    ($([$(($num: tt, $big: ident, $small: ident)),*]),*,) => {
        $(
            impl<$($big: ValidArg,)*> ValidArg for ($($big,)*) {
                fn pack(self) -> Arg {
                    $(let $small = self.$num.pack();)*
                    Arg::Compound(CompoundArg::Struct(vec![$($small,)*]))
                }
                fn unpack(arg: Arg) -> Option<Self> {
                    if let Arg::Compound(CompoundArg::Struct(mut fields)) = arg {
                        $(let $small = $big::unpack(fields.remove(0))?;)*
                        Some(($($small,)*))
                    }
                    else { None }
                }
                fn kinds() -> Vec<ArgKind> {
                    let mut result = Vec::with_capacity(6);
                    result.push(ArgKind::StructBegin);
                    $(result.extend($big::kinds());)*
                    result.push(ArgKind::StructEnd);
                    result
                }
            }
        )*
    };
}

impl_valid_arg_tuple!(
    [(0, T0, t0)],
    [(0, T0, t0), (1, T1, t1)],
    [(0, T0, t0), (1, T1, t1), (2, T2, t2)],
    [(0, T0, t0), (1, T1, t1), (2, T2, t2), (3, T3, t3)],
);

fn as_simple_arg(arg: Arg) -> SimpleArg {
    if let Arg::Simple(val) = arg { val }
    else { unreachable!() }
}

#[derive(Debug)]
pub enum ParseError {
    /// this error will never appear in the public interface,
    /// it is used to signal that a message is incomplete right now
    Partial,
    /// invalid data was received
    InvalidData,
    /// fatal protocol error, e.g. version mismatch
    Protocol,
}

#[derive(Debug)]
pub enum RequestError {
    Io(io::Error),
    Bus(MethodError),
    Parse(ParseError),
    AlreadyOwned,
}

pub type MethodResult<T> = Result<T, MethodError>;

/// An error reply.
#[derive(Debug)]
pub struct MethodError {
    caller: Option<MethodCaller>,
    name: String,
    msg: String,
}

impl MethodError {

    pub fn new(to: &MethodCall, name: &str, msg: &str) -> Self {
        Self {
            caller: to.caller.clone(),
            name: name.to_string(),
            msg: msg.to_string(),
        }
    }

    pub fn other(to: &MethodCall, msg: &str) -> Self {
        Self {
            caller: to.caller.clone(),
            name: "lsg.other".to_string(),
            msg: msg.to_string(),
        }
    }

    pub fn unimplemented(to: &MethodCall) -> Self {
        Self {
            caller: to.caller.clone(),
            name: "lsg.unimplemented".to_string(),
            msg: "this method is not implemented, as of now".to_string(),
        }
    }

    pub fn serialize(self) -> GenericMessage {

        let mut msg = GenericMessage::default();

        msg.kind = MessageKind::Error;

        msg.fields.push(header_field(FieldCode::ErrName, SimpleArg::String(self.name)));
        msg.args.push(Arg::Simple(SimpleArg::String(self.msg)));

        // add reply information

        let caller = self.caller.unwrap();

        msg.fields.push(header_field(FieldCode::ReplySerial, SimpleArg::U32(caller.serial)));
        msg.fields.push(header_field(FieldCode::Dest, SimpleArg::String(caller.name)));

        msg
        
    }

    pub fn deserialize(mut msg: GenericMessage) -> Result<Self, ParseError> {
        Ok(Self {
            caller: None, // this would be ourselves
            name: msg.take_field(FieldCode::ErrName).ok_or(ParseError::InvalidData)?,
            msg: String::unpack(msg.args.remove(0)).ok_or(ParseError::InvalidData)?,
        })
    }

}

pub type RequestResult<T> = Result<T, RequestError>;

impl From<io::Error> for RequestError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

// impl From<ParseError> for DbusError {
//     fn from(value: ParseError) -> Self {
//         Self::Parse(value)
//     }
// }

// ####### actual desktop-env interface implementation #######

pub struct NotifyProxy {
    con: Connection,
}

impl NotifyProxy {

    pub fn new(con: &Connection) -> Self {
        Self { con: con.clone() }
    }

    pub async fn send(&self, notif: Notif<'_>) -> RequestResult<NotifId> {

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

        let mut hints: HashMap<&str, Variant> = HashMap::new();
        if let Some(urgency) = notif.urgency {
            hints.insert("urgency", Variant::new(Arg::Simple(SimpleArg::Byte(urgency.num()))));
        }
        if let Some(category) = notif.category {
            hints.insert("category", Variant::new(Arg::Simple(SimpleArg::String(category.name().to_string()))));
        }

        call.arg(hints); // hints

        call.arg(notif.timeout.map(|dur|
            if dur == Duration::MAX { 0 }
            else { i32::try_from(dur.as_millis()).unwrap_or(i32::MAX) }
        ).unwrap_or(-1)); // expiration timeout
        
        let mut resp = self.con.method_call(call).await?;
        let id: u32 = resp.arg(0);

        Ok(id)
        
    }

    /// Forcefully close a notification.
    pub async fn close(&self, id: NotifId) -> RequestResult<()> {

        let mut call = MethodCall::new(
            "org.freedesktop.Notifications",
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
            "CloseNotification",
        );

        call.arg(id as u32);

        self.con.method_call(call).await?;

        Ok(())
        
    }

    /// Wait until an action is invoked.
    pub async fn invoked(&self, id: NotifId) -> RequestResult<InvokedAction> {

        let signal = SignalMatch::new(
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
            "ActionInvoked"
        );

        let mut stream = self.con.listen_on(signal).await?;
        let mut key;

        loop {
            let resp = stream.next_signal().await?; // wait for a dbus signal to arrive
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

