
pub mod client {

    use std::{borrow::Cow, collections::HashMap, convert::identity, env, error::Error as StdError, fmt, hash::{Hash, Hasher}, io::{self, Read, Write}, iter, mem, os::{fd::{AsRawFd, OwnedFd}, unix::net::UnixStream}};

    use async_channel as channel;
    use futures_lite::FutureExt;

    #[test]
    fn call() {

        async_io::block_on(async {
    
            let mut con = Connection::new().unwrap();

            let call = MethodCall::new(
                 "org.freedesktop.DBus",
                 "/org/freedesktop/DBus",
                 "org.freedesktop.DBus.Debug.Stats",
                 "GetStats",
            );

            con.method_call(call);

            let _resp = con.next().await.unwrap();
              // ^^^^ parsing this is enough (maps, variants, etc. covered)

        })
    
    }

    #[test]
    fn service() {

        // let subscriber = tracing_subscriber::FmtSubscriber::builder()
        //     .with_max_level(tracing::Level::TRACE)
        //     .finish();

        // tracing::subscriber::set_global_default(subscriber).unwrap();

        async_io::block_on(async {

            let mut con = Connection::new().unwrap();

            // let mut service = Service::new("org.freedesktop.StatusNotifierItem-X-X");

            con.request_name("lsg.test");

            loop {

                let event = con.next().await.unwrap();

                if let Incoming::Call(msg) = event {
                    println!("{:?}", msg);
                    let reply = MethodError::unimplemented(&msg);
                    con.method_reply(Err(reply));
                }

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
        /// the bus connection
        bus: async_io::Async<UnixStream>,
        /// current serial, counting up from 1
        serial: u32,
        ///  actions the reactor should do
        actions: channel::Receiver<Outgoing>,
        /// current message buffer
        buf: Vec<u8>,
    }

    impl Reactor {

        pub(self) async fn run(&mut self) -> io::Result<Incoming> {

            loop {

                enum Either {
                    /// We should execute this request
                    Request(Outgoing),
                    /// We have read more data from the socket
                    MoreData,
                }

                let request = async {
                    // TODO: benchmark cancellation speed/cost of async channels (async_channel vs flume)
                    let val = self.actions.recv().await.unwrap();
                    io::Result::Ok(Either::Request(val))
                };

                let readable = async {

                    let mut buf = [0; 256];

                    let bytes = unsafe { self.bus.read_with_mut(|it| it.read(&mut buf)) }.await?;
                    if bytes == 0 { return Err(io::Error::other("kicked by broker")) };
                    self.buf.extend_from_slice(&buf[..bytes]);

                    Ok(Either::MoreData)

                };

                match readable.or(request).await? {
                    Either::Request(request) => process_request(self, request).await?,
                    Either::MoreData => match process_incoming(self) {
                        Ok(result) => return Ok(result),
                        Err(ParseError::Partial) => continue,
                        Err(other) => return Err(io::Error::other(other)),
                    },
                }

            }

        }

        async fn flush(&mut self) -> io::Result<()> {

            let len = self.actions.len();

            while let Ok(request) = self.actions.try_recv() {

                // don't perform the auth if we have nothing to do afterwards
                if !(len == 1 && matches!(request, Outgoing::InitialAuth)) {
                    process_request(self, request).await?;
                }

            }

            Ok(())
            
        }

    }

    async fn initial_auth(stream: &mut async_io::Async<UnixStream>) -> io::Result<()> {

        let inner = unsafe { stream.get_mut() };

        write!(inner, "\0")?;
        write!(inner, "AUTH EXTERNAL ")?;

        // write the current uid in a hex ascii representation

        let uid = nix::unistd::Uid::current();

        let num = uid.as_raw();
        let mut divisor = 1;

        while num / divisor >= 10 {
            divisor *= 10;
        }

        while divisor > 0 {
            let digit = (num / divisor) % 10;
            write!(inner, "{:02x}", digit + b'0' as u32)?;
            divisor /= 10;
        }

        write!(inner, "\r\n")?;
        inner.flush()?;

        let mut buf = [0; 128];
        unsafe { stream.read_with_mut(|it| it.read(&mut buf)) }.await?;
        if &buf[..2] != b"OK" { return Err(io::Error::other("sasl authentication failed")) }

        // begin the session, no more sasl messages will be send after this
    
        let inner = unsafe { stream.get_mut() };
        write!(inner, "BEGIN\r\n")?;

        // send the `Hello` message

        let hello = MethodCall::hello();

        let mut msg = hello.serialize();
        msg.serial = u32::MAX; // any valid serial works here
        let data = msg.serialize();

        inner.write_all(&data)?;
        inner.flush()?;

        // the reply will just be discarded later

        Ok(())

    }

    fn process_incoming(rt: &mut Reactor) -> Result<Incoming, ParseError> {

        let result = GenericMessage::deserialize(&rt.buf);

        match result {

            Ok((offset, mut msg)) => {

                // remove the data that was parsed
                rt.buf.drain(..offset);

                let incoming = match msg.kind {

                    MessageKind::Invalid => return Err(ParseError::Invalid),

                    MessageKind::Error => {
                        let serial: u32 = msg.take_field(FieldCode::ReplySerial)
                            .ok_or(ParseError::Invalid)?;
                        let err = MethodError::deserialize(msg)?;
                        Incoming::Reply(serial, Err(err))
                    },

                    MessageKind::MethodReply => {
                        let serial: u32 = msg.take_field(FieldCode::ReplySerial)
                            .ok_or(ParseError::Invalid)?;
                        let reply = MethodReply::deserialize(msg);
                        Incoming::Reply(serial, Ok(reply))
                    },

                    MessageKind::Signal => {
                        let signal = SignalTrigger::deserialize(msg)?;
                        Incoming::Signal(signal)
                    }

                    MessageKind::MethodCall => {
                        let call = MethodCall::deserialize(msg)?;
                        Incoming::Call(call)
                    },

                };

                Ok(incoming)

            }

            Err(ParseError::Partial) => Err(ParseError::Partial), // do nothing and wait for more data
            Err(other) => Err(other),

        }

    }

    async fn process_request(rt: &mut Reactor, request: Outgoing) -> io::Result<()> {

        match request {

            Outgoing::InitialAuth => {
                // lazily perform the auth
                initial_auth(&mut rt.bus).await?;
            }

            Outgoing::MethodCall { serial, payload } => {

                let mut msg = payload.serialize();
                msg.serial = serial;
                let data = msg.serialize();

                let stream = unsafe { rt.bus.get_mut() };
                stream.write_all(&data)?;

            },

            Outgoing::MethodCallVoid { payload } => {

                let mut msg = payload.serialize();
                msg.serial = u32::MAX;
                let data = msg.serialize();

                let stream = unsafe { rt.bus.get_mut() };
                stream.write_all(&data)?;

            },

            Outgoing::MethodReply { payload } => {

                let mut msg = payload.serialize();
                msg.serial = u32::MAX;
                let data = msg.serialize();

                let stream = unsafe { rt.bus.get_mut() };
                stream.write_all(&data)?;

            },

            Outgoing::MethodError { payload } => {

                let mut msg = payload.serialize();
                msg.serial = u32::MAX;
                let data = msg.serialize();

                let stream = unsafe { rt.bus.get_mut() };
                stream.write_all(&data)?;

            },

        }

        Ok(())
    
    }

    pub struct Connection {
        reactor: Reactor,
        /// handle to send requests
        requests: channel::Sender<Outgoing>,
    }

    impl Connection {

        pub fn new() -> io::Result<Self> {

            let addr = env::var("DBUS_SESSION_BUS_ADDRESS")
                .map_err(io::Error::other)?;

            let path = addr.strip_prefix("unix:path=")
                .ok_or(io::Error::other("invalid bus address format"))?;

            let stream = async_io::Async::new(
                UnixStream::connect(path)?
            )?;

            let actions = channel::bounded(4);

            // authenticate and send the hello message
            actions.0.try_send(Outgoing::InitialAuth).unwrap();

            let reactor = Reactor {
                bus: stream,
                actions: actions.1,
                serial: 1, // 0 is not allowed, so start at 1
                buf: Vec::new(),
            };

            Ok(Self {
                reactor,
                requests: actions.0,
            })
        
        }
    
        pub async fn next(&mut self) -> io::Result<Incoming> {
            self.reactor.run().await
        }

        pub async fn flush(&mut self) -> io::Result<()> {
            self.reactor.flush().await
        }

        pub fn method_call(&mut self, payload: MethodCall) -> OutgoingId {

            let serial = self.reactor.serial;
            self.reactor.serial += 1;
        
            let req = Outgoing::MethodCall { serial, payload };
            self.requests.try_send(req).unwrap();
            
            serial

        }

        /// Only use serials near u32::MAX. u32::MAX itself is reserved
        // TODO: ^^ express this thru a type
        pub fn method_call_with_serial(&mut self, payload: MethodCall, serial: u32) -> OutgoingId {
        
            let req = Outgoing::MethodCall { serial, payload };
            self.requests.try_send(req).unwrap();
            
            serial

        }

        pub fn method_reply(&mut self, result: MethodResult<MethodReply>) {
        
            match result {
                Ok(payload) => {
                    let req = Outgoing::MethodReply { payload };
                    self.requests.try_send(req).unwrap();
                },
                Err(payload) => {
                    let req = Outgoing::MethodError { payload };
                    self.requests.try_send(req).unwrap();
                },
            }

        }

        pub fn add_match(&mut self, matching: SignalMatch) {

            let call = MethodCall::add_match(matching.rule.clone());
            self.method_call(call);

        }

        pub fn remove_match(&mut self, matching: SignalMatch) {

            let call = MethodCall::remove_match(matching.rule.clone());
            self.method_call(call);

        }

        pub fn request_name<S1>(&mut self, name: S1)
            where S1: Into<String> {

            let call = MethodCall::request_name(
                name.into(),
                0x4 /* don't enqueue if already owned */
            );

            self.method_call(call);

            // let val: u32 = resp.arg(0);
            // if val >= 3 { return Err(RegisterError::AlreadyOwned) } TODO: reimplement AlreadyOwned error
        
        }

        pub fn release_name<S1>(&mut self, name: S1)
            where S1: Into<String> {

            let call = MethodCall::release_name(name.into());
            self.method_call(call);
        
        }

    }

    // fn hash_signal(path: &str, iface: &str, member: &str) -> u64 {
    //     let mut hasher = DefaultHasher::new();
    //     path.hash(&mut hasher);
    //     iface.hash(&mut hasher);
    //     member.hash(&mut hasher);
    //     hasher.finish()
    // }

    // fn hash_signal_origin(path: &str, iface: &str) -> u64 {
    //     hash_signal(path, iface, "")
    // }

    pub type OutgoingId = u32;

    #[derive(Debug)]
    pub struct MethodCall {
        pub dest: Cow<'static, str>,
        pub path: Cow<'static, str>,
        pub iface: Cow<'static, str>,
        pub member: Cow<'static, str>,
        pub no_reply: bool,
        pub allow_interactive_auth: bool,
        pub args: Vec<Arg>,
        caller: MethodCaller,
    }

    impl MethodCall {

        pub fn new<S1, S2, S3, S4>(dest: S1, path: S2, iface: S3, member: S4) -> Self
            where S1: Into<Cow<'static, str>>,
                  S2: Into<Cow<'static, str>>,
                  S3: Into<Cow<'static, str>>,
                  S4: Into<Cow<'static, str>> {

            Self {
                caller: MethodCaller::Ourselves,
                dest:   dest.into(),
                path:   path.into(),
                iface:  iface.into(),
                member: member.into(),
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
                caller: MethodCaller::Peer {
                    name: Cow::Owned(msg.take_field(FieldCode::Sender).ok_or(ParseError::Invalid)?),
                    serial: msg.serial,
                },
                dest:   Cow::Owned(msg.take_field(FieldCode::Dest)   .ok_or(ParseError::Invalid)?),
                path:   Cow::Owned(msg.take_field(FieldCode::ObjPath).ok_or(ParseError::Invalid)?),
                iface:  Cow::Owned(msg.take_field(FieldCode::Iface)  .ok_or(ParseError::Invalid)?),
                member: Cow::Owned(msg.take_field(FieldCode::Member) .ok_or(ParseError::Invalid)?),
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
    pub enum MatchKind {
        Specific,
        Originating,
    }

    #[derive(Debug, Clone)]
    pub struct SignalMatch {
        rule: String,
        // hash: u64,
    }

    impl SignalMatch {

        pub fn specific(path: &str, iface: &str, member: &str) -> Self {

            let rule = format!(
                "path={},interface={},member={}",
                path, iface, member
            );

            // let hash = hash_signal(path, iface, member);

            Self {
                rule,
                // hash
            }
        }

        pub fn originating(path: &str, iface: &str) -> Self {

            let rule = format!(
                "path={},interface={}",
                path, iface
            );

            // let hash = hash_signal_origin(path, iface);

            Self {
                rule,
                // hash
            }

        }

    }

    #[derive(Debug)]
    pub struct MethodReply {
        args: Vec<Option<Arg>>,
        caller: MethodCaller,
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

            if let MethodCaller::Peer { name, serial } = self.caller {
            
                msg.fields.push(header_field(FieldCode::ReplySerial, SimpleArg::U32(serial)));
                msg.fields.push(header_field(FieldCode::Dest, SimpleArg::String(name)));

            }

            msg

        }

        fn deserialize(msg: GenericMessage) -> Self {

            Self {
                caller: MethodCaller::Ourselves,
                args: msg.args.into_iter().map(Some).collect()
            }
        
        }
    
    }

    #[derive(Debug)]
    pub struct SignalTrigger {
        pub path: String,
        pub iface: String,
        pub member: String,
        args: Vec<Option<Arg>>,
    }

    impl SignalTrigger {

        pub fn new<S1, S2, S3>(path: S1, iface: S2, member: S3) -> Self
            where S1: Into<String>,
                  S2: Into<String>,
                  S3: Into<String> {

            Self {
                path: path.into(),
                iface: iface.into(),
                member: member.into(),
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

            Ok(Self {
                path: msg.take_field(FieldCode::ObjPath).ok_or(ParseError::Invalid)?,
                iface: msg.take_field(FieldCode::Iface).ok_or(ParseError::Invalid)?,
                member: msg.take_field(FieldCode::Member).ok_or(ParseError::Invalid)?,
                args: msg.args.into_iter().map(Some).collect()
            })
        
        }
    
    }

    pub enum Outgoing {
        InitialAuth,
        MethodCall     { serial: u32, payload: MethodCall },
        MethodCallVoid { payload: MethodCall },
        MethodReply    { payload: MethodReply },
        MethodError    { payload: MethodError },
    }

    pub enum Incoming {
        Reply(u32, MethodResult<MethodReply>),
        Signal(SignalTrigger),
        Call(MethodCall),
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
        ObjPath      = 1,
        Iface        = 2,
        Member       = 3,
        ErrName      = 4,
        ReplySerial  = 5,
        Dest         = 6,
        Sender       = 7,
        Signature    = 8,
        // NumFds       = 9,
    }

    // impl FieldCode {
    //     fn from_raw(num: u8) -> Option<Self> {
    //         match num {
    //             1 => Some(Self::ObjPath),
    //             2 => Some(Self::Iface),
    //             3 => Some(Self::Member),
    //             4 => Some(Self::ErrName),
    //             5 => Some(Self::ReplySerial),
    //             6 => Some(Self::Dest),
    //             7 => Some(Self::Sender),
    //             8 => Some(Self::Signature),
    //             9 => Some(Self::NumFds),
    //             _ => None
    //         }
    //     }
    // }

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
            Arg::serialize((endianess as u8).pack(), &mut buf);

            // message kind
            Arg::serialize((self.kind as u8).pack(), &mut buf);

            // flags, interactive authorization is enabled
            Arg::serialize((0x4u8).pack(), &mut buf);

            // version 1
            Arg::serialize(1u8.pack(), &mut buf);

            // body length, filled in later
            let idx = buf.data.len();
            Arg::serialize(0u32.pack(), &mut buf);

            // unique serial
            Arg::serialize(self.serial.pack(), &mut buf);

            // construct message signature

            let mut kinds = Vec::new();
            for it in self.args.iter() { skind(it, &mut kinds) };

            self.fields.push(header_field(FieldCode::Signature, SimpleArg::Signature(kinds)));

            // TODO: impl unix fds (also need to add a special header field!!!)

            // serialize header fields
            Arg::serialize(self.fields.pack(), &mut buf);

            // ### body ###

            buf.insert_pad(8); // body must be 8-byte aligned

            let orig = buf.data.len();

            for arg in self.args {
                Arg::serialize(arg, &mut buf);
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

            let raw: u8 = unpack(&mut buf)?;
            let endianess = char::from_u32(raw as u32).ok_or(ParseError::Invalid)?;
            buf.endianess = endianess; // correct the endianess

            let raw: u8 = unpack(&mut buf)?;
            let kind = MessageKind::from_raw(raw).ok_or(ParseError::Partial)?;

            let raw: u8 = unpack(&mut buf)?;
            let no_reply = raw & 0x1 == 1;
            let allow_interactive_auth = raw & 0x4 == 1;

            let proto_version: u8 = unpack(&mut buf)?;
            if proto_version != 1 { return Err(ParseError::Invalid) };

            let body_len: u32 = unpack(&mut buf)?;
            let serial: u32 = unpack(&mut buf)?;

            if buf.data.len() - buf.offset < body_len as usize {
                // no need to do more parsing, we haven't got enough data anyways
                return Err(ParseError::Partial);
            }
        
            let mut header_fields: Vec<(u8, Variant)> = unpack(&mut buf)?;

            let Signature(signature) = take_field(&mut header_fields, FieldCode::Signature)
                .unwrap_or_default();
        
            buf.consume_pad(8);

            let before_body = buf.offset;
            let mut args = Vec::with_capacity(2);
            for CompleteKind(kinds) in iter_complete_types(&signature) {
                let arg = Arg::deserialize(&mut buf, &kinds)?;
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

    // #### type util ####

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

    fn unpack<T: ValidArg>(buf: &mut DeserializeBuf) -> Result<T, ParseError> {
        T::unpack(Arg::deserialize(buf, &T::kinds())?).ok_or(ParseError::Invalid)
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
            let mut out = Vec::with_capacity(1);
            skind(self, &mut out);
            out
        }

        fn serialize(self, buf: &mut SerializeBuf) {

            fn align_from_signature(t: Vec<ArgKind>) -> usize {
                t[0].align()
            }

            match self {

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
                            Arg::serialize(arg, buf);
                        }

                        // update the size
                        let size = buf.data.len() - orig;

                        assert!(
                            size < (2usize.pow(26)),
                            "array length too long and disallowed by the spec ({} bytes)", size
                        );

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
                            Arg::serialize(Arg::Simple(key), buf);
                            Arg::serialize(val, buf);
                        }
                
                        let size = buf.data.len() - orig;
                        assert!(size < 2^26);
                        buf.data.splice(idx..idx + 4, (size as u32).to_ne_bytes());

                    },

                    CompoundArg::Variant(val) => {

                        // the signature string
                        let signature = val.kinds();
                        Arg::serialize(Arg::Simple(SimpleArg::Signature(signature)), buf);

                        // the argument
                        Arg::serialize(*val, buf);

                    }

                    CompoundArg::EmptyVariant => panic!("EmptyVariant cannot be sent"),
            
                    CompoundArg::Struct(fields) => {

                        buf.insert_pad(8);
                        for val in fields {
                            Arg::serialize(val, buf);
                        }

                    }
            
                }
            }

        }

        fn deserialize(buf: &mut DeserializeBuf, kinds: &[ArgKind]) -> Result<Self, ParseError> {

            // TOOD: make this more robust, eg. don't panic on out-of-bounds index
    
            macro_rules! parse {
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

            match kinds {

                [ArgKind::Byte] => {
                    buf.consume_pad(1);
                    let val = parse!(buf, u8);
                    Ok(Arg::Simple(SimpleArg::Byte(val)))
                },
        
                [ArgKind::Bool] => {
                    buf.consume_pad(4);
                    let val = parse!(buf, u32);
                    Ok(Arg::Simple(SimpleArg::Bool(val == 1)))
                },
        
                [ArgKind::I16] => {
                    buf.consume_pad(2);
                    let val = parse!(buf, i16);
                    Ok(Arg::Simple(SimpleArg::I16(val)))
                },
        
                [ArgKind::U16] => {
                    buf.consume_pad(2);
                    let val = parse!(buf, u16);
                    Ok(Arg::Simple(SimpleArg::U16(val)))
                },
        
                [ArgKind::I32] => {
                    buf.consume_pad(4);
                    let val = parse!(buf, i32);
                    Ok(Arg::Simple(SimpleArg::I32(val)))
                },
        
                [ArgKind::U32] => {
                    buf.consume_pad(4);
                    let val = parse!(buf, u32);
                    Ok(Arg::Simple(SimpleArg::U32(val)))
                },
        
                [ArgKind::I64] => {
                    buf.consume_pad(8);
                    let val = parse!(buf, i64);
                    Ok(Arg::Simple(SimpleArg::I64(val)))
                },
        
                [ArgKind::U64] => {
                    buf.consume_pad(8);
                    let val = parse!(buf, u64);
                    Ok(Arg::Simple(SimpleArg::U64(val)))
                },

                [ArgKind::String | ArgKind::ObjPath] => {
                    let len = unpack::<u32>(buf)? as usize;
                    let raw = buf.consume_bytes(len + 1).ok_or(ParseError::Partial)?; // +1 for the \0
                    let val = String::from_utf8(raw[..len].to_vec()).expect("verify utf8");
                    Ok(Arg::Simple(SimpleArg::String(Cow::Owned(val))))
                },

                [ArgKind::Signature] => {
                    let len = unpack::<u8>(buf)? as usize;
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

                    let len: u32 = unpack(buf)?;

                    let mut map = HashMap::new();

                    let start = buf.offset;
                    while buf.offset - start < len as usize {

                        buf.consume_pad(8); // dict entries are always 8-byte padded

                        let key = Self::deserialize(buf, &key_kinds)?;
                        let val = Self::deserialize(buf, &val_kinds)?;

                        let Arg::Simple(skey) = key else { panic!("a map key must not be a container") };

                        map.insert(skey, val);

                    }

                    Ok(Arg::Compound(CompoundArg::Map(key_kinds[0], val_kinds, map)))
            
                },

                [ArgKind::Array, contents @ ..] => {

                    let len: u32 = unpack(buf)?;

                    let mut args = Vec::new();
                    let start = buf.offset;
                    while buf.offset - start < len as usize {
                        let arg = Self::deserialize(buf, contents)?;
                        args.push(arg);
                    }

                    Ok(Arg::Compound(CompoundArg::Array(contents.to_vec(), args)))
            
                },

                [ArgKind::StructBegin,
                 contents @ ..,
                 ArgKind::StructEnd] => {

                    buf.consume_pad(8);

                    let mut args = Vec::new();
                    for CompleteKind(kinds) in iter_complete_types(contents) {
                        let arg = Self::deserialize(buf, &kinds)?;
                        args.push(arg);
                    }

                    Ok(Arg::Compound(CompoundArg::Struct(args)))
            
                },

                [ArgKind::Variant] => {

                    let Signature(kinds) = unpack(buf)?;

                    if kinds.len() > 0 {
                        let arg = Self::deserialize(buf, &kinds)?; // read the actual value
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

    }

    fn skind(arg: &Arg, out: &mut Vec<ArgKind>) {
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
                    SimpleArg::ObjPath(..)   => ArgKind::ObjPath,
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
        String(Cow<'static, str>),
        ObjPath(Cow<'static, str>),
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
        Map(ArgKind, Vec<ArgKind>, HashMap<SimpleArg, Arg>),
        Variant(Box<Arg>),
        EmptyVariant,
        Struct(Vec<Arg>),
    }

    pub trait ValidArg {
        fn pack(self) -> Arg where Self: Sized;
        fn unpack(arg: Arg) -> Option<Self> where Self: Sized;
        fn kinds() -> Vec<ArgKind>;
    }
    
    impl ValidArg for &'static str {
        fn pack(self) -> Arg {
            Arg::Simple(SimpleArg::String(Cow::Borrowed(self)))
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
            Arg::Simple(SimpleArg::String(Cow::Owned(self)))
        }
        fn unpack(arg: Arg) -> Option<Self> {
            if let Arg::Simple(SimpleArg::String(val)) = arg { Some(val.into_owned()) }
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
            Arg::Compound(CompoundArg::Variant(Box::new(val)))
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
        fn kinds() -> Vec<ArgKind> {
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
            let map = self.into_iter().map(|(k, v)|
                (as_simple_arg(k.pack()), v.pack())
            ).collect();
            Arg::Compound(CompoundArg::Map(tkey, tval, map))
        }
        fn unpack(arg: Arg) -> Option<Self> {
            if let Arg::Compound(CompoundArg::Map(.., items)) = arg {
                let mut out = HashMap::with_capacity(items.len());
                for (rkey, rval) in items {
                    let key = K::unpack(Arg::Simple(rkey))?;
                    let val = V::unpack(rval)?;
                    out.insert(key, val);
                }
                Some(out)
            } else {
                None
            }
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
        Invalid,
    }

    impl fmt::Display for ParseError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Self::Partial => unreachable!(),
                Self::Invalid => write!(f, "received invalid data which can't be read as any message"),
            }
        }
    }

    impl StdError for ParseError {}

    pub type RegisterResult<T> = Result<T, RegisterError>;

    #[derive(Debug)]
    pub enum RegisterError {
        AlreadyOwned,
        Other(MethodError),
    }

    pub type MethodResult<T> = Result<T, MethodError>;

    #[derive(Debug)]
    pub struct MethodError {
        caller: MethodCaller,
        pub kind: MethodErrorKind,
    }

    #[derive(Debug)]
    pub enum MethodErrorKind {
        /// this request was terminated because of a reactor i/o error
        Reactor,
        /// a object specific error
        Service { name: Cow<'static, str>, msg: Cow<'static, str> },
    }

    impl MethodError {

        pub fn new<S1, S2>(to: &MethodCall, name: S1, msg: S2) -> Self
            where S1: Into<Cow<'static, str>>,
                  S2: Into<Cow<'static, str>> {
            Self {
                caller: to.caller.clone(),
                kind: MethodErrorKind::Service {
                    name: name.into(),
                    msg: msg.into()
                }
            }
        }

        pub fn other<S1>(to: &MethodCall, msg: S1) -> Self
            where S1: Into<Cow<'static, str>> {
            Self {
                caller: to.caller.clone(),
                kind: MethodErrorKind::Service {
                    name: "lsg.other".into(),
                    msg: msg.into()
                }
            }
        }

        pub fn unimplemented(to: &MethodCall) -> Self {
            Self {
                caller: to.caller.clone(),
                kind: MethodErrorKind::Service {
                    name: "lsg.unimplemented".into(),
                    msg: "this member is not implemented, as of now".into(),
                }
            }
        }

        pub fn serialize(self) -> GenericMessage {

            let mut msg = GenericMessage::default();

            msg.kind = MessageKind::Error;

            if let MethodErrorKind::Service { name, msg: desc } = self.kind {

                msg.fields.push(header_field(FieldCode::ErrName, SimpleArg::String(name)));
                msg.args.push(Arg::Simple(SimpleArg::String(desc)));

            } else {
                panic!("cannot serialize \"custom\" MethodError");
            };

            // add reply information

            if let MethodCaller::Peer { name, serial } = self.caller {
            
                msg.fields.push(header_field(FieldCode::ReplySerial, SimpleArg::U32(serial)));
                msg.fields.push(header_field(FieldCode::Dest, SimpleArg::String(name)));

            }

            msg
        
        }

        pub fn deserialize(mut msg: GenericMessage) -> Result<Self, ParseError> {
            Ok(Self {
                caller: MethodCaller::Ourselves,
                kind: MethodErrorKind::Service {
                    name: Cow::Owned(msg.take_field(FieldCode::ErrName).ok_or(ParseError::Invalid)?),
                    msg: Cow::Owned(String::unpack(msg.args.remove(0)).ok_or(ParseError::Invalid)?),
                }
            })
        }

    }

    #[derive(Debug, Clone)]
    enum MethodCaller {
        Ourselves,
        Peer {
            name: Cow<'static, str>,
            serial: u32,
        }
    }

}

// ####### actual desktop-env interface implementation #######

#[test]
fn notifs() {

    async_io::block_on(async {

        // let con = Connection::new().unwrap();

        // let mut proxy = NotifyProxy::new(&mut con);

        // let notif = Notif::new("lsg-test (dbus)", "show me the magic")
        //     .body("some text in the body")
        //     .urgency(NotifUrgency::Critical)
        //     .action(NotifAction::default("default selection"))
        //     .action(NotifAction::new("a", "selection A"))
        //     .action(NotifAction::new("b", "selection B"));

        // proxy.send(notif).await.unwrap();

        // let (id, action) = proxy.action().await;
        // println!("invoked action: {}", action.name);

        // proxy.close(id).await;

    })

}

pub mod notif {
    
    use std::{collections::HashMap, time::Duration};

    use super::client::*;

    pub fn register_signals(con: &mut Connection) {

        let rule = SignalMatch::originating(
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
        );

        con.add_match(rule);
        
    }

    pub fn send(con: &mut Connection, notif: Notif<'_>) -> OutgoingId {

        let mut call = MethodCall::new(
            "org.freedesktop.Notifications",
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
            "Notify",
        );

        call.arg(notif.app.to_string()); // app name
        call.arg(notif.replaces.unwrap_or(0) as u32); // replaces id
        // call.arg(notif.icon.map(Icon::name).unwrap_or("")); // icon
        call.arg(""); // icon, TODO: implement icons using the ones from wayland.rs
        call.arg(notif.summary.to_string()); // summary
        call.arg(notif.body.unwrap_or("").to_string()); // body

        let actions: Vec<String> = notif.actions
            .into_iter()
            .map(|it| [it.name.to_string(), it.display.to_string()])
            .flatten()
            .collect();

        call.arg(actions); // actions

        let mut hints: HashMap<&str, Variant> = HashMap::new();
        if let Some(urgency) = notif.urgency {
            hints.insert("urgency", Variant::new(Arg::Simple(SimpleArg::Byte(urgency.num()))));
        }
        if let Some(category) = notif.category {
            hints.insert("category", Variant::new(Arg::Simple(SimpleArg::String(category.name().into()))));
        }

        call.arg(hints); // hints

        call.arg(notif.timeout.map(|dur|
            if dur == Duration::MAX { 0 }
            else { i32::try_from(dur.as_millis()).unwrap_or(i32::MAX) }
        ).unwrap_or(-1)); // expiration timeout
        
        con.method_call(call)
        
    }

    /// Close a notification.
    /// Should be invoked in all cases, eg. also when the user selected an action,
    /// since not all notification daemons dismiss a notificaition in that case.
    pub fn close(con: &mut Connection, id: NotifId) {

        let mut call = MethodCall::new(
            "org.freedesktop.Notifications",
            "/org/freedesktop/Notifications",
            "org.freedesktop.Notifications",
            "CloseNotification",
        );

        call.arg(id as u32);
        con.method_call(call);
        
    }

    pub type NotifId = u32;

    pub struct Notif<'a> {
        // required
        app: &'a str,
        summary: &'a str,
        // optional
        body: Option<&'a str>,
        replaces: Option<usize>,
        // icon: Option<Icon>, // TODO: add support for custom Icons (the same as used in the wayland code)
        actions: Vec<NotifAction<'a>>,
        timeout: Option<Duration>,
        // hints
        urgency: Option<NotifUrgency>,
        category: Option<Category>,
    }

    impl<'a> Notif<'a> {
        pub fn new(app: &'a str, summary: &'a str) -> Self {
            Self {
                app,
                summary,
                body: None,
                replaces: None,
                // icon: None,
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
        // pub fn icon(mut self, icon: Icon) -> Self {
        //     self.icon = Some(icon);
        //     self
        // }
        pub fn body(mut self, text: &'a str) -> Self {
            self.body = Some(text);
            self
        }
        pub fn action(mut self, action: NotifAction<'a>) -> Self {
            self.actions.push(action);
            self
        }
        pub fn timeout(mut self, timeout: Duration) -> Self {
            self.timeout = Some(timeout);
            self
        }
        pub fn urgency(mut self, urgency: NotifUrgency) -> Self {
            self.urgency = Some(urgency);
            self
        }
        pub fn category(mut self, category: Category) -> Self {
            self.category = Some(category);
            self
        }
    }

    #[derive(Debug)]
    pub struct NotifAction<'a> {
        pub name: &'a str,
        pub display: &'a str,
    }

    impl<'a> NotifAction<'a> {
        pub fn default(display: &'a str) -> Self {
            Self { name: "default", display }
        }
        pub fn new(name: &'a str, display: &'a str) -> Self {
            Self { name, display }
        }
    }

    /// An action that was invoked.
    ///
    /// The default action will have the id "default".
    /// If the user closed/dismissed the notification, the id will be "closed".
    /// You can check for these special values using the respective methods.
    #[derive(Debug)]
    pub struct InvokedNotifAction {
        pub name: String,
    }

    impl InvokedNotifAction {
        pub fn is_default(&self) -> bool {
            self.name == "default"
        }
        pub fn is_closed(&self) -> bool {
            self.name == "closed"
        }
    }

    pub enum Category {
        Device,              // device
        DeviceAdded,         // device.added
        DeviceError,         // device.error
        DeviceRemoved,       // device.removed
        Email,               // email
        EmailArrived,        // email.arrived
        EmailBounced,        // email.bounced
        Im,                  // im (NOTE: probably means instant message)
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

    pub enum NotifUrgency {
        Low,
        Normal,
        Critical
    }
    impl NotifUrgency {
        fn num(&self) -> u8 {
            match self {
                Self::Low => 0,
                Self::Normal => 1,
                Self::Critical => 2,
            }
        }
    }

}

// impl NotifyProxy {

//     pub fn new(con: &mut Connection) -> Self {

//         let rule = SignalMatch::originating(
//             "/org/freedesktop/Notifications",
//             "org.freedesktop.Notifications",
//         );

//         con.register(rule);

//         Self {}

//     }

//     /// Wait until an action is invoked.
//     /// If the notification is closed, a synthetic "close" action will be
//     /// generated.
//     pub async fn action(&mut self) -> (NotifId, InvokedNotifAction) {

//         // wait for a signal to arrive
//         let resp = self.listener.next().await;
//         let id: u32 = resp.arg(0);
//         let key = resp.get(1).unwrap_or("closed".into());

//         (
//             id,
//             InvokedNotifAction { name: key }
//         )
        
//     }

// }


