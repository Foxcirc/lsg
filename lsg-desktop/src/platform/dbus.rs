
use std::{
    any::type_name, collections::{hash_map::DefaultHasher, HashMap, HashSet}, convert::Infallible, env, hash::{Hash, Hasher}, io::{self, Read, Write}, iter, mem, os::{fd::{AsRawFd, OwnedFd}, unix::net::UnixStream}, sync::Arc, time::Duration
};

use async_lock::Mutex as AsyncMutex;
use futures_lite::{pin, FutureExt};

#[test]
fn call() {

    async_io::block_on(async {
    
        let con = Connection::new().unwrap();

        // let call = MethodCall::new(
        //      "org.freedesktop.DBus",
        //      "/org/freedesktop/DBus",
        //      "org.freedesktop.DBus.Debug.Stats",
        //      "GetStats",
        // );
        
        // let _resp = con.send(call).await.unwrap();
        //   ^^^^ parsing this is enough (maps, variants, etc. covered)

        con.reactor.run().await.unwrap();

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
    bus: async_io::Async<UnixStream>,
    /// current serial
    serial: u32,
    /// messages to send
    reqs: async_channel::Receiver<ClientMessage>,
    /// received responses to send back to the tasks
    resps: HashMap<Id, async_channel::Sender<MethodReply>>,
    /// received signals to send back to the tasks
    signals: async_broadcast::Sender<Arc<SignalTrigger>>,
    /// signals we are already listening on
    listening: HashSet<Id>,
    /// registered services
    services: HashMap<String, Service>,
    /// current message buffer
    buf: Vec<u8>,
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

            enum Either {
                /// We should executre this request
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

                let mut buf = [0; 1024];
                let mut read = 1;

                while read > 0 {
                    read = unsafe { guard.bus.read_with_mut(|it| it.read(&mut buf) ) }.await?;
                    guard.buf.extend_from_slice(&buf[..read]);
                }

                io::Result::Ok(Either::MoreData)

            };

            // BUG: tested some time ago!!!, on load, the `readable` future never resolves even though the connection
            // is readable. this seems to be a bug in async-io where a thread is incorrectly parked and never unparked or something
            // this is my conclusion because as soon as another system thread interacts with our program, the future resolves.
            // this bug also goes away if the `request` future is replaced by `pending().await`
            // the bug also happens when you replace the channel recv with future::ready, so it just always happens when the other future resolves (first?)

            match readable.or(request).await? {
                Either::Request(request) => process_request(&mut guard, request).await?,
                Either::MoreData => process_incoming(&mut guard)?,
            }

        }

    }
    
}

fn process_incoming(guard: &mut async_lock::MutexGuard<'_, BusData>) -> DbusResult<()> {

    let result = GenericMessage::deserialize(&guard.buf);
    if let Result::Ok((offset, msg)) = result {

        // remove the data that was parsed
        guard.buf.drain(..offset);

        match msg.kind {

            MessageKind::Invalid => unreachable!(),

            MessageKind::Error => {
                todo!("got error msg");
            },

            MessageKind::MethodReply => {

                let id = Id::Serial(msg.serial);
                let send = guard.resps.remove(&id).unwrap();

                let reply = MethodReply::from_generic_msg(msg);
                send.try_send(reply).unwrap();

            },

            MessageKind::Signal => {

                let signal = SignalTrigger::from_generic_msg(msg);
                guard.signals.try_broadcast(Arc::new(signal)).unwrap(); // wait for other tasks if full

            }

            MessageKind::MethodCall => {

                let call = MethodCall::from_generic_msg(msg);

                if let Some(service) = guard.services.get_mut(&call.dest) {

                    let result = service.handle(call);
                    let mut msg = result_to_generic_msg(result);
                    // don't need to set serial because we don't care about it

                    todo!("send reply");

                }
        
            },

        }

    } else if let Result::Err(ParseState::Error) = result {
        todo!("parse error");
    }

    Ok(())

}

fn result_to_generic_msg(result: Result<MethodReply, MethodError>) -> GenericMessage {
    match result {
        Ok(reply) => reply.to_generic_msg(),
        Err(err) => err.to_generic_msg(),
    }
}

async fn process_request(guard: &mut async_lock::MutexGuard<'_, BusData>, request: ClientMessage) -> DbusResult<()> {

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

            unsafe { guard.bus.read_with_mut(|it| {
                let mut buf = [0; 1024];
                it.read(&mut buf)?;
                if &buf[..2] == b"OK" { Ok(()) }
                else { Err(io::Error::other("sasl authentication failed")) }
            } ) }.await?;

            // begin the session, no more sasl messages will be send after this
            
            let stream = unsafe { guard.bus.get_mut() };
            write!(stream, "BEGIN\r\n")?;
            stream.flush()?;

            // send the `Hello` message

            let hello = MethodCall::hello();

            let mut msg = hello.to_generic_msg();
            msg.serial = 1; // any valid serial works here
            let data = msg.serialize();

            println!("{data:?}");

            stream.write_all(&data)?;
            stream.flush()?;
            
            unsafe { guard.bus.read_with_mut(|it| {
                let mut buf = [0; 2048];
                let offset = it.read(&mut buf)?;
                if offset == 0 { todo!("error: got disconnected") };
                println!("{:?}", String::from_utf8_lossy(&buf[..offset]));
                let result = GenericMessage::deserialize(&buf[..offset]).unwrap();
                dbg!(&result);
                Ok(())
            } ) }.await?;

        },

        ClientMessage::MethodCallVoid(call) => {

            let serial = guard.serial;
            guard.serial += 1;

            let mut msg = call.to_generic_msg();
            msg.serial = serial;
            let data = msg.serialize();

            let stream = unsafe { guard.bus.get_mut() };
            stream.write_all(&data)?;

        },

        ClientMessage::MethodCall(send, call) => {

            // TODO: this would be cool:
            // store senders in an arena that is reset (but not reallocated) everytime there are no more active senders left
            // then just use these pointers as the serial, no need for a hashmap anymore
            
            let serial = guard.serial;
            guard.serial += 1;
            guard.resps.insert(Id::Serial(serial), send);

            let mut msg = call.to_generic_msg();
            msg.serial = serial;
            let data = msg.serialize();

            let stream = unsafe { guard.bus.get_mut() };
            stream.write_all(&data)?;

        },

        ClientMessage::SignalMatch(signal) => {

            let hash = hash_signal(&signal.path, &signal.iface, &signal.member);
            let id = Id::SignalHash(hash);

            guard.listening.insert(id);

            todo!("add match on signal");
    
        },

        ClientMessage::AddService(mut service) => {

            println!("adding service {}", &service.name);

            todo!("AddService: request bus name");

            let mut paths = HashMap::new(); // unique paths

            for (info, iface) in &mut service.ifaces {

                // only add paths once but add the slot to all interfaces
                // with the same path

                paths.entry(&info.path)
                    .and_modify(|slot| {

                        // unsafe { sys::sd_bus_slot_ref(*slot) };
                        // iface.slot = *slot;

                    })
                    .or_insert_with(|| {
                        
                        todo!("register object path and get slot");

                        // iface.slot = slot; // don't need to ref it here
                        // slot

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
    signals: async_broadcast::Receiver<Arc<SignalTrigger>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Id {
    Serial(u32),
    SignalHash(u64)
}

// unsafe impl Send for DbusConnection {}
// unsafe impl Sync for DbusConnection {} TODO:

impl Connection {

    pub fn new() -> io::Result<Self> {

        let addr = env::var("DBUS_SESSION_BUS_ADDRESS")
            .map_err(io::Error::other)?;

        let path = addr.strip_prefix("unix:path=")
            .ok_or(io::Error::other("invalid bus address format"))?;

        let stream = UnixStream::connect(path)?;

        let reqs = async_channel::bounded(8);
        let mut signals = async_broadcast::broadcast(16);
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
                listening: HashSet::new(),
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
    
    pub async fn send(&self, call: MethodCall) -> DbusResult<MethodReply> {
        
        let (send, recv) = async_channel::bounded(1);
        let req = ClientMessage::MethodCall(send, call);
        self.reqs.try_send(req).unwrap();

        enum Either {
            Resp(MethodReply),
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

        let hash = hash_signal(&signal.path, &signal.iface, &signal.member);
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
    recv: async_broadcast::Receiver<Arc<SignalTrigger>>,
}

impl Listener {

    pub async fn next(&mut self) -> DbusResult<Arc<SignalTrigger>> {

        enum Either {
            Resp(Arc<SignalTrigger>),
            Reactor(DbusResult<Infallible>),
        }

        loop {

            let next = async { Either::Resp(self.recv.recv_direct().await.unwrap()) };
            let reactor = async { Either::Reactor(self.con.reactor.run().await) };

            match next.or(reactor).await {

                Either::Resp(resp) => {
                    let hash = hash_signal(&resp.path, &resp.iface, &resp.member);
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

fn serialize_arg(arg: Arg, buf: &mut Vec<u8>) {

    match arg {

        Arg::Empty => panic!("cannot serialize 'empty' arg"),

        // basic kind

        Arg::Simple(simple) => {

            match simple {

                // numbers
                SimpleArg::Byte(val)   => { buf.pad_to(1); buf.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::I16(val)    => { buf.pad_to(2); buf.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::U16(val)    => { buf.pad_to(2); buf.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::I32(val)    => { buf.pad_to(4); buf.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::U32(val)    => { buf.pad_to(4); buf.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::I64(val)    => { buf.pad_to(8); buf.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::U64(val)    => { buf.pad_to(8); buf.extend_from_slice(&val.to_ne_bytes()) },
                SimpleArg::Double(val) => { buf.pad_to(8); buf.extend_from_slice(&val.inner.to_ne_bytes()) },

                // other
                SimpleArg::Bool(val) => { buf.pad_to(4); buf.extend_from_slice(&(val as i32).to_ne_bytes()) },
                SimpleArg::Fd(val) => {
                    buf.pad_to(4);
                    buf.extend_from_slice(&val.inner.as_raw_fd().to_ne_bytes());
                    mem::forget(val); // don't `drop` the OwnedFd since we transfer ownership here
                },

                SimpleArg::String(val) | SimpleArg::ObjPath(val) => {
                    // the length of the string, in bytes (u32)
                    buf.pad_to(4);
                    buf.extend_from_slice(&(val.len() as u32).to_ne_bytes());
                    // the data
                    buf.extend_from_slice(val.as_bytes());
                    buf.extend_from_slice(&[0]); // zero-terminated
                },

                SimpleArg::Signature(val) => {
                    // the length of the string, in bytes (u8)
                    buf.extend_from_slice(&(val.len() as u8).to_ne_bytes());
                    // the data
                    buf.extend(val.iter().map(|it| *it as u8));
                    buf.extend_from_slice(&[0]); // zero-terminated
                }

            };

        },

        // container kinds

        Arg::Compound(compound) => match compound {

            CompoundArg::Array(t, items) => {

                // the length of the array, in bytes (u32)
                // this is a dummy value which is replaced later
                buf.pad_to(4);
                let idx = buf.len();
                buf.extend_from_slice(&0u32.to_ne_bytes());

                // the items
                buf.pad_to(signature_align(t)); // pad even if empty
                let orig = buf.len();
                for arg in items {
                    serialize_arg(arg, buf);
                }

                // update the size
                let size = buf.len() - orig;
                assert!(size < (2usize.pow(26)), "array length too long for dbus ({} bytes)", size);
                buf.splice(idx..idx + 4, (size as u32).to_ne_bytes());

            },

            CompoundArg::Map(.., map) => {

               // the length of the map, in bytes (u32)
                buf.pad_to(4);
                let idx = buf.len();
                buf.extend_from_slice(&(map.len() as u32).to_ne_bytes());

                buf.pad_to(8); // pad even if the map is empty
                let orig = buf.len();

                // the items
                for (key, val) in map {
                    buf.pad_to(8);
                    serialize_arg(Arg::Simple(key), buf);
                    serialize_arg(val, buf);
                }
                
                let size = buf.len() - orig;
                assert!(size < 2^26);
                buf.splice(idx..idx + 4, (size as u32).to_ne_bytes());

            },

            CompoundArg::Variant(val) => {

                // the signature string
                let signature = val.kinds();
                serialize_arg(Arg::Simple(SimpleArg::Signature(signature)), buf);

                // the argument
                serialize_arg(*val, buf);

            }
            
            CompoundArg::Struct(fields) => {

                buf.pad_to(8);
                for val in fields {
                    serialize_arg(val, buf);
                }

            }
            
        }
    };

}

fn signature_align(t: Vec<ArgKind>) -> usize {
    t[0].align()
}

macro_rules! deserialize_int {
    ($buf:ident, $endianess:expr, $typ:ident) => {
        {
            let size = mem::size_of::<$typ>();
            let bytes = $buf.consume_bytes(size)?.try_into().unwrap();
            if $endianess == 'l' { $typ::from_le_bytes(bytes) } else if $endianess == 'B' { $typ::from_be_bytes(bytes) } else { unreachable!() }
        }
    };
}

#[track_caller]
fn deserialize_arg(buf: &mut DeserializeBuf, kinds: &[ArgKind]) -> Option<Arg> {
    
    let res = match kinds {

        [ArgKind::Byte] => {
            buf.consume_pad(1);
            let val = deserialize_int!(buf, buf.endianess, u8);
            Some(Arg::Simple(SimpleArg::Byte(val)))
        },
        
        [ArgKind::Bool] => {
            buf.consume_pad(4);
            let val = deserialize_int!(buf, buf.endianess, u32);
            Some(Arg::Simple(SimpleArg::Bool(val == 1)))
        },
        
        [ArgKind::I16] => {
            buf.consume_pad(2);
            let val = deserialize_int!(buf, buf.endianess, i16);
            Some(Arg::Simple(SimpleArg::I16(val)))
        },
        
        [ArgKind::U16] => {
            buf.consume_pad(2);
            let val = deserialize_int!(buf, buf.endianess, u16);
            Some(Arg::Simple(SimpleArg::U16(val)))
        },
        
        [ArgKind::I32] => {
            buf.consume_pad(4);
            let val = deserialize_int!(buf, buf.endianess, i32);
            Some(Arg::Simple(SimpleArg::I32(val)))
        },
        
        [ArgKind::U32] => {
            buf.consume_pad(4);
            let val = deserialize_int!(buf, buf.endianess, u32);
            Some(Arg::Simple(SimpleArg::U32(val)))
        },
        
        [ArgKind::I64] => {
            buf.consume_pad(8);
            let val = deserialize_int!(buf, buf.endianess, i64);
            Some(Arg::Simple(SimpleArg::I64(val)))
        },
        
        [ArgKind::U64] => {
            buf.consume_pad(8);
            let val = deserialize_int!(buf, buf.endianess, u64);
            Some(Arg::Simple(SimpleArg::U64(val)))
        },

        [ArgKind::String | ArgKind::ObjPath] => {
            let len: u32 = unpack_arg(buf)?;
            let raw = buf.consume_bytes(len as usize)?; // ignores the trailing \0
            let val = String::from_utf8(raw.to_vec()).expect("verify utf8");
            Some(Arg::Simple(SimpleArg::String(val)))
        },

        [ArgKind::Signature] => {
            let len: u8 = unpack_arg(buf)?;
            let raw = buf.consume_bytes(len as usize)?; // ignores the trailing \0
            let val = raw.into_iter()
                .map(|it| ArgKind::deserialize(*it).expect("verify signature"))
                .collect();
            Some(Arg::Simple(SimpleArg::Signature(val)))
        },

        [ArgKind::Array, ArgKind::PairBegin, contents @ .., ArgKind::PairEnd] => {

            let mut iter = iter_complete_types(contents);

            let tkey = iter.next().expect("get first element of dict entry");
            let tval = iter.next().expect("get first element of dict entry");
            assert_eq!(tval.len(), 1); // key type must not be a container
            assert_eq!(iter.next(), None); // should only contain 2 complete types

            let len: u32 = unpack_arg(buf)?;

            let mut map = HashMap::new(); // TODO: arena

            let start = buf.offset;
            while buf.offset - start < len as usize {

                buf.consume_pad(8); // dict entries are always 8-byte padded

                let key = deserialize_arg(buf, &tkey)?;
                let val = deserialize_arg(buf, &tval)?;

                let Arg::Simple(skey) = key else { panic!("a map key must not be a container") };

                map.insert(skey, val);

            }

            Some(Arg::Compound(CompoundArg::Map(tkey[0], tval, map)))
            
        },

        [ArgKind::Array, contents @ ..] => {

            let len: u32 = unpack_arg(buf)?;

            let mut args = Vec::new();
            let start = buf.offset;
            while buf.offset - start < len as usize {
                let arg = deserialize_arg(buf, contents)?;
                args.push(arg);
            }

            Some(Arg::Compound(CompoundArg::Array(contents.to_vec(), args))) // TODO: arena
            
        },

        [ArgKind::StructBegin,
         contents @ ..,
         ArgKind::StructEnd] => {

            buf.consume_pad(8);

            let mut args = Vec::new();
            for kinds in iter_complete_types(contents) {
                let arg = deserialize_arg(buf, &kinds)?;
                args.push(arg);
            }

            Some(Arg::Compound(CompoundArg::Struct(args)))
            
        },

        [ArgKind::Variant] => {

            let Signature(kinds) = unpack_arg(buf)?;

            let arg = if kinds.len() > 0 {
                deserialize_arg(buf, &kinds)? // read the actual value
            } else {
                Arg::Empty
            };
           
            Some(Arg::Compound(CompoundArg::Variant(Box::new(arg))))
            
        },
        
        other => panic!("cannot deserialize signature {:?}", other),
        
    };

    res
    
}

fn iter_complete_types<'d>(contents: &'d [ArgKind]) -> impl Iterator<Item = Vec<ArgKind>> + 'd { // TODO return some newtype CompleteKind, to prevent dumb errors

    let mut outer = contents.iter();

    iter::from_fn(move || {

        enum State {
            OneMore,
            Struct,
        }

        let mut kinds = Vec::with_capacity(2);
        let mut state = State::OneMore;

        for kind in &mut outer {

            kinds.push(*kind);

            match state {
                State::OneMore => match kind {

                    // simple kinds
                    ArgKind::Byte      => break,
                    ArgKind::Bool      => break,
                    ArgKind::I16       => break,
                    ArgKind::U16       => break,
                    ArgKind::I32       => break,
                    ArgKind::U32       => break,
                    ArgKind::I64       => break,
                    ArgKind::U64       => break,
                    ArgKind::Double    => break,
                    ArgKind::String    => break,
                    ArgKind::ObjPath   => break,
                    ArgKind::Signature => break,
                    ArgKind::UnixFd    => break,
                    ArgKind::Variant   => break,

                    // array
                    ArgKind::Array => continue, // parse one more complete type

                    // structs
                    ArgKind::StructBegin => state = State::Struct,
                    ArgKind::StructEnd   => unreachable!(),

                    ArgKind::PairBegin => state = State::Struct,
                    ArgKind::PairEnd   => unreachable!(),

                    ArgKind::Pair   => panic!(),
                    ArgKind::Struct => panic!(),

                },

                State::Struct => if *kind == ArgKind::StructEnd ||
                                    *kind == ArgKind::PairEnd { break }

            }

        }

        if kinds.is_empty() {
            None
        } else {
            Some(kinds)
        }
    })

}

#[track_caller]
fn unpack_arg<T: ValidArg>(buf: &mut DeserializeBuf) -> Option<T> {
    T::unpack(deserialize_arg(buf, &T::kinds())?)
}

fn hash_signal<S: AsRef<[u8]>>(path: S, iface: S, member: S) -> u64 {
    let mut hasher = DefaultHasher::new();
    path.as_ref().hash(&mut hasher);
    iface.as_ref().hash(&mut hasher);
    member.as_ref().hash(&mut hasher);
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

    fn handle(&mut self, req: MethodCall) -> MethodResult<MethodReply> {

        dbg!(&req);

        let info = IfaceInfo {
            path: req.path.clone(), // TODO: not clone every time here
            name: req.iface.clone(),
        };

        if req.member == "Introspect" {
            
        }
        
        else if let Some(iface) = self.ifaces.get_mut(&info) {
            if let Some(exposed) = iface.exposed.get_mut(&req.member) {

                
            }
        };

        Err(MethodError::unimplemented("return reply to handeled method call"))
        
    }
    
}

enum Exposed {
    Property(Arg),
    Method(Box<dyn Fn(MethodCall) -> MethodReply>), // todo: arenas of dyn
}

#[derive(PartialEq, Eq, Hash)]
pub struct IfaceInfo {
    path: String,
    name: String,
}

pub struct Iface {
    exposed: HashMap<String, Exposed>,
}

impl Iface {

    pub fn new() -> Self {
        Self {
            exposed: HashMap::new(),
        }
    }

    pub fn property<T: ValidArg>(&mut self, name: &str, val: T) {
        self.exposed.insert(
            name.to_string(),
            Exposed::Property(val.pack())
        );
    }

    pub fn member<F: Fn(MethodCall) -> MethodReply + 'static>(&mut self, name: &str, cb: F) {
        self.exposed.insert(
            name.to_string(),
            Exposed::Method(Box::new(cb))
        );
    }
    
}

#[derive(Debug)] // TODO: derive debug for everything
pub struct MethodCall {
    dest: String, // todo: arena :(
    path: String,
    iface: String,
    member: String,
    args: Vec<Arg>,
}

impl MethodCall {

    pub fn new(dest: &str, path: &str, iface: &str, member: &str) -> Self {

        Self {
            dest: dest.to_string(), // Todo: arena
            path: path.to_string(),
            iface: iface.to_string(),
            member: member.to_string(),
            args: Vec::new(),
        }

    }
    
    pub fn arg<A: ValidArg>(&mut self, input: A) {
        self.args.push(input.pack());
    }

    pub(self) fn to_generic_msg(self) -> GenericMessage {

        let mut msg = GenericMessage::default();

        msg.kind = MessageKind::MethodCall;
        msg.args = self.args;

        msg.fields.push(header_field(FieldCode::Dest,    SimpleArg::String(self.dest)));
        msg.fields.push(header_field(FieldCode::ObjPath, SimpleArg::ObjPath(self.path)));
        msg.fields.push(header_field(FieldCode::Iface,   SimpleArg::String(self.iface)));
        msg.fields.push(header_field(FieldCode::Member,  SimpleArg::String(self.member)));
        
        msg

    }

    fn from_generic_msg(msg: GenericMessage) -> Self {
        todo!("deseal Methdod Call")
    }

    pub fn hello() -> Self {
        MethodCall::new(
             "org.freedesktop.DBus",
             "/org/freedesktop/DBus",
             "org.freedesktop.DBus",
             "Hello",
        )
    }
    
}

pub struct SignalMatch {
    path: String,
    iface: String,
    member: String,
}

impl SignalMatch {

    pub fn new(path: &str, iface: &str, member: &str) -> Self {
        Self {
            path: path.to_string(), // TODO: arena
            iface: iface.to_string(),
            member: member.to_string(),
        }
    }

}

#[derive(Debug)]
pub struct MethodReply {
    args: Vec<Option<Arg>>,
}

impl MethodReply {

    pub fn new() -> Self {
        Self {
            args: Vec::new()
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
        T::unpack(arg).ok_or(ArgError::InvalidType)
    }

    fn from_generic_msg(msg: GenericMessage) -> MethodReply {
        todo!("reply from generic msg")
    }

    pub(self) fn to_generic_msg(&self) -> GenericMessage {
        todo!("reply to generic msg")
    }
    
}

pub struct SignalTrigger {
    path: String, // todo: arena
    iface: String,
    member: String,
    args: Vec<Option<Arg>>,
}

impl SignalTrigger {

    pub fn new(path: &str, iface: &str, member: &str) -> Self {

        Self {
            path: path.to_string(),
            iface: iface.to_string(),
            member: member.to_string(),
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
        T::unpack(arg).ok_or(ArgError::InvalidType)
    }

    fn from_generic_msg(msg: GenericMessage) -> SignalTrigger {
        todo!("signal trigger from generic msg");
    }
    
}

enum ClientMessage {
    /// authenticate the client (send lazily on startup)
    Authenticate,
    MethodCall(async_channel::Sender<MethodReply>, MethodCall),
    MethodCallVoid(MethodCall),
    SignalMatch(SignalMatch),
    AddService(Service),
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
    pub fields: Vec<(u8, Arg)>, // header fields
    pub args: Vec<Arg>, // message body
}

impl GenericMessage {

    pub fn serialize(mut self) -> Vec<u8> {

        let mut buf = Vec::with_capacity(1024);

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
        let idx = buf.len();
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

        buf.pad_to(8); // body must be 8-byte aligned

        let orig = buf.len();

        for arg in self.args {
            serialize_arg(arg, &mut buf);
        }

        // update the body len
        let size = buf.len() - orig;
        buf.splice(idx..idx + 4, (size as u32).to_ne_bytes());

        buf

    }

    pub fn deserialize(data: &[u8]) -> Result<(usize, GenericMessage), ParseState> {

        let mut buf = DeserializeBuf {
            data,
            offset: 0,
            endianess: 'l', // is overwritten with the first byte read
        };

        let raw: u8 = unpack_arg(&mut buf).ok_or(ParseState::Partial)?;
        let endianess = char::from_u32(raw as u32).ok_or(ParseState::Error)?;
        buf.endianess = endianess; // correct the endianess

        let raw: u8 = unpack_arg(&mut buf).ok_or(ParseState::Partial)?;
        let kind = MessageKind::from_raw(raw).ok_or(ParseState::Partial)?; // TODO: return error instead of panicking everywhere (whole library)

        let raw: u8 = unpack_arg(&mut buf).ok_or(ParseState::Partial)?;
        let no_reply = raw & 0x1 == 1;
        let allow_interactive_auth = raw & 0x4 == 1;

        let proto_version: u8 = unpack_arg(&mut buf).ok_or(ParseState::Partial)?;
        assert!(proto_version == 1); // TODO: dont panick but error gracefully (see above) ^^

        let body_len: u32 = unpack_arg(&mut buf).ok_or(ParseState::Partial)?;
        let serial: u32 = unpack_arg(&mut buf).ok_or(ParseState::Partial)?;

        if buf.data.len() - buf.offset < body_len as usize {
            // no need to do more parsing, we haven't got enough data anyways
            return Err(ParseState::Partial);
        }
        
        let mut header_fields: Vec<(u8, Arg)> = unpack_arg(&mut buf).ok_or(ParseState::Partial)?;

        buf.consume_pad(8);

        let Signature(signature) = header_fields.iter()
            .position(|(kind, ..)| *kind == 8)
            .map(|idx| header_fields.swap_remove(idx))
            .and_then(|(.., val)| Signature::unpack(val))
            .ok_or(ParseState::Error)?;

        let mut args = Vec::with_capacity(2);
        for kinds in iter_complete_types(&signature) {
            let arg = deserialize_arg(&mut buf, &kinds).ok_or(ParseState::Partial)?;
            args.push(arg);
        }

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

}

struct DeserializeBuf<'a> {
    data: &'a [u8],
    offset: usize,
    endianess: char,
}
impl DeserializeBuf<'_> {

    #[track_caller]
    fn consume_pad(&mut self, padding: usize) {
        let needed = (padding - self.offset % padding) % padding;
        self.consume_bytes(needed);
    }

    #[track_caller]
    fn consume_bytes(&mut self, len: usize) -> Option<&[u8]> {
        if len <= self.data.len() {
            let (bytes, rest) = self.data.split_at(len); // TODO: replace with split_at_checked when it becomes stabilzed
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

// #[derive(Default)]
// pub struct Message {
//     // used while parsing
//     pub buffer: Vec<u8>, // TODO: fuck fuck fuck
//     // general infos
//     pub sender: String,
//     pub dest: String,
//     pub path: String,
//     pub iface: String,
//     pub member: String,
//     pub errname: String,
//     pub args: Vec<Arg>, // todo: arena
//     pub interactive: bool,
//     // implementation details
//     pub serial: u32,
//     pub rserial: u32, // reṕly serial
// }
   
    // #[track_caller]
    // pub fn arg<'a, T: ValidArg<'a>>(&'a self, idx: usize) -> T {
    //     match self.get(idx) {
    //         Ok(val) => val,
    //         Err(err) => panic!("cannot read arg #{idx}: {err:?}")
    //     }
    // }

    // pub fn get<'a, T: ValidArg<'a>>(&'a self, idx: usize) -> Result<T, ArgError> {
    //     let arg = self.args.get(idx).ok_or(ArgError::DoesntExist)?;
    //     T::unpack(arg).ok_or(ArgError::InvalidType)
    // }

fn header_field(code: FieldCode, arg: SimpleArg) -> (u8, Arg) {
    (code as u8, Arg::Simple(arg))
}

// fn darg(msg: *mut sys::SdMessage, kind: sys::SdBasicKind, contents: *const sys::SdBasicKind) -> ParseResult {
    
//     match kind {

//         // basic kinds

//         sys::SdBasicKind::Null => {
//             unreachable!();
//         },
//         sys::SdBasicKind::Byte => {
//             let mut val: u8 = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u8 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::Byte(val))))
//         },
//         sys::SdBasicKind::Bool => {
//             let mut val: ffi::c_int = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i32 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::Bool(val == 1))))
//         },
//         sys::SdBasicKind::I16 => {
//             let mut val: i16 = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i16 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::I16(val))))
//         },
//         sys::SdBasicKind::U16 => {
//             let mut val: u16 = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u16 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::U16(val))))
//         },
//         sys::SdBasicKind::I32 => {
//             let mut val: i32 = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i32 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::I32(val))))
//         },
//         sys::SdBasicKind::U32 => {
//             let mut val: u32 = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u32 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::U32(val))))
//         },
//         sys::SdBasicKind::I64 => {
//             let mut val: i64 = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut i64 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::I64(val))))
//         },
//         sys::SdBasicKind::U64 => {
//             let mut val: u64 = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut u64 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::U64(val))))
//         },
//         sys::SdBasicKind::Double => {
//             let mut val: f64 = 0.0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut f64 as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::Double(val.to_bits()))))
//         },
//         sys::SdBasicKind::String |
//         sys::SdBasicKind::Signature |
//         sys::SdBasicKind::ObjPath => {
//             let mut val: *const ffi::c_char = ptr::null();
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut *const ffi::c_char as _) }.ok()?;
//             assert!(!val.is_null());
//             let cstr = unsafe { CStr::from_ptr(val) };
//             Ok(Some(RawArg::Simple(SimpleRawArg::String(cstr))))
//         },
//         sys::SdBasicKind::UnixFd => {
//             let mut val: RawFd = 0;
//             unsafe { sys::sd_bus_message_read_basic(msg, kind, (&mut val) as *mut RawFd as _) }.ok()?;
//             Ok(Some(RawArg::Simple(SimpleRawArg::UnixFd(val))))
//         },

//         // compound kinds

//         sys::SdBasicKind::Array => {

//             if let sys::SdBasicKind::PairBegin = unsafe { *contents } {

//                 // this is a map

//                 let mut map: HashMap<SimpleRawArg, RawArg> = HashMap::new();

//                 // enter the array
//                 unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok()?;
            
//                 // TODO: remove all the debug unwraps and cascade the error

//                 loop {

//                     // unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok().unwrap();

//                     let Some((kind, contents)) = peek_kind(msg) else { break };
//                     assert!(matches!(kind, sys::SdBasicKind::Pair));

//                     // enter the pair
//                     unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok()?;

//                     let (kind, contents) = peek_kind(msg).ok_or(ParseError::eof())?;
//                     let key = read_arg(msg, kind, ptr::null())?.ok_or(ParseError::eof())?;
//                     assert!(contents.is_null()); // cannot have contents since this must be a basic value

//                     let (kind, contents) = peek_kind(msg).ok_or(ParseError::eof())?;
//                     let val = read_arg(msg, kind, contents)?.ok_or(ParseError::eof())?;

//                     let RawArg::Simple(key) = key else { return Err(ParseError::protocol()) };
//                     map.insert(key, val);

//                     unsafe { sys::sd_bus_message_exit_container(msg) }.ok()?;
//                     unsafe { sys::sd_bus_message_skip(msg, ptr::null()) }; // consume the "pair"

//                 };

//                 unsafe { sys::sd_bus_message_exit_container(msg) }.ok()?;
//                 // TODO: do we need to skip the map here?

//                 Ok(Some(RawArg::Compound(CompoundRawArg::Map(map))))
                
//             } else {

//                 // this is an array

//                 let mut args = Vec::new();
                
//                 unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok()?;
            
//                 loop {

//                     let Some((kind, contents)) = peek_kind(msg) else { break };
//                     let arg = read_arg(msg, kind, contents)?.ok_or(ParseError::eof())?;
//                     args.push(arg);

//                 };

//                 unsafe { sys::sd_bus_message_exit_container(msg) }.ok()?;

//                 Ok(Some(RawArg::Compound(CompoundRawArg::Array(args))))
                
//             }

//         },
//         sys::SdBasicKind::Struct |
//         sys::SdBasicKind::Variant => {

//             unsafe { sys::sd_bus_message_enter_container(msg, kind, contents) }.ok()?;

//             let mut args = Vec::new();

//             loop {

//                 // TODO: there seems to be a case where read_arg expects contents to be non-null without asserting it

//                 let Some((kind, contents)) = peek_kind(msg) else { break };
//                 let arg = read_arg(msg, kind, contents)?.ok_or(ParseError::eof())?;
//                 args.push(arg);
                
//             }

//             unsafe { sys::sd_bus_message_exit_container(msg) }.ok()?;
//             unsafe { sys::sd_bus_message_skip(msg, ptr::null()) }; // consume the "variant"

//             Ok(Some(RawArg::Compound(CompoundRawArg::Struct(args))))

//         },
//         sys::SdBasicKind::Pair        => { unreachable!() },
//         sys::SdBasicKind::StructBegin => { unreachable!(); },
//         sys::SdBasicKind::StructEnd   => { unreachable!(); },
//         sys::SdBasicKind::PairBegin   => { Ok(Some(RawArg::Ignore)) },
//         sys::SdBasicKind::PairEnd     => { Ok(Some(RawArg::Ignore)) },

//     }

// }

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
    InvalidType,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Arg {
    Simple(SimpleArg),
    Compound(CompoundArg),
    Empty,
}

impl Arg {

    fn kinds(&self) -> Vec<ArgKind> {
        let mut out = Vec::new(); // TODO: not use Vec since it can't be more then 5 entries anyways
        skind(self, &mut out);
        out
    }

    fn align(&self) -> usize {
        self.kinds()[0].align() // TODO: not use vec in `kinds` so this is efficient
    }

}

fn skind(arg: &Arg, out: &mut Vec<ArgKind>) { // TODO: think about the relation of this and "ValidArg::kinds"
    match arg {
        Arg::Empty => panic!("cannot serialize kind of empty arg"),
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
    Double(PanicOnEq<f64>),
    String(String),
    ObjPath(String),
    Signature(Vec<ArgKind>),
    Fd(OpsOwnedFd),
}

#[derive(Debug)]
struct PanicOnEq<T> {
    pub inner: T
}

impl<T> PartialEq for PanicOnEq<T> {
    fn eq(&self, _: &Self) -> bool {
        let name = type_name::<T>();
        panic!(
            "the type {:?} cannot be compared in this case,
             you can't use it in a PartialEq/Eq context",
            name
        );
    }
}

impl<T> Eq for PanicOnEq<T> {}

impl<T> Hash for PanicOnEq<T> {
    fn hash<H: Hasher>(&self, _: &mut H) {
        let name = type_name::<T>();
        panic!(
            "the type {:?} cannot be hashed in this case,
             you can't use it in a Hash context",
            name
        );
    }
}

#[derive(Debug)]
struct OpsOwnedFd {
    pub inner: OwnedFd,
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

#[derive(Debug, PartialEq, Eq)]
pub enum CompoundArg {
    Array(Vec<ArgKind>, Vec<Arg>),
    Map(ArgKind , Vec<ArgKind>, HashMap<SimpleArg, Arg>),
    Variant(Box<Arg>),
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

struct Signature(Vec<ArgKind>);

impl ValidArg for Signature {
    fn pack(self) -> Arg {
        Arg::Simple(SimpleArg::Signature(self.0))
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
        Arg::Simple(SimpleArg::Double(PanicOnEq { inner: self }))
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

impl ValidArg for Arg { // variadic arg, TODO: Variadic<Arg> type for less errors
    fn pack(self) -> Arg {
        Arg::Compound(CompoundArg::Variant(Box::new(self))) // TODO: arenaaaaa
    }
    fn unpack(arg: Arg) -> Option<Self> {
        Some(arg)
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
    }
    fn kinds() -> Vec<ArgKind> {
        vec![ArgKind::Array]
    }
}

// impl<'a, T0: ValidArg<'a>, T1: ValidArg<'a>> ValidArg<'a> for (T0, T1) {
//     fn pack(self) -> Arg {
//         let t0 = self.0.pack();
//         let t1 = self.1.pack();
//         Arg::Compound(CompoundArg::Struct(vec![t0, t1]))
//     }
//     fn unpack(arg: &'a Arg) -> Option<Self> {
//         if let Arg::Compound(CompoundArg::Struct(fields)) = arg {
//             let t0 = T0::unpack(&fields[0])?;
//             let t1 = T1::unpack(&fields[1])?;
//             Some((t0, t1))
//         }
//         else { None }
//     }
//     fn kind() -> ArgKind {
//         ArgKind::Struct
//     }
// }

/// Implement ValidArg for tuple types, which are packed into a dbus `struct`.
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

// TODO: seperate SimpleArg and CompoundArg traits.... this is kinda important

fn as_simple_arg(arg: Arg) -> SimpleArg {
    if let Arg::Simple(val) = arg { val }
    else { unreachable!() }
}

#[derive(Debug)]
pub enum ParseState {
    Partial,
    Error,
}

#[derive(Debug)]
pub enum DbusError {
    Dbus(io::Error),
    Parse(()), // TODO: parse error type
}

impl DbusError {
    fn invalid_arguments() -> Self {
        Self::Dbus(io::Error::new(
            io::ErrorKind::InvalidInput,
            "invalid dbus parameter (eg. object path not starting with `/`)"
        ))
    }
}

pub type MethodResult<T> = Result<T, MethodError>;

/// An error reply.
pub struct MethodError {
    name: String,
    msg: String,
}

impl MethodError {

    pub fn unimplemented(msg: &str) -> Self {
        Self::new("lsg.unimplemented", msg)
    }

    pub fn new(name: &str, msg: &str) -> Self {
        Self { name: name.to_string(), msg: msg.to_string() } // TODO: arena
    }

    pub fn to_generic_msg(self) -> GenericMessage {

        let mut msg = GenericMessage::default();

        msg.kind = MessageKind::Error;

        msg.fields.push(header_field(FieldCode::ErrName, SimpleArg::String(self.name)));
        msg.args.push(Arg::Simple(SimpleArg::String(self.msg)));

        msg
        
    }

}

pub type DbusResult<T> = Result<T, DbusError>;

impl From<io::Error> for DbusError {
    fn from(value: io::Error) -> Self {
        Self::Dbus(value)
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
        
        let mut resp = self.con.send(call).await?;
        let id: u32 = resp.arg(0);

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
            todo!(); // TODO: implement Clone for Arg and not use Arc but clone it for signals
            // let event: u32 = resp.arg(0);
            // key = resp.arg(1);
            // if event == id { break };
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

trait PadTo { // TODO: newtype instead of trait
    fn pad_to(&mut self, padding: usize);
}

impl PadTo for Vec<u8> {
    fn pad_to(&mut self, padding: usize) {
        let needed = (padding - self.len() % padding) % padding;
        self.resize(self.len() + needed, 0);
    }
}

// mod sys {

//     use std::{ffi, io};

//     #[repr(transparent)]
//     pub struct SdBus {
//         inner: ffi::c_void
//     }

//     #[repr(transparent)]
//     pub struct SdSlot {
//         inner: ffi::c_void
//     }

//     #[repr(transparent)]
//     pub struct SdMessage {
//         inner: ffi::c_void
//     }

//     #[repr(C)]
//     pub struct SdError {
//         /// can be null
//         pub name: *mut ffi::c_char,
//         /// can be null
//         pub msg: *mut ffi::c_char,
//         /// more unspecified fields, uuhh this is unsafe
//         _need_free: i32, // whatever, it's internal
//     }

//     #[derive(Clone, Copy)]
//     #[repr(transparent)]
//     pub struct SdResult {
//         inner: ffi::c_int
//     }

//     impl SdResult {

//         pub const OK: Self = Self::new(0);
//         pub const ERR: Self = Self::new(-1);

//         pub const fn new(code: i32) -> Self {
//             Self { inner: code }
//         }

//         pub fn io(err: io::Error) -> Self {
//             Self { inner: - err.raw_os_error().unwrap_or(1) }
//         }

//         pub fn ok(&self) -> Result<u32, io::Error> {

//             if self.inner < 0 {
//                 Err(io::Error::from_raw_os_error(-self.inner))
//             } else {
//                 Ok(self.inner as u32)
//             }

//         }

//     }

//     #[derive(Debug, Clone, Copy)]
//     #[repr(u64)]
//     pub enum SdDumpKind { // this should've been a bool honestly
//         WithHeader  = 1 << 0,
//         SubtreeOnly = 1 << 1,
//     }

//     #[derive(Debug, Clone, Copy)]
//     #[repr(u8)]
//     pub enum SdMessageKind { // i do not fear UB
//         Null = 0,
//         MethodCall = 1,
//         Response = 2,
//         Error = 3,
//         Signal = 4,
//     }

//     #[derive(Debug, Clone, Copy)]
//     #[repr(i32)]
//     pub enum SdHandlerStatus { // i do not fear UB
//         /// Use the `out` argument to return a more precise error.
//         Error = -1,
//         /// Also call other handlers.
//         Forward = 0,
//         /// Don't call other handlers.
//         Consume = 1,
//     }

//     pub type SdMessageFilter = extern fn(
//         msg: *mut SdMessage,
//         data: *mut ffi::c_void,
//         out: *mut SdError
//     ) -> SdHandlerStatus;
    
//     pub type SdMessageHandler = extern fn(
//         msg: *mut SdMessage,
//         data: *mut ffi::c_void,
//         out: *mut SdError
//     ) -> SdResult;
    
//     #[derive(Clone, Copy, Debug, PartialEq, Eq)]
//     #[repr(i8)] // ffi::c_char, i do not fear UB
//     pub enum SdBasicKind { // i do not fear UB
//         // invalid
//         Null  = 0,
//         // simple types
//         Byte      = 'y' as u32 as i8, // uint8_t *
//         Bool      = 'b' as u32 as i8, // int * (not bool *)
//         I16       = 'n' as u32 as i8, // int16_t *
//         U16       = 'q' as u32 as i8, // uint16_t *
//         I32       = 'i' as u32 as i8, // int32_t *
//         U32       = 'u' as u32 as i8, // uint32_t *
//         I64       = 'x' as u32 as i8, // int64_t *
//         U64       = 't' as u32 as i8, // uint64_t *
//         Double    = 'd' as u32 as i8, // double *
//         String    = 's' as u32 as i8, // const char **
//         ObjPath   = 'o' as u32 as i8, // const char **
//         Signature = 'g' as u32 as i8, // const char **
//         UnixFd    = 'h' as u32 as i8, // int *
//         // compound types
//         Array       = 'a' as u32 as i8, // int (len), ... (items)
//         Variant     = 'v' as u32 as i8, // const char* (signature), any
//         StructBegin = '(' as u32 as i8, // ... (items)
//         StructEnd   = ')' as u32 as i8, // 
//         PairBegin   = '{' as u32 as i8, // any, any (a pair)
//         PairEnd     = '}' as u32 as i8, // 
//         Pair        = 'e' as u32 as i8, // dict entry phantom type
//         Struct      = 'r' as u32 as i8, // struct phantom type
//     }

//     extern {

//         // bus

//         pub fn sd_bus_open_user(out: &mut *mut SdBus) -> SdResult;

//         pub fn sd_bus_unref(bus: *mut SdBus) -> *mut SdBus;

//         pub fn sd_bus_flush(bus: *mut SdBus) -> SdResult;
//         pub fn sd_bus_close(bus: *mut SdBus) -> SdResult;

//         pub fn sd_bus_get_fd(bus: *mut SdBus) -> SdResult;
//         pub fn sd_bus_get_events(bus: *mut SdBus) -> SdResult;
//         pub fn sd_bus_process(bus: *mut SdBus, out: &mut *mut SdMessage) -> SdResult;
//         pub fn sd_bus_wait(bus: *mut SdBus, timeout: u64) -> SdResult;

//         // services

//         pub fn sd_bus_request_name(bus: *mut SdBus, name: *const ffi::c_char, flags: u64) -> SdResult;
//         pub fn sd_bus_release_name(bus: *mut SdBus, name: *const ffi::c_char) -> SdResult;
        
//         pub fn sd_bus_add_object(
//             bus: *mut SdBus,
//             out: &mut *mut SdSlot,
//             // path
//             path: *const ffi::c_char,
//             // cb
//             callback: SdMessageHandler,
//             data: *mut ffi::c_void,
//         );

//         // signals

//         pub fn sd_bus_match_signal(
//             // obj
//             bus: *mut SdBus, out: &mut *mut SdSlot,
//             // dest
//             sender: *const ffi::c_char, path: *const ffi::c_char,
//             interface: *const ffi::c_char, member: *const ffi::c_char,
//             // cb
//             callback: SdMessageFilter,
//             data: *mut ffi::c_void
//         ) -> SdResult;

//         // message

//         pub fn sd_bus_send(bus: *mut SdBus, msg: *mut SdMessage, cookie: &mut u64) -> SdResult;

//         pub fn sd_bus_message_new_member_call(
//             // obj
//             bus: *mut SdBus, out: &mut *mut SdMessage,
//             // dest
//             dest: *const ffi::c_char, path: *const ffi::c_char,
//             iface: *const ffi::c_char, member: *const ffi::c_char
//         ) -> SdResult;

//         pub fn sd_bus_message_new_member_return(
//             call: *mut SdMessage, out: &mut *mut SdMessage,
//         ) -> SdResult;

//         pub fn sd_bus_message_new_member_error(
//             call: *mut SdMessage, out: &mut *mut SdMessage, err: *const SdError,
//         ) -> SdResult;

//         pub fn sd_bus_message_ref(msg: *mut SdMessage) -> *mut SdMessage;
//         pub fn sd_bus_message_unref(msg: *mut SdMessage) -> *mut SdMessage;

//         pub fn sd_bus_message_dump(msg: *mut SdMessage, out: *mut ffi::c_void /* libc::FILE, pass NULL for stdout */, kind: SdDumpKind) -> SdResult;
//         pub fn sd_bus_message_seal(msg: *mut SdMessage, cookie: u64, timeout_usec: u64) -> SdResult;

//         pub fn sd_bus_message_get_type(msg: *mut SdMessage, out: &mut SdMessageKind) -> SdResult;
//         pub fn sd_bus_message_get_reply_cookie(msg: *mut SdMessage, out: &mut u64) -> SdResult;
//         pub fn sd_bus_message_get_error(msg: *mut SdMessage) -> *mut SdError;

//         pub fn sd_bus_message_get_sender(msg: *mut SdMessage) -> *const ffi::c_char;
//         pub fn sd_bus_message_get_destination(msg: *mut SdMessage) -> *const ffi::c_char;
//         pub fn sd_bus_message_get_path(msg: *mut SdMessage) -> *const ffi::c_char;
//         pub fn sd_bus_message_get_interface(msg: *mut SdMessage) -> *const ffi::c_char;
//         pub fn sd_bus_message_get_member(msg: *mut SdMessage) -> *const ffi::c_char;

//         pub fn sd_bus_message_peek_type(msg: *mut SdMessage, kind: &mut SdBasicKind, contents: &mut *mut SdBasicKind) -> SdResult;
//         pub fn sd_bus_message_skip(msg: *mut SdMessage, kinds: *const SdBasicKind /* null-terminated array */) -> SdResult;
//         pub fn sd_bus_message_read_basic(msg: *mut SdMessage, kind: SdBasicKind, out: *mut ffi::c_void) -> SdResult;
//         pub fn sd_bus_message_enter_container(msg: *mut SdMessage, kind: SdBasicKind, contents: *const SdBasicKind) -> SdResult;
//         pub fn sd_bus_message_exit_container(msg: *mut SdMessage) -> SdResult;

//         pub fn sd_bus_message_append_basic(msg: *mut SdMessage, kind: SdBasicKind, out: *const ffi::c_void) -> SdResult;
//         pub fn sd_bus_message_open_container(msg: *mut SdMessage, kind: SdBasicKind, contents: *const SdBasicKind) -> SdResult;
//         pub fn sd_bus_message_close_container(msg: *mut SdMessage) -> SdResult;

//         // error

//         pub fn sd_bus_error_set(err: &mut SdError, name: *const ffi::c_char, msg: *const ffi::c_char) -> SdResult;
//         pub fn sd_bus_error_is_set(err: &mut SdError) -> SdResult;
//         pub fn sd_bus_error_free(err: *mut SdError);

//         // slot

//         pub fn sd_bus_slot_ref(slot: *mut SdSlot) -> *mut SdSlot;
//         pub fn sd_bus_slot_unref(slot: *mut SdSlot) -> *mut SdSlot;

//         pub fn sd_bus_slot_set_floating(slot: *mut SdSlot, val: ffi::c_int) -> *mut SdSlot;
    
//     }
    
// }
