
// Browser-side JS host which provides the C-Api defined in the `import` module.

import * as types from "./types.js"
import * as backend from "./backend.js"

const decoder = new TextDecoder();

export class App {

  /** @type {WebAssembly.Instance} */ #instance;
  /** @type {WebAssembly.Memory} */   #memory;
  #env;

  constructor() {

    this.#instance = null;
    this.#memory   = null;
    this.#env = newEnv(this);

  }

  init(instance) {
    this.#instance = instance;
    this.#memory = instance.exports.memory;
    console.log(Object.keys(this.#instance.exports));
  }

  env() {
    return this.#env
  }

  // =====================================================
  // HANDLE REGISTRY
  // =====================================================
  #nextHandle = 0x1000;
  #handles = new Map();

  allocHandle(obj = null) {
    const handle = this.#nextHandle++;
    this.#handles.set(handle, obj);
    return handle;
  }

  freeHandle(handle) {
    this.#handles.delete(handle);
  }

  // =====================================================
  // CACHED MEMORY VIEWS
  // =====================================================
  /** @type {WebAssembly.Memory} */ #lastMemory = null;
  /** @type {UInt8Array}  */ #viewU8  = null;
  /** @type {UInt16Array} */ #viewU16 = null;
  /** @type {Int16Array}  */ #viewI16 = null;
  /** @type {Uint32Array} */ #viewU32 = null;

  refreshMemoryViews() {
    const current = this.#memory.buffer;
    if (current !== this.#lastMemory) {
      this.#lastMemory = current;
      this.#viewU8  = new Uint8Array(current);
      this.#viewU16 = new Uint16Array(current);
      this.#viewI16 = new Int16Array(current);
      this.#viewU32 = new Uint32Array(current);
    }
  }

  ensureMemory(minBytes) {
    const len = this.#memory.buffer.byteLength;
    if (len >= minBytes) return;

    const page = 64 * 1024;
    const pages = Math.ceil((minBytes - len) / page);
    this.#memory.grow(pages);

    this.refreshMemoryViews();
  }

  // =====================================================
  // BASIC HELPERS
  // =====================================================
  readBool(v) {
    return v !== 0;
  }

  readCString(ptr) {
    if (!ptr) throw "readCString with null pointer";
    this.refreshMemoryViews();

    let end = ptr;
    while (this.#viewU8[end] !== 0) end++;

    return decoder.decode(this.#viewU8.subarray(ptr, end));
  }

  // =====================================================
  // CALLBACK HELPERS
  // =====================================================
  callEventLoopHandler(fnPtr, evlPtr, statePtr) {
    this.#instance.exports.call_event_loop_handler(fnPtr, evlPtr, statePtr);
  }

  // =====================================================
  // STRUCT READERS
  // =====================================================

  // EventLoopConfig
  // +0 appid ptr
  // +4 intercept bool (can be treated like u32)
readEventLoopConfig(ptr) {
    this.refreshMemoryViews();
    const base = ptr >>> 2; // Convert ptr to index into viewU32.

    const appidPtr = this.#viewU32[base + 0];
    const intercept = this.readBool(this.#viewU32[base + 1]);

    return {
      appidPtr,
      appid: this.readCString(appidPtr),
      intercept,
    };
  }

  readPollContextRust(ptr) {
    this.refreshMemoryViews();
    const base = ptr >>> 2; // Convert ptr to index into viewU32.

    const statePtr  = this.#viewU32[base + 0];
    const vtablePtr = this.#viewU32[base + 1];

    return {
      statePtr,
      vtablePtr
    }
  }

  // LogicalSize / PhysicalSize
  // +0 w u16
  // +2 h u16
  readSizeU16(ptr) {
    this.refreshMemoryViews();
    const base = ptr >>> 1; // Convert ptr to index into view16.

    return {
      w: this.#viewU16[base + 0],
      h: this.#viewU16[base + 1],
    };
  }

  // LogicalPoint / PhysicalPoint
  // +0 x i16
  // +2 y i16
  readPointI16(ptr) {
    this.refreshMemoryViews();
    const base = ptr >>> 1; // Convert ptr to index into view16.

    return {
      x: this.#viewI16[base + 0],
      y: this.#viewI16[base + 1],
    };
  }

  // WriteSlice / ReadSlice / DataKindsSlice
  // +0 ptr
  // +4 len
  readSliceHeader(ptr) {
    this.refreshMemoryViews();
    const base = ptr >>> 2;

    return {
      ptr: this.#viewU32[base + 0],
      len: this.#viewU32[base + 1],
    };
  }

  readWriteSlice(ptr) {
    const s = this.readSliceHeader(ptr);
    return {
      ...s,
      bytes: this.#viewU8.subarray(s.ptr, s.ptr + s.len),
    };
  }

  readReadSlice(ptr) {
    const s = this.readSliceHeader(ptr);
    return {
      ...s,
      bytes: this.#viewU8.subarray(s.ptr, s.ptr + s.len),
    };
  }

  readDataKindsSlice(ptr) {
    const s = this.readSliceHeader(ptr);

    return {
      ptr: s.ptr,
      len: s.len,
      values: this.#viewU32.subarray(s.ptr, s.ptr + s.len),
    };
  }

  // =====================================================
  // STRUCT WRITERS
  // =====================================================

  // PhysicalSize out-struct
  // +0 w u16
  // +2 h u16
  writePhysicalSize(ptr, w, h) {
    this.refreshMemoryViews();
    const base = ptr >>> 1;

    this.#viewU16[base + 0] = w;
    this.#viewU16[base + 1] = h;
  }

}

// =====================================================
// IMPORT ENV
// =====================================================
/** @param {App} app  */
function newEnv(app) {

  return {

    logs(ptr) {
      let s = app.readCString(ptr);
      console.log("DEBUG_LOG called:", s);
    },

    // ==========================================
    // EVENT LOOP
    // ==========================================
    event_loop_run(configPtr, handlerFnPtr, statePtr) {
      const config = app.readEventLoopConfig(configPtr);
      // config.appid -> decoded string
      // config.intercept -> bool
      let handler = function(evlObject) {
        // Call the rust callback.
        if (!handlerFnPtr) throw "handler fn is null"
        const evlHandle = app.allocHandle({ kind: "EventLoop", ...evlObject });
        app.callEventLoopHandler(handlerFnPtr, evlHandle, statePtr);

      }

      backend.eventLoopRun(config, handler);

      return types.EvlResult.Ok;
    },

    event_loop_poll_rust(evlPtr, rawcxPtr, handlersPtr, statePtr) {
      const evlObject = handles[evlPtr];
      const rawcx = app.readPollContextRust(rawcxPtr);

      backend.eventLoopPollRust(evlObject, rawcx, handlersPtr, statePtr);

      return types.Poll.Pending;
    },

    event_loop_suspend(thisPtr) {},
    event_loop_resume(thisPtr) {},
    event_loop_quit(thisPtr) {},

    event_loop_display_ptr(thisPtr) {
      return 0;
    },

    // ==========================================
    // MONITOR
    // ==========================================
    monitor_info_drop(ptr) {},
    monitor_drop(ptr) {
      freeHandle(ptr);
    },

    // ==========================================
    // CUSTOM ICON
    // ==========================================
    custom_icon_new(evlPtr, sizePtr, format, dataSlicePtr) {
      const size = readSizeU16(sizePtr);
      const data = readWriteSlice(dataSlicePtr);

      return allocHandle({
        kind: "CustomIcon",
        size,
        format,
        bytes: data.bytes,
      });
    },

    // ==========================================
    // HOVERED ITEM
    // ==========================================
    hovered_item_drop(ptr) {
      freeHandle(ptr);
    },

    hovered_item_advertise(ptr, kindsSlicePtr) {
      const kinds = readDataKindsSlice(kindsSlicePtr);
    },

    // ==========================================
    // DATA READABLE
    // ==========================================
    data_readable_drop(ptr) {
      freeHandle(ptr);
    },

    data_readable_kinds(outPtr, thisPtr) {
      // returned struct via out ptr
      // DataKindsSlice { ptr, len }
      refreshMemoryViews();
      const base = outPtr >>> 2;
      viewU32[base + 0] = 0;
      viewU32[base + 1] = 0;
    },

    data_readable_receive(thisPtr, evlPtr, kind) {
      return allocHandle({ kind: "DataReader" });
    },

    // ==========================================
    // DATA READER
    // ==========================================
    data_reader_drop(ptr) {
      freeHandle(ptr);
    },

    data_reader_as_fd(ptr) {
      return -1;
    },

    data_reader_read(ptr, outSlicePtr) {
      const out = readReadSlice(outSlicePtr);
      return 0;
    },

    // ==========================================
    // DATA WRITABLE
    // ==========================================
    data_writable_drop(ptr) {
      freeHandle(ptr);
    },

    data_writable_id(ptr) {
      return 1;
    },

    data_writable_selection(evlPtr, offersPtr) {
      const offers = readDataKindsSlice(offersPtr);
      return allocHandle({ kind: "Selection" });
    },

    data_writable_dnd(wndPtr, offersPtr, iconPtr) {
      const offers = readDataKindsSlice(offersPtr);
      return allocHandle({ kind: "DnD" });
    },

    // ==========================================
    // DATA WRITER
    // ==========================================
    data_writer_drop(ptr) {
      freeHandle(ptr);
    },

    data_writer_as_fd(ptr) {
      return -1;
    },

    data_writer_write(ptr, srcSlicePtr) {
      const src = readWriteSlice(srcSlicePtr);
      return src.len;
    },

    data_writer_flush(ptr) {},

    // ==========================================
    // WINDOW
    // ==========================================
    window_drop(ptr) {
      freeHandle(ptr);
    },

    window_new(evlPtr) {
      return allocHandle({
        kind: "Window",
        size: { w: 800, h: 600 },
        title: "",
      });
    },

    window_id(ptr) {
      return ptr >>> 0;
    },

    window_present(ptr) {},
    window_redraw(ptr) {},

    window_transparency(ptr, value) {
      const transparent = readBool(value);
    },

    window_decorations(ptr, value) {
      const enabled = readBool(value);
    },

    window_title(ptr, textPtr) {
      const title = readCString(textPtr);
      const wnd = handles.get(ptr);
      if (wnd) wnd.title = title;
    },

    window_maximize(ptr, value) {},
    window_fullscreen(ptr, value, monitorPtr) {},

    window_sizehint(ptr, sizePtr) {
      const size = readSizeU16(sizePtr);
    },

    window_minsize(ptr, sizePtr) {
      const size = readSizeU16(sizePtr);
    },

    window_minsize_unset(ptr) {},

    window_maxsize(ptr, sizePtr) {
      const size = readSizeU16(sizePtr);
    },

    window_maxsize_unset(ptr) {},

    window_alert(ptr, urgency) {},

    window_ptr(ptr) {
      return ptr >>> 0;
    },

    // hidden ABI out-ptr:
    // window_size(outPtr, thisPtr)
    window_size(outPtr, thisPtr) {
      const wnd = handles.get(thisPtr);
      const size = wnd?.size ?? { w: 800, h: 600 };

      writePhysicalSize(outPtr, size.w, size.h);
    }

  };

}
