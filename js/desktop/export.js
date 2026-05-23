
import { Glue, NullPointerError } from "../glue.js";
import * as backend from "./backend.js";
import * as types from "./types.js"

/** @param {Glue} glue  */
function newHelpers(glue) {

  return {

    // =====================================================
    // CALL HELPERS
    // =====================================================

    /** @param {WasmPtr} lhs
    *   @param {WasmPtr} rhs
    *   @returns {number} */
    wakerWakeSame(lhs, rhs) {
      /** @ts-ignore */
      return glue.instance.exports.waker_wake_same(lhs, rhs);
    },

    /** @param {WasmPtr} waker */
    wakerWake(waker) {
      /** @ts-ignore */
      glue.instance.exports.waker_wake(waker);
    },

    /** @param {WasmPtr} waker
     *  @returns {WasmPtr} */
    wakerCloneBoxed(waker) {
      /** @ts-ignore */
      return glue.instance.exports.waker_clone_boxed(waker);
    },

    /** @param {WasmPtr} waker */
    wakerDropBoxed(waker) {
      /** @ts-ignore */
      glue.instance.exports.waker_drop_boxed(waker);
    },

    callEventLoopHandler(fnPtr, evlPtr, statePtr) {
      glue.instance.exports.call_event_loop_handler(fnPtr, evlPtr, statePtr);
    },

    // =====================================================
    // STRUCT READERS
    // =====================================================

    // EventLoopConfig
    // +0 appid ptr
    // +4 intercept bool
    readEventLoopConfig(ptr) {
      const appidPtr  = glue.readU32(ptr);
      const appid     = glue.readCString(appidPtr);
      const intercept = glue.readBool(ptr + 4);
      return { appidPtr, appid, intercept };
    },

  //   // LogicalSize / PhysicalSize
  //   // +0 w u16
  //   // +2 h u16
  //   readSizeU16(ptr) {
  //     this.refreshMemoryViews();
  //     const base = ptr >>> 1; // Convert ptr to index into view16.

  //     return {
  //       w: this.#viewU16[base + 0],
  //       h: this.#viewU16[base + 1],
  //     };
  //   },

  //   // LogicalPoint / PhysicalPoint
  //   // +0 x i16
  //   // +2 y i16
  //   readPointI16(ptr) {
  //     this.refreshMemoryViews();
  //     const base = ptr >>> 1; // Convert ptr to index into view16.

  //     return {
  //       x: this.#viewI16[base + 0],
  //       y: this.#viewI16[base + 1],
  //     };
  //   },

  //   // WriteSlice / ReadSlice / DataKindsSlice
  //   // +0 ptr
  //   // +4 len
  //   readSliceHeader(ptr) {
  //     this.refreshMemoryViews();
  //     const base = ptr >>> 2;

  //     return {
  //       ptr: this.#viewU32[base + 0],
  //       len: this.#viewU32[base + 1],
  //     };
  //   },

  //   readWriteSlice(ptr) {
  //     const s = this.readSliceHeader(ptr);
  //     return {
  //       ...s,
  //       bytes: this.#viewU8.subarray(s.ptr, s.ptr + s.len),
  //     };
  //   },

  //   readReadSlice(ptr) {
  //     const s = this.readSliceHeader(ptr);
  //     return {
  //       ...s,
  //       bytes: this.#viewU8.subarray(s.ptr, s.ptr + s.len),
  //     };
  //   },

  //   readDataKindsSlice(ptr) {
  //     const s = this.readSliceHeader(ptr);

  //     return {
  //       ptr: s.ptr,
  //       len: s.len,
  //       values: this.#viewU32.subarray(s.ptr, s.ptr + s.len),
  //     };
  //   },

  //   // =====================================================
  //   // STRUCT WRITERS
  //   // =====================================================

  //   // PhysicalSize out-struct
  //   // +0 w u16
  //   // +2 h u16
  //   writePhysicalSize(ptr, w, h) {
  //     this.refreshMemoryViews();
  //     const base = ptr >>> 1;

  //     this.#viewU16[base + 0] = w;
  //     this.#viewU16[base + 1] = h;
  //   },

  }

}

// =====================================================
// IMPORT ENV
// =====================================================
/** @param {Glue} glue  */
export function newEnv(glue) {

  const helpers = newHelpers(glue);

  let currentWakerPtr = 0;

  setInterval(() => {
    if (currentWakerPtr) {
      console.log("calling waker_wake, currentWakerPtr =", currentWakerPtr);
      helpers.wakerWake(currentWakerPtr);
    }
  }, 1000);

  return {

    logs(ptr) {
      let s = glue.readCString(ptr);
      console.log("DEBUG_LOG called:", s);
    },

    // ==========================================
    // EVENT LOOP
    // ==========================================
    event_loop_run(configPtr, handlerPtr, statePtr) {

      const config = helpers.readEventLoopConfig(configPtr);

      let handler = function(evlObject) {

        if (!handlerPtr) throw NullPointerError;

        // Call the rust callback.
        const evlHandle = glue.allocHandle("EventLoop", evlObject);
        helpers.callEventLoopHandler(handlerPtr, evlHandle, statePtr);

      }

      backend.eventLoopRun(config, handler);

      return types.EvlResult.Ok;

    },

    event_loop_poll(evlHandle, wakerPtr, handlersPtr, statePtr) {
      const evlObject = glue.getHandle(evlHandle);
      // const result = backend.eventLoopPoll(evlObject, wakerPtr, handlersPtr, statePtr);

      console.log("event_loop_poll entry, wakerPtr =, currentPtr =", wakerPtr, currentWakerPtr);

      if (currentWakerPtr == 0) {
        console.log("init waker");
        currentWakerPtr = helpers.wakerCloneBoxed(wakerPtr);
      } else {

        let update = helpers.wakerWakeSame(wakerPtr, currentWakerPtr) == 0;

        console.log("event_loop_poll wakers need updating retuned:", update);
        if (update) {
          // Only clone and overwrite if necessarry.
          // Also drop the stored waker correclty!
          console.log("event_loop_poll are not equal so we clone");
          let cloned = helpers.wakerCloneBoxed(wakerPtr);
          helpers.wakerDropBoxed(currentWakerPtr);
          currentWakerPtr = cloned;
        }

      }

      console.log("event_loop_poll done.");

      return types.Poll.Pending;
    },

  //   event_loop_suspend(thisPtr) {},
  //   event_loop_resume(thisPtr) {},
  //   event_loop_quit(thisPtr) {},

  //   event_loop_display_ptr(thisPtr) {
  //     return 0;
  //   },

  //   // ==========================================
  //   // MONITOR
  //   // ==========================================
  //   monitor_info_drop(ptr) {},
  //   monitor_drop(ptr) {
  //     freeHandle(ptr);
  //   },

  //   // ==========================================
  //   // CUSTOM ICON
  //   // ==========================================
  //   custom_icon_new(evlPtr, sizePtr, format, dataSlicePtr) {
  //     const size = readSizeU16(sizePtr);
  //     const data = readWriteSlice(dataSlicePtr);

  //     return allocHandle({
  //       kind: "CustomIcon",
  //       size,
  //       format,
  //       bytes: data.bytes,
  //     });
  //   },

  //   // ==========================================
  //   // HOVERED ITEM
  //   // ==========================================
  //   hovered_item_drop(ptr) {
  //     freeHandle(ptr);
  //   },

  //   hovered_item_advertise(ptr, kindsSlicePtr) {
  //     const kinds = readDataKindsSlice(kindsSlicePtr);
  //   },

  //   // ==========================================
  //   // DATA READABLE
  //   // ==========================================
  //   data_readable_drop(ptr) {
  //     freeHandle(ptr);
  //   },

  //   data_readable_kinds(outPtr, thisPtr) {
  //     // returned struct via out ptr
  //     // DataKindsSlice { ptr, len }
  //     refreshMemoryViews();
  //     const base = outPtr >>> 2;
  //     viewU32[base + 0] = 0;
  //     viewU32[base + 1] = 0;
  //   },

  //   data_readable_receive(thisPtr, evlPtr, kind) {
  //     return allocHandle({ kind: "DataReader" });
  //   },

  //   // ==========================================
  //   // DATA READER
  //   // ==========================================
  //   data_reader_drop(ptr) {
  //     freeHandle(ptr);
  //   },

  //   data_reader_as_fd(ptr) {
  //     return -1;
  //   },

  //   data_reader_read(ptr, outSlicePtr) {
  //     const out = readReadSlice(outSlicePtr);
  //     return 0;
  //   },

  //   // ==========================================
  //   // DATA WRITABLE
  //   // ==========================================
  //   data_writable_drop(ptr) {
  //     freeHandle(ptr);
  //   },

  //   data_writable_id(ptr) {
  //     return 1;
  //   },

  //   data_writable_selection(evlPtr, offersPtr) {
  //     const offers = readDataKindsSlice(offersPtr);
  //     return allocHandle({ kind: "Selection" });
  //   },

  //   data_writable_dnd(wndPtr, offersPtr, iconPtr) {
  //     const offers = readDataKindsSlice(offersPtr);
  //     return allocHandle({ kind: "DnD" });
  //   },

  //   // ==========================================
  //   // DATA WRITER
  //   // ==========================================
  //   data_writer_drop(ptr) {
  //     freeHandle(ptr);
  //   },

  //   data_writer_as_fd(ptr) {
  //     return -1;
  //   },

  //   data_writer_write(ptr, srcSlicePtr) {
  //     const src = readWriteSlice(srcSlicePtr);
  //     return src.len;
  //   },

  //   data_writer_flush(ptr) {},

  //   // ==========================================
  //   // WINDOW
  //   // ==========================================
  //   window_drop(ptr) {
  //     freeHandle(ptr);
  //   },

  //   window_new(evlPtr) {
  //     return allocHandle({
  //       kind: "Window",
  //       size: { w: 800, h: 600 },
  //       title: "",
  //     });
  //   },

  //   window_id(ptr) {
  //     return ptr >>> 0;
  //   },

  //   window_present(ptr) {},
  //   window_redraw(ptr) {},

  //   window_transparency(ptr, value) {
  //     const transparent = readBool(value);
  //   },

  //   window_decorations(ptr, value) {
  //     const enabled = readBool(value);
  //   },

  //   window_title(ptr, textPtr) {
  //     const title = readCString(textPtr);
  //     const wnd = handles.get(ptr);
  //     if (wnd) wnd.title = title;
  //   },

  //   window_maximize(ptr, value) {},
  //   window_fullscreen(ptr, value, monitorPtr) {},

  //   window_sizehint(ptr, sizePtr) {
  //     const size = readSizeU16(sizePtr);
  //   },

  //   window_minsize(ptr, sizePtr) {
  //     const size = readSizeU16(sizePtr);
  //   },

  //   window_minsize_unset(ptr) {},

  //   window_maxsize(ptr, sizePtr) {
  //     const size = readSizeU16(sizePtr);
  //   },

  //   window_maxsize_unset(ptr) {},

  //   window_alert(ptr, urgency) {},

  //   window_ptr(ptr) {
  //     return ptr >>> 0;
  //   },

  //   // hidden ABI out-ptr:
  //   // window_size(outPtr, thisPtr)
  //   window_size(outPtr, thisPtr) {
  //     const wnd = handles.get(thisPtr);
  //     const size = wnd?.size ?? { w: 800, h: 600 };

  //     writePhysicalSize(outPtr, size.w, size.h);
  //   }

  };

}
