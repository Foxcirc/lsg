
/** @ts-check */

import { Glue, NullPointerError } from "../glue.js";
import * as backend from "./backend.js";
import * as types from "./types.js"

/** @import { WasmPtr } from "../glue.js" */
/** @import { EvlConfig, EvlObject, WindowObject } from "./types.js" */

/** @param {Glue} glue  */
export function newEnv(glue) {

  const helpers = newHelpers(glue);

  /** @type {WasmPtr} */
  let currentWakerPtr = 0;

  return {

    logs(ptr) {
      let msg = glue.readCString(ptr);
      console.log(msg);
    },

    // ==========================================
    // EVENT LOOP
    // ==========================================
    event_loop_run(configPtr, handlerPtr, statePtr) {

      const config = helpers.readEventLoopConfig(configPtr);

      if (!handlerPtr) throw NullPointerError;

      const evlHandle = glue.allocHandle("EventLoop", {
        events: [{ kind: "Resume" }]
      });

      // Call the rust callback.
      helpers.callEventLoopHandler(handlerPtr, evlHandle, statePtr);

      return types.EvlResult.Ok;

    },

    event_loop_poll(evlHandle, wakerPtr, handlerTablePtr, handlerStatePtr) {
      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);

      // Always init/update our stored waker.

      if (currentWakerPtr === 0) {
        currentWakerPtr = helpers.wakerClone(wakerPtr);
      } else {
        // Only clone and overwrite if necessarry.
        if (!helpers.wakerWakeSame(wakerPtr, currentWakerPtr)) {
          let cloned = helpers.wakerClone(wakerPtr);
          // Drop the stored waker correclty!
          helpers.wakerDrop(currentWakerPtr);
          currentWakerPtr = cloned;
        }
      }

      // Check if we got new events.

      const lastEvent = evlObject.events.pop();

      if (lastEvent) {

        // If we get an event, we call it's handler depending on the `kind`.

        switch (lastEvent.kind) {
          case "Resume":  helpers.evntResume(handlerTablePtr, handlerStatePtr); break;
          case "Suspend": helpers.evntSuspend(handlerTablePtr, handlerStatePtr); break;
          case "Quit":    helpers.evntSuspend(handlerTablePtr, handlerStatePtr, lastEvent.reason); break;
        }

      } else {
        return types.PollResult.Pending;
      }

    },

    event_loop_suspend(evlHandle) {
      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);
      evlObject.events.push({ kind: "Suspend" });
    },

    event_loop_resume(evlHandle) {
      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);
      evlObject.events.push({ kind: "Resume" });
    },

    event_loop_quit(evlHandle) {
      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);
      evlObject.events.push({
        kind: "Quit",
        reason: types.QuitReason.Program
      });
    },

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

    // ==========================================
    // WINDOW
    // ==========================================
    window_drop(ptr) {
      freeHandle(ptr);
    },

    window_new(evlHandle) {
      /** @type {evlObject} */
      const evlObject = glue.getHandle(evlHandle);
      return glue.allocHandle("Window", {
      });
    },

    // This function sets the target element for this window.
    window_target(wndHandle, textPtr) {
      /** @type {WindowObject} */
      const wndObject = glue.getHandle(wndHandle);
      const targetId = glue.readCString(textPtr);
      const targetElement = document.getElementById(targetId);
      if (!targetElement) throw new Error("TODO: Invalid element ID provided.");
      wndObject.target = targetElement;
    },

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

    // window_title(wndHandle, textPtr) {},

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

/** @param {Glue} glue  */
function newHelpers(glue) {

  return {

    // =====================================================
    // CALL HELPERS
    // =====================================================

    /** @param {WasmPtr} lhs
    *   @param {WasmPtr} rhs
    *   @returns {boolean} */
    wakerWakeSame(lhs, rhs) {
      /** @ts-ignore */
      return glue.instance.exports.waker_wake_same(lhs, rhs) == 1;
    },

    /** @param {WasmPtr} waker */
    wakerWake(waker) {
      /** @ts-ignore */
      glue.instance.exports.waker_wake(waker);
    },

    /** @param {WasmPtr} waker
     *  @returns {WasmPtr} */
    wakerClone(waker) {
      /** @ts-ignore */
      return glue.instance.exports.waker_clone(waker);
    },

    /** @param {WasmPtr} waker */
    wakerDrop(waker) {
      /** @ts-ignore */
      glue.instance.exports.waker_drop(waker);
    },

    callEventLoopHandler(fnPtr, evlPtr, statePtr) {
      glue.instance.exports.call_event_loop_handler(fnPtr, evlPtr, statePtr);
    },

    // =====================================================
    // CALL HELPERS (EVENT HANDLERS)
    // =====================================================

    evntResume  (handlerTablePtr, handlerStatePtr)         { glue.instance.exports.call_resume  (handlerTablePtr, handlerStatePtr) },
    evntSuspend (handlerTablePtr, handlerStatePtr)         { glue.instance.exports.call_suspend (handlerTablePtr, handlerStatePtr) },
    evntQuit    (handlerTablePtr, handlerStatePtr, reason) { glue.instance.exports.call_quit    (handlerTablePtr, handlerStatePtr, reason) },

    evntMonitorUpdate (handlerTablePtr, handlerStatePtr, id, infoPtr, monitorHandle) { glue.instance.exports.call_monitor_update(handlerTablePtr, handlerStatePtr, id, infoPtr, monitorHandle) },
    evntMonitorRemove (handlerTablePtr, handlerStatePtr, id)                         { glue.instance.exports.call_monitor_remove(handlerTablePtr, handlerStatePtr, id) },

    evntWindowShouldClose (handlerTablePtr, handlerStatePtr, id) { glue.instance.exports.call_window_should_close (handlerTablePtr, handlerStatePtr, id) },
    evntWindowRedraw      (handlerTablePtr, handlerStatePtr, id) { glue.instance.exports.call_window_redraw       (handlerTablePtr, handlerStatePtr, id) },
    evntWindowEnter       (handlerTablePtr, handlerStatePtr, id) { glue.instance.exports.call_window_enter        (handlerTablePtr, handlerStatePtr, id) },
    evntWindowLeave       (handlerTablePtr, handlerStatePtr, id) { glue.instance.exports.call_window_leave        (handlerTablePtr, handlerStatePtr, id) },

    evntWindowResize      (handlerTablePtr, handlerStatePtr, id, pointPtr, fullscreen) { glue.instance.exports.call_window_resize      (handlerTablePtr, handlerStatePtr, id, pointPtr, fullscreen) },
    evntWindowRescale     (handlerTablePtr, handlerStatePtr, id, scaleFloat)           { glue.instance.exports.call_window_rescale     (handlerTablePtr, handlerStatePtr, id, scaleFloat) },
    evntWindowDecorations (handlerTablePtr, handlerStatePtr, id, enabled)              { glue.instance.exports.call_window_decorations (handlerTablePtr, handlerStatePtr, id, enabled) },

    evntWindowMouseEnter  (handlerTablePtr, handlerStatePtr, id)                   { glue.instance.exports.call_window_mouse_enter  (handlerTablePtr, handlerStatePtr, id) },
    evntWindowMouseLeave  (handlerTablePtr, handlerStatePtr, id)                   { glue.instance.exports.call_window_mouse_leave  (handlerTablePtr, handlerStatePtr, id) },
    evntWindowMouseMotion (handlerTablePtr, handlerStatePtr, id, pointPtr)         { glue.instance.exports.call_window_mouse_motion (handlerTablePtr, handlerStatePtr, pointPtr) },
    evntWindowMouseDown   (handlerTablePtr, handlerStatePtr, id, pointPtr, button) { glue.instance.exports.call_window_mouse_down   (handlerTablePtr, handlerStatePtr, pointPtr, button) },
    evntWindowMouseUp     (handlerTablePtr, handlerStatePtr, id, pointPtr, button) { glue.instance.exports.call_window_mouse_up     (handlerTablePtr, handlerStatePtr, pointPtr, button) },
    evntWindowMouseScroll (handlerTablePtr, handlerStatePtr, id, axis, value)      { glue.instance.exports.call_window_mouse_scroll (handlerTablePtr, handlerStatePtr, axis, value) },

    evntWindowKeyDownSpecial (handlerTablePtr, handlerStatePtr, id, key, repeat)       { glue.instance.exports.call_window_key_down_special (handlerTablePtr, handlerStatePtr, id, key, repeat) },
    evntWindowKeyDownChar    (handlerTablePtr, handlerStatePtr, id, chr, dead, repeat) { glue.instance.exports.call_window_key_down_char    (handlerTablePtr, handlerStatePtr, id, chr, dead, repeat) },
    evntWindowKeyDownUnknown (handlerTablePtr, handlerStatePtr, id, key, repeat)       { glue.instance.exports.call_window_key_down_unknown (handlerTablePtr, handlerStatePtr, id, key, repeat) },

    evntWindowKeyUpSpecial (handlerTablePtr, handlerStatePtr, id, key)       { glue.instance.exports.call_window_key_up_special (handlerTablePtr, handlerStatePtr, id, key) },
    evntWindowKeyUpChar    (handlerTablePtr, handlerStatePtr, id, chr, dead) { glue.instance.exports.call_window_key_up_char    (handlerTablePtr, handlerStatePtr, id, chr, key, dead) },
    evntWindowKeyUpUnknown (handlerTablePtr, handlerStatePtr, id, key)       { glue.instance.exports.call_window_key_up_unknown (handlerTablePtr, handlerStatePtr, id, key) },

    evntWindowTextInput          (handlerTablePtr, handlerStatePtr, id, chr) { glue.instance.exports.call_window_text_input          (handlerTablePtr, handlerStatePtr, id, chr) },
    evntWindowTextCompose        (handlerTablePtr, handlerStatePtr, id, chr) { glue.instance.exports.call_window_text_compose        (handlerTablePtr, handlerStatePtr, id, chr) },
    evntWindowTextCompose_cancel (handlerTablePtr, handlerStatePtr, id)      { glue.instance.exports.call_window_text_compose_cancel (handlerTablePtr, handlerStatePtr, id) },

    evntWindowDndMotion (handlerTablePtr, handlerStatePtr, id, sameapp, x, y, itemHandle)     { glue.instance.exports.call_window_dnd_motion (handlerTablePtr, handlerStatePtr, id, sameapp, x, y, itemHandle) },
    evntWindowDndDrop   (handlerTablePtr, handlerStatePtr, id, sameapp, x, y, readableHandle) { glue.instance.exports.call_window_dnd_drop   (handlerTablePtr, handlerStatePtr, id, sameapp, x, y, readableHandle) },
    evntWindowDndCancel (handlerTablePtr, handlerStatePtr, id, sameapp)                       { glue.instance.exports.call_window_dnd_cancel (handlerTablePtr, handlerStatePtr, id, sameapp) },

    evntDataSourceSend    (handlerTablePtr, handlerStatePtr, id, kind, writerHandle) { glue.instance.exports.call_data_source_send    (handlerTablePtr, handlerStatePtr, id, kind, writerHandle) },
    evntDataSourceSuccess (handlerTablePtr, handlerStatePtr, id)                     { glue.instance.exports.call_data_source_success (handlerTablePtr, handlerStatePtr, id) },
    evntDataSourceClose   (handlerTablePtr, handlerStatePtr, id)                     { glue.instance.exports.call_data_source_close   (handlerTablePtr, handlerStatePtr, id) },

    evntSelectionUpdate (handlerTablePtr, handlerStatePtr, readableHandle) { glue.instance.exports.call_selection_update(handlerTablePtr, handlerStatePtr, readableHandle) },

    // =====================================================
    // STRUCT READERS
    // =====================================================

    /** @param {WasmPtr} ptr
     *  @returns {EvlConfig} */
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
