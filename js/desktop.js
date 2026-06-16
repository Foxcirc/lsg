
/** @ts-check */

import { NullPointerError } from "./glue.js";

// =====================================================
// TYPEDEFS
// =====================================================

/** @import { Glue, WasmPtr } from "./glue.js" */

/** @typedef {{
  appidPtr: WasmPtr,
  appid: string,
  intercept: boolean
}} EvlConfig */

/** @typedef {{
  kind: "EventLoop",
  helpers: any,
  events: { kind: string }[],
  currentWakerPtr: WasmPtr,
  canvas: HTMLCanvasElement,
}} EvlObject */

/** @typedef {{
  kind: "Window",
  evlObject: EvlObject,
  targetElement: HTMLElement,
  scale: number,
  animationFrameRequested: boolean,
  resizeObserver: ResizeObserver,
}} WindowObject */

// =====================================================
// ENUMS
// =====================================================

export const EvlResult = Object.freeze({
  Ok: 0,
  Err: 1,
});

export const PollResult = Object.freeze({
  Ready: 0,
  Pending: 1,
  Err: 2,
});

export const DataKind = Object.freeze({
  Text: 0,
  Xml: 1,
  Html: 2,
  Zip: 3,
  Json: 4,
  Jpeg: 5,
  Png: 6,
  Other: 7,
});

export const QuitReason = Object.freeze({
  Program: 0,
  System: 1,
  CtrlC: 2,
});

export const SpecialKey = Object.freeze({
  Escape: 0,
  Tab: 1,
  CapsLock: 2,
  Shift: 3,
  Control: 4,
  Alt: 5,
  AltGr: 6,
  Super: 7,
  AppMenu: 8,
  Return: 9,
  Backspace: 10,
  Space: 11,
  ArrowUp: 12,
  ArrowDown: 13,
  ArrowLeft: 14,
  ArrowRight: 15,
  F1: 16, F2: 17, F3: 18, F4: 19, F5: 20, F6: 21,
  F7: 22, F8: 23, F9: 24, F10: 25, F11: 26, F12: 27,
});

export const MouseButton = Object.freeze({
  Left: 0,
  Right: 1,
  Middle: 2,
  X1: 3,
  X2: 4,
  Unknown: 5,
});

export const ScrollAxis = Object.freeze({
  Vertical: 0,
  Horizontal: 1,
});

// =====================================================
// IMPLEMENTATION
// =====================================================

/** @param {Glue} glue  */
export function newEnv(glue, targetElement) {

  const alreadyInitialized = Symbol(); // Used on `targetElement`
  const helpers = newHelpers(glue);

  return {

    logs(ptr) {
      let msg = glue.readCString(ptr);
      console.log(msg);
    },

    // ==========================================
    // EVENT LOOP
    // ==========================================
    event_loop_run(configPtr, handlerPtr, statePtr) {

      if (!configPtr)  throw NullPointerError;
      if (!handlerPtr) throw NullPointerError;

      const config = helpers.readEventLoopConfig(configPtr);

      /** @type {EvlObject} */
      const evlObject = {
        helpers,
        events: [],
        currentWakerPtr: null,
      };

      // Setup event handlers for global events.
      addEventListener("visibilitychange", () => {
        const hidden = document.hidden;
        if (hidden) newEvent(evlObject, { kind: "Suspend" });
        else        newEvent(evlObject, { kind: "Resume"  });
      });

      const evlHandle = glue.allocHandle("EventLoop", evlObject);

      // Call the rust callback.
      helpers.callEventLoopHandler(handlerPtr, evlHandle, statePtr);

      // On WASM, the handler will not block but return promptly. This, however,
      // doesn't mean the app will terminate, as instead it runs asynchronously.
      return types.EvlResult.Ok;

    },

    event_loop_poll(evlHandle, wakerPtr, handlerTablePtr, handlerStatePtr) {

      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);

      if (!evlObject.currentWakerPtr) {
        evlObject.currentWakerPtr = helpers.wakerClone(wakerPtr);
      } else {
        // Only clone and overwrite if necessarry.
        if (!helpers.wakerWakeSame(wakerPtr, evlObject.currentWakerPtr)) {
          let cloned = helpers.wakerClone(wakerPtr);
          // Drop the stored waker correclty!
          helpers.wakerDrop(evlObject.currentWakerPtr);
          evlObject.currentWakerPtr = cloned;
        }
      }

      // Check if we got new events.

      const event = evlObject.events.pop();

      const ht = handlerTablePtr;
      const hs = handlerStatePtr;

      if (event) {

        // If we get an event, we call it's handler depending on the `kind`.
        switch (event.kind) {


          case "Resume":  helpers.evntResume  (ht, hs); break;
          case "Suspend": helpers.evntSuspend (ht, hs); break;
          case "Quit":    helpers.evntQuit    (ht, hs, event.reason); break;

          case "WindowRedraw": helpers.evntWindowRedraw (ht, hs, event.wndHandle); break;
          case "WindowEnter":  helpers.evntWindowEnter  (ht, hs, event.wndHandle); break;
          case "WindowLeave":  helpers.evntWindowLeave  (ht, hs, event.wndHandle); break;

          case "WindowResize":      helpers.evntWindowResize      (ht, hs, event.wndHandle, event.w, event.h, event.fullscreen); break;
          case "WindowRescale":     helpers.evntWindowRescale     (ht, hs, event.wndHandle, event.scale); break;
          case "WindowDecorations": helpers.evntWindowDecorations (ht, hs, event.wndHandle, event.enabled); break;

          case "WindowMouseEnter":  helpers.evntWindowMouseEnter  (ht, hs, event.wndHandle); break;
          case "WindowMouseLeave":  helpers.evntWindowMouseLeave  (ht, hs, event.wndHandle); break;
          case "WindowMouseMotion": helpers.evntWindowMouseMotion (ht, hs, event.wndHandle, event.x, event.y); break;
          case "WindowMouseDown":   helpers.evntWindowMouseDown   (ht, hs, event.wndHandle, event.x, event.y, event.button); break;
          case "WindowMouseUp":     helpers.evntWindowMouseUp     (ht, hs, event.wndHandle, event.x, event.y, event.button); break;
          case "WindowMouseScroll": helpers.evntWindowMouseScroll (ht, hs, event.wndHandle, event.dx, event.dy); break;

          case "WindowKeyDownSpecial": helpers.evntWindowKeyDownSpecial (ht, hs, event.wndHandle, event.knownSpecialKey, event.repeat); break;
          case "WindowKeyDownChar":    helpers.evntWindowKeyDownChar    (ht, hs, event.wndHandle, event.chr, false, event.repeat); break;
          case "WindowKeyDownUnknown": helpers.evntWindowKeyDownUnknown (ht, hs, event.wndHandle, event.key, event.repeat); break;

          case "WindowKeyUpSpecial": helpers.evntWindowKeyUpSpecial (ht, hs, event.wndHandle, event.knownSpecialKey); break;
          case "WindowKeyUpChar":    helpers.evntWindowKeyUpChar    (ht, hs, event.wndHandle, event.chr, false); break;
          case "WindowKeyUpUnknown": helpers.evntWindowKeyUpUnknown (ht, hs, event.wndHandle, event.key); break;

        };

        return types.PollResult.Ready;

      } else {
        return types.PollResult.Pending;
      }

    },

    event_loop_suspend(evlHandle) {
      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);
      newEvent(evlObject, { kind: "Suspend" });
    },

    event_loop_resume(evlHandle) {
      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);
      newEvent(evlObject, { kind: "Resume" });
    },

    event_loop_quit(evlHandle) {
      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);
      newEvent(evlObject, {
        kind: "Quit",
        reason: types.QuitReason.Program
      });
    },

    event_loop_display_ptr(evlHandle) {
      return evlHandle;
    },

    // ==========================================
    // MONITOR
    // ==========================================
    monitor_info_drop(ptr) {
      console.error("unimplemented callback: monitor_info_drop");
      return 0;
    },

    monitor_drop(ptr) {
      console.error("unimplemented callback: monitor_drop");
      return 0;
    },

    // ==========================================
    // CUSTOM ICON
    // ==========================================
    custom_icon_new(evlPtr, sizePtr, format, dataSlicePtr) {
      console.error("unimplemented callback: custom_icon_new");
      return 0;
    },

    // ==========================================
    // HOVERED ITEM
    // ==========================================
    hovered_item_drop(ptr) {
      console.error("unimplemented callback: hovered_item_drop");
    },

    hovered_item_advertise(ptr, kindsSlicePtr) {
      console.error("unimplemented callback: hovered_item_advertise");
    },

    // ==========================================
    // DATA READABLE
    // ==========================================
    data_readable_drop(ptr) {
      console.error("unimplemented callback: data_readable_drop");
    },

    data_readable_kinds(outPtr, thisPtr) {
      console.error("unimplemented callback: data_readable_kinds");
      return 0;
    },

    data_readable_receive(thisPtr, evlPtr, kind) {
      console.error("unimplemented callback: data_readable_receive");
      return 0;
    },

    // ==========================================
    // DATA READER
    // ==========================================
    data_reader_drop(ptr) {
      console.error("unimplemented callback: data_reader_drop");
      return 0;
    },

    data_reader_as_fd(ptr) {
      console.error("unimplemented callback: data_reader_as_fd");
      return 0;
    },

    data_reader_read(ptr, outSlicePtr) {
      console.error("unimplemented callback: data_reader_read");
      return 0;
    },

    // ==========================================
    // DATA WRITABLE
    // ==========================================
    data_writable_drop(ptr) {
      console.error("unimplemented callback: data_writable_drop");
      return 0;
    },

    data_writable_id(ptr) {
      console.error("unimplemented callback: data_writable_id");
      return 0;
    },

    data_writable_selection(evlPtr, offersPtr) {
      console.error("unimplemented callback: data_writable_selection");
      return 0;
    },

    data_writable_dnd(wndPtr, offersPtr, iconPtr) {
      console.error("unimplemented callback: data_writable_dnd");
      return 0;
    },

    // ==========================================
    // DATA WRITER
    // ==========================================
    data_writer_drop(ptr) {
      console.error("unimplemented callback: data_writer_drop");
      return 0;
    },

    data_writer_as_fd(ptr) {
      console.error("unimplemented callback: data_writer_as_fd");
      return 0;
    },

    data_writer_write(ptr, srcSlicePtr) {
      console.error("unimplemented callback: data_writer_write");
      return 0;
    },

    data_writer_flush(ptr) {
      console.error("unimplemented callback: data_writer_flush");
      return 0;
    },

    // ==========================================
    // WINDOW
    // ==========================================
    window_drop(ptr) {
      freeHandle(ptr);
    },

    window_new(evlHandle) {

      /** @type {EvlObject} */
      const evlObject = glue.getHandle(evlHandle);

      // Prevent this being called twice:
      if (targetElement[alreadyInitialized]) {
        throw new Error("`Window` can only be created once");
      } else {
        targetElement[alreadyInitialized] = true;
      }

      // Setup our `targetElement` for rendering and events.

      return glue.allocHandle(
        "Window",
        setupWindowForElement(glue, evlObject, targetElement)
      );

    },

    window_id(wndHandle) {
      return wndHandle;
    },

    window_present(_wndHandle) {
      // We don't need to do anything here on WASM.
    },

    window_redraw(wndHandle) {

      /** @type {WindowObject} */
      const wndObject = glue.getHandle(wndHandle);

      if (!wndObject.animationFrameRequested) {
        wndObject.animationFrameRequested = true;
        requestAnimationFrame(() => {
          wndObject.animationFrameRequested = false;
          // The redraw event will be handled on the next poll.
          wndObject.pushEvent(evlObject, { kind: "WindowRedraw", wndHandle })
          helpers.wakerWake(wndObject.evlObject.currentWakerPtr);
        });
      }

    },

    window_transparency(wndHandle, value) {}, // It always implicitly enabled.
    window_decorations(wndHandle, value) {},
    window_title(wndHandle, textPtr) {},
    window_maximize(wndHandle, value) {},
    window_fullscreen(wndHandle, value, monitorPtr) {},
    window_sizehint(wndHandle, sizePtr) {},
    window_minsize(wndHandle, sizePtr) {},
    window_minsize_unset(wndHandle) {},
    window_maxsize(wndHandle, sizePtr) {},
    window_maxsize_unset(wndHandle) {},
    window_alert(wndHandle, urgency) {},

    window_ptr(wndHandle) {
      return wndHandle;
    },

    window_size(outPtr, wndHandle) { // TODO: TEST
      //        ^^^^^^ return type PhysicalSize passed by out pointer
      /** @type {WindowObject} */
      const wndObject = helpers.getHandle(wndHandle);
      const size = wndObject.currentSize;
      helpers.writePhysicalSize(outPtr, size.w, size.h);
    }

  };

}

/** @param {Glue} glue  */
function newHelpers(glue) {

  return {

    // =====================================================
    // CALL HELPERS
    // =====================================================

    /**
     * @param {WasmPtr} lhs
     * @param {WasmPtr} rhs
     * @returns {boolean}
     */
    wakerWakeSame(lhs, rhs) {
      /** @ts-ignore */
      return glue.instance.exports.waker_wake_same(lhs, rhs) == 1;
    },

    /** @param {WasmPtr} waker */
    wakerWake(waker) {
      /** @ts-ignore */
      glue.instance.exports.waker_wake(waker);
    },

    /**
     * @param {WasmPtr} waker
     * @returns {WasmPtr}
     */
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

    // evntMonitorUpdate (handlerTablePtr, handlerStatePtr, id, infoPtr, monitorHandle) { glue.instance.exports.call_monitor_update(handlerTablePtr, handlerStatePtr, id, infoPtr, monitorHandle) },
    // evntMonitorRemove (handlerTablePtr, handlerStatePtr, id)                         { glue.instance.exports.call_monitor_remove(handlerTablePtr, handlerStatePtr, id) },

    // evntWindowShouldClose (handlerTablePtr, handlerStatePtr, id) { glue.instance.exports.call_window_should_close (handlerTablePtr, handlerStatePtr, id) },
    evntWindowRedraw      (handlerTablePtr, handlerStatePtr, id) { glue.instance.exports.call_window_redraw       (handlerTablePtr, handlerStatePtr, id) },
    evntWindowEnter       (handlerTablePtr, handlerStatePtr, id) { glue.instance.exports.call_window_enter        (handlerTablePtr, handlerStatePtr, id) },
    evntWindowLeave       (handlerTablePtr, handlerStatePtr, id) { glue.instance.exports.call_window_leave        (handlerTablePtr, handlerStatePtr, id) },

    evntWindowResize      (handlerTablePtr, handlerStatePtr, id, sizeW, sizeH, fullscreen) { glue.instance.exports.call_window_resize      (handlerTablePtr, handlerStatePtr, id, sizeW, sizeH, fullscreen) },
    evntWindowRescale     (handlerTablePtr, handlerStatePtr, id, scale)                    { glue.instance.exports.call_window_rescale     (handlerTablePtr, handlerStatePtr, id, scale) },
    evntWindowDecorations (handlerTablePtr, handlerStatePtr, id, enabled)                  { glue.instance.exports.call_window_decorations (handlerTablePtr, handlerStatePtr, id, enabled) },

    evntWindowMouseEnter  (handlerTablePtr, handlerStatePtr, id)                         { glue.instance.exports.call_window_mouse_enter  (handlerTablePtr, handlerStatePtr, id) },
    evntWindowMouseLeave  (handlerTablePtr, handlerStatePtr, id)                         { glue.instance.exports.call_window_mouse_leave  (handlerTablePtr, handlerStatePtr, id) },
    evntWindowMouseMotion (handlerTablePtr, handlerStatePtr, id, pointX, pointY)         { glue.instance.exports.call_window_mouse_motion (handlerTablePtr, handlerStatePtr, pointX, pointY) },
    evntWindowMouseDown   (handlerTablePtr, handlerStatePtr, id, pointX, pointY, button) { glue.instance.exports.call_window_mouse_down   (handlerTablePtr, handlerStatePtr, pointX, pointY, button) },
    evntWindowMouseUp     (handlerTablePtr, handlerStatePtr, id, pointX, pointY, button) { glue.instance.exports.call_window_mouse_up     (handlerTablePtr, handlerStatePtr, pointX, pointY, button) },
    evntWindowMouseScroll (handlerTablePtr, handlerStatePtr, id, dx, dy)                 { glue.instance.exports.call_window_mouse_scroll (handlerTablePtr, handlerStatePtr, dx, dy) },

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

    writePhysicalSize(outPtr, width, height) {
      glue.refreshMemoryViews();
      glue.writeU16(base + 0, width);
      glue.writeU16(base + 2, height);
    },

  }

}

/**
 * @param {number} button
 * @return {number}
 */
function convertMouseButton(button) {
  switch (button) {
    case 0: return types.MouseButton.Left;
    case 1: return types.MouseButton.Middle;
    case 2: return types.MouseButton.Right;
    case 3: return types.MouseButton.X1;
    case 4: return types.MouseButton.X2;
    default: return types.MouseButton.Unknown;
  }
}

/**
 * @param {WasmPtr} wndHandle
 * @param {MouseEvent} event
 * @param {string} direction
 */
function newMouseButtonEvent(wndHandle, event, direction) {
  return {
    kind: `WindowMouse${direction}`,
    wndHandle,
    x: event.clientX,
    y: event.clientY,
    button: convertMouseButton(event.button)
  };
}

/**
 * @param {WasmPtr} wndHandle
 * @param {MouseEvent} event
 */
function newMouseMotionEvent(wndHandle, event) {
  return {
    kind: "WindowMouseMotion",
    wndHandle,
    x: event.clientX,
    y: event.clientY
  };
}

/**
 * @param {WasmPtr} wndHandle
 * @param {WheelEvent} event
 */
function newMouseScrollEvent(wndHandle, event) {
  return {
    kind: "WindowMouseScroll",
    wndHandle,
    dx: event.deltaX,
    dy: event.deltaY
  };
}

/**
 * @param {string} key
 * @returns {number | null}
 */
export function convertSpecialKey(key) {
  const SpecialKey = types.SpecialKey;
  switch (key) {
    case 'Escape':   return SpecialKey.Escape;
    case 'Tab':      return SpecialKey.Tab;
    case 'CapsLock': return SpecialKey.CapsLock;
    case 'Shift':    return SpecialKey.Shift;
    case 'Control':  return SpecialKey.Control;
    case 'Alt':      return SpecialKey.Alt;
    case 'AltRight': return SpecialKey.AltGr;
    case 'Meta':
    case 'OS':          return SpecialKey.Super;
    case 'ContextMenu': return SpecialKey.AppMenu;
    case 'Enter':       return SpecialKey.Return;
    case 'Backspace':   return SpecialKey.Backspace;
    case ' ':
    case 'Spacebar':   return SpecialKey.Space;
    case 'ArrowUp':    return SpecialKey.ArrowUp;
    case 'ArrowDown':  return SpecialKey.ArrowDown;
    case 'ArrowLeft':  return SpecialKey.ArrowLeft;
    case 'ArrowRight': return SpecialKey.ArrowRight;
    case 'F1':  return SpecialKey.F1;
    case 'F2':  return SpecialKey.F2;
    case 'F3':  return SpecialKey.F3;
    case 'F4':  return SpecialKey.F4;
    case 'F5':  return SpecialKey.F5;
    case 'F6':  return SpecialKey.F6;
    case 'F7':  return SpecialKey.F7;
    case 'F8':  return SpecialKey.F8;
    case 'F9':  return SpecialKey.F9;
    case 'F10': return SpecialKey.F10;
    case 'F11': return SpecialKey.F11;
    case 'F12': return SpecialKey.F12;
    default: return null;
  }
}

/**
 * @param {string} str
 * @returns {number}
 */
function fnvHashString(str) {
  let hash = 0x811c9dc5;
  for (let i = 0; i < str.length; i++) {
    hash ^= str.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193);
  }
  return hash >>> 0;
}

/**
 * @param {WasmPtr} wndHandle
 * @param {KeyboardEvent} event
 */
function newKeyDownEvent(wndHandle, event) {
  if (event.key.length === 1) {
    // The key is a character key.
    return {
      kind: "WindowKeyDownChar",
      wndHandle,
      chr: event.key.codePointAt(0),
      repeat: event.repeat
    };
  }
  else {
    // Could be a known special key, or unknown.
    const knownSpecialKey = convertSpecialKey(event.key);
    if (knownSpecialKey) {
      return {
        kind: "WindowKeyDownSpecial",
        wndHandle,
        knownSpecialKey,
        repeat: event.repeat
      };
    } else {
      // Unknown keys are opaque although still unique identfiers.
      return {
        kind: "WindowKeyDownUnknown",
        wndHandle,
        key: fnvHashString(event.key), // Make it unique.
        repeat: event.repeat
      };
    }
  }
}

/**
 * @param {EvlObject} evlObject
 * @param {any} event
 */
function newEvent(evlObject, event) {
  evlObject.events.push(event);
  // We might push an event before the event loop has be polled
  // for the first time. In this case there is nothing to wake.
  if (evlObject.currentWakerPtr)
    evlObject.helpers.wakerWake(evlObject.currentWakerPtr);
}

/**
 * @param {Glue} glue
 * @param {EvlObject} evlObject
 * @param {HTMLDivElement} targetElement
 * @returns {WindowObject}
 */
function setupWindowForElement(glue, evlObject, targetElement) {
  // TODO: maybe remove glue   ^^^^

  const wndObject = {
    evlObject,
    targetElement,
    scale: 1.0,
    animationFrameRequested: false,
    // resizeObserver: filled in later,
  };

  // Setup the target element.

  wndObject.targetElement = targetElement;
  const el = wndObject.targetElement;

  // This makes `target` focusable and able to receive key events.
  if (!el.tabIndex) {
    el.tabIndex = 0
  }

  // We track keyboard focus.
  el.addEventListener("focus", () => newEvent(evlObject, { kind: "WindowEnter", wndHandle }));
  el.addEventListener("blur",  () => newEvent(evlObject, { kind: "WindowLeave", wndHandle }));

  // We track mouse focus.
  el.addEventListener("mouseenter", () => newEvent(evlObject, { kind: "WindowMouseEnter", wndHandle }));
  el.addEventListener("mouseleave", () => newEvent(evlObject, { kind: "WindowMouseLeave", wndHandle }));

  // We track mouse movement, buttons and scrolling.
  el.addEventListener("mousemove", (event) => newEvent(evlObject, newMouseMotionEvent(wndHandle, event)));
  el.addEventListener("mousedown", (event) => newEvent(evlObject, newMouseButtonEvent(wndHandle, event, "Down")));
  el.addEventListener("mouseup",   (event) => newEvent(evlObject, newMouseButtonEvent(wndHandle, event, "Up")));
  el.addEventListener("wheel",     (event) => newEvent(evlObject, newMouseScrollEvent(wndHandle, event)));

  // We track key events.
  el.addEventListener("keydown", (event) => newEvent(evlObject, newKeyDownEvent (wndHandle, event)));
  el.addEventListener("keyup",   (event) => newEvent(evlObject, newKeyUpEvent   (wndHandle, event)));

  // We listen for resize of the element.
  wndObject.resizeObserver = new ResizeObserver((entries) => {

    const [entry] = entries;

    // Resize our canvas and emit an event.
    const fullscreen = document.fullscreenElement === entry.target;
    const w = entry.contentRect.width;
    const h = entry.contentRect.height;
    evlObject.canvas.width  = w;
    evlObject.canvas.height = h;
    newEvent(evlObject, { kind: "WindowResize", wndHandle, w, h, fullscreen });

    // React to scale change. (Will be consumed before the resize event!)
    const scale = window.devicePixelRatio;
    if (wndObject.scale !== scale) {
      wndObject.scale = scale;
      newEvent(evlObject, { kind: "WindowRescale", wndHandle, scale });
    }
  });

  wndObject.resizeObserver.observe(wndObject.targetElement);

  // We never use decorations on WASM.
  newEvent(evlObject, { kind: "WindowDecorations", enabled: false });

}
