
import { NullPointerError } from "./glue";
import type { Glue, Size, WasmPtr } from "./glue";

// =====================================================
// TYPEDEFS
// =====================================================

export interface EvlConfig {
  appidPtr: WasmPtr;
  appid: string;
  intercept: boolean;
}

export interface EvlObject {
  kind: "EventLoop";
  // helpers: ReturnType<typeof newHelpers>;
  events: AppEvent[]; // Optimal here, would be a VecDeque.
  currentWakerPtr: WasmPtr | null;
}

export interface WindowObject {
  kind: "Window";
  evlObject: EvlObject;
  targetElement: HTMLElement;
  scale: number;
  size: Size,
  animationFrameRequested: boolean;
  resizeObserver: ResizeObserver | null;
}

// =====================================================
// ENUMS
// =====================================================

export enum EvlResult {
  Ok = 0,
  Err = 1,
}

export enum PollResult {
  Ready = 0,
  Pending = 1,
  Err = 2,
}

export enum DataKind {
  Text = 0,
  Xml = 1,
  Html = 2,
  Zip = 3,
  Json = 4,
  Jpeg = 5,
  Png = 6,
  Other = 7,
}

export enum QuitReason {
  Program = 0,
  System = 1,
  CtrlC = 2,
}

export enum SpecialKey {
  Escape = 0,
  Tab = 1,
  CapsLock = 2,
  Shift = 3,
  Control = 4,
  Alt = 5,
  AltGr = 6,
  Super = 7,
  AppMenu = 8,
  Return = 9,
  Backspace = 10,
  Space = 11,
  ArrowUp = 12,
  ArrowDown = 13,
  ArrowLeft = 14,
  ArrowRight = 15,
  F1 = 16, F2 = 17, F3 = 18, F4 = 19, F5 = 20, F6 = 21,
  F7 = 22, F8 = 23, F9 = 24, F10 = 25, F11 = 26, F12 = 27,
}

export enum MouseButton {
  Left = 0,
  Right = 1,
  Middle = 2,
  X1 = 3,
  X2 = 4,
  Unknown = 5,
}

export enum ScrollAxis {
  Vertical = 0,
  Horizontal = 1,
}

export type AppEvent =
  | { kind: "Suspend" }
  | { kind: "Resume" }
  | { kind: "Quit"; reason: QuitReason }
  | { kind: "WindowRedraw"; wndHandle: WasmPtr }
  | { kind: "WindowEnter"; wndHandle: WasmPtr }
  | { kind: "WindowLeave"; wndHandle: WasmPtr }
  | { kind: "WindowResize"; wndHandle: WasmPtr; w: number; h: number; fullscreen: boolean }
  | { kind: "WindowRescale"; wndHandle: WasmPtr; scale: number }
  | { kind: "WindowDecorations"; enabled: boolean; wndHandle?: WasmPtr }
  | { kind: "WindowMouseEnter"; wndHandle: WasmPtr }
  | { kind: "WindowMouseLeave"; wndHandle: WasmPtr }
  | { kind: "WindowMouseMotion"; wndHandle: WasmPtr; x: number; y: number }
  | { kind: "WindowMouseDown"; wndHandle: WasmPtr; x: number; y: number; button: MouseButton }
  | { kind: "WindowMouseUp"; wndHandle: WasmPtr; x: number; y: number; button: MouseButton }
  | { kind: "WindowMouseScroll"; wndHandle: WasmPtr; dx: number; dy: number }
  | { kind: "WindowKeyDownSpecial"; wndHandle: WasmPtr; knownSpecialKey: SpecialKey; repeat: boolean }
  | { kind: "WindowKeyDownChar"; wndHandle: WasmPtr; chr: number; repeat: boolean }
  | { kind: "WindowKeyDownUnknown"; wndHandle: WasmPtr; key: number; repeat: boolean }
  | { kind: "WindowKeyUpSpecial"; wndHandle: WasmPtr; knownSpecialKey: SpecialKey }
  | { kind: "WindowKeyUpChar"; wndHandle: WasmPtr; chr: number }
  | { kind: "WindowKeyUpUnknown"; wndHandle: WasmPtr; key: number };

// =====================================================
// IMPLEMENTATION
// =====================================================

export function newEnv(glue: Glue, targetElement: HTMLElement) {

  const helpers = newHelpers(glue);
  let alreadyInitialized = false;

  return {

    logs(ptr: WasmPtr) {
      const msg = glue.readCString(ptr);
      console.log(msg);
    },

    // ==========================================
    // EVENT LOOP
    // ==========================================
    event_loop_run(configPtr: WasmPtr, handlerPtr: WasmPtr, statePtr: WasmPtr): EvlResult {

      if (!configPtr) throw new NullPointerError();
      if (!handlerPtr) throw new NullPointerError();

      const _config = helpers.readEventLoopConfig(configPtr);

      const evlObject: EvlObject = {
        kind: "EventLoop",
        // helpers,
        events: [],
        currentWakerPtr: null,
      };

      addEventListener("visibilitychange", () => {
        const hidden = document.hidden;
        if (hidden) newEvent(helpers, evlObject, { kind: "Suspend" });
        else        newEvent(helpers, evlObject, { kind: "Resume" });
      });

      const evlHandle = glue.allocHandle("EventLoop", evlObject);
      helpers.callEventLoopHandler(handlerPtr, evlHandle, statePtr);

      return EvlResult.Ok;

    },

    event_loop_poll(evlHandle: WasmPtr, wakerPtr: WasmPtr, handlerTablePtr: WasmPtr, handlerStatePtr: WasmPtr): PollResult {

      const evlObject = glue.getHandle<EvlObject>(evlHandle);

      // Clone and store the waker if it is different from the already stored one.
      if (!evlObject.currentWakerPtr) {
        evlObject.currentWakerPtr = helpers.wakerClone(wakerPtr);
      } else {
        if (!helpers.wakerWakeSame(wakerPtr, evlObject.currentWakerPtr)) {
          const cloned = helpers.wakerClone(wakerPtr);
          helpers.wakerDrop(evlObject.currentWakerPtr);
          evlObject.currentWakerPtr = cloned;
        }
      }

      const ev = evlObject.events.pop();
      const ht = handlerTablePtr;
      const hs = handlerStatePtr;

      if (ev) {
        switch (ev.kind) {

          case "Resume":        helpers.evntResume  (ht, hs); break;
          case "Suspend":       helpers.evntSuspend (ht, hs); break;
          case "Quit":          helpers.evntQuit    (ht, hs, ev.reason); break;

          case "WindowRedraw":      helpers.evntWindowRedraw       (ht, hs, ev.wndHandle); break;
          case "WindowEnter":       helpers.evntWindowEnter        (ht, hs, ev.wndHandle); break;
          case "WindowLeave":       helpers.evntWindowLeave        (ht, hs, ev.wndHandle); break;
          case "WindowResize":      helpers.evntWindowResize       (ht, hs, ev.wndHandle, ev.w, ev.h, ev.fullscreen); break;
          case "WindowRescale":     helpers.evntWindowRescale      (ht, hs, ev.wndHandle, ev.scale); break;
          case "WindowDecorations": helpers.evntWindowDecorations  (ht, hs, ev.wndHandle || 0, ev.enabled); break;

          case "WindowMouseEnter":   helpers.evntWindowMouseEnter  (ht, hs, ev.wndHandle); break;
          case "WindowMouseLeave":   helpers.evntWindowMouseLeave  (ht, hs, ev.wndHandle); break;
          case "WindowMouseMotion":  helpers.evntWindowMouseMotion (ht, hs, ev.wndHandle, ev.x, ev.y); break;
          case "WindowMouseDown":    helpers.evntWindowMouseDown   (ht, hs, ev.wndHandle, ev.x, ev.y, ev.button); break;
          case "WindowMouseUp":      helpers.evntWindowMouseUp     (ht, hs, ev.wndHandle, ev.x, ev.y, ev.button); break;
          case "WindowMouseScroll":  helpers.evntWindowMouseScroll (ht, hs, ev.wndHandle, ev.dx, ev.dy); break;

          case "WindowKeyDownSpecial": helpers.evntWindowKeyDownSpecial (ht, hs, ev.wndHandle, ev.knownSpecialKey, ev.repeat); break;
          case "WindowKeyDownChar":    helpers.evntWindowKeyDownChar    (ht, hs, ev.wndHandle, ev.chr, false, ev.repeat); break;
          case "WindowKeyDownUnknown": helpers.evntWindowKeyDownUnknown (ht, hs, ev.wndHandle, ev.key, ev.repeat); break;
          case "WindowKeyUpSpecial":   helpers.evntWindowKeyUpSpecial   (ht, hs, ev.wndHandle, ev.knownSpecialKey); break;
          case "WindowKeyUpChar":      helpers.evntWindowKeyUpChar      (ht, hs, ev.wndHandle, ev.chr, false); break;
          case "WindowKeyUpUnknown":   helpers.evntWindowKeyUpUnknown   (ht, hs, ev.wndHandle, ev.key); break;

        }
        return PollResult.Ready;
      } else {
        return PollResult.Pending;
      }
    },

    event_loop_suspend(evlHandle: WasmPtr) {
      const evlObject = glue.getHandle<EvlObject>(evlHandle);
      newEvent(helpers, evlObject, { kind: "Suspend" });
    },

    event_loop_resume(evlHandle: WasmPtr) {
      const evlObject = glue.getHandle<EvlObject>(evlHandle);
      newEvent(helpers, evlObject, { kind: "Resume" });
    },

    event_loop_quit(evlHandle: WasmPtr) {
      const evlObject = glue.getHandle<EvlObject>(evlHandle);
      newEvent(helpers, evlObject, {
        kind: "Quit",
        reason: QuitReason.Program,
      });
    },

    event_loop_display_ptr(evlHandle: WasmPtr): WasmPtr {
      return evlHandle;
    },

    // ==========================================
    // MONITOR
    // ==========================================
    monitor_info_drop(ptr: WasmPtr) {
      console.error("unimplemented callback: monitor_info_drop");
      return 0;
    },

    monitor_drop(ptr: WasmPtr) {
      console.error("unimplemented callback: monitor_drop");
      return 0;
    },

    // ==========================================
    // CUSTOM ICON
    // ==========================================
    custom_icon_new(evlPtr: WasmPtr, sizePtr: WasmPtr, format: number, dataSlicePtr: WasmPtr) {
      console.error("unimplemented callback: custom_icon_new");
      return 0;
    },

    // ==========================================
    // HOVERED ITEM
    // ==========================================
    hovered_item_drop(ptr: WasmPtr) {
      console.error("unimplemented callback: hovered_item_drop");
    },

    hovered_item_advertise(ptr: WasmPtr, kindsSlicePtr: WasmPtr) {
      console.error("unimplemented callback: hovered_item_advertise");
    },

    // ==========================================
    // DATA READABLE
    // ==========================================
    data_readable_drop(ptr: WasmPtr) {
      console.error("unimplemented callback: data_readable_drop");
    },

    data_readable_kinds(outPtr: WasmPtr, thisPtr: WasmPtr) {
      console.error("unimplemented callback: data_readable_kinds");
      return 0;
    },

    data_readable_receive(thisPtr: WasmPtr, evlPtr: WasmPtr, kind: number) {
      console.error("unimplemented callback: data_readable_receive");
      return 0;
    },

    // ==========================================
    // DATA READER
    // ==========================================
    data_reader_drop(ptr: WasmPtr) {
      console.error("unimplemented callback: data_reader_drop");
      return 0;
    },

    data_reader_as_fd(ptr: WasmPtr) {
      console.error("unimplemented callback: data_reader_as_fd");
      return 0;
    },

    data_reader_read(ptr: WasmPtr, outSlicePtr: WasmPtr) {
      console.error("unimplemented callback: data_reader_read");
      return 0;
    },

    // ==========================================
    // DATA WRITABLE
    // ==========================================
    data_writable_drop(ptr: WasmPtr) {
      console.error("unimplemented callback: data_writable_drop");
      return 0;
    },

    data_writable_id(ptr: WasmPtr) {
      console.error("unimplemented callback: data_writable_id");
      return 0;
    },

    data_writable_selection(evlPtr: WasmPtr, offersPtr: WasmPtr) {
      console.error("unimplemented callback: data_writable_selection");
      return 0;
    },

    data_writable_dnd(wndPtr: WasmPtr, offersPtr: WasmPtr, iconPtr: WasmPtr) {
      console.error("unimplemented callback: data_writable_dnd");
      return 0;
    },

    // ==========================================
    // DATA WRITER
    // ==========================================
    data_writer_drop(ptr: WasmPtr) {
      console.error("unimplemented callback: data_writer_drop");
      return 0;
    },

    data_writer_as_fd(ptr: WasmPtr) {
      console.error("unimplemented callback: data_writer_as_fd");
      return 0;
    },

    data_writer_write(ptr: WasmPtr, srcSlicePtr: WasmPtr) {
      console.error("unimplemented callback: data_writer_write");
      return 0;
    },

    data_writer_flush(ptr: WasmPtr) {
      console.error("unimplemented callback: data_writer_flush");
      return 0;
    },

    // ==========================================
    // WINDOW
    // ==========================================
    window_drop(ptr: WasmPtr) {
      glue.freeHandle(ptr);
    },

    window_new(evlHandle: WasmPtr): WasmPtr {

      const evlObject = glue.getHandle<EvlObject>(evlHandle);

      if (alreadyInitialized) {
        throw new Error("`Window` can only be created once.");
      } else {
        alreadyInitialized = true;
      }

      const wndObject = {
        kind: "Window",
        evlObject,
        targetElement,
        scale: 1.0,
        size: { w: 0, h: 0 },
        animationFrameRequested: false,
        resizeObserver: null as ResizeObserver | null,
      } as WindowObject;

      const wndHandle = glue.allocHandle("Window", wndObject);

      // Setup event handlers etc.
      setupWindowForElement(helpers, evlObject, wndObject, wndHandle);

      return wndHandle;

    },

    window_id(wndHandle: WasmPtr): WasmPtr {
      return wndHandle;
    },

    window_present(_wndHandle: WasmPtr) {},

    window_redraw(wndHandle: WasmPtr) {

      const wndObject = glue.getHandle<WindowObject>(wndHandle);

      if (!wndObject.animationFrameRequested) {
        wndObject.animationFrameRequested = true;

        requestAnimationFrame(() => {
          wndObject.animationFrameRequested = false;

          newEvent(helpers, wndObject.evlObject, { kind: "WindowRedraw", wndHandle });
          if (wndObject.evlObject.currentWakerPtr) {
            helpers.wakerWake(wndObject.evlObject.currentWakerPtr);
          }
        });
      }

    },

    window_transparency(wndHandle: WasmPtr, value: boolean) {},
    window_decorations(wndHandle: WasmPtr, value: boolean) {},
    window_title(wndHandle: WasmPtr, textPtr: WasmPtr) {},
    window_maximize(wndHandle: WasmPtr, value: boolean) {},
    window_fullscreen(wndHandle: WasmPtr, value: boolean, monitorPtr: WasmPtr) {},
    window_sizehint(wndHandle: WasmPtr, sizePtr: WasmPtr) {},
    window_minsize(wndHandle: WasmPtr, sizePtr: WasmPtr) {},
    window_minsize_unset(wndHandle: WasmPtr) {},
    window_maxsize(wndHandle: WasmPtr, sizePtr: WasmPtr) {},
    window_maxsize_unset(wndHandle: WasmPtr) {},
    window_alert(wndHandle: WasmPtr, urgency: number) {},

    window_ptr(wndHandle: WasmPtr): WasmPtr {
      return wndHandle;
    },

    window_size(outPtr: WasmPtr, wndHandle: WasmPtr) {
      const wndObject = glue.getHandle<WindowObject>(wndHandle);
      const size = wndObject.size;
      helpers.writePhysicalSize(outPtr, size.w, size.h);
    },
  };
}

function newHelpers(glue: Glue) {
  return {
    // =====================================================
    // CALL HELPERS
    // =====================================================
    wakerWakeSame(lhs: WasmPtr, rhs: WasmPtr): boolean {
      return glue.exports.waker_wake_same(lhs, rhs) == 1;
    },

    wakerWake(waker: WasmPtr) {
      glue.exports.waker_wake(waker);
    },

    wakerClone(waker: WasmPtr): WasmPtr {
      return glue.exports.waker_clone(waker);
    },

    wakerDrop(waker: WasmPtr) {
      glue.exports.waker_drop(waker);
    },

    callEventLoopHandler(fnPtr: WasmPtr, evlPtr: WasmPtr, statePtr: WasmPtr) {
      glue.exports.call_event_loop_handler(fnPtr, evlPtr, statePtr);
    },

    // =====================================================
    // CALL HELPERS (EVENT HANDLERS)
    // =====================================================
    evntResume(ht: WasmPtr, hs: WasmPtr) { glue.exports.call_resume(ht, hs) },
    evntSuspend(ht: WasmPtr, hs: WasmPtr) { glue.exports.call_suspend(ht, hs) },
    evntQuit(ht: WasmPtr, hs: WasmPtr, reason: QuitReason) { glue.exports.call_quit(ht, hs, reason) },

    evntWindowRedraw(ht: WasmPtr, hs: WasmPtr, id: WasmPtr) { glue.exports.call_window_redraw(ht, hs, id) },
    evntWindowEnter(ht: WasmPtr, hs: WasmPtr, id: WasmPtr) { glue.exports.call_window_enter(ht, hs, id) },
    evntWindowLeave(ht: WasmPtr, hs: WasmPtr, id: WasmPtr) { glue.exports.call_window_leave(ht, hs, id) },

    evntWindowResize(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, sizeW: number, sizeH: number, fullscreen: boolean) { glue.exports.call_window_resize(ht, hs, id, sizeW, sizeH, fullscreen) },
    evntWindowRescale(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, scale: number) { glue.exports.call_window_rescale(ht, hs, id, scale) },
    evntWindowDecorations(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, enabled: boolean) { glue.exports.call_window_decorations(ht, hs, id, enabled) },

    evntWindowMouseEnter(ht: WasmPtr, hs: WasmPtr, id: WasmPtr) { glue.exports.call_window_mouse_enter(ht, hs, id) },
    evntWindowMouseLeave(ht: WasmPtr, hs: WasmPtr, id: WasmPtr) { glue.exports.call_window_mouse_leave(ht, hs, id) },
    evntWindowMouseMotion(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, pointX: number, pointY: number) { glue.exports.call_window_mouse_motion(ht, hs, pointX, pointY) },
    evntWindowMouseDown(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, pointX: number, pointY: number, button: MouseButton) { glue.exports.call_window_mouse_down(ht, hs, pointX, pointY, button) },
    evntWindowMouseUp(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, pointX: number, pointY: number, button: MouseButton) { glue.exports.call_window_mouse_up(ht, hs, pointX, pointY, button) },
    evntWindowMouseScroll(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, dx: number, dy: number) { glue.exports.call_window_mouse_scroll(ht, hs, dx, dy) },

    evntWindowKeyDownSpecial(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, key: SpecialKey, repeat: boolean) { glue.exports.call_window_key_down_special(ht, hs, id, key, repeat) },
    evntWindowKeyDownChar(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, chr: number, dead: boolean, repeat: boolean) { glue.exports.call_window_key_down_char(ht, hs, id, chr, dead, repeat) },
    evntWindowKeyDownUnknown(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, key: number, repeat: boolean) { glue.exports.call_window_key_down_unknown(ht, hs, id, key, repeat) },

    evntWindowKeyUpSpecial(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, key: SpecialKey) { glue.exports.call_window_key_up_special(ht, hs, id, key) },
    evntWindowKeyUpChar(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, chr: number, dead: boolean) { glue.exports.call_window_key_up_char(ht, hs, id, chr, dead) },
    evntWindowKeyUpUnknown(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, key: number) { glue.exports.call_window_key_up_unknown(ht, hs, id, key) },

    evntWindowTextInput(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, chr: number) { glue.exports.call_window_text_input(ht, hs, id, chr) },
    evntWindowTextCompose(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, chr: number) { glue.exports.call_window_text_compose(ht, hs, id, chr) },
    evntWindowTextCompose_cancel(ht: WasmPtr, hs: WasmPtr, id: WasmPtr) { glue.exports.call_window_text_compose_cancel(ht, hs, id) },

    evntWindowDndMotion(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, sameapp: boolean, x: number, y: number, itemHandle: WasmPtr) { glue.exports.call_window_dnd_motion(ht, hs, id, sameapp, x, y, itemHandle) },
    evntWindowDndDrop(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, sameapp: boolean, x: number, y: number, readableHandle: WasmPtr) { glue.exports.call_window_dnd_drop(ht, hs, id, sameapp, x, y, readableHandle) },
    evntWindowDndCancel(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, sameapp: boolean) { glue.exports.call_window_dnd_cancel(ht, hs, id, sameapp) },

    evntDataSourceSend(ht: WasmPtr, hs: WasmPtr, id: WasmPtr, kind: DataKind, writerHandle: WasmPtr) { glue.exports.call_data_source_send(ht, hs, id, kind, writerHandle) },
    evntDataSourceSuccess(ht: WasmPtr, hs: WasmPtr, id: WasmPtr) { glue.exports.call_data_source_success(ht, hs, id) },
    evntDataSourceClose(ht: WasmPtr, hs: WasmPtr, id: WasmPtr) { glue.exports.call_data_source_close(ht, hs, id) },

    evntSelectionUpdate(ht: WasmPtr, hs: WasmPtr, readableHandle: WasmPtr) { glue.exports.call_selection_update(ht, hs, readableHandle) },

    // =====================================================
    // STRUCT READERS
    // =====================================================
    readEventLoopConfig(ptr: WasmPtr): EvlConfig {
      const appidPtr = glue.readU32(ptr);
      const appid = glue.readCString(appidPtr);
      const intercept = glue.readBool(ptr + 4);
      return { appidPtr, appid, intercept };
    },

    // =====================================================
    // STRUCT WRITERS
    // =====================================================
    writePhysicalSize(outPtr: WasmPtr, width: number, height: number) {
      glue.refreshMemoryViews();
      glue.writeU16(outPtr + 0, width);
      glue.writeU16(outPtr + 2, height);
    },
  };
}

function convertMouseButton(button: number): MouseButton {
  switch (button) {
    case 0: return MouseButton.Left;
    case 1: return MouseButton.Middle;
    case 2: return MouseButton.Right;
    case 3: return MouseButton.X1;
    case 4: return MouseButton.X2;
    default: return MouseButton.Unknown;
  }
}

function newMouseButtonEvent(wndHandle: WasmPtr, event: MouseEvent, direction: "Down" | "Up"): AppEvent {
  return {
    kind: `WindowMouse${direction}` as "WindowMouseDown" | "WindowMouseUp",
    wndHandle,
    x: event.clientX,
    y: event.clientY,
    button: convertMouseButton(event.button),
  };
}

function newMouseMotionEvent(wndHandle: WasmPtr, event: MouseEvent): AppEvent {
  return {
    kind: "WindowMouseMotion",
    wndHandle,
    x: event.clientX,
    y: event.clientY,
  };
}

function newMouseScrollEvent(wndHandle: WasmPtr, event: WheelEvent): AppEvent {
  return {
    kind: "WindowMouseScroll",
    wndHandle,
    dx: event.deltaX,
    dy: event.deltaY,
  };
}

export function convertSpecialKey(key: string): SpecialKey | null {
  switch (key) {
    case "Escape": return SpecialKey.Escape;
    case "Tab": return SpecialKey.Tab;
    case "CapsLock": return SpecialKey.CapsLock;
    case "Shift": return SpecialKey.Shift;
    case "Control": return SpecialKey.Control;
    case "Alt": return SpecialKey.Alt;
    case "AltRight": return SpecialKey.AltGr;
    case "Meta":
    case "OS": return SpecialKey.Super;
    case "ContextMenu": return SpecialKey.AppMenu;
    case "Enter": return SpecialKey.Return;
    case "Backspace": return SpecialKey.Backspace;
    case " ":
    case "Spacebar": return SpecialKey.Space;
    case "ArrowUp": return SpecialKey.ArrowUp;
    case "ArrowDown": return SpecialKey.ArrowDown;
    case "ArrowLeft": return SpecialKey.ArrowLeft;
    case "ArrowRight": return SpecialKey.ArrowRight;
    case "F1": return SpecialKey.F1;
    case "F2": return SpecialKey.F2;
    case "F3": return SpecialKey.F3;
    case "F4": return SpecialKey.F4;
    case "F5": return SpecialKey.F5;
    case "F6": return SpecialKey.F6;
    case "F7": return SpecialKey.F7;
    case "F8": return SpecialKey.F8;
    case "F9": return SpecialKey.F9;
    case "F10": return SpecialKey.F10;
    case "F11": return SpecialKey.F11;
    case "F12": return SpecialKey.F12;
    default: return null;
  }
}

function fnvHashString(str: string): number {
  let hash = 0x811c9dc5;
  for (let i = 0; i < str.length; i++) {
    hash ^= str.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193);
  }
  return hash >>> 0;
}

function newKeyEvent(wndHandle: WasmPtr, event: KeyboardEvent, direction: "Down" | "Up"): AppEvent {
  if (event.key.length === 1) {
    return {
      kind: `WindowKey${direction}Char`,
      wndHandle,
      chr: event.key.codePointAt(0) || 0,
      repeat: event.repeat,
    };
  } else {
    const knownSpecialKey = convertSpecialKey(event.key);
    if (knownSpecialKey !== null) {
      return {
        kind: `WindowKey${direction}Special`,
        wndHandle,
        knownSpecialKey,
        repeat: event.repeat,
      };
    } else {
      return {
        kind: `WindowKey${direction}Unknown`,
        wndHandle,
        key: fnvHashString(event.key),
        repeat: event.repeat,
      };
    }
  }
}

function newEvent(helpers: ReturnType<typeof newHelpers>, evlObject: EvlObject, event: AppEvent) {
  evlObject.events.push(event);
  if (evlObject.currentWakerPtr) {
    helpers.wakerWake(evlObject.currentWakerPtr);
  }
}

function setupWindowForElement(helpers: ReturnType<typeof newHelpers>, evlObject: EvlObject, wndObject: WindowObject, wndHandle: WasmPtr) {
  const el = wndObject.targetElement;

  if (!el.tabIndex) {
    el.tabIndex = 0;
  }

  el.addEventListener("focus", () => newEvent(helpers, evlObject, { kind: "WindowEnter", wndHandle }));
  el.addEventListener("blur", () => newEvent(helpers, evlObject, { kind: "WindowLeave", wndHandle }));

  el.addEventListener("mouseenter", () => newEvent(helpers, evlObject, { kind: "WindowMouseEnter", wndHandle }));
  el.addEventListener("mouseleave", () => newEvent(helpers, evlObject, { kind: "WindowMouseLeave", wndHandle }));

  el.addEventListener("mousemove", (event) => newEvent(helpers, evlObject, newMouseMotionEvent(wndHandle, event)));
  el.addEventListener("mousedown", (event) => newEvent(helpers, evlObject, newMouseButtonEvent(wndHandle, event, "Down")));
  el.addEventListener("mouseup", (event) => newEvent(helpers, evlObject, newMouseButtonEvent(wndHandle, event, "Up")));
  el.addEventListener("wheel", (event) => newEvent(helpers, evlObject, newMouseScrollEvent(wndHandle, event)));

  el.addEventListener("keydown", (event) => newEvent(helpers, evlObject, newKeyEvent(wndHandle, event, "Down")));
  el.addEventListener("keyup", (event) => newEvent(helpers, evlObject, newKeyEvent(wndHandle, event, "Up")));

  wndObject.resizeObserver = new ResizeObserver((entries) => {
    const [entry] = entries;

    const fullscreen = document.fullscreenElement === entry!.target;
    const w = entry!.contentRect.width;
    const h = entry!.contentRect.height;

    wndObject.size = { w, h };

    newEvent(helpers, evlObject, { kind: "WindowResize", wndHandle, w, h, fullscreen });

    const scale = window.devicePixelRatio;
    if (wndObject.scale !== scale) {
      wndObject.scale = scale;
      newEvent(helpers, evlObject, { kind: "WindowRescale", wndHandle, scale });
    }
  });

  wndObject.resizeObserver.observe(wndObject.targetElement);
  newEvent(helpers, evlObject, { kind: "WindowDecorations", enabled: false, wndHandle });
}
