
/** @import { WasmPtr } from "../glue";*/

// =====================================================
// TYPEDEFS
// =====================================================

/** @typedef {{appidPtr: WasmPtr, appid: string, intercept: boolean}} EvlConfig */

/** @typedef {{ kind: "EventLoop", events: { kind: string }[] }} EvlObject */
/** @typedef {{ kind: "Window" }} WindowObject */

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

// dummy values since not provided
export const MouseButton = Object.freeze({
  Left: 0,
  Right: 1,
  Middle: 2,
});

export const ScrollAxis = Object.freeze({
  Vertical: 0,
  Horizontal: 1,
});
