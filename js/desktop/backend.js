
import * as types from "./types.js";

// /** @typedef {{ events: { kind: string }[] }} evlObject */

// /** @param {{appidPtr: number, appid: string, intercept: Boolean}} config */
/** @param {boolean} config */
/** @param {(evlObject: evlObject)} handler */
export function eventLoopRun(config, handler) {

  console.log("event_loop_run encountered", config, handler)

  const evlObject = {
    events: { kind: "Resume" }
  };

  handler(evlObject);

  types.EvlResult.Ok

}

/** @param {evlObject} evlObject */
/** @param {{statePtr: number, vtablePtr: number}} rawcx */
/** @param {number} handlersPtr */
/** @param {number} statePtr */
export function eventLoopPollRust(evlObject, rawcx, handlersPtr, statePtr) {

  let events = evlObject.events.pop();



}
