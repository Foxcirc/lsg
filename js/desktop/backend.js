
import * as types from "./types.js";

/** @typedef {{ events: { kind: string }[] }} EvlObject */

/** @param {any} config */
/** @param {any} handler */
export function eventLoopRun(config, handler) {

  console.log("event_loop_run encountered", config, handler)

  /** @type {EvlObject} */
  const evlObject = {
    events: { kind: "Resume" }
  };

  handler(evlObject);

  types.EvlResult.Ok

}

/** @param {EvlObject} evlObject */
/** @param {number} wakerPtr */
/** @param {number} handlersPtr */
/** @param {number} statePtr */
export function eventLoopPoll(evlObject, wakerPtr, handlersPtr, statePtr) {

  // let events = evlObject.events.pop();

}
