
// @ts-check

/** @import {Glue} from "../glue.js" */

/** @typedef {number} WasmPtr */
/** @typedef {ReturnType<typeof newHelpers>} Helpers */
/** @typedef {{
 *   shouldBeQueued: boolean,
 *   wasDropped: boolean,
 *   pollFn: () => void
}} Task */

const PollResult = Object.freeze({
  Pending: 0,
  Ready: 1,
});

/** @param {Glue} glue  */
export function newEnv(glue) {

  /** @type {Helpers} */ const helpers = newHelpers(glue);

  /** @type {Map<number, Task>} */ const taskMap = new Map();
  /** @type {number} */            let nextTaskId = 0x1000;

  return {

    /** @param {number} state  */
    waker_wake_browser_handler(state) {
      // Get the handler associated with this task's id.
      const task = taskMap.get(state);
      if (!task) throw new Error("invalid taskId");
      // Queue it as a microtask to avoid
      // infinitely looping on wakeup inside poll.
      if (task.shouldBeQueued) {
        task.shouldBeQueued = false;
        queueMicrotask(() => {
          if (!task.wasDropped) {
            task.shouldBeQueued = true;
            task.pollFn();
          }
        });
      }
    },

    /** @param {number} state  */
    waker_drop_browser_handler(state) {
      // Cleanup is done immediatly when a
      // task completes, so this is empty.
    },

    /** @param {WasmPtr} futPtr
    *  @param {WasmPtr} vtablePtr */
    spawn(futPtr, vtablePtr) {

      const vtable = helpers.readFutureVTable(vtablePtr);

      // Create a new waker, which is captured by the `pollFn`.
      const taskId = nextTaskId += 1;
      const wakerPtr = helpers.wakerNewBrowser(taskId);

      /** @type {Task} */
      let task = {
        shouldBeQueued: true,
        wasDropped: false,
        pollFn() {
          const pollResult = helpers.callFutureVTablePoll(vtable.pollFnPtr, futPtr, wakerPtr);
          if (pollResult === PollResult.Ready) {
            // The task is done and we forcefully drop all its data.
            // We don't wait until WASM releases all it's clones of the waker, since
            // these clones might only be released when the WASM future is dropped.
            helpers.wakerDropBrowser(wakerPtr);
            taskMap.delete(taskId);
            helpers.callFutureVTableDrop(vtable.dropFnPtr, futPtr);
            // This is important, so an already dropped task is not polled,
            // even if it woke itself up in the last poll.
            this.wasDropped = true;
          }
        }
      };

      // Insert and wake up to shedule it.
      taskMap.set(taskId, task);
      helpers.wakerWakeBrowser(wakerPtr);

    },

  }

}

/** @param {Glue} glue  */
function newHelpers(glue) {

  return {

    // =====================================================
    // STRUCT READERS
    // =====================================================

    /** @param {WasmPtr} ptr  */
    readFutureVTable(ptr) {
      const pollFnPtr = glue.readU32(ptr + 0);
      const dropFnPtr = glue.readU32(ptr + 4);
      return { pollFnPtr, dropFnPtr }
    },

    // =====================================================
    // CALLBACK HELPERS
    // =====================================================

    /** @param {number} state
     *  @returns {WasmPtr} */
    wakerNewBrowser(state) {
      /** @ts-ignore */
      return glue.instance.exports.waker_new_browser(state);
    },

    /** @param {WasmPtr} waker */
    wakerWakeBrowser(waker) {
      /** @ts-ignore */
      return glue.instance.exports.waker_wake_browser(waker);
    },

    /** @param {WasmPtr} waker */
    wakerDropBrowser(waker) {
      /** @ts-ignore */
      return glue.instance.exports.waker_drop_browser(waker);
    },

    /** @param {WasmPtr} fnPtr
    /** @param {WasmPtr} futPtr
     *  @param {WasmPtr} wakerPtr
     *  @returns {number} */
    callFutureVTablePoll(fnPtr, futPtr, wakerPtr) {
      /** @ts-ignore */
      return glue.instance.exports.call_future_vtable_poll(fnPtr, futPtr, wakerPtr);
    },

    /** @param {WasmPtr} fnPtr
    /** @param {WasmPtr} futPtr
     *  @returns {number} */
    callFutureVTableDrop(fnPtr, futPtr) {
      /** @ts-ignore */
      return glue.instance.exports.call_future_vtable_drop(fnPtr, futPtr);
    },

  }

}
