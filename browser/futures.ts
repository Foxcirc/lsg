
import type { Glue, WasmPtr } from "./glue";

// =====================================================
// TYPEDEFS
// =====================================================

export interface Task {
  shouldBeQueued: boolean;
  wasDropped: boolean;
  pollFn: () => void;
}

export interface FutureVTable {
  pollFnPtr: WasmPtr;
  dropFnPtr: WasmPtr;
}

// =====================================================
// ENUMS
// =====================================================

export enum PollResult {
  Pending = 0,
  Ready = 1,
}

// =====================================================
// IMPLEMENTATION
// =====================================================

export function newEnv(glue: Glue) {

  const helpers = newHelpers(glue);

  const taskMap = new Map<number, Task>();
  let nextTaskId = 0x1000;

  return {

    waker_wake_browser_handler(state: number) {
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

    waker_drop_browser_handler(_state: number) {
      // Cleanup is done immediately when a
      // task completes, so this is empty.
    },

    spawn(futPtr: WasmPtr, vtablePtr: WasmPtr) {

      const vtable = helpers.readFutureVTable(vtablePtr);

      // Create a new waker, which is captured by the `pollFn`.
      const taskId = nextTaskId += 1;
      const wakerPtr = helpers.wakerNewBrowser(taskId);

      const task: Task = {

        shouldBeQueued: true,
        wasDropped: false,

        pollFn() {

          // Call the extern `poll` function.
          const pollResult = helpers.callFutureVTablePoll(vtable.pollFnPtr, futPtr, wakerPtr);

          if (pollResult === PollResult.Ready) {
            // The task is done and we forcefully drop all its data.
            // We don't wait until WASM releases all its clones of the waker, since
            // these clones might only be released when the WASM future is dropped.
            helpers.wakerDrop(wakerPtr);
            taskMap.delete(taskId);
            helpers.callFutureVTableDrop(vtable.dropFnPtr, futPtr);

            // This is important, so an already dropped task is not polled,
            // even if it woke itself up in the last poll.
            task.wasDropped = true; // Fixed: strictly bound to the closure's `task` reference
          }
        }

      };

      // Store and shedule the task.
      taskMap.set(taskId, task);
      helpers.wakerWake(wakerPtr);

    },
  };
}

function newHelpers(glue: Glue) {

  return {

    // =====================================================
    // STRUCT READERS
    // =====================================================

    readFutureVTable(ptr: WasmPtr): FutureVTable {
      const pollFnPtr = glue.readU32(ptr + 0);
      const dropFnPtr = glue.readU32(ptr + 4);
      return { pollFnPtr, dropFnPtr };
    },

    // =====================================================
    // CALL HELPERS
    // =====================================================

    wakerNewBrowser(state: number): WasmPtr {
      return glue.exports.waker_new_browser(state);
    },

    wakerWake(waker: WasmPtr): void {
      glue.exports.waker_wake(waker);
    },

    wakerDrop(waker: WasmPtr): void {
      glue.exports.waker_drop(waker);
    },

    callFutureVTablePoll(fnPtr: WasmPtr, futPtr: WasmPtr, wakerPtr: WasmPtr): PollResult {
      return glue.exports.call_future_vtable_poll(fnPtr, futPtr, wakerPtr) as PollResult;
    },

    callFutureVTableDrop(fnPtr: WasmPtr, futPtr: WasmPtr): void {
      glue.exports.call_future_vtable_drop(fnPtr, futPtr);
    },
  };

}
