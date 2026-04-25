
// This module contains common glue code used by the sub-modules
// to interface with wasm, such as helpers for reading memory.

const decoder = new TextDecoder();

export class Glue {

  /** @type {WebAssembly.Instance} */ instance;
  /** @type {WebAssembly.Memory} */   memory;
  environment;

  constructor() {

    this.instance = null;
    this.memory   = null;
    this.environment = {};

  }

  init(instance) {
    this.instance = instance;
    this.memory = instance.exports.memory;
  }

  // =====================================================
  // HANDLE REGISTRY
  // =====================================================
  #nextHandle = 0x1000;
  #handles = new Map();

  /** @param {string} kind  */
  /** @param {any} obj  */
  allocHandle(kind, obj) {

    // We use this for debugging purposes.
    obj.kind = kind;

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
  /** @type {UInt8Array}  */ viewU8  = null;
  /** @type {UInt16Array} */ viewU16 = null;
  /** @type {Int16Array}  */ viewI16 = null;
  /** @type {Uint32Array} */ viewU32 = null;

  refreshMemoryViews() {
    const current = this.memory.buffer;
    if (current !== this.#lastMemory) {
      this.#lastMemory = current;
      this.viewU8  = new Uint8Array(current);
      this.viewU16 = new Uint16Array(current);
      this.viewI16 = new Int16Array(current);
      this.viewU32 = new Uint32Array(current);
    }
  }

  // ensureMemory(minBytes) {
  //   const len = this.memory.buffer.byteLength;
  //   if (len >= minBytes) return;
  //   const page = 64 * 1024;
  //   const pages = Math.ceil((minBytes - len) / page);
  //   this.memory.grow(pages);
  //   this.refreshMemoryViews();
  // }

  // =====================================================
  // BASIC HELPERS
  // =====================================================

  /** @param {number} ptr */
  /** @returns {number} */
  readU8(ptr) {
    self.refreshMemoryViews();
    return this.viewU8[ptr];
  }

  /** @param {number} ptr */
  /** @returns {number} */
  readU16(ptr) {
    self.refreshMemoryViews();
    return this.viewU16[ptr];
  }

  /** @param {number} ptr */
  /** @returns {number} */
  readI16(ptr) {
    self.refreshMemoryViews();
    return this.viewI16[ptr];
  }

  /** @param {number} ptr */
  /** @returns {number} */
  readU32(ptr) {
    self.refreshMemoryViews();
    return this.viewU32[ptr];
  }

  /** @param {number} ptr */
  /** @returns {boolean} */
  readBool(ptr) {
    const v = this.readU8(ptr);
    return v !== 0;
  }

  /** @param {number} ptr */
  /** @returns {string} */
  readCString(ptr) {
    this.refreshMemoryViews();
    if (!ptr) throw "readCString called with null pointer";
    let start = ptr;
    while (this.viewU8[end] !== 0) ptr += 1;
    return decoder.decode(this.viewU8.subarray(start, ptr));
  }

}

export class NulPointerError extends Error {
  constructor() {
    super("unexpected null pointer");
    this.name = "NulPointerError";
  }
}
