
// This module contains common glue code used by the sub-modules
// to interface with wasm, such as helpers for reading memory.

export type WasmPtr = number;

export type Size = {
  w: number, h: number
}

export type Pos = {
  x: number, y: number
}

export type Rect = {
  size: Size, pos: Pos
}

const decoder = new TextDecoder();

type SliceHeader = {
  ptr: WasmPtr,
  len: number
}

// Objects stored in the handles registry.
type HandleObject = {
  kind: string,
  [key: string]: any
}

export class Glue {

  // =====================================================
  // ASSOCIATED WASM INSTANCE
  // =====================================================
  instance: WebAssembly.Instance | null = null;
  memory: WebAssembly.Memory | null = null;
  exports: any; // To stop annoying me when trying to call WASM funcs.

  // =====================================================
  // HANDLE REGISTRY
  // =====================================================
  #nextHandle: number = 0x1000;
  handles: Map<number, HandleObject> = new Map();

  // =====================================================
  // CACHED MEMORY VIEWS
  // =====================================================
  #lastMemory: ArrayBuffer | null = null;
  viewU8:  Uint8Array  | null = null;
  viewU16: Uint16Array | null = null;
  viewI16: Int16Array  | null = null;
  viewU32: Uint32Array | null = null;

  init(instance: WebAssembly.Instance): void {
    this.instance = instance;
    this.memory = instance.exports.memory as WebAssembly.Memory;
    this.exports = instance.exports as any;
  }

  allocHandle(kind: string, obj: any): number {

    // We use this for debugging purposes.
    obj.kind = kind;

    const handle = this.#nextHandle;
    this.#nextHandle += 1;
    this.handles.set(handle, obj as HandleObject);

    return handle;

  }

  getHandle<T>(handle: number): T {
    return this.handles.get(handle)! as T;
  }

  freeHandle(handle: number): void {
    this.handles.delete(handle);
  }

  refreshMemoryViews(): void {

    if (!this.memory) { throw new Error("Glue not initialized. Call init() first."); }

    const current = this.memory.buffer;
    if (current !== this.#lastMemory) {
      this.#lastMemory = current;
      this.viewU8  = new Uint8Array(current);
      this.viewU16 = new Uint16Array(current);
      this.viewI16 = new Int16Array(current);
      this.viewU32 = new Uint32Array(current);
    }

  }

  // =====================================================
  // BASIC HELPERS
  // =====================================================

  readU8(ptr: WasmPtr): number {
    this.refreshMemoryViews();
    return this.viewU8![ptr]!;
  }

  readU16(ptr: WasmPtr): number {
    this.refreshMemoryViews();
    const idx = ptr >>> 1; // dividing by 2
    return this.viewU16![idx]!;
  }

  readI16(ptr: WasmPtr): number {
    this.refreshMemoryViews();
    const idx = ptr >>> 1; // dividing by 2
    return this.viewI16![idx]!;
  }

  readU32(ptr: WasmPtr): number {
    this.refreshMemoryViews();
    const idx = ptr >>> 2; // dividing by 4
    return this.viewU32![idx]!;
  }

  readBool(ptr: WasmPtr): boolean {
    const v = this.readU8(ptr);
    return v !== 0;
  }

  readCString(ptr: WasmPtr): string {
    this.refreshMemoryViews();
    if (!ptr) throw new NullPointerError();
    let end = ptr;
    while (this.viewU8![end]! !== 0) end += 1;
    const subView = this.viewU8!.subarray(ptr, end);
    return decoder.decode(subView);
  }

  readSliceHeader(structPtr: WasmPtr): SliceHeader {
    // We expect all slice structs to have a layout like this:
    const dataPtr = this.readU32(structPtr + 0); // "ptr" field (*const T)
    const len =     this.readU32(structPtr + 4); // "len" field (usize)
    return { ptr: dataPtr, len };
  }

  readSliceOf<T>(
    structPtr: WasmPtr,
    readElem: (ptr: WasmPtr) => T,
    elemSize: number
  ): T[] {

    const header = this.readSliceHeader(structPtr);
    const buf: T[] = [];

    // Iterate exactly 'len' times, reading from the slice's data pointer
    for (let i = 0; i < header.len; i++) {
      buf.push(readElem(header.ptr + i * elemSize));
    }

    return buf;

  }

  writeU16(ptr: WasmPtr, value: number): void {
    this.refreshMemoryViews();
    const idx = ptr >>> 1; // dividing by 2
    this.viewU16![idx] = value;
  }

  // =====================================================
  // LIL' SPECIAL HELPERS
  // =====================================================

  readPos(ptr: WasmPtr): { x: number; y: number } {
    return {
      x: this.readU16(ptr + 0),
      y: this.readU16(ptr + 2),
    };
  }

  readRect(ptr: WasmPtr): { pos: { x: number; y: number }; size: { w: number; h: number } } {
    return {
      pos: this.readPos(ptr),
      size: this.readSize(ptr + 4),
    };
  }

  readSize(ptr: WasmPtr): { w: number; h: number } {
    return {
      w: this.readU16(ptr + 0),
      h: this.readU16(ptr + 2),
    };
  }

  writeSize(ptr: WasmPtr, size: { w: number; h: number }): void {
    this.writeU16(ptr + 0, size.w);
    this.writeU16(ptr + 2, size.h);
  }


}

export class NullPointerError extends Error {
  constructor() {
    super("unexpected null pointer passed from wasm to javascript");
    this.name = "NullPointerError";
  }
}

export class UnsupportedError extends Error {
  constructor() {
    super("this feature is currently not supported in a browser context");
    this.name = "UnsupportedError";
  }
}
