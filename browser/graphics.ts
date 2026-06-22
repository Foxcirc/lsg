
import type { Glue, WasmPtr, Size } from "./glue";

// =====================================================
// TYPES & INTERFACES
// =====================================================

export interface WindowObjectMin {
  targetElement: HTMLElement;
  size: Size;
}

export interface GraphicsObject {
  canvas: HTMLCanvasElement;
  gl: WebGL2RenderingContext;
  scratchFbo: WebGLFramebuffer;
}

export interface ProgramObject {
  gpObject: GraphicsObject;
  glProgram: WebGLProgram;
}

export interface SurfaceObject {
  gpObject: GraphicsObject;
  size: Size,
}

export interface VertexBufferObject {
  gpObject: GraphicsObject;
  glVbo: WebGLBuffer;
  glVao: WebGLVertexArrayObject;
  vertsize: number;
  totalsize: number;
}

export interface TextureObject {
  gpObject: GraphicsObject;
  glTexture: WebGLTexture;
  size: Size,
}

export interface ShaderSource {
  shaderType: SourceKind;
  data: string;
  dataPtr: WasmPtr;
}

export interface TextureAttrib {
  src: TextureObject;
  samplerLocation: number;
}

export interface VertexAttrib {
  kind: DataType;
  count: number;
  divisor: number;
  location: number;
}

export interface DrawOptions {
  primitiveType: PrimitiveType;
  blendMode: BlendMode;
  polygonMode: PolygonMode;
}

export interface DrawCmd {
  vertexBufferObject: VertexBufferObject;
  programObject: ProgramObject;
  textures: TextureAttrib[];
  options: DrawOptions;
}

// =====================================================
// ENUMS
// =====================================================

export enum SourceKind {
  Vertex = 0,
  Fragment = 1,
}

export enum DataType {
  F32 = 0,
  U32 = 1,
  I32 = 2,
  U16 = 3,
  I16 = 4,
  U8 = 5,
  I8 = 6,
}

export enum PrimitiveType {
  Triangles = 0,
}

export enum PolygonMode {
  Filled = 0,
  Outline = 1,
}

export enum BlendMode {
  None = 0,
  OrderedTransparency = 1,
}

function glDataTypeCast(gl: WebGL2RenderingContext, glType: DataType): number {
  switch (glType) {
    case DataType.F32: return gl.FLOAT;
    case DataType.U32: return gl.UNSIGNED_INT;
    case DataType.I32: return gl.INT;
    case DataType.U16: return gl.UNSIGNED_SHORT;
    case DataType.I16: return gl.SHORT;
    case DataType.U8:  return gl.UNSIGNED_BYTE;
    case DataType.I8:  return gl.BYTE;
  }
}

function sizeOfGlDataType(glType: DataType): number {
  switch (glType) {
    case DataType.F32: return 4;
    case DataType.U32: return 4;
    case DataType.I32: return 4;
    case DataType.U16: return 2;
    case DataType.I16: return 2;
    case DataType.U8:  return 1;
    case DataType.I8:  return 1;
  }
}

// =====================================================
// IMPLEMENTATION
// =====================================================

export function newEnv(glue: Glue) {

  const helpers = newHelpers(glue);

  // We assign it to a variable, since it needs to call its own functions
  // but "this" will not be bound when called from WASM.
  const env = {

    graphics_new(_displayPtr: number): number {

      // Create the global canvas and webgl2 context.
      const canvas = document.createElement("canvas");
      const gl = canvas.getContext("webgl2", { antialias: false, premultipliedAlpha: true });
      if (!gl) { throw new Error("webgl2 not supported") };

      // Setup a scratch framebuffer.
      const scratchFbo = gl.createFramebuffer();
      if (!scratchFbo) { throw new Error("Failed to create scratch framebuffer") };

      return glue.allocHandle("Graphics", { canvas, gl, scratchFbo });

    },

    graphics_drop(gpHandle: WasmPtr): void {
      glue.freeHandle(gpHandle);
    },

    program_new(gpHandle: number, sourcesPtr: WasmPtr): number {

      const gpObject = glue.getHandle<GraphicsObject>(gpHandle);
      const sources = helpers.readSourcesSlice(sourcesPtr);

      const { gl } = gpObject;

      const glProgram = gl.createProgram();
      if (!glProgram) { throw new Error("Cannot create WebGL program.") };

      for (const source of sources) {

        let glKind = 0;
        switch (source.shaderType) {
          case SourceKind.Vertex:   glKind = gl.VERTEX_SHADER; break;
          case SourceKind.Fragment: glKind = gl.FRAGMENT_SHADER; break;
        }

        const glShader = gl.createShader(glKind);
        if (!glShader) throw new Error("Cannot create shader.");

        gl.shaderSource(glShader, source.data);
        gl.compileShader(glShader);

        if (!gl.getShaderParameter(glShader, gl.COMPILE_STATUS)) {
          const info = gl.getShaderInfoLog(glShader);
          throw new Error(`Shader compilation failed: ${info}`);
        }

        gl.attachShader(glProgram, glShader);

      }

      gl.linkProgram(glProgram);
      if (!gl.getProgramParameter(glProgram, gl.LINK_STATUS)) {
        const info = gl.getProgramInfoLog(glProgram);
        throw new Error(`Program linking failed: ${info}`);
      }

      return glue.allocHandle("Program", { gpObject, glProgram });

    },

    program_drop(programHandle: number): void {
      glue.freeHandle(programHandle);
    },

    program_uniformloc(programHandle: number, namePtr: WasmPtr): number {

      const programObject = glue.getHandle<ProgramObject>(programHandle);
      const { gl } = programObject.gpObject;
      const name = glue.readCString(namePtr);

      const loc = gl.getUniformLocation(programObject.glProgram, name);
      if (!loc) throw new Error(`Invalid uniform name: ${name}`);

      return glue.allocHandle("UniformLocation", loc);

    },

    surface_new(gpHandle: number, windowPtr: WasmPtr): number {

      const gpObject = glue.getHandle<GraphicsObject>(gpHandle);
      const canvas = gpObject.canvas;
      const wndObjectMin = glue.getHandle<WindowObjectMin>(windowPtr);

      console.log(windowPtr, wndObjectMin);

      const size = {
        w: wndObjectMin.size.w,
        h: wndObjectMin.size.h,
      };

      canvas.width = size.w;
      canvas.height = size.h;
      wndObjectMin.targetElement.appendChild(canvas);

      return glue.allocHandle("Surface", { gpObject, size });

    },

    surface_drop(surfaceHandle: number): void {
      glue.freeHandle(surfaceHandle);
    },

    surface_resize(surfaceHandle: number, sizePtr: WasmPtr): void {

      const surfaceObject = glue.getHandle<SurfaceObject>(surfaceHandle);
      const gpObject = surfaceObject.gpObject;
      const size = glue.readSize(sizePtr);

      surfaceObject.size = size;
      gpObject.canvas.width = size.w;
      gpObject.canvas.height = size.h;

    },

    surface_swap(_surfaceHandle: number): void {
      // Presentation is managed by the browser.
    },

    surface_draw(surfaceHandle: number, drawCmdPtr: WasmPtr): void {

      const surfaceObject = glue.getHandle<SurfaceObject>(surfaceHandle);
      const { gl, canvas } = surfaceObject.gpObject;
      const cmd = helpers.readDrawCmd(drawCmdPtr);

      const w = canvas.width;
      const h = canvas.height;
      gl.viewport(0, 0, w, h);

      drawToCurrentFramebuffer(gl, cmd, glue);

    },

    surface_blit(surfaceHandle: number, textureHandle: number): void {

      const surfaceObject = glue.getHandle<SurfaceObject>(surfaceHandle);
      const { gl, scratchFbo } = surfaceObject.gpObject;
      const texObject = glue.getHandle<TextureObject>(textureHandle);

      const size = surfaceObject.size;
      if (size.w !== texObject.size.w || size.h !== texObject.size.h) {
        throw new Error("Texture and surface must be equally sized for blitting.");
      }

      gl.bindFramebuffer(gl.READ_FRAMEBUFFER, scratchFbo);
      gl.framebufferTexture2D(
        gl.READ_FRAMEBUFFER,
        gl.COLOR_ATTACHMENT0,
        gl.TEXTURE_2D,
        texObject.glTexture,
        0
      );

      gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, null); // Draw to the "default" FBO.

      gl.blitFramebuffer(
        0, 0, size.w, size.h,
        0, 0, size.w, size.h,
        gl.COLOR_BUFFER_BIT,
        gl.LINEAR
      );

      gl.bindFramebuffer(gl.READ_FRAMEBUFFER, null); // Unbind for cleanliness.

    },

    texture_maxsize(gpHandle: number): number {
      const { gl } = glue.getHandle<GraphicsObject>(gpHandle);
      return gl.getParameter(gl.MAX_TEXTURE_SIZE);
    },

    texture_new(gpHandle: number, sizePtr: WasmPtr, dataSlicePtr: WasmPtr): number {

      const gpObject = glue.getHandle<GraphicsObject>(gpHandle);
      const { gl, scratchFbo } = gpObject;
      const size = glue.readSize(sizePtr);
      const view = helpers.viewOptionalByteSlice(dataSlicePtr);

      if (size.w === 0 || size.h === 0)
        throw new Error("Texture cannot be zero sized.");

      const glTexture = gl.createTexture();
      if (!glTexture) throw new Error("Failed to create WebGL Texture.");

      gl.bindTexture(gl.TEXTURE_2D, glTexture);

      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
      gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);

      console.log("(texture_new) uploading following data:", size, view);

      gl.texImage2D(
        gl.TEXTURE_2D, 0, gl.RGBA8,
        size.w, size.h, 0,
        gl.RGBA, gl.UNSIGNED_BYTE,
        view
      );

      const handle = glue.allocHandle("Texture", {
        gpObject, glTexture, size
      });

      // If no data was provided, we clear the texture, so it is not left in an
      // uninitialized state, which would generate a warning in WebGL.
      if (!view) {
        gl.bindFramebuffer(gl.FRAMEBUFFER, scratchFbo);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, glTexture, 0);

        gl.clearColor(0, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT);
      }

      return handle;

    },

    texture_drop(texHandle: number): void {
      glue.freeHandle(texHandle);
    },

    texture_size(texHandle: number, outPtr: WasmPtr): void {
      const texObject = glue.getHandle<TextureObject>(texHandle);
      glue.writeSize(outPtr, texObject.size);
    },

    texture_resize(texHandle: number, sizePtr: WasmPtr, dataSlicePtr: WasmPtr): void {

      const texObject = glue.getHandle<TextureObject>(texHandle);
      const { gpObject: { gl }, glTexture } = texObject;
      const size = glue.readSize(sizePtr);

      // Update our size!
      texObject.size = size;

      const view = helpers.viewOptionalByteSlice(dataSlicePtr);

      console.log("(texture_resize) uploading following data:", size, view);

      gl.bindTexture(gl.TEXTURE_2D, glTexture);
      gl.texImage2D(
        gl.TEXTURE_2D, 0, gl.RGBA8,
        size.w, size.h, 0,
        gl.RGBA, gl.UNSIGNED_BYTE,
        view
      );

    },

    texture_clear(texHandle: number, r: number, g: number, b: number, a: number): void {

      const texObject = glue.getHandle<TextureObject>(texHandle);
      const { gpObject: { gl, scratchFbo }, glTexture } = texObject;

      console.log("clearing texture of size", texObject.size, "with color", { r, g, b, a });

      gl.bindFramebuffer(gl.FRAMEBUFFER, scratchFbo);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, glTexture, 0);

      gl.clearColor(r, g, b, a);
      gl.clear(gl.COLOR_BUFFER_BIT);

    },


    texture_inspect(texHandle: number, byteSliceOut: WasmPtr): void {

      const texObject = glue.getHandle<TextureObject>(texHandle);
      const { gpObject: { gl }, glTexture, size } = texObject;

      const view = glue.viewU8!.subarray(byteSliceOut, byteSliceOut + size.w * size.h * 4);

      gl.bindTexture(gl.TEXTURE_2D, glTexture);
      gl.readPixels(0, 0, size.w, size.h, gl.RGBA, gl.UNSIGNED_BYTE, view);

    },

    texture_frombuf(texHandle: number, srcPtr: WasmPtr, dstRectPtr: WasmPtr): void {

      const texObject = glue.getHandle<TextureObject>(texHandle);
      const { gpObject: { gl }, glTexture } = texObject;

      const dstRect = glue.readRect(dstRectPtr);
      const view = helpers.viewByteSlice(srcPtr);

      gl.bindTexture(gl.TEXTURE_2D, glTexture);
      gl.texSubImage2D(
        gl.TEXTURE_2D, 0,
        dstRect.pos.x,
        dstRect.pos.y,
        dstRect.size.w,
        dstRect.size.h,
        gl.RGBA,
        gl.UNSIGNED_BYTE,
        view
      );

    },

    texture_fromtex(dstTexHandle: number, srcTexHandle: number, srcRectPtr: WasmPtr, dstRectPtr: WasmPtr): void {

      const srcTexObject = glue.getHandle<TextureObject>(srcTexHandle);
      const dstTexObject = glue.getHandle<TextureObject>(dstTexHandle);
      const { gpObject: { gl, scratchFbo } } = srcTexObject;

      const srcRect = glue.readRect(srcRectPtr);
      const dstRect = glue.readRect(dstRectPtr);

      gl.bindTexture(gl.TEXTURE_2D, dstTexObject.glTexture);
      gl.bindFramebuffer(gl.FRAMEBUFFER, scratchFbo);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, dstTexObject.glTexture, 0);

      gl.copyTexSubImage2D(
        gl.TEXTURE_2D, 0,
        srcRect.pos.x,
        srcRect.pos.y,
        dstRect.pos.x,
        dstRect.pos.y,
        srcRect.size.w,
        srcRect.size.h
      );

    },

    texture_draw(texHandle: number, drawCmdPtr: WasmPtr): void {

      const texObject = glue.getHandle<TextureObject>(texHandle);
      const { gpObject: { gl, scratchFbo }, glTexture, size } = texObject;
      const cmd = helpers.readDrawCmd(drawCmdPtr);

      gl.bindTexture(gl.TEXTURE_2D, glTexture);
      gl.bindFramebuffer(gl.FRAMEBUFFER, scratchFbo);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, glTexture, 0);

      gl.viewport(0, 0, size.w, size.h);

      console.log("drawing to texture of size", texObject.size);

      drawToCurrentFramebuffer(gl, cmd, glue);

    },

    vertex_buffer_new(gpHandle: number, layoutPtr: WasmPtr): number {

      const gpObject = glue.getHandle<GraphicsObject>(gpHandle);
      const { gl } = gpObject;
      const layout = helpers.readVertexAttribSlice(layoutPtr);

      const glVbo = gl.createBuffer();
      if (!glVbo) throw new Error("Failed to create WebGL buffer.");

      const glVao = gl.createVertexArray();
      if (!glVao) throw new Error("Failed to create WebGL vertex array.");

      const vertsize = layout.reduce(
        (acc, it) => acc + (sizeOfGlDataType(it.kind) * it.count), 0
      );

      gl.bindVertexArray(glVao);
      gl.bindBuffer(gl.ARRAY_BUFFER, glVbo);

      let offset = 0;

      for (const it of layout) {

        if (it.kind == DataType.F32) {
          // Float.
          gl.vertexAttribPointer(
            it.location, it.count,
            glDataTypeCast(gl, it.kind),
            false, vertsize, offset
          );
        } else {
          // Integer types.
          gl.vertexAttribIPointer(
            it.location, it.count,
            glDataTypeCast(gl, it.kind),
            vertsize, offset
          );
        }

        gl.enableVertexAttribArray(it.location);

        const size = sizeOfGlDataType(it.kind);
        if (offset % size != 0 || vertsize % size != 0)
          throw new Error(`Alignment mismatch in attribute ${it.location}`);

        offset += sizeOfGlDataType(it.kind) * it.count;

      }

      gl.bindVertexArray(glVao);

      for (let loc = 0; loc < 8; ++loc) {
          console.log("attrib", loc, {
              enabled: gl.getVertexAttrib(loc, gl.VERTEX_ATTRIB_ARRAY_ENABLED),
              buffer: gl.getVertexAttrib(loc, gl.VERTEX_ATTRIB_ARRAY_BUFFER_BINDING),
              size: gl.getVertexAttrib(loc, gl.VERTEX_ATTRIB_ARRAY_SIZE),
              stride: gl.getVertexAttrib(loc, gl.VERTEX_ATTRIB_ARRAY_STRIDE),
              offset: gl.getVertexAttribOffset(
                  loc,
                  gl.VERTEX_ATTRIB_ARRAY_POINTER
              ),
          });
      }

      const vbObject: VertexBufferObject = {
        gpObject,
        glVbo,
        glVao,
        vertsize,
        totalsize: 0
      };

      return glue.allocHandle("VertexBuffer", vbObject);

    },

    vertex_buffer_drop(vbHandle: number): void {
      glue.freeHandle(vbHandle);
    },

    vertex_buffer_frombuf(vbHandle: number, srcPtr: WasmPtr): void {

      const vbObject = glue.getHandle<VertexBufferObject>(vbHandle);
      const { gpObject: { gl }, glVbo, vertsize } = vbObject;
      const view = helpers.viewByteSlice(srcPtr);

      gl.bindBuffer(gl.ARRAY_BUFFER, glVbo);
      gl.bufferData(gl.ARRAY_BUFFER, view, gl.DYNAMIC_DRAW);

      vbObject.totalsize = view.byteLength / vertsize;

    },

    vertex_buffer_vertsize(vbHandle: number): number {
      const vbObject = glue.getHandle<VertexBufferObject>(vbHandle);
      return vbObject.vertsize;
    },

  };

  return env;

}

// =====================================================
// HELPERS
// =====================================================

function newHelpers(glue: Glue) {

  return {

    viewOptionalByteSlice(ptr: WasmPtr): Uint8Array | null {
      const header = glue.readSliceHeader(ptr);
      if (header.ptr)
        return glue.viewU8!.subarray(header.ptr, header.ptr + header.len);
      else return null;
    },

    viewByteSlice(ptr: WasmPtr): Uint8Array {
      const header = glue.readSliceHeader(ptr);
      return glue.viewU8!.subarray(header.ptr, header.ptr + header.len);
    },

    readSourcesSlice(ptr: WasmPtr): ShaderSource[] {
      return glue.readSliceOf(ptr, this.readSource.bind(this), 8);
    },

    readSource(ptr: WasmPtr): ShaderSource {
      const shaderType = glue.readU32(ptr + 0);
      const dataPtr = glue.readU32(ptr + 4);
      const data = glue.readCString(dataPtr);
      return { shaderType, data, dataPtr };
    },

    readVertexAttribSlice(ptr: WasmPtr): VertexAttrib[] {
      return glue.readSliceOf(ptr, this.readVertexAttrib.bind(this), 16);
    },

    readVertexAttrib(ptr: WasmPtr): VertexAttrib {
      const kind = glue.readU8(ptr + 0);
      const count = glue.readU32(ptr + 4);
      const divisor = glue.readU32(ptr + 8);
      const location = glue.readU32(ptr + 12);
      return { kind, count, divisor, location };
    },

    readDrawCmd(ptr: WasmPtr): DrawCmd {
      const vertexBufferHandle = glue.readU32(ptr + 0);
      const programHandle = glue.readU32(ptr + 4);
      const textures = this.readTextureAttribSlice(ptr + 8);
      const optionsPtr = glue.readU32(ptr + 16);

      const vertexBufferObject = glue.getHandle<VertexBufferObject>(vertexBufferHandle);
      const programObject = glue.getHandle<ProgramObject>(programHandle);
      const options = this.readDrawOptions(optionsPtr);

      return { vertexBufferObject, programObject, textures, options };
    },

    readDrawOptions(ptr: WasmPtr): DrawOptions {
      const primitiveType = glue.readU8(ptr + 0);
      const blendMode = glue.readU8(ptr + 1);
      const polygonMode = glue.readU8(ptr + 2);
      return { primitiveType, blendMode, polygonMode };
    },

    readTextureAttribSlice(ptr: WasmPtr): TextureAttrib[] {
      return glue.readSliceOf(ptr, this.readTextureAttrib.bind(this), 8);
    },

    readTextureAttrib(ptr: WasmPtr): TextureAttrib {
      const textureHandle = glue.readU32(ptr + 0);
      const samplerLocation = glue.readU32(ptr + 4);
      const textureObject = glue.getHandle<TextureObject>(textureHandle);
      return {
        src: textureObject,
        samplerLocation,
      };
    },
  };
}

// =====================================================
// CORE OPENGL DRAW CALLS
// =====================================================

function drawToCurrentFramebuffer(gl: WebGL2RenderingContext, cmd: DrawCmd, glue: Glue): void {

  gl.useProgram(cmd.programObject.glProgram);
  gl.bindVertexArray(cmd.vertexBufferObject.glVao);

  console.log(cmd);

  switch (cmd.options.blendMode) {
    case BlendMode.None: {
      gl.disable(gl.BLEND);
      break;
    }
    case BlendMode.OrderedTransparency: {
      gl.enable(gl.BLEND);
      gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
      break;
    }
  }

  let idx = 0;
  for (const texture of cmd.textures) {

    gl.activeTexture(gl.TEXTURE0 + idx);
    gl.bindTexture(gl.TEXTURE_2D, texture.src.glTexture);

    const loc = glue.getHandle<WebGLUniformLocation>(texture.samplerLocation);
    gl.uniform1i(loc, idx);
    idx += 1;

  }

  let glPrimitiveType = gl.TRIANGLES;
  switch (cmd.options.primitiveType) {
    case PrimitiveType.Triangles: glPrimitiveType = gl.TRIANGLES; break;
  }

  gl.drawArrays(glPrimitiveType, 0, cmd.vertexBufferObject.totalsize);

  // For efficiency we don't clear these, since they might not change:
  // gl.bindVertexArray(null);
  // gl.useProgram(null);

}
