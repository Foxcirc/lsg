
// @ts-check

// =====================================================
// TYPEDEFS
// =====================================================

/** @import { Glue, WasmPtr } from "./glue.js" */

/** @typedef {number} WasmPtr */
/** @typedef {ReturnType<typeof newHelpers>} Helpers */

/** @typedef {{ canvas: HTMLCanvasElement }} EvlObjectMin */

/** @typedef {{
  canvas: HTMLCanvasElement,
  gl: WebGL2RenderingContext,
  scratchFbo: WebGLFramebuffer,
}} GraphicsObject */

/** @typedef {{
  gpObject: GraphicsObject,
  glProgram: WebGLProgram,
}} ProgramObject */

/** @typedef {{
  size: { w: number, h: number },
  gpObject: GraphicsObject,
}} SurfaceObject */

/** @typedef {{
  gl: WebGL2RenderingContext,
  glVbo: WebGLBuffer,
  glVao: WebGLVertexArrayObject,
  vertsize: number,
  totalsize: number,
}} VertexBufferObject */

/** @typedef {{
  gpObject: GraphicsObject,
  gl: WebGL2RenderingContext,
  glTexture: WebGLTexture,
  size: { w: number, h: number },
}} TextureObject */

/** @typedef {{
  shaderType: number,
  data: string,
  dataPtr: WasmPtr
}} ShaderSource */

/** @typedef {{
  src: TextureObject,
  samplerLocation: number
}} TextureAttrib */

/** @typedef {{
  kind: number,
  count: number,
  divisor: number,
  location: number
}} VertexAttrib */

/** @typedef {{
  primitiveType: number,
  blendMode: number,
  polygonMode: number
}} DrawOptions */

/** @typedef {{
  vertexBufferObject: VertexBufferObject,
  programObject: ProgramObject,
  textures: TextureAttrib[],
  options: DrawOptions
}} DrawCmd */

// =====================================================
// ENUMS
// =====================================================

const glDataType = Object.freeze({
  F32: 0,
  U32: 1,
  I32: 2,
  U16: 3,
  I16: 4,
  U8: 5,
  I8: 6
});

/**
 * @param {WebGL2RenderingContext} gl
 * @param {number} glType
 * @returns {number}
 */
function glDataTypeCast(gl, glType) {
  switch (glType) {
    case (glDataType.F32): return gl.FLOAT;
    case (glDataType.U32): return gl.UNSIGNED_INT;
    case (glDataType.I32): return gl.INT;
    case (glDataType.U16): return gl.UNSIGNED_SHORT;
    case (glDataType.I16): return gl.SHORT;
    case (glDataType.U8):  return gl.UNSIGNED_BYTE;
    case (glDataType.I8):  return gl.BYTE
    default: throw new Error("invalid enum value");
  }
}

/**
 * @param {number} glType
 * @returns {number}
 */
function sizeOfGlDataType(glType) {
  switch (glType) {
    case (glDataType.F32): return 4;
    case (glDataType.U32): return 4;
    case (glDataType.I32): return 4;
    case (glDataType.U16): return 2;
    case (glDataType.I16): return 2;
    case (glDataType.U8):  return 1;
    case (glDataType.I8):  return 1
    default: throw new Error("invalid enum value");
  }
}

// =====================================================
// IMPLEMENTATION
// =====================================================

/** @param {Glue} glue  */
export function newEnv(glue) {

  /** @type {Helpers} */ const helpers = newHelpers(glue);

  return {

    /**
     * @param {number} _displayPtr
     * @returns {number}
     */
    graphics_new(_displayPtr) {

      // Here we only create a new canvas and store it. Later, when a
      // surface is created, the canvas is attached to the target element.

      const canvas = document.createElement("canvas");

      // Create our webgl2 context.
      const gl = canvas.getContext("webgl2");
      if (!gl) { throw new Error("webgl2 not supported"); }

      const scratchFbo = gl.createFramebuffer();

      return glue.allocHandle("Graphics", {
        canvas, gl, scratchFbo
      })

    },

    /**
     * @param {WasmPtr} gpHandle
     */
    graphics_drop(gpHandle) {
      glue.freeHandle(gpHandle);
    },

    /**
     * @param {number} gpHandle
     * @param {WasmPtr} sourcesPtr
     * @returns {number}
     */
    program_new(gpHandle, sourcesPtr) {

      /** @type {GraphicsObject} */
      const gpObject = glue.getHandle(gpHandle);
      const sources = helpers.readSourcesSlice(sourcesPtr);
      const gl = gpObject.gl;

      // Compile the shaders and link together the OpenGL program.

      const glProgram = gl.createProgram();

      for (const source of sources) {

        let glKind = 0;
        switch (source.shaderType) {
          case 0: glKind = gl.VERTEX_SHADER; break;
          case 1: glKind = gl.FRAGMENT_SHADER; break;
        }

        const glShader = gl.createShader(glKind);
        if (!glShader) throw new Error("Cannot create shader.");

        gl.shaderSource(glShader, source.data);
        gl.compileShader(glShader);
        gl.attachShader(glProgram, glShader);

      }

      gl.linkProgram(glProgram);

      return glue.allocHandle("Program", {
        gpObject,
        glProgram
      });

    },

    /**
     * @param {number} programHandle
     */
    program_drop(programHandle) {
      glue.freeHandle(programHandle)
    },

    /**
     * @param {number} programHandle
     * @param {WasmPtr} namePtr
     * @returns {WebGLUniformLocation}
     */
    program_uniformloc(programHandle, namePtr) {

      /** @type {ProgramObject} */
      const programObject = glue.getHandle(programHandle);
      const gl = programObject.gpObject.gl;

      const name = glue.readCString(namePtr);

      const loc = gl.getUniformLocation(programObject.glProgram, name);
      if (!loc) throw new Error(`Invalid uniform name: ${name}`)

      // `loc` is an opaque object, but we need a numeric index
      return glue.allocHandle("UniformLocation", loc);

    },

    /**
     * @param {number} gpHandle
     * @param {number} windowPtr
     * @returns {number}
     */
    surface_new(gpHandle, windowPtr) {

      /** @type {GraphicsObject} */
      const gpObject = glue.getHandle(gpHandle);
      const canvas = gpObject.canvas;

      // `windowPtr` has to be a handle which stores the target element.
      // We insert our canvas into the target element here.

      /** @type {HTMLElement} */
      const targetElement = glue.getHandle(windowPtr);

      const size = {
        w: targetElement.clientWidth,
        h: targetElement.clientHeight
      };

      canvas.width  = size.w;
      canvas.height = size.h;
      targetElement.appendChild(canvas);

      return glue.allocHandle("Surface", {
        size, gpObject
      });

    },

    /**
     *  @param {number} surfaceHandle
     */
    surface_drop(surfaceHandle) {
      glue.freeHandle(surfaceHandle)
    },

    /**
     *  @param {number} surfaceHandle
     *  @param {WasmPtr} sizePtr
     */
    surface_resize(surfaceHandle, sizePtr) {

      /** @type {SurfaceObject} */
      const surfaceObject = glue.getHandle(surfaceHandle);
      const gpObject = surfaceObject.gpObject;
      const size = helpers.readSize(sizePtr);

      surfaceObject.size = size;

      // Resize the backing canvas.
      gpObject.canvas.width  = size.w;
      gpObject.canvas.height = size.h;

    },

    /**
     *  @param {number} _surfaceHandle
     */
    surface_swap(_surfaceHandle) {
      // Presentation is managed by the browser.
    },

    /**
     *  @param {number} surfaceHandle
     *  @param {WasmPtr} drawCmdPtr
     */
    surface_draw(surfaceHandle, drawCmdPtr) {

      /** @type {SurfaceObject} */
      const surfaceObject = glue.getHandle(surfaceHandle);
      const gpObject = surfaceObject.gpObject;
      const cmd = helpers.readDrawCmd(drawCmdPtr);

      const w = gpObject.canvas.width;
      const h = gpObject.canvas.height;
      gpObject.gl.viewport(0, 0, w, h);

      drawToCurrentFramebuffer(gpObject.gl, cmd);

    },

    /**
     *  @param {number} surfaceHandle
     *  @param {number} textureHandle
     */
    surface_blit(surfaceHandle, textureHandle) {

      /** @type {SurfaceObject} */
      const surfaceObject = glue.getHandle(surfaceHandle);
      const gpObject = surfaceObject.gpObject;
      const gl = gpObject.gl;
      /** @type {TextureObject} */
      const textureObject = glue.getHandle(textureHandle);

      const size = surfaceObject.size;
      if (size != textureObject.size)
        throw new Error("texture and surface must be equally sized for blitting");

      // todo: missing binds

      gl.blitFramebuffer(
        0, 0, size.w, size.h, // src rect
        0, 0, size.w, size.h, // dst rect
        gl.COLOR_BUFFER_BIT, gl.LINEAR // flags
      );

    },

    /**
     * @param {number} gpHandle
     * @returns {number}
     */
    texture_maxsize(gpHandle) {

      /** @type {GraphicsObject} */
      const gpObject = glue.getHandle(gpHandle);
      const gl = gpObject.gl;

      return gl.getParameter(gl.MAX_TEXTURE_SIZE);

    },

    /**
     * @param {number} gpHandle
     * @param {WasmPtr} sizePtr
     * @param {WasmPtr} dataPtr
     * @returns {number}
     */
    texture_new(gpHandle, sizePtr, dataPtr) {

      /** @type {GraphicsObject} */
      const gpObject = glue.getHandle(gpHandle);
      const gl = gpObject.gl;
      const size = helpers.readSize(sizePtr);

      // `dataPtr` may be null, which means no data is provided.
      let view = null;
      if (dataPtr) view = helpers.viewByteSlice(dataPtr);

      const glTexture = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, glTexture);

      // Set some default parameters, if not set textures may behave weirdly.
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
      gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);

      gl.texImage2D(
        gl.TEXTURE_2D,
        0, // no mipmapping
        gl.RGBA8, // gpu color format
        size.w, size.h,
        0, // some cursed shit
        gl.RGBA, gl.UNSIGNED_BYTE, // cpu color format
        view
      );

      return glue.allocHandle("Texture", {
        gpObject, gl, glTexture, size
      });

    },

    /**
     * @param {number} texHandle
     */
    texture_drop(texHandle) {
      glue.freeHandle(texHandle);
    },

    /**
     * @param {number} texHandle
     * @param {WasmPtr} sizeOut
     */
    texture_size(texHandle, sizeOut) {
      /** @type {TextureObject} */
      const texObject = glue.getHandle(texHandle);
      helpers.writeSize(sizeOut, texObject.size);
    },

    /**
    * @param {number} texHandle
    * @param {WasmPtr} sizePtr
    * @param {WasmPtr} dataPtr
    */
    texture_resize(texHandle, sizePtr, dataPtr) {

      /** @type {TextureObject} */
      const texObject = glue.getHandle(texHandle);
      const { gl, glTexture } = texObject;

      const newSize = helpers.readSize(sizePtr);
      texObject.size = newSize;

      // `dataPtr` may be null, which means no data is provided.
      let view = null;
      if (dataPtr) view = helpers.viewByteSlice(dataPtr);

      gl.bindTexture(gl.TEXTURE_2D, glTexture);
      gl.texImage2D(
        gl.TEXTURE_2D,
        0, // no mipmapping
        gl.RGBA8, // gpu color format
        newSize.w, newSize.h,
        0, // some cursed shit
        gl.RGBA, gl.UNSIGNED_BYTE, // cpu color format
        view
      );

    },

    /**
    * @param {number} texHandle
    * @param {number} r
    * @param {number} g
    * @param {number} b
    * @param {number} a
    */
    texture_clear(texHandle, r, g, b, a) {

      /** @type {TextureObject} */
      const texObject = glue.getHandle(texHandle);
      const { gpObject, gl, glTexture } = texObject;

      gl.bindFramebuffer(gl.FRAMEBUFFER, gpObject.scratchFbo);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, glTexture, 0);
      gl.clearColor(r, g, b, a);
      gl.clear(gl.COLOR_BUFFER_BIT);

    },

    /**
    * @param {number} texHandle
    * @param {WasmPtr} byteSliceOut
    */
    texture_inspect(texHandle, byteSliceOut) {

      /** @type {TextureObject} */
      const texObject = glue.getHandle(texHandle);
      const { gpObject, gl, glTexture, size } = texObject;

      const view = glue.viewU8.subarray(byteSliceOut, size.w * size.h * 4);

      gl.bindTexture(gl.TEXTURE_2D, glTexture);
      gl.readPixels(0, 0, size.w, size.h, gl.RGBA, gl.UNSIGNED_BYTE, view)

    },

    /**
    * @param {number} texHandle
    * @param {WasmPtr} srcPtr,
    * @param {WasmPtr} dstRectPtr
    */
    texture_frombuf(texHandle, srcPtr, dstRectPtr) {

      /** @type {TextureObject} */
      const texObject = glue.getHandle(texHandle);
      const { gpObject, gl, glTexture, size } = texObject;

      const dstRect = helpers.readRect(dstRectPtr);
      const view = helpers.viewByteSlice(srcPtr);

      gl.bindTexture(gl.TEXTURE_2D, glTexture);
      gl.texSubImage2D(
        gl.TEXTURE_2D, 0,
        dstRect.pos.x, dstRect.pos.y, dstRect.size.w, dstRect.size.h,
        gl.RGBA, gl.UNSIGNED_BYTE, view
      );

    },

    /**
    * @param {number} dstTexHandle
    * @param {number} srcTexHandle
    * @param {WasmPtr} srcRectPtr,
    * @param {WasmPtr} dstRectPtr
    */
    texture_fromtex(dstTexHandle, srcTexHandle, srcRectPtr, dstRectPtr) {

      /** @type {TextureObject} */
      const srcTexObject = glue.getHandle(srcTexHandle);
      /** @type {TextureObject} */
      const dstTexObject = glue.getHandle(dstTexHandle);
      const { gpObject, gl } = srcTexObject;

      const srcRect = helpers.readRect(srcRectPtr);
      const dstRect = helpers.readRect(dstRectPtr);

      gl.bindTexture(gl.TEXTURE_2D, dstTexObject.glTexture);
      gl.bindFramebuffer(gl.FRAMEBUFFER, gpObject.scratchFbo);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, dstTexObject.glTexture, 0);

      gl.bindTexture(gl.TEXTURE_2D, dstTexObject.glTexture);
      gl.copyTexSubImage2D(
        gl.TEXTURE_2D, 0,
        srcRect.pos.x, srcRect.pos.y, // src-offset
        dstRect.pos.x, dstRect.pos.y, // dst-offset
        srcRect.size.w, srcRect.size.h // size
      );

    },

    /**
     *  @param {number} texHandle
     *  @param {WasmPtr} drawCmdPtr
     */
    texture_draw(texHandle, drawCmdPtr) {

      /** @type {TextureObject} */
      const texObject = glue.getHandle(texHandle);
      const { gpObject, gl, glTexture, size } = texObject;
      const cmd = helpers.readDrawCmd(drawCmdPtr);

      gl.bindTexture(gl.TEXTURE_2D, glTexture);
      gl.bindFramebuffer(gl.FRAMEBUFFER, gpObject.scratchFbo);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, glTexture, 0);

      gl.viewport(0, 0, size.w, size.h);

      drawToCurrentFramebuffer(gl, cmd);

    },

    /**
     * @param {number} gpHandle
     * @param {WasmPtr} layoutPtr
     * @returns {number}
     */
    vertex_buffer_new(gpHandle, layoutPtr) {

      /** @type {GraphicsObject} */
      const gpObject = glue.getHandle(gpHandle);
      const { gl } = gpObject;

      const layout = helpers.readVertexAttribSlice(layoutPtr);

      const glVbo = gl.createBuffer();
      const glVao = gl.createVertexArray();

      const vertsize = layout
        .map((it) => sizeOfGlDataType(it.kind))
        .reduce((acc, it) => acc + it, 0);

      gl.bindVertexArray(glVao);
      gl.bindBuffer(gl.ARRAY_BUFFER, glVbo);

      let loc = 0;
      let offset = 0;

      for (const it of layout) {

        gl.vertexAttribPointer(
          loc,
          it.count,
          glDataTypeCast(gl, it.kind),
          false,
          vertsize,
          offset
        );

        loc += 1;
        offset += sizeOfGlDataType(it.kind);

      }

      return glue.allocHandle("VertexBuffer", {
        gl, glVbo, glVao, vertsize, totalsize: 0
      });

    },

    /**
     * @param {number} vbHandle
     */
    vertex_buffer_drop(vbHandle) {
      glue.freeHandle(vbHandle);
    },

    /**
     * @param {number} vbHandle
     * @param {WasmPtr} srcPtr
     */
    vertex_buffer_frombuf(vbHandle, srcPtr) {

        /** @type {VertexBufferObject} */
        const vbObject = glue.getHandle(vbHandle);
        const { gl, glVbo } = vbObject;

        const view = helpers.viewByteSlice(srcPtr);

        gl.bindBuffer(gl.ARRAY_BUFFER, glVbo);
        gl.bufferData(gl.ARRAY_BUFFER, view, gl.DYNAMIC_DRAW);

    },

    /**
     * @param {number} vbHandle
     * @returns {number}
     */
     vertex_buffer_vertsize(vbHandle) {
       /** @type {VertexBufferObject} */
       const vbObject = glue.getHandle(vbHandle);
       return vbObject.vertsize;
     }

  }

}

/** @param {Glue} glue  */
function newHelpers(glue) {

  return {

    /**
     * @param {WasmPtr} ptr
     * @returns {{ x: number, y: number }}
     */
    readPos(ptr) {
      return {
        x: glue.readU16(ptr + 0),
        y: glue.readU16(ptr + 2)
      };
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {{ pos: { x: number, y: number }, size: { w: number, h: number } }}
     */
    readRect(ptr) {
      return {
        pos: this.readPos(ptr),
        size: this.readSize(ptr + 4)
      };
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {{ w: number, h: number }}
     */
    readSize(ptr) {
      return {
        w: glue.readU16(ptr + 0),
        h: glue.readU16(ptr + 2)
      };
    },

    /**
     * @param {WasmPtr} ptr
     * @param {{ w: number, h: number }} size
     */
    writeSize(ptr, size) {
      glue.writeU16(ptr + 0, size.w);
      glue.writeU16(ptr + 2, size.h);
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {Uint8Array}
     */
    viewByteSlice(ptr) {
      const header = glue.readSliceHeader(ptr);
      return glue.viewU8.subarray(header.ptr, header.len);
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {ShaderSource[]}
     */
    readSourcesSlice(ptr) {
      return glue.readSliceOf(ptr, this.readSource, 8);
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {ShaderSource}
     */
    readSource(ptr) {
      const shaderType = glue.readU32(ptr + 0); // u8 (+ padding)
      const dataPtr    = glue.readU32(ptr + 4); // *const i8
      const data       = glue.readCString(dataPtr);
      return { shaderType, data, dataPtr };
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {VertexAttrib[]}
     */
    readVertexAttribSlice(ptr) {
      return glue.readSliceOf(ptr, this.readVertexAttrib, 16);
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {VertexAttrib}
     */
    readVertexAttrib(ptr) {
      const kind      = glue.readU8(ptr + 0); // u8 (+ padding)
      const count     = glue.readU32(ptr + 4); // usize
      const divisor   = glue.readU32(ptr + 8); // usize
      const location  = glue.readU32(ptr + 12); // usize
      return { kind, count, divisor, location };
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {DrawCmd}
     */
    readDrawCmd(ptr) {
      const vertexBufferHandle = glue.readU32(ptr + 0); // ptr field
      const programHandle      = glue.readU32(ptr + 1); // ptr field
      const textures           = this.readTextureAttribSlice(ptr + 2); // slice field
      const options            = this.readDrawOptions(ptr + 4); // ptr field
      const vertexBufferObject = glue.getHandle(vertexBufferHandle);
      const programObject      = glue.getHandle(programHandle);
      return { vertexBufferObject, programObject, textures, options };
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {DrawOptions}
     */
    readDrawOptions(ptr) {
      const primitiveType = glue.readU8(ptr + 0);
      const blendMode     = glue.readU8(ptr + 1);
      const polygonMode   = glue.readU8(ptr + 2);
      return { primitiveType, blendMode, polygonMode };
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {TextureAttrib[]}
     */
    readTextureAttribSlice(ptr) {
      return glue.readSliceOf(ptr, this.readTextureAttrib, 8);
    },

    /**
     * @param {WasmPtr} ptr
     * @returns {TextureAttrib}
     */
    readTextureAttrib(ptr) {
      const textureHandle   = glue.readU32(ptr + 0);
      const samplerLocation = glue.readU32(ptr + 4);
      const textureObject = glue.getHandle(textureHandle);
      return {
        src: textureObject,
        samplerLocation,
      };
    },

  }

}

/**
 * @param {WebGL2RenderingContext} gl
 * @param {DrawCmd} cmd
 */
function drawToCurrentFramebuffer(gl, cmd) {

  switch (cmd.options.blendMode) {
    case 0: {
      gl.disable(gl.BLEND);
      break;
    }
    case 1: {
      gl.enable(gl.BLEND);
      gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
      break;
    }
  }

  let idx = 0;
  for (const texture of cmd.textures) {
    // texture.src
  }

  let glPrimitiveType = 0;
  switch (cmd.options.primitiveType) {
    case 0: glPrimitiveType = gl.TRIANGLES; break;
  }

  // TODO: missing all the binds
  gl.drawArrays(glPrimitiveType, 0, cmd.vertexBufferObject.totalsize);

}
