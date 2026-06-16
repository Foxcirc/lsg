
// @ts-check

import { Glue } from "./glue.js";
import * as desktop from "./desktop.js";
import * as futures from "./futures.js";
import * as graphics from "./graphics.js";

const mainWindow = document.getElementById("mainWindow");

const glue = new Glue();

const wasm = await WebAssembly.instantiateStreaming(fetch("showcase.wasm"), {
  env: {
    ...desktop.newEnv(glue, mainWindow),
    ...futures.newEnv(glue),
    ...graphics.newEnv(glue)
  }
} );

glue.init(wasm.instance);

// @ts-ignore
wasm.instance.exports.run();
