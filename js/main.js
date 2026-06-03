
// @ts-check

import { Glue } from "./glue.js";
import * as desktop from "./desktop.js";
import * as futures from "./futures.js";

const glue = new Glue();

const desktopEnv = desktop.newEnv(glue);
const futuresEnv = futures.newEnv(glue);

const wasm = await WebAssembly.instantiateStreaming(fetch("showcase.wasm"), {
  env: { ...desktopEnv, ...futuresEnv }
} );

glue.init(wasm.instance);

// @ts-ignore
wasm.instance.exports.run();
