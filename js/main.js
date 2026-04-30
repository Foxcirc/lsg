
// @ts-check

import { Glue } from "./glue.js";
import * as desktop from "./desktop/export.js";
import * as futures from "./futures/export.js";

const glue = new Glue();

const desktopEnv = desktop.newEnv(glue);
const futuresEnv = futures.newEnv(glue);

console.log("init wasm..", futuresEnv);

const wasm = await WebAssembly.instantiateStreaming(fetch("showcase.wasm"), {
  env: { ...desktopEnv, ...futuresEnv }
} );

glue.init(wasm.instance);

// @ts-ignore
wasm.instance.exports.run();
