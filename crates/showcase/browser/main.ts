
import { Glue } from "../../../browser/glue";
import * as desktop from "../../../browser/desktop";
import * as futures from "../../../browser/futures";
import * as graphics from "../../../browser/graphics";

const mainWindow = document.getElementById("mainWindow");
if (!mainWindow) throw new Error("`mainWindow` element not present");

const glue = new Glue();

const wasm = await WebAssembly.instantiateStreaming(fetch("blob.wasm"), {
  env: {
    ...desktop.newEnv(glue, mainWindow),
    ...futures.newEnv(glue),
    ...graphics.newEnv(glue)
  }
} );

glue.init(wasm.instance);

(wasm.instance.exports as any).run();
