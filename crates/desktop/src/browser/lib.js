
import { App } from "./export.js";

// =====================================================
// LOAD
// =====================================================
export async function loadApp(path = "./app.wasm") {

  const app = new App();

  const result = await WebAssembly.instantiateStreaming(
    fetch(path),
    { env: app.env() }
  );

  app.init(result.instance);

  result.instance.exports.run();

}
