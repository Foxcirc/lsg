
build-browser:
	cargo build -p showcase --target wasm32-unknown-unknown --features browser
	esbuild crates/showcase/browser/main.ts --bundle --outfile=crates/showcase/browser/out.js --platform=browser --sourcemap --format=esm
	cp target/wasm32-unknown-unknown/debug/showcase.wasm crates/showcase/browser/blob.wasm
