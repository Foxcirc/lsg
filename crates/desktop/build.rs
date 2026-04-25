
use std::{env, fs, path::PathBuf};

fn main() {

    let import = has_feature("import");
    if import {

        // println!("cargo::rerun-if-changed={}", "Cargo.toml");

        let outdir = PathBuf::from(env::var("OUT_DIR")
            .expect("OUT_DIR not present"));

        let lsgdir = outdir.join("lsg");

        if !lsgdir.exists() {
            fs::create_dir(&lsgdir)
                .expect("create artifact dir");
        }

        fs::copy(
            "/home/moritz/Projects/lsg/target/debug/lsg/libdesktop.a",
            lsgdir.join("libdesktop.a")
        ).expect("copy library into artifact dir");
        // fs::copy("libdesktop.a", lsgdir.join("libdesktop.a")).expect("copy library into artifact dir");

        let target = get_target_triple();

        match &target[..] {
            // Static link targets. Here the program will be built as
            // a native executable and will be loaded by the OS.
            "x86_64-unknown-linux-gnu" => {

                println!("cargo::rustc-link-search=native={}", lsgdir.display());
                println!("cargo:rustc-link-lib=desktop");
                println!("cargo:rustc-link-lib=static=desktop");

                // If building a binary, we also need to include the dependencies
                // explicitly since `libdesktop.a` does not pass them on.
                println!("cargo:rustc-link-lib=xkbcommon");
                println!("cargo:rustc-link-lib=wayland-client");

            },
            // Dynamic link targets. Here the program will be built
            // as a dynlib (or wasm module) and loaded by our native loader.
            "wasm32-unknown-unknown" => {
                // Don't link anything right now. The loader will know
                // what to provide.
            },
            other => {
                panic!("unsupported target: {}", other);
            }
        }

    }

}

fn has_feature(name: &str) -> bool {
    env::var(format!("CARGO_FEATURE_{}", name.to_ascii_uppercase())).is_ok()
}

fn get_target_triple() -> String {
    env::var("TARGET").expect("`TARGET` not present")
}
