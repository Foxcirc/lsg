
fn main() {

    #[cfg(feature = "import")]
    {

        use std::{env, fs, path::PathBuf};

        // println!("cargo::rerun-if-changed={}", "Cargo.toml");

        let outdir = PathBuf::from(env::var("OUT_DIR")
            .expect("OUT_DIR not present"));

        let lsgdir = outdir.join("lsg");

        if !lsgdir.exists() {
            fs::create_dir(&lsgdir)
                .expect("create artifact dir");
        }



        // fs::copy("/home/moritz/Projects/lsg/target/debug/libdesktop.a", "libdesktop.a").expect("copy library into artifact dir");
        // fs::copy("libdesktop.a", lsgdir.join("libdesktop.a")).expect("copy library into artifact dir");

        let target = env::var("TARGET")
            .expect("TARGET not present");

        match &target[..] {
            "x86_64-unknown-linux-gnu" => {
                // Static link targets. Here the program will be built as
                // a native executable and will be loaded by the OS.
                println!("cargo::rustc-link-search=native={}", lsgdir.display());
                println!("cargo:rustc-link-lib=desktop");
                println!("cargo:rustc-link-lib=static=desktop");
            },
            "wasm32-unknown-unknown" => {
                // Dynamic link targets. Here the program will be built
                // as a dynlib (or wasm module) and loaded by our native loader.
                // println!("cargo:rustc-link-lib=desktop");
            },
            other => {
                panic!("unsupported target: {}", other);
            }
        }

    }

}
