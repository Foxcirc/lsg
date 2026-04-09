
fn main() {

    #[cfg(feature = "import")]
    {

        use std::{env, fs, path::PathBuf};

        // println!("cargo::rerun-if-changed={}", "Cargo.toml");

        let outdir = PathBuf::from(env::var("OUT_DIR").expect("env var not present"));
        let lsgdir = outdir.join("lsg");

        if !lsgdir.exists() {
            fs::create_dir(&lsgdir).expect("create artifact dir");
        }

        fs::copy("/home/moritz/Projects/lsg/target/debug/libdesktop.a", "libdesktop.a").expect("copy library into artifact dir");
        fs::copy("libdesktop.a", lsgdir.join("libdesktop.a")).expect("copy library into artifact dir");

        println!("cargo::rustc-link-search=native={}", lsgdir.display());
        println!("cargo:rustc-link-lib=static=desktop");

    }

}
