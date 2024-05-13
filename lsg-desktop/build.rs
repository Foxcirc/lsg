
use core::panic;
use std::{env, error::Error as StdError, fs, path, process::Command};

fn main() -> Result<(), Box<dyn StdError>> {

    #[cfg(target_os = "linux")]
    {

           println!("cargo::rustc-link-lib=dylib=systemd");

   }

//         // linkage of basu (sd-dbus)

//         let out_dir = env::var("OUT_DIR")?;
//         let build_dir = path::PathBuf::from(out_dir);

//         // link to the library that will be generated

//         println!("cargo::rustc-link-search=native={}", build_dir.to_string_lossy());
//         println!("cargo::rustc-link-lib=static={}", "basu");


//         // build basu using meson + ninja

//         fs::create_dir_all(&build_dir)?; // make sure this exists

//         // first check if it needs to be rebuild

//         let iter = fs::read_dir(&build_dir)?;
//         for it in iter {
//             if it?.file_name().as_encoded_bytes() == b"libbasu.a" {
//                 return Ok(()) // already build
//             }
//         }

//         // meson setup

//         let status = Command::new("meson")
//             .arg("setup")
//             .arg(&build_dir)
//             .arg("src/platform/basu")
//             .status()?;

//         if !status.success() {
//             panic!("meson (setup) exited with status {}", status);
//         }

//         // meson configure

//         let status = Command::new("meson")
//             .arg("configure")
//             .arg("-Daudit=disabled")
//             .arg("-Dlibcap=disabled")
//             .arg(&build_dir)
//             .status()?;

//         if !status.success() {
//             panic!("meson (configure) exited with status {}", status);
//         }

//         // ninja

//         let status = Command::new("ninja")
//             .arg("-C")
//             .arg(&build_dir)
//             .status()?;

//         if !status.success() {
//             panic!("ninja exited with status {}", status);
//         }
    
    Ok(())
    
}
