
use std::error::Error as StdError;

fn main() -> Result<(), Box<dyn StdError>> {

    #[cfg(target_os = "linux")]
    println!("cargo::rustc-link-lib=dylib=systemd");

    Ok(())
    
}
