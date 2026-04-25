
fn main() {

    println!("cargo:rustc-link-lib=xkbcommon");
    println!("cargo:rustc-link-lib=wayland-client");

}
