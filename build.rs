// build.rs
use std::process::Command;
fn main() {
    // note: add error checking yourself.
    let output = Command::new("git").args(&["describe", "--long", "--dirty", "--abbrev=10", "--tags", "--always"]).output().unwrap();
    let version = String::from_utf8(output.stdout).unwrap();
    println!("cargo:rustc-env=COMPLGEN_VERSION={}", version);
    println!("cargo:rerun-if-changed=.git/HEAD");
}
