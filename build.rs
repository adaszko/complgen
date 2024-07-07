use std::process::Command;

fn main() {
    let Ok(output) = Command::new("git").args(&["describe", "--long", "--dirty", "--abbrev=10", "--tags", "--always"]).output() else {
        println!("cargo:warning=Git binary not found.  `complgen version` output will be inaccurate");
        println!("cargo:rustc-env=COMPLGEN_VERSION={}", "fallback");
        return;
    };
    let version = String::from_utf8(output.stdout).unwrap();
    println!("cargo:rustc-env=COMPLGEN_VERSION={}", version);
    println!("cargo:rerun-if-changed=.git/HEAD");
}
