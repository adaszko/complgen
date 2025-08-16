use std::process::Command;

fn main() {
    let Ok(output) = Command::new("git")
        .args([
            "describe",
            "--long",
            "--dirty",
            "--abbrev=10",
            "--tags",
            "--always",
        ])
        .output()
    else {
        println!(
            "cargo:warning=`git describe` command failed.  Falling back to CARGO_PKG_VERSION.  `complgen version` output will be imprecise"
        );
        println!(
            "cargo:rustc-env=COMPLGEN_VERSION={}",
            env!("CARGO_PKG_VERSION")
        );
        return;
    };
    let version = String::from_utf8(output.stdout).unwrap();
    println!("cargo:rustc-env=COMPLGEN_VERSION={}", version);
    println!("cargo:rerun-if-changed=.git/HEAD");
}
