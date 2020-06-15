use std::io;
use std::process::Command;

mod parse;
mod translate;
use translate::Translator;

fn main() {
    let output = Command::new("cargo")
        .args(vec!["build", "--color", "always"])
        .output()
        .unwrap()
        .stderr;
    Translator::new(io::stderr()).translate(output);
}
