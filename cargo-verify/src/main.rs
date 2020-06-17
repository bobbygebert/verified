use std::io;
use std::process::Command;
use std::vec;

mod parse;
mod translate;
use translate::Translator;

pub struct Syn;
pub struct UInt;
pub struct UTerm;
pub struct B0;
pub struct B1;

#[macro_export]
macro_rules! ty {
    (UInt) => {{
        let zero = std::any::type_name::<verified::UInt<verified::UTerm, verified::B0>>();
        zero[..zero.find('<').unwrap()].as_bytes()
    }};
    (UTerm) => {
        std::any::type_name::<verified::UTerm>().as_bytes()
    };
    (B0) => {
        std::any::type_name::<verified::B0>().as_bytes()
    };
    (B1) => {
        std::any::type_name::<verified::B1>().as_bytes()
    };
}

#[macro_export]
macro_rules! token {
    (_) => {
        "_".as_bytes()
    };
    (<) => {
        "<".as_bytes()
    };
    (>) => {
        ">".as_bytes()
    };
    (,) => {
        ",".as_bytes()
    };
    (::) => {
        "::".as_bytes()
    };
    (=) => {
        "=".as_bytes()
    };
    (Output) => {
        "Output".as_bytes()
    };
}

#[macro_export]
macro_rules! op {
    (!) => {
        "std::ops::Not".as_bytes()
    };
    (+) => {
        "std::ops::Add".as_bytes()
    };
    (&) => {
        "std::ops::BitAnd".as_bytes()
    };
    (|) => {
        "std::ops::BitOr".as_bytes()
    };
    (^) => {
        "std::ops::BitXor".as_bytes()
    };
    (/) => {
        "std::ops::Div".as_bytes()
    };
    (==) => {
        "typenum::type_operators::IsEqual".as_bytes()
    };
    (>=) => {
        "typenum::type_operators::IsGreaterOrEqual".as_bytes()
    };
    (>) => {
        "typenum::type_operators::IsGreater".as_bytes()
    };
    (<=) => {
        "typenum::type_operators::IsLessOrEqual".as_bytes()
    };
    (<) => {
        "typenum::type_operators::IsLess".as_bytes()
    };
    (*) => {
        "std::ops::Mul".as_bytes()
    };
    (%) => {
        "std::ops::Rem".as_bytes()
    };
    (!=) => {
        "typenum::type_operators::IsNotEqual".as_bytes()
    };
    (<<) => {
        "std::ops::Shl".as_bytes()
    };
    (>>) => {
        "std::ops::Shr".as_bytes()
    };
    (-) => {
        "std::ops::Sub".as_bytes()
    };
}

fn main() {
    let output = Command::new("cargo")
        .args(vec!["build", "--color", "always"])
        .output()
        .unwrap()
        .stderr;
    Translator::new(io::stderr()).translate(output);
}
