#![feature(portable_simd)]

// main.rs is also a binary crate root, where its feature attribute is needed.
#[allow(unused_attributes)]
#[path = "../main.rs"]
mod kansei;

// Keep the interpreter's crate-relative module paths available in this binary.
use kansei::*;

fn main() -> rustyline::Result<()>
{
    kansei::main()
}
