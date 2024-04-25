use std::fmt::Debug;

mod parser;

use crate::parser::combinators::*;

fn execute<'a, O: Debug, Err: Debug>(parser: impl parser::Parser<'a, O, Err>, input: &'a str) {
    match parser.parse(input) {
        Ok((result, rest)) => {
            println!("{:?} {:?}", result, rest);
        }
        Err(err) => {
            println!("{:?}", err);
        }
    }
}

fn main() {
    let file = std::fs::File::open("main.osta").unwrap();
    let mmap = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let input = unsafe { std::str::from_utf8_unchecked(&mmap) };

    execute(
        pair(literal("hello"), pair(regex(r"\s+"), literal("world"))),
        input,
    );
}
