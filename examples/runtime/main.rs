#[allow(unused)]
use regex_compiler::{compile, parse};
use regex_runtime::{bytecode::FromBytecode, Instructions};

const TEST_BIN: [u8; 240] = [
    240, 240, 1, 0, 1, 0, 0, 0, 12, 0, 0, 0, 48, 0, 0, 0, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 26, 26, 0, 0, 1, 0, 0, 0, 97, 0, 0, 0, 122, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0,
    0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 34, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 7, 0,
    0, 0, 9, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 6, 0,
    0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 34,
    0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
];

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Test(String),
}

fn main() -> Result<(), String> {
    let program = Instructions::from_bytecode(TEST_BIN).map_err(|e| e.to_string())?;

    if let Some(val) = regex_runtime::run::<1>(&program, "\"abcd\"") {
        println!("{:?}", val);
        return Ok(());
    }

    Err("no match".to_string())
}
