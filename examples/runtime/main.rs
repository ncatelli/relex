use regex_runtime::{bytecode::FromBytecode, Instructions, SaveGroupSlot};

const TEST_BIN: [u8; 416] = [
    240, 240, 0, 0, 1, 0, 0, 0, 23, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 26, 26, 0, 0, 1, 0, 0, 0, 48, 0, 0, 0, 57, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0,
    1, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 15, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 43,
    0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 59, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0,
    0, 23, 0, 0, 0, 21, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0,
    0, 0, 20, 0, 0, 0, 0, 0, 0, 0,
];

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // "^([0-9]+)"
    Number(i32),
    // "^(+)"
    Plus,
    // "^(;)"
    Semicolon,
}

fn main() -> Result<(), String> {
    let program = Instructions::from_bytecode(TEST_BIN).map_err(|e| e.to_string())?;
    let input = "1+2;";
    let mut curr_input = &input[0..];

    loop {
        let res = regex_runtime::run::<1>(&program, curr_input);
        match res {
            Some(
                [SaveGroupSlot::Complete {
                    expression_id,
                    start,
                    end,
                }],
            ) => {
                match expression_id {
                    0 => print!("Token::Number "),
                    1 => print!("Token::Plus "),
                    2 => print!("Token::SemiColon "),
                    _ => return Err("no match".to_string()),
                };

                print!("{} ", &curr_input[start..end]);
                curr_input = &curr_input[end..];
                println!("[remaining: {:?}]", &curr_input);
            }

            None => return Ok(()),
            _ => return Err("no match".to_string()),
        }
    }
}
