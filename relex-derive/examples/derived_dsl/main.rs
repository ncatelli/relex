use relex_derive::Relex;

#[derive(Relex, Debug, PartialEq)]
pub enum Token {
    #[matches(r"[0-9]+[.][0-9]+", |lex: &str| { lex.parse::<f32>().ok() })]
    FloatLit(f32),
    #[matches(r"[0-9]+", |lex: &str| { lex.parse::<i32>().ok() })]
    IntLit(i32),
    #[matches("\"[a-zA-Z]+\"", |lex: &str| { Some(lex.trim_matches('\"').to_string()) })]
    StringLit(String),
    #[matches(r"+")]
    Plus,
    #[matches(r"-")]
    Minus,
    #[matches("\n")]
    Newline,
    #[skip(" |\t")]
    WhiteSpace,
    #[matches("[.]")]
    Dot,
}

fn main() -> Result<(), String> {
    let stream = token_stream_from_input("2 + 12-3 + 45.0+ 4\n\t\"hello\"")?;
    for token in stream {
        println!("{:?}", token);
    }

    Ok(())
}
