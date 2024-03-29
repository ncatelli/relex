use relex_derive::{Relex, VariantKind};

fn float_validator(lexed_input: &str) -> Option<f32> {
    lexed_input.parse::<f32>().ok()
}

#[derive(Relex, VariantKind, Debug, PartialEq)]
pub enum Token {
    #[matches(r"[0-9]+[.][0-9]+", float_validator)]
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
    #[eoi]
    Eoi,
}

fn main() -> Result<(), String> {
    let stream = token_stream_from_input("2 + 12-3 + 45.0+ 4\n\t\"hello\"")?;
    for token in stream {
        // convert the token variant to it's representable kind.
        let kind = token.variant.to_variant_kind();

        println!("{:?}", kind);

        if kind == TokenVariantKind::Eoi {
            break;
        }
    }

    Ok(())
}
