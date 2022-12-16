# relex
A lexer generator for the first principles project.

## Examples
```rust
use relex_derive::Relex;

fn float_validator(lexed_input: &str) -> Option<f32> {
    lexed_input.parse::<f32>().ok()
}

#[derive(Relex, Debug, PartialEq)]
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
}

fn main() -> Result<(), String> {
    let stream = token_stream_from_input("2 + 12-3 + 45.0+ 4\n\t\"hello\"")?;
    for token in stream {
        println!("{:?}", token);
    }

    Ok(())
}
```

### Attributes
#### matches
`matches` attempts to match a regular expression, associating the match with a corresponding token. Additionally, an optional closure or function can be passed that takes a single `&str` parameter and returns an `Option<T>` for converting the match to the corresponding field enclosed on the token's variant.

```rust
...
#[matches(r"+")]
Plus,
#[matches(r"[0-9]+[.][0-9]+", |lex: &str| { lex.parse::<f32>().ok() })]
FloatLit(f32),
...
```

#### skip
`skip` attempts to match a regular expression of which will the match will then be ignored from the token stream. An example of such a use case would be whitespace in C. 

```rust
...
#[skip(" |\t")]
WhiteSpace,
...
```