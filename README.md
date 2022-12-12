# relex
A lexer generator for the first principles project.

## Examples
### Using the custom DSL in build.rs

in a `build.rs` script

```rust
// build.rs

use std::env;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::Path;


const RULE_SPEC: &str = "
RULE Number(digit: i32) [([0-9]+)] => %%{ val
                        .parse::<i32>()
                        .ok() }%%\n
RULE Plus [(+)] => %%{ }%%
RULE Semicolon [(;)] => %%{ }%%
";

fn main() -> Result<(), String> { 
	let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("unicode_mappings.rs");
    let cur_dir = env::current_dir().unwrap();

    let input = RULE_SPEC.chars().enumerate().collect::<Vec<_>>();
    let rule_set = relex::parser::parse(&input).map_err(|e| e.to_string())?;
    let generated = relex::codegen::codegen(&rule_set)?;

    fs::write(&dest_path, generated).map_err(|e| e.to_string())?;
    println!("cargo:rerun-if-changed=build.rs");

	Ok(())
}
```

#### Grammar
The grammar can be found at [relex.ebnf](./docs/relex.ebnf). Additionally this includes an [xhtml version](./docs/relex.xhtml) for viewing in a browser. This was directly generated from [relex.ebnf](./docs/relex.ebnf) with [rr](https://githug.com/ncatelli/rr-docker.git).

### Using proc-macros'

```rust
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
```

#### Attributes
##### matches
`matches` attempts to match a regular expression, associating the match with a corresponding token. Additionally, an optional closure can be passed that returns an `Option<T>` for converting the match to the corresponding field enclosed on the token's variant.

```rust
...
#[matches(r"+")]
Plus,
#[matches(r"[0-9]+[.][0-9]+", |lex: &str| { lex.parse::<f32>().ok() })]
FloatLit(f32),
...
```

##### skip
`skip` attempts to match a regular expression of which will the match will then be ignored from the token stream. An example of such a use case would be whitespace in C. 

```rust
...
#[skip(" |\t")]
WhiteSpace,
...
```