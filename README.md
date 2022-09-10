# relex
A lexer generator for the first principles project.

## Examples

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

## Grammar
The grammar can be found at [relex.ebnf](./docs/relex.ebnf). Additionally this includes an [xhtml version](./docs/relex.xhtml) for viewing in a browser. This was directly generated from [relex.ebnf](./docs/relex.ebnf) with [rr](https://githug.com/ncatelli/rr-docker.git).
