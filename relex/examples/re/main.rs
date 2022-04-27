use std::io::{self, BufRead};

const USAGE: &str = "grep-analog PATTERN [FILE]";

fn main() -> Result<(), String> {
    let mut args = std::env::args();
    let arg_len = args.len();

    let (pattern, input) = match arg_len {
        2 => args
            .nth(1)
            .ok_or_else(|| USAGE.to_string())
            .map(|pattern| (pattern, io::stdin())),
        _ => Err(USAGE.to_string()),
    }?;

    let pattern_input: Vec<(usize, char)> = pattern.chars().enumerate().collect();
    let program = relex::parse(&pattern_input)
        .map_err(|e| format!("{:?}", e))
        .and_then(relex::compile)?;

    for line in input.lock().lines() {
        match line {
            Ok(line) => match relex_runtime::run::<0>(&program, &line) {
                Some(_) => println!("{}", line),
                None => continue,
            },
            Err(e) => return Err(format!("{}", e)),
        }
    }

    Ok(())
}
