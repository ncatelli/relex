use std::io::{self, BufRead};

const USAGE: &str = "grep-analog PATTERN [FILE]";

fn main() -> Result<(), String> {
    let (debug, args) = std::env::args()
        .skip(1)
        .fold((false, vec![]), |(debug, mut args), arg| {
            if arg == "--debug" || arg == "-d" {
                (true, args)
            } else {
                args.push(arg);
                (debug, args)
            }
        });
    let arg_len = args.len();

    let (pattern, input) = match arg_len {
        1 => args
            .get(0)
            .map(|pattern| (pattern, io::stdin()))
            .ok_or_else(|| USAGE.to_string()),
        _ => Err(USAGE.to_string()),
    }?;

    let pattern_input: Vec<(usize, char)> = pattern.chars().enumerate().collect();
    let program = relex::parse(&pattern_input)
        .map_err(|e| format!("{:?}", e))
        .and_then(relex::compile)?;

    if debug {
        println!(
            "DEBUG
--------
{}--------
",
            program
        )
    }

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