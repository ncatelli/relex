const RULE_SPEC: &str = "
{{{}}}
RULE Number(digit: i32) [([0-9]+)] => %%{ val
                        .parse::<i32>()
                        .map_err(|e| e.to_string())
                        .map(TokenVariant::Number)
                        .ok() }%%\n
RULE Plus [(+)] => %%{ Some(TokenVariant::Plus) }%%
RULE Semicolon [(;)] => %%{ Some(TokenVariant::Semicolon) }%%
";

fn main() -> Result<(), String> {
    let input = RULE_SPEC.chars().enumerate().collect::<Vec<_>>();
    let rule_set = relex::parser::parse(&input).map_err(|e| e.to_string())?;
    let generated = relex::codegen::codegen(&rule_set)?;

    println!("{}", &generated);

    Ok(())
}
