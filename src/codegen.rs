use crate::ast::CaptureType;

use super::ast;

trait ToRust {
    type Error;

    fn to_rust_code(&self) -> Result<String, Self::Error>;
}

impl ToRust for CaptureType {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let repr = self.0.clone();

        Ok(repr)
    }
}

struct Token<'a>(Vec<TokenVariant<'a>>);

impl<'a> ToRust for Token<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let prefix = "#[derive(Debug, Clone, PartialEq)]\npub enum Token {\n".to_string();
        let suffix = "}".to_string();

        let variants = self
            .0
            .iter()
            .map(|variant| variant.to_rust_code().map(|s| format!("{},\n", s)));

        [Ok(prefix)]
            .into_iter()
            .chain(variants)
            .chain([Ok(suffix)].into_iter())
            .collect()
    }
}

struct TokenVariant<'a> {
    id: &'a str,
    captures: Vec<&'a CaptureType>,
    #[allow(unused)]
    pattern: Vec<u8>,
}

impl<'a> ToRust for TokenVariant<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let variant = if self.captures.is_empty() {
            self.id.to_string()
        } else {
            let captures: Result<Vec<_>, _> =
                self.captures.iter().map(|ct| ct.to_rust_code()).collect();
            let captures = captures.map_err(|_| ())?;

            format!("{}({})", self.id, captures.join(", "))
        };

        Ok(variant)
    }
}

pub fn codegen(rules: ast::Rules) -> Result<String, String> {
    let rules = rules.as_ref();

    let id_captures = rules.iter().map(|rule| {
        let captures = rule
            .capture
            .as_ref()
            .map_or_else(Vec::new, |caps| caps.0.iter().map(|ci| &ci.ty).collect());

        (rule.identifier.as_ref(), captures)
    });

    let patterns = rules.iter().map(|rule| {
        use regex_compiler::bytecode::ToBytecode;

        regex_compiler::parse(rule.pattern.0.to_string())
            .map_err(|e| format!("{:?}", e))
            .and_then(regex_compiler::compile)
            .map(|inst| inst.to_bytecode())
    });

    let variants = id_captures
        .zip(patterns)
        .map(|((id, captures), program)| {
            program.map(|program| TokenVariant {
                id,
                captures,
                pattern: program,
            })
        })
        .collect::<Result<Vec<TokenVariant<'_>>, String>>()?;

    Token(variants)
        .to_rust_code()
        .map_err(|_| "unable to generate token enum".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    #[test]
    fn should_codgen_valid_input() {
        let pattern = Pattern(PatternItem("ab".chars().map(Char::from).collect()));
        let action = Action(ActionItem("Some(())".chars().map(Char::from).collect()));
        let rules = Rules(vec![Rule::new(
            Identifier("Test".to_string()),
            None,
            pattern,
            action,
        )]);

        assert!(codegen(rules).is_ok())
    }

    #[test]
    fn should_generate_tokens_without_error() {
        let int8_ct = CaptureType::try_new("i8").unwrap();
        let uint64_ct = CaptureType::try_new("u64").unwrap();
        let str_ct = CaptureType::try_new("String").unwrap();
        let bool_ct = CaptureType::try_new("bool").unwrap();

        let inputs = vec![
            Token(vec![TokenVariant {
                id: "Test",
                captures: vec![],
                pattern: vec![],
            }]),
            Token(vec![TokenVariant {
                id: "Test",
                captures: vec![&int8_ct],
                pattern: vec![],
            }]),
            Token(vec![TokenVariant {
                id: "Test",
                captures: vec![&int8_ct, &bool_ct, &str_ct, &uint64_ct],
                pattern: vec![],
            }]),
        ];

        for (test, input) in inputs.into_iter().enumerate() {
            assert_eq!((test, true), (test, input.to_rust_code().is_ok()));
        }
    }
}
