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

pub fn codegen(rule_set: ast::RuleSet) -> Result<String, String> {
    let header = rule_set.header.as_ref().map(|h| h.as_ref()).unwrap_or("");
    let rules = rule_set.rules.as_ref();

    let id_captures = rules.iter().map(|rule| {
        let captures = rule
            .capture
            .as_ref()
            .map_or_else(Vec::new, |caps| caps.0.iter().map(|ci| &ci.ty).collect());

        (rule.identifier.as_ref(), captures)
    });

    let patterns = rules
        .iter()
        .map(|rule| {
            use regex_compiler::bytecode::ToBytecode;

            regex_compiler::parse(rule.pattern.0.to_string())
                .map_err(|e| format!("{:?}", e))
                .and_then(regex_compiler::compile)
                .map(|inst| inst.to_bytecode())
        })
        .collect::<Result<Vec<_>, _>>()?;

    let _pattern_action = patterns.iter().zip(rules.iter().map(|rule| &rule.action));

    let variants = id_captures
        .map(|(id, captures)| TokenVariant { id, captures })
        .collect::<Vec<TokenVariant<'_>>>();

    Token(variants)
        .to_rust_code()
        // merge the header and variants
        .map(|variants| vec![header.to_string(), variants].join("\n"))
        .map_err(|_| "unable to generate token enum".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;

    #[test]
    fn should_codgen_valid_input() {
        let pattern = Pattern(PatternItem("ab".chars().map(Char::from).collect()));
        let action = Action(ActionItem(
            "Some(Token::Test)".chars().map(Char::from).collect(),
        ));
        let rules = Rules(vec![Rule::new(
            Identifier("Test".to_string()),
            None,
            pattern,
            action,
        )]);

        let rule_set = RuleSet::new(None, rules);

        let expected = "
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
Test,
}";

        assert_eq!(Ok(expected.to_string()), codegen(rule_set))
    }

    #[test]
    fn should_codgen_valid_input_with_header() {
        let pattern = Pattern(PatternItem("ab".chars().map(Char::from).collect()));
        let action = Action(ActionItem(
            "Some(Token::Test)".chars().map(Char::from).collect(),
        ));
        let rules = Rules(vec![Rule::new(
            Identifier("Test".to_string()),
            None,
            pattern,
            action,
        )]);

        let header = "const UNIT: () = ();";

        let rule_set = RuleSet::new(None, rules).with_header(Header::new(header));

        let expected = "const UNIT: () = ();
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
Test,
}";

        assert_eq!(Ok(expected.to_string()), codegen(rule_set))
    }

    #[test]
    fn should_generate_tokens_without_error() {
        let int8_ct = CaptureType::try_new("i8").unwrap();
        let uint64_ct = CaptureType::try_new("u64").unwrap();
        let str_ct = CaptureType::try_new("String").unwrap();
        let bool_ct = CaptureType::try_new("bool").unwrap();

        let inputs = vec![
            (
                Token(vec![TokenVariant {
                    id: "Test",
                    captures: vec![],
                }]),
                "#[derive(Debug, Clone, PartialEq)]\npub enum Token {\nTest,\n}".to_string(),
            ),
            (
                Token(vec![TokenVariant {
                    id: "Test",
                    captures: vec![&int8_ct],
                }]),
                "#[derive(Debug, Clone, PartialEq)]\npub enum Token {\nTest(i8),\n}".to_string(),
            ),
            (
                Token(vec![TokenVariant {
                    id: "Test",
                    captures: vec![&int8_ct, &bool_ct, &str_ct, &uint64_ct],
                }]),
                "#[derive(Debug, Clone, PartialEq)]\npub enum Token {\nTest(i8, bool, String, u64),\n}".to_string(),
            ),
        ];

        for (test_id, (input, expected)) in inputs.into_iter().enumerate() {
            assert_eq!((test_id, Ok(expected)), (test_id, input.to_rust_code()));
        }
    }
}
