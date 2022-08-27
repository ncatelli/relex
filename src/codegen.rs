use regex_compiler::{bytecode::ToBytecode, compile_many};

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
        let prefix = "#[derive(Debug, PartialEq)]\npub enum Token {\n".to_string();
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

struct PatterMatcher<'a> {
    program_binary: &'a [u8],
}

impl<'a> PatterMatcher<'a> {
    fn new(program_binary: &'a [u8]) -> Self {
        Self { program_binary }
    }
}

impl<'a> ToRust for PatterMatcher<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let bin_len = self.program_binary.len();

        Ok(format!(
            "const PROG_BINARY: [u8; {}] = {:?};",
            bin_len, self.program_binary
        ))
    }
}

struct TokenVariant<'a> {
    /// Represents id of the variant. This aligns with the expression id of
    /// the regex matcher program.
    id: &'a str,
    capture_ty: Option<&'a CaptureType>,
}

impl<'a> ToRust for TokenVariant<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let variant = if self.capture_ty.is_none() {
            self.id.to_string()
        } else {
            let capture: Result<_, _> = self.capture_ty.map(|ct| ct.to_rust_code()).unwrap();

            format!("{}({})", self.id, capture.map_err(|_| ())?)
        };

        Ok(variant)
    }
}

pub fn codegen(rule_set: ast::RuleSet) -> Result<String, String> {
    let header = rule_set.header.as_ref().map(|h| h.as_ref()).unwrap_or("");
    let rules = rule_set.rules.as_ref();

    let id_captures = rules.iter().map(|rule| {
        let captures = rule.capture.as_ref().map(|cap| &cap.0.ty);

        (rule.identifier.as_ref(), captures)
    });

    let patterns = rules
        .iter()
        .map(|rule| {
            regex_compiler::parse(rule.pattern.0.to_string()).map_err(|e| format!("{:?}", e))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let pattern_binary = compile_many(patterns).map(|insts| insts.to_bytecode())?;
    let _pattern_action = rules.iter().map(|rule| &rule.action);
    let pattern_matcher = PatterMatcher::new(&pattern_binary);

    let pattern_matcher_str_repr = pattern_matcher
        .to_rust_code()
        .map_err(|_| "unabled to serialize pattern binary")?;

    let variants = id_captures
        .map(|(id, captures)| TokenVariant {
            id,
            capture_ty: captures,
        })
        .collect::<Vec<TokenVariant<'_>>>();

    let variants_str_repr = Token(variants)
        .to_rust_code()
        .map_err(|_| "unable to generate token enum".to_string())?;

    Ok(vec![
        header.to_string(),
        pattern_matcher_str_repr,
        variants_str_repr,
    ]
    .join("\n"))
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
const PROG_BINARY: [u8; 144] = [240, 240, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 97, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
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
const PROG_BINARY: [u8; 144] = [240, 240, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 97, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
Test,
}";

        let result = codegen(rule_set);
        let contains_str = result.as_ref().map(|res| res.contains(&expected));
        let expected: Result<String, ()> = Ok(expected.to_string());

        assert_eq!(
            Ok(true),
            contains_str,
            "  left: `{:?}`\n right: `{:?}`",
            expected,
            result
        );
    }

    #[test]
    fn should_generate_tokens_without_error() {
        let int8_ct = CaptureType::try_new("i8").unwrap();

        let inputs = vec![
            (
                Token(vec![TokenVariant {
                    id: "Test",
                    capture_ty: None,
                }]),
                "#[derive(Debug, Clone, PartialEq)]\npub enum Token {\nTest,\n}".to_string(),
            ),
            (
                Token(vec![TokenVariant {
                    id: "Test",
                    capture_ty: Some(&int8_ct),
                }]),
                "#[derive(Debug, Clone, PartialEq)]\npub enum Token {\nTest(i8),\n}".to_string(),
            ),
        ];

        for (test_id, (input, expected)) in inputs.into_iter().enumerate() {
            assert_eq!((test_id, Ok(expected)), (test_id, input.to_rust_code()));
        }
    }
}
