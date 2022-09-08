use regex_compiler::{bytecode::ToBytecode, compile_many};

use crate::ast::CaptureType;

use super::ast;

#[allow(unused)]
const LEXER_TYPE_DEFS: &str = "
use regex_runtime::{Instructions, SaveGroupSlot};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl From<Span> for std::ops::Range<usize> {
    fn from(span: Span) -> Self {
        let start = span.start;
        let end = span.end;

        start..end
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    span: Span,
    variant: TokenVariant,
}

impl Token {
    pub fn new(span: Span, variant: TokenVariant) -> Self {
        Self { span, variant }
    }

    pub fn as_span(&self) -> Span {
        self.span
    }

    pub fn to_variant(self) -> TokenVariant {
        self.variant
    }
}

pub fn token_stream_from_input(input: &str) -> Result<TokenStream<'_>, String> {
    use regex_runtime::bytecode::FromBytecode;

    let program = Instructions::from_bytecode(PROG_BINARY).map_err(|e| e.to_string())?;
    Ok(TokenStream::new(program, input))
}

pub struct TokenStream<'a> {
    input_stream: &'a str,
    program: Instructions,
    offset: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(program: Instructions, input_stream: &'a str) -> Self {
        Self {
            input_stream,
            program,
            offset: 0,
        }
    }
}
";

const TOKEN_STREAM_HEAD: &str = "impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let res = regex_runtime::run::<1>(&self.program, self.input_stream);

        let (tok, next_input, next_offset) = match res {
            Some(
                [SaveGroupSlot::Complete {
                    expression_id,
                    start,
                    end,
                }],
            ) => {
                let val = self.input_stream.get(start..end)?;
";

const TOKEN_STREAM_TAIL: &str = "
                let next_input = self.input_stream.get(end..)?;
                // the next match should always start at 0, so the end value marks consumed chars.
                let consumed = end;

                let adjusted_start = self.offset + start;
                let adjusted_end = self.offset + end;

                variant
                    .map(|tv| Token::new(Span::from(adjusted_start..adjusted_end), tv))
                    .map(|tok| (tok, next_input, consumed))
            }

            _ => None,
        }?;

        // advance the stream
        self.input_stream = next_input;
        self.offset += next_offset;
        Some(tok)
    }
}
";

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

struct IrToken<'a>(Vec<IrTokenVariant<'a>>);

impl<'a> ToRust for IrToken<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let prefix = "#[derive(Debug, PartialEq, Eq)]\npub enum TokenVariant {\n".to_string();
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

struct PatternBin<'a> {
    program_binary: &'a [u8],
}

impl<'a> PatternBin<'a> {
    fn new(program_binary: &'a [u8]) -> Self {
        Self { program_binary }
    }
}

impl<'a> ToRust for PatternBin<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let bin_len = self.program_binary.len();

        Ok(format!(
            "const PROG_BINARY: [u8; {}] = {:?};",
            bin_len, self.program_binary
        ))
    }
}

enum ActionKind<'a> {
    Capturing(&'a ast::Action),
    NonCapturing(&'a ast::Action),
}

struct ActionVariantDispatcher<'a> {
    token_variant: &'a ast::Identifier,
    expr_idx: usize,
    action: ActionKind<'a>,
}

impl<'a> ActionVariantDispatcher<'a> {
    fn new(token_variant: &'a ast::Identifier, expr_idx: usize, action: ActionKind<'a>) -> Self {
        Self {
            token_variant,
            expr_idx,
            action,
        }
    }
}

impl<'a> ToRust for ActionVariantDispatcher<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        match self.action {
            ActionKind::Capturing(action) => Ok(format!(
                "{} => {{{}.map(TokenVariant::{})}},\n",
                self.expr_idx,
                action.0,
                self.token_variant.as_ref()
            )),
            ActionKind::NonCapturing(_) => Ok(format!(
                "{} => {{Some(TokenVariant::{})}},\n",
                self.expr_idx,
                self.token_variant.as_ref()
            )),
        }
    }
}

struct ActionDispatcher<'a> {
    actions: &'a [ActionVariantDispatcher<'a>],
}

impl<'a> ActionDispatcher<'a> {
    fn new(actions: &'a [ActionVariantDispatcher<'a>]) -> Self {
        Self { actions }
    }

    const HEADER: &'static str = "let variant = match expression_id {";
    const TAIL: &'static str = "_ => unreachable!(),\n};";
}

impl<'a> ToRust for ActionDispatcher<'a> {
    type Error = ();

    fn to_rust_code(&self) -> Result<String, Self::Error> {
        let action_variant_repr = self
            .actions
            .iter()
            .map(ToRust::to_rust_code)
            .collect::<Result<_, ()>>();

        let action_variants = action_variant_repr?;

        Ok([
            Self::HEADER.to_string(),
            action_variants,
            Self::TAIL.to_string(),
        ]
        .join("\n\n"))
    }
}

struct IrTokenVariant<'a> {
    /// Represents id of the variant. This aligns with the expression id of
    /// the regex matcher program.
    id: &'a str,
    capture_ty: Option<&'a CaptureType>,
}

impl<'a> ToRust for IrTokenVariant<'a> {
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

pub fn codegen(rule_set: &ast::RuleSet) -> Result<String, String> {
    let header = rule_set.header.as_ref().map(|h| h.as_ref()).unwrap_or("");
    let rules = rule_set.rules.as_ref();

    let id_captures = rules.iter().map(|rule| {
        let captures = rule.capture.as_ref().map(|cap| &cap.0.ty);

        (rule.identifier.as_ref(), captures)
    });

    let patterns = rules
        .iter()
        .map(|rule| regex_compiler::parse(rule.pattern.0.to_string()).map_err(|e| e.to_string()))
        .collect::<Result<Vec<_>, _>>()?;

    let compiled_bin = compile_many(patterns).map(|insts| insts.to_bytecode())?;
    let pattern_bin = PatternBin::new(&compiled_bin)
        .to_rust_code()
        .map_err(|_| "unabled to serialize pattern binary")?;

    let variants = id_captures
        .map(|(id, captures)| IrTokenVariant {
            id,
            capture_ty: captures,
        })
        .collect::<Vec<IrTokenVariant<'_>>>();

    let variants_str_repr = IrToken(variants)
        .to_rust_code()
        .map_err(|_| "unable to generate token enum".to_string())?;

    let actions = rules
        .iter()
        .enumerate()
        .map(|(expr_idx, rule)| {
            if rule.capture.is_some() {
                ActionVariantDispatcher::new(
                    &rule.identifier,
                    expr_idx,
                    ActionKind::Capturing(&rule.action),
                )
            } else {
                ActionVariantDispatcher::new(
                    &rule.identifier,
                    expr_idx,
                    ActionKind::NonCapturing(&rule.action),
                )
            }
        })
        .collect::<Vec<_>>();
    let dispatcher = ActionDispatcher::new(actions.as_slice())
        .to_rust_code()
        .map_err(|_| "unable to generate variant dispatcher".to_string())?;

    let token_stream_iterator = format!(
        "{}\n{}\n{}",
        TOKEN_STREAM_HEAD, dispatcher, TOKEN_STREAM_TAIL
    );

    Ok(vec![
        header.to_string(),
        LEXER_TYPE_DEFS.to_string(),
        pattern_bin,
        variants_str_repr,
        token_stream_iterator,
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
        let action = Action(ActionItem {
            block: "Some(TokenVariant::Test)".chars().map(Char::from).collect(),
        });
        let rules = Rules(vec![Rule::new(
            Identifier("Test".to_string()),
            None,
            pattern,
            action,
        )]);

        let rule_set = RuleSet::new(None, rules);

        let expected = ["
const PROG_BINARY: [u8; 144] = [240, 240, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 97, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];",
"#[derive(Debug, PartialEq, Eq)]
pub enum TokenVariant {
Test,
}"];

        let generated_res = codegen(&rule_set);
        for expected_substr in expected {
            let contains = (&generated_res)
                .as_ref()
                .map(|str| str.contains(expected_substr))
                .unwrap_or(false);
            assert!(contains);
        }
    }

    #[test]
    fn should_codgen_valid_input_with_header() {
        let pattern = Pattern(PatternItem("ab".chars().map(Char::from).collect()));
        let action = Action(ActionItem {
            block: "Some(TokenVariant::Test)".chars().map(Char::from).collect(),
        });
        let rules = Rules(vec![Rule::new(
            Identifier("Test".to_string()),
            None,
            pattern,
            action,
        )]);

        let header = "const UNIT: () = ();";

        let rule_set = RuleSet::new(None, rules).with_header(Header::new(header));

        let expected_substrs = ["const UNIT: () = ();",
"const PROG_BINARY: [u8; 144] = [240, 240, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 97, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];",
"#[derive(Debug, PartialEq, Eq)]
pub enum TokenVariant {
Test,
}"];

        let generated_res = codegen(&rule_set);
        for expected_substr in expected_substrs {
            let contains = (&generated_res)
                .as_ref()
                .map(|str| str.contains(expected_substr))
                .unwrap_or(false);
            assert!(contains);
        }
    }

    #[test]
    fn should_generate_tokens_without_error() {
        let int8_ct = CaptureType::try_new("i8").unwrap();

        let inputs = vec![
            (
                IrToken(vec![IrTokenVariant {
                    id: "Test",
                    capture_ty: None,
                }]),
                "#[derive(Debug, PartialEq, Eq)]\npub enum TokenVariant {\nTest,\n}".to_string(),
            ),
            (
                IrToken(vec![IrTokenVariant {
                    id: "Test",
                    capture_ty: Some(&int8_ct),
                }]),
                "#[derive(Debug, PartialEq, Eq)]\npub enum TokenVariant {\nTest(i8),\n}"
                    .to_string(),
            ),
        ];

        for (test_id, (input, expected)) in inputs.into_iter().enumerate() {
            assert_eq!((test_id, Ok(expected)), (test_id, input.to_rust_code()));
        }
    }
}
