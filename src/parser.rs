use parcel::{parsers::character::expect_character, prelude::v1::*};

use super::ast;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrKind {
    InvalidRule,
    Other,
}

impl std::fmt::Display for ParseErrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Other => write!(f, "undefined parse error"),
            Self::InvalidRule => write!(f, "provided rule is invalid",),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseErr {
    kind: ParseErrKind,
    data: Option<String>,
}

impl ParseErr {
    pub fn new(kind: ParseErrKind) -> Self {
        Self { kind, data: None }
    }

    pub fn with_data_mut(&mut self, data: String) {
        self.data = Some(data)
    }

    pub fn with_data(mut self, data: String) -> Self {
        self.with_data_mut(data);
        self
    }
}

impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            Some(ctx) => write!(f, "{}: {}", &self.kind, ctx),
            None => write!(f, "{}", &self.kind),
        }
    }
}

/// Attempts to parse a set of relex rule DSL into it's corresponding AST.
///
/// # Example
///
/// ```
/// use parcel::prelude::v1::*;
/// use relex::parser;
/// use relex::ast;
///
/// let input = "RULE Zero [0] => %%{ Some(0) }%%\n
/// RULE Zero(digit: u8) [(0)] => %%{ if digit==\"0\" { Some(0) } }%%\n
/// RULE Zero(other: String) [(0)(.*)] => %%{ if other.chars().len()==0 { Some(0) } }%%"
///     .chars()
///     .enumerate()
///     .collect::<Vec<(usize, char)>>();
///
/// let parse_result = parser::parse(&input);
/// assert!(match parse_result {
///     Ok(ast::RuleSet{header: None, rules}) => rules.as_ref().len() == 3,
///     _ => false,
/// })
/// ```
pub fn parse(input: &[(usize, char)]) -> Result<ast::RuleSet, ParseErr> {
    match ruleset().parse(input) {
        Ok(MatchStatus::Match { inner, .. }) => Ok(inner),
        Ok(MatchStatus::NoMatch { .. }) => Err(ParseErr::new(ParseErrKind::InvalidRule)),
        Err(e) => Err(ParseErr::new(ParseErrKind::Other).with_data(e)),
    }
}

fn ruleset<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::RuleSet> {
    parcel::join(parcel::optional(header()), rules())
        .map(|(header, rules)| ast::RuleSet::new(header, rules))
}

fn header<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Header> {
    whitespace_wrapped(str_wrapped("{{{", "}}}", header_item())).map(ast::Header::new)
}

pub fn header_item<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], String> {
    move |input: &'a [(usize, char)]| {
        let start = 0;

        for end in 1..=input.len() {
            let sub = input.get(start..end);

            match sub {
                Some([.., (_, '}'), (_, '}'), (_, '}')]) => {
                    return Ok(MatchStatus::Match {
                        span: start..end - 3,
                        remainder: &input[end - 3..],
                        inner: (input[start..end - 3])
                            .iter()
                            .map(|(_, c)| c)
                            .copied()
                            .collect(),
                    })
                }
                // provide an early return in case it falls through to another block
                Some([.., (_, '%'), (_, '%'), (_, '{')]) => return Ok(MatchStatus::NoMatch(input)),
                _ => continue,
            }
        }

        Ok(MatchStatus::NoMatch(input))
    }
}

fn rules<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Rules> {
    whitespace_wrapped(parcel::one_or_more(rule())).map(ast::Rules)
}

fn rule<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Rule> {
    use parcel::parsers::character::expect_str;

    parcel::right(parcel::join(
        non_newline_whitespace_wrapped(expect_str("RULE")),
        parcel::join(
            identifier(),
            parcel::join(
                parcel::optional(capture()),
                parcel::join(
                    pattern(),
                    parcel::right(parcel::join(whitespace_wrapped(expect_str("=>")), action())),
                ),
            ),
        ),
    ))
    .map(|(identifier, (capture, (pattern, action)))| {
        ast::Rule::new(identifier, capture, pattern, action)
    })
}

fn identifier<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Identifier> {
    use parcel::parsers::character;

    parcel::join(
        character::alphabetic().predicate(|c| c.is_ascii_uppercase()),
        parcel::zero_or_more(parcel::or(character::alphabetic(), || {
            character::expect_character('_')
        })),
    )
    .map(|(head, tail)| [head].into_iter().chain(tail.into_iter()).collect())
    .map(ast::Identifier)
}

fn capture<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Capture> {
    str_wrapped("(", ")", capture_item()).map(ast::Capture)
}

fn capture_item<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CaptureItem> {
    parcel::join(
        capture_identifier(),
        parcel::right(parcel::join(
            expect_character(':'),
            whitespace_wrapped(capture_type()),
        )),
    )
    .map(|(ident, ty)| ast::CaptureItem::new(ident, ty))
}

fn capture_type<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CaptureType> {
    use parcel::parsers::character;

    parcel::zero_or_more(parcel::or(character::alphabetic(), || {
        parcel::or(character::digit(10), || character::expect_character('_'))
    }))
    .map(|chars| chars.into_iter().collect())
    .map(ast::CaptureType)
}

fn capture_identifier<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CaptureIdentifier>
{
    use parcel::parsers::character;

    parcel::zero_or_more(parcel::or(character::alphabetic(), || {
        character::expect_character('_')
    }))
    .map(|chars| chars.into_iter().collect())
    .map(ast::CaptureIdentifier)
}

fn pattern<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Pattern> {
    str_wrapped("[", "]", pattern_item()).map(ast::Pattern)
}

pub fn pattern_item<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::PatternItem> {
    move |input: &'a [(usize, char)]| {
        let start = 0;

        for end in 1..=input.len() {
            let sub = input.get(start..end);

            match sub {
                Some([.., (_, ']'), (_, ' ')])
                | Some([.., (_, ']'), (_, '\t')])
                | Some([.., (_, ']'), (_, '\n')])
                | Some([.., (_, ']'), (_, '=')]) => {
                    return Ok(MatchStatus::Match {
                        span: start..end - 2,
                        remainder: &input[end - 2..],
                        inner: ast::PatternItem(
                            (input[start..end - 2])
                                .iter()
                                .map(|(_, c)| c)
                                .copied()
                                .map(ast::Char)
                                .collect(),
                        ),
                    })
                }
                _ => continue,
            }
        }

        Ok(MatchStatus::NoMatch(input))
    }
}

fn action<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Action> {
    str_wrapped("%%{", "}%%", action_item()).map(ast::Action)
}

pub fn action_item<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::ActionItem> {
    move |input: &'a [(usize, char)]| {
        let start = 0;

        for end in 1..=input.len() {
            let sub = input.get(start..end);

            match sub {
                Some([.., (_, '}'), (_, '%'), (_, '%')]) => {
                    return Ok(MatchStatus::Match {
                        span: start..end - 3,
                        remainder: &input[end - 3..],
                        inner: ast::ActionItem {
                            block: (input[start..end - 3])
                                .iter()
                                .map(|(_, c)| c)
                                .copied()
                                .map(ast::Char)
                                .collect(),
                        },
                    })
                }
                // provide an early return in case it falls through to another block
                Some([.., (_, '%'), (_, '%'), (_, '{')]) => return Ok(MatchStatus::NoMatch(input)),
                _ => continue,
            }
        }

        Ok(MatchStatus::NoMatch(input))
    }
}

#[allow(unused)]
fn char<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Char> {
    use parcel::parsers::character;

    character::any_character().map(ast::Char)
}

fn whitespace_wrapped<'a, P, B>(parser: P) -> impl Parser<'a, &'a [(usize, char)], B>
where
    B: 'a,
    P: Parser<'a, &'a [(usize, char)], B> + 'a,
{
    use parcel::parsers::character::whitespace;

    parcel::right(parcel::join(
        parcel::zero_or_more(whitespace()),
        parcel::left(parcel::join(parser, parcel::zero_or_more(whitespace()))),
    ))
}

fn non_newline_whitespace_wrapped<'a, P, B>(parser: P) -> impl Parser<'a, &'a [(usize, char)], B>
where
    B: 'a,
    P: Parser<'a, &'a [(usize, char)], B> + 'a,
{
    use parcel::parsers::character::non_newline_whitespace;

    parcel::right(parcel::join(
        parcel::zero_or_more(non_newline_whitespace()),
        parcel::left(parcel::join(
            parser,
            parcel::zero_or_more(non_newline_whitespace()),
        )),
    ))
}

fn str_wrapped<'a, P, B>(
    prefix: &'static str,
    suffix: &'static str,
    parser: P,
) -> impl Parser<'a, &'a [(usize, char)], B>
where
    B: 'a,
    P: Parser<'a, &'a [(usize, char)], B> + 'a,
{
    use parcel::parsers::character::expect_str;

    parcel::right(parcel::join(
        whitespace_wrapped(expect_str(prefix)),
        parcel::left(parcel::join(parser, whitespace_wrapped(expect_str(suffix)))),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_identifier() {
        let inputs = vec!["Test", "TestIdentifier", "Test_Identifier"]
            .into_iter()
            .map(|input| input.chars().enumerate().collect::<Vec<(usize, char)>>());

        for input in inputs {
            let parse_result = identifier().parse(&input);
            assert!(matches!(
                parse_result,
                Ok(parcel::MatchStatus::Match { .. }),
            ))
        }
    }

    #[test]
    fn should_parse_capture() {
        let inputs = vec!["(first: u8)", "(first: String)"]
            .into_iter()
            .map(|input| input.chars().enumerate().collect::<Vec<(usize, char)>>());

        for input in inputs {
            let parse_result = capture().parse(&input);
            assert!(matches!(
                parse_result,
                Ok(parcel::MatchStatus::Match { .. }),
            ))
        }
    }

    #[test]
    fn should_parse_pattern() {
        let inputs = vec!["[0] "]
            .into_iter()
            .map(|input| input.chars().enumerate().collect::<Vec<(usize, char)>>());

        for input in inputs {
            let parse_result = pattern().parse(&input);
            assert!(matches!(
                parse_result,
                Ok(parcel::MatchStatus::Match { .. }),
            ))
        }
    }

    #[test]
    fn should_parse_action() {
        let inputs = vec!["%%{ Some(0) }%%", "%%{ if digit==\"0\" { Some(0) } }%%"]
            .into_iter()
            .map(|input| input.chars().enumerate().collect::<Vec<(usize, char)>>());

        for input in inputs {
            let parse_result = action().parse(&input);
            assert!(matches!(
                parse_result,
                Ok(parcel::MatchStatus::Match { .. }),
            ))
        }
    }

    #[test]
    fn should_parse_rule() {
        let inputs = vec![
            "RULE Zero [0] => %%{ Some(0) }%%",
            "RULE Zero(digit: u8) [(0)] => %%{ if digit==\"0\" { Some(0) } }%%",
            "RULE Zero(other: String) [(0)(.*)] => %%{ if other.chars().len()==0 { Some(0) } }%%",
        ]
        .into_iter()
        .map(|input| input.chars().enumerate().collect::<Vec<(usize, char)>>());

        for input in inputs {
            let parse_result = rule().parse(&input);
            assert!(matches!(
                parse_result,
                Ok(parcel::MatchStatus::Match { .. }),
            ))
        }
    }

    #[test]
    fn should_parse_multiple_rules() {
        let inputs = vec![
            "RULE Zero [0] => %%{ Some(0) }%%\n
RULE Zero(digit: u8) [(0)] => %%{ if digit==\"0\" { Some(0) } }%%\n
RULE Zero(other: String) [(0)(.*)] => %%{ if other.chars().len == 0 { Some(0) } }%%",
        ]
        .into_iter()
        .map(|input| input.chars().enumerate().collect::<Vec<(usize, char)>>());

        for input in inputs {
            let parse_result = rules().parse(&input);
            assert!(match parse_result {
                Ok(parcel::MatchStatus::Match {
                    inner: ast::Rules(rules),
                    ..
                }) => rules.len() == 3,
                _ => false,
            })
        }
    }

    #[test]
    fn should_parse_header_with_rule() {
        let inputs = vec![
            // header
            "{{{();}}}
RULE Zero [0] => %%{ Some(0) }%%\n
RULE Zero(digit: u8) [(0)] => %%{ if digit==\"0\" { Some(0) } }%%\n
RULE Zero(other: String) [(0)(.*)] => %%{ if other.chars().len == 0 { Some(0) } }%%",
            "
            
{{{();}}}
RULE Zero [0] => %%{ Some(0) }%%\n
RULE Zero(digit: u8) [(0)] => %%{ if digit==\"0\" { Some(0) } }%%\n
RULE Zero(other: String) [(0)(.*)] => %%{ if other.chars().len == 0 { Some(0) } }%%",
            // no header
            "RULE Zero [0] => %%{ Some(0) }%%\n
RULE Zero(digit: u8) [(0)] => %%{ if digit==\"0\" { Some(0) } }%%\n
RULE Zero(other: String) [(0)(.*)] => %%{ if other.chars().len == 0 { Some(0) } }%%",
            // no header with leading whitespace
            "
            
RULE Zero [0] => %%{ Some(0) }%%\n
RULE Zero(digit: u8) [(0)] => %%{ if digit==\"0\" { Some(0) } }%%\n
RULE Zero(other: String) [(0)(.*)] => %%{ if other.chars().len == 0 { Some(0) } }%%",
        ]
        .into_iter()
        .map(|input| input.chars().enumerate().collect::<Vec<(usize, char)>>());

        for (test_id, input) in inputs.into_iter().enumerate() {
            let parse_result = parse(&input).map(|ruleset| ruleset.rules.0.len());

            assert_eq!((test_id, Ok(3)), (test_id, parse_result));
        }
    }
}
