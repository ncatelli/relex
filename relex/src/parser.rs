use parcel::{parsers::character::expect_character, prelude::v1::*};

use super::ast;

#[derive(PartialEq)]
pub enum ParseErr {
    InvalidRule,
    Undefined(String),
}

impl std::fmt::Debug for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undefined(err) => write!(f, "undefined parse error: {}", err),
            Self::InvalidRule => write!(f, "provided rule is invalid",),
        }
    }
}

pub fn parse(input: &[(usize, char)]) -> Result<ast::Rules, ParseErr> {
    match rules().parse(input) {
        Ok(MatchStatus::Match { inner, .. }) => Ok(inner),
        Ok(MatchStatus::NoMatch { .. }) => Err(ParseErr::InvalidRule),
        Err(e) => Err(ParseErr::Undefined(format!("{:?}", e))),
    }
}

fn rules<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Rules> {
    parcel::one_or_more(rule()).map(ast::Rules)
}

fn rule<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Rule> {
    use parcel::parsers::character::expect_str;

    str_wrapped(
        "RULE",
        "ENDRULE",
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
    )
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
    str_wrapped(
        "(",
        ")",
        parcel::join(
            parcel::zero_or_more(parcel::left(parcel::join(
                capture_item(),
                whitespace_wrapped(expect_character(',')),
            ))),
            capture_item(),
        )
        .map(|(mut head, tail)| {
            head.push(tail);
            head
        })
        .or(|| parcel::zero_or_more(capture_item())),
    )
    .map(ast::Capture)
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

    parcel::or(
        character::expect_str("String").map(|_| ast::CaptureType::String),
        || {
            parcel::or(int_type().map(ast::CaptureType::Int), || {
                character::expect_str("bool").map(|_| ast::CaptureType::Bool)
            })
        },
    )
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

fn int_type<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::IntType> {
    parcel::join(int_type_sign(), int_type_bit_width())
        .map(|(sign, width)| ast::IntType::new(sign, width))
}

fn int_type_sign<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::IntTypeSign> {
    parcel::or(
        expect_character('i').map(|_| ast::IntTypeSign::Signed),
        || expect_character('u').map(|_| ast::IntTypeSign::Unsigned),
    )
}

fn int_type_bit_width<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::IntTypeBitWidth> {
    use parcel::parsers::character::expect_str;

    parcel::or(expect_str("8").map(|_| ast::IntTypeBitWidth::Eight), || {
        parcel::or(
            expect_str("16").map(|_| ast::IntTypeBitWidth::Sixteen),
            || {
                parcel::or(
                    expect_str("32").map(|_| ast::IntTypeBitWidth::ThirtyTwo),
                    || expect_str("64").map(|_| ast::IntTypeBitWidth::SixtyFour),
                )
            },
        )
    })
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
                            (&input[start..end - 2])
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
                        inner: ast::ActionItem(
                            (&input[start..end - 3])
                                .iter()
                                .map(|(_, c)| c)
                                .copied()
                                .map(ast::Char)
                                .collect(),
                        ),
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
        let inputs = vec!["(first: u8)", "(first: String, second: u8)"]
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
            "RULE Zero [0] => %%{ if digit==\"0\" { Some(0) } }%% ENDRULE",
            "RULE Zero(digit: u8) [0] => %%{ if digit==\"0\" { Some(0) } }%% ENDRULE",
            "RULE Zero(digit: u8, other: String) [0] => %%{ if digit==\"0\" { Some(0) } }%% ENDRULE"

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
}
