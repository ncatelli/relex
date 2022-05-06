use parcel::{one_or_more, prelude::v1::*};

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

    parcel::join(
        identifier(),
        parcel::join(
            parcel::optional(capture_type()),
            parcel::join(
                pattern(),
                parcel::right(parcel::join(expect_str("=>"), action())),
            ),
        ),
    )
    .map(|(identifier, (capture_type, (pattern, action)))| {
        ast::Rule::new(identifier, capture_type, pattern, action)
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

fn capture_type<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CaptureType> {
    use parcel::parsers::character::expect_character;

    expect_character('(')
        .and_then(|_| {
            parcel::left(parcel::join(
                parcel::join(
                    parcel::zero_or_more(parcel::left(parcel::join(
                        capture_type_item(),
                        expect_character(','),
                    ))),
                    capture_type_item(),
                )
                .map(|(mut head, tail)| {
                    head.push(tail);
                    head
                })
                .or(|| parcel::zero_or_more(capture_type_item())),
                expect_character(')'),
            ))
        })
        .map(ast::CaptureType)
}

fn capture_type_item<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CaptureTypeItem> {
    use parcel::parsers::character;

    parcel::or(
        character::expect_str("String").map(|_| ast::CaptureTypeItem::String),
        || {
            parcel::or(int_type().map(ast::CaptureTypeItem::Int), || {
                character::expect_str("bool").map(|_| ast::CaptureTypeItem::Bool)
            })
        },
    )
}

fn int_type<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::IntType> {
    parcel::join(int_type_sign(), int_type_bit_width())
        .map(|(sign, width)| ast::IntType::new(sign, width))
}

fn int_type_sign<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::IntTypeSign> {
    use parcel::parsers::character::expect_character;

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
    use parcel::parsers::character;

    parcel::right(parcel::join(
        character::expect_character('['),
        parcel::left(parcel::join(
            pattern_item(),
            character::expect_character(']'),
        )),
    ))
    .map(ast::Pattern)
}

fn pattern_item<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::PatternItem> {
    one_or_more(char().predicate(|ast::Char(c)| *c != ']')).map(ast::PatternItem)
}

fn action<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Action> {
    use parcel::parsers::character;

    parcel::right(parcel::join(
        character::expect_character('{'),
        parcel::left(parcel::join(
            action_item(),
            character::expect_character('}'),
        )),
    ))
    .map(ast::Action)
}

fn action_item<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::ActionItem> {
    one_or_more(char().predicate(|ast::Char(c)| *c != '}')).map(ast::ActionItem)
}

fn char<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Char> {
    use parcel::parsers::character;

    character::any_character().map(ast::Char)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_minimal_expression_with_no_errors() {
        let inputs = vec!["Test[aabb]=>{1}"]
            .into_iter()
            .map(|input| input.chars().enumerate().collect::<Vec<(usize, char)>>());

        for input in inputs {
            let parse_result = parse(&input);
            assert!(parse_result.is_ok())
        }
    }
}
