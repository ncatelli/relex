use parcel::parsers::character::{alphabetic, any_character, digit, expect_character};
use parcel::prelude::v1::*;

use crate::ast;

pub enum ParseErr {
    Undefined(&'static str),
}

impl std::fmt::Debug for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undefined(err) => write!(f, "undefined parse error: {}", err),
        }
    }
}

pub fn parse(input: &[(usize, char)]) -> Result<(), ParseErr> {
    todo!()
}

// Character Classes

fn character_group<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterGroup> {
    parcel::join(
        parcel::right(parcel::join(
            expect_character('['),
            parcel::optional(expect_character('^')).map(|negation| negation.is_some()),
        )),
        parcel::left(parcel::join(
            parcel::one_or_more(character_group_item()),
            expect_character(']'),
        )),
    )
    .map(|(negation, character_group_items)| match negation {
        true => ast::CharacterGroup::NegatedItems(character_group_items),
        false => ast::CharacterGroup::Items(character_group_items),
    })
}

fn character_group_item<'a>(
) -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterGroupItem> {
    parcel::or(character_class().map(Into::into), || {
        parcel::or(
            character_class_from_unicode_category().map(Into::into),
            || parcel::or(character_range().map(Into::into), || char().map(Into::into)),
        )
    })
}

fn character_class<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterClass> {
    parcel::or(character_class_any_word().map(Into::into), || {
        parcel::or(character_class_any_word_inverted().map(Into::into), || {
            parcel::or(character_class_any_decimal_digit().map(Into::into), || {
                character_class_any_decimal_digit_inverted().map(Into::into)
            })
        })
    })
}

fn character_class_any_word<'a>(
) -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterClassAnyWord> {
    parcel::join(expect_character('\\'), expect_character('w')).map(|_| ast::CharacterClassAnyWord)
}

fn character_class_any_word_inverted<'a>(
) -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterClassAnyWordInverted> {
    parcel::join(expect_character('\\'), expect_character('W'))
        .map(|_| ast::CharacterClassAnyWordInverted)
}

fn character_class_any_decimal_digit<'a>(
) -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterClassAnyDecimalDigit> {
    parcel::join(expect_character('\\'), expect_character('d'))
        .map(|_| ast::CharacterClassAnyDecimalDigit)
}

fn character_class_any_decimal_digit_inverted<'a>(
) -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterClassAnyDecimalDigitInverted> {
    parcel::join(expect_character('\\'), expect_character('D'))
        .map(|_| ast::CharacterClassAnyDecimalDigitInverted)
}

fn character_class_from_unicode_category<'a>(
) -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterClassFromUnicodeCategory> {
    parcel::right(parcel::join(
        parcel::join(expect_character('\\'), expect_character('p')),
        parcel::right(parcel::join(
            expect_character('{'),
            parcel::left(parcel::join(unicode_category_name(), expect_character('}'))),
        )),
    ))
    .map(ast::CharacterClassFromUnicodeCategory)
}

fn unicode_category_name<'a>(
) -> impl parcel::Parser<'a, &'a [(usize, char)], ast::UnicodeCategoryName> {
    letters().map(ast::UnicodeCategoryName)
}

fn character_range<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::CharacterRange> {
    parcel::join(
        char(),
        parcel::optional(parcel::right(parcel::join(expect_character('-'), char()))),
    )
    .map(|(lower_bound, upper_bound)| ast::CharacterRange::new(lower_bound, upper_bound))
}

// Quantifiers

/// Represents all variants of regex quantifiers with an optionally lazy modifier.
fn quantifier<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Quantifier> {
    parcel::join(quantifier_type(), parcel::optional(lazy_modifier())).map(
        |(quantifier_ty, lazy_modifier)| match lazy_modifier {
            Some(_) => ast::Quantifier::Lazy(quantifier_ty),
            None => ast::Quantifier::Eager(quantifier_ty),
        },
    )
}

fn lazy_modifier<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::LazyModifier> {
    expect_character('?').map(|_| ast::LazyModifier)
}

fn quantifier_type<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::QuantifierType> {
    parcel::or(zero_or_more_quantifier().map(Into::into), || {
        parcel::or(one_or_more_quantifier().map(Into::into), || {
            parcel::or(zero_or_one_quantifier().map(Into::into), || {
                range_quantifier().map(Into::into)
            })
        })
    })
}

fn range_quantifier<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::RangeQuantifier> {
    parcel::left(parcel::join(
        parcel::right(parcel::join(
            expect_character('{'),
            parcel::join(
                range_quantifier_lower_bound(),
                parcel::optional(parcel::right(parcel::join(
                    expect_character(','),
                    parcel::optional(range_quantifier_upper_bound()),
                ))),
            ),
        )),
        expect_character('}'),
    ))
    .map(|(lower_bound, upper_bound)| ast::RangeQuantifier::new(lower_bound, upper_bound))
}

fn range_quantifier_lower_bound<'a>(
) -> impl Parser<'a, &'a [(usize, char)], ast::RangeQuantifierLowerBound> {
    integer().map(ast::RangeQuantifierLowerBound)
}

fn range_quantifier_upper_bound<'a>(
) -> impl Parser<'a, &'a [(usize, char)], ast::RangeQuantifierUpperBound> {
    integer().map(ast::RangeQuantifierUpperBound)
}

fn zero_or_more_quantifier<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::QuantifierType> {
    expect_character('*').map(|_| ast::QuantifierType::ZeroOrMore)
}

fn one_or_more_quantifier<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::QuantifierType> {
    expect_character('+').map(|_| ast::QuantifierType::OneOrMore)
}

fn zero_or_one_quantifier<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::QuantifierType> {
    expect_character('?').map(|_| ast::QuantifierType::ZeroOrOne)
}

// Backreferences

fn backreference<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::Backreference> {
    parcel::right(parcel::join(expect_character('\\'), integer())).map(ast::Backreference)
}

// Anchors

fn start_of_string_anchor<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::StartOfStringAnchor> {
    expect_character('^').map(|_| ast::StartOfStringAnchor)
}

fn anchor<'a>() -> impl parcel::Parser<'a, &'a [(usize, char)], ast::Anchor> {
    parcel::or(anchor_word_boundary().map(Into::into), || {
        parcel::or(anchor_nonword_boundary().map(Into::into), || {
            parcel::or(anchor_start_of_string_only().map(Into::into), || {
                parcel::or(
                    anchor_end_of_string_only_not_newline().map(Into::into),
                    || {
                        parcel::or(anchor_end_of_string_only().map(Into::into), || {
                            parcel::or(anchor_previous_match_end().map(Into::into), || {
                                anchor_end_of_string().map(Into::into)
                            })
                        })
                    },
                )
            })
        })
    })
}

fn anchor_word_boundary<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::AnchorWordBoundary> {
    parcel::join(expect_character('\\'), expect_character('b')).map(|_| ast::AnchorWordBoundary)
}

fn anchor_nonword_boundary<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::AnchorNonWordBoundary>
{
    parcel::join(expect_character('\\'), expect_character('B')).map(|_| ast::AnchorNonWordBoundary)
}

fn anchor_start_of_string_only<'a>(
) -> impl Parser<'a, &'a [(usize, char)], ast::AnchorStartOfStringOnly> {
    parcel::join(expect_character('\\'), expect_character('A'))
        .map(|_| ast::AnchorStartOfStringOnly)
}

fn anchor_end_of_string_only_not_newline<'a>(
) -> impl Parser<'a, &'a [(usize, char)], ast::AnchorEndOfStringOnlyNotNewline> {
    parcel::join(expect_character('\\'), expect_character('z'))
        .map(|_| ast::AnchorEndOfStringOnlyNotNewline)
}

fn anchor_end_of_string_only<'a>(
) -> impl Parser<'a, &'a [(usize, char)], ast::AnchorEndOfStringOnly> {
    parcel::join(expect_character('\\'), expect_character('Z')).map(|_| ast::AnchorEndOfStringOnly)
}

fn anchor_previous_match_end<'a>(
) -> impl Parser<'a, &'a [(usize, char)], ast::AnchorPreviousMatchEnd> {
    parcel::join(expect_character('\\'), expect_character('G')).map(|_| ast::AnchorPreviousMatchEnd)
}

fn anchor_end_of_string<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::AnchorEndOfString> {
    expect_character('$').map(|_| ast::AnchorEndOfString)
}

// Terminals

fn integer<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::Integer> {
    move |input: &'a [(usize, char)]| {
        let preparsed_input = input;
        let res = parcel::join(
            expect_character('-').optional(),
            parcel::one_or_more(digit(10)),
        )
        .map(|(negative, digits)| {
            let vd: String = match negative {
                Some(_) => "-",
                None => "",
            }
            .chars()
            .chain(digits.into_iter())
            .collect();

            vd.parse::<isize>()
        })
        .parse(input);

        match res {
            Ok(MatchStatus::Match {
                span,
                remainder,
                inner: Ok(int),
            }) => Ok(MatchStatus::Match {
                span,
                remainder,
                inner: ast::Integer(int),
            }),

            Ok(MatchStatus::Match {
                span: _,
                remainder: _,
                inner: Err(_),
            }) => Ok(MatchStatus::NoMatch(preparsed_input)),

            Ok(MatchStatus::NoMatch(remainder)) => Ok(MatchStatus::NoMatch(remainder)),
            Err(e) => Err(e),
        }
    }
}

fn letters<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::Letters> {
    parcel::one_or_more(alphabetic().predicate(|c| c.is_ascii_alphabetic())).map(ast::Letters)
}

fn char<'a>() -> impl Parser<'a, &'a [(usize, char)], ast::Char> {
    any_character().map(ast::Char)
}
