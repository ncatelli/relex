use parcel::parsers::character::{digit, expect_character};
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
