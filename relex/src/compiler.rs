use super::ast;
use relex_runtime::*;

/// A internal representation of the `relex_runtime::Opcode` type, with relative
/// addressing.
///
/// ## Note
/// This type is meant to exist only internally and should be
/// refined to the `relex_runtime::Opcode type
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
enum RelativeOpcode {
    Any,
    Consume(char),
    ConsumeSet(InstConsumeSet),
    Split(isize, isize),
    Jmp(isize),
    StartSave(usize),
    EndSave(usize),
    Match,
}

impl RelativeOpcode {
    fn into_opcode_with_index(self, idx: usize) -> Option<Opcode> {
        match self {
            RelativeOpcode::Any => Some(Opcode::Any),
            RelativeOpcode::Consume(c) => Some(Opcode::Consume(InstConsume::new(c))),
            RelativeOpcode::ConsumeSet(i) => Some(Opcode::ConsumeSet(i)),
            RelativeOpcode::Split(rel_x, rel_y) => {
                let signed_idx = idx as isize;
                let x: usize = (signed_idx + rel_x).try_into().ok()?;
                let y: usize = (signed_idx + rel_y).try_into().ok()?;

                Some(Opcode::Split(InstSplit::new(
                    InstIndex::from(x),
                    InstIndex::from(y),
                )))
            }
            RelativeOpcode::Jmp(rel_jmp_to) => {
                let signed_idx = idx as isize;
                let jmp_to: usize = (signed_idx + rel_jmp_to).try_into().ok()?;

                Some(Opcode::Jmp(InstJmp::new(InstIndex::from(jmp_to))))
            }
            RelativeOpcode::StartSave(slot) => Some(Opcode::StartSave(InstStartSave::new(slot))),
            RelativeOpcode::EndSave(slot) => Some(Opcode::EndSave(InstEndSave::new(slot))),
            RelativeOpcode::Match => Some(Opcode::Match),
        }
    }

    fn into_opcode_with_index_unchecked(self, idx: usize) -> Opcode {
        self.into_opcode_with_index(idx).unwrap()
    }
}

type Opcodes = Vec<Opcode>;
type RelativeOpcodes = Vec<RelativeOpcode>;

pub fn compile(regex_ast: ast::Regex) -> Result<Instructions, String> {
    let suffix = [RelativeOpcode::Match];

    let relative_ops: Result<RelativeOpcodes, _> = match regex_ast {
        ast::Regex::StartOfStringAnchored(expr) => {
            expression(expr).map(|expr| expr.into_iter().chain(suffix.into_iter()).collect())
        }
        ast::Regex::Unanchored(expr) => expression(expr).map(|expr| {
            // match anything
            let prefix = [
                RelativeOpcode::Split(3, 1),
                RelativeOpcode::Any,
                RelativeOpcode::Jmp(-2),
            ];

            prefix
                .into_iter()
                .chain(expr.into_iter())
                .chain(suffix.into_iter())
                .collect()
        }),
    };

    relative_ops
        .map(|rel_ops| {
            rel_ops
                .into_iter()
                .enumerate()
                .map(|(idx, op)| op.into_opcode_with_index_unchecked(idx))
                .collect::<Opcodes>()
        })
        .map(Instructions::new)
}

fn expression(expr: ast::Expression) -> Result<RelativeOpcodes, String> {
    let ast::Expression(subexprs) = expr;

    subexprs
        .into_iter()
        .map(subexpression)
        .collect::<Result<Vec<_>, _>>()
        .map(|opcodes| opcodes.into_iter().flatten().collect())
}

fn subexpression(subexpr: ast::SubExpression) -> Result<RelativeOpcodes, String> {
    let ast::SubExpression(items) = subexpr;

    items
        .into_iter()
        .map(|subexpr_item| match subexpr_item {
            ast::SubExpressionItem::Match(m) => match_item(m),
            ast::SubExpressionItem::Group(_) => todo!(),
            ast::SubExpressionItem::Anchor(_) => todo!(),
            ast::SubExpressionItem::Backreference(_) => unimplemented!(),
        })
        .collect::<Result<Vec<_>, _>>()
        .map(|opcodes| opcodes.into_iter().flatten().collect())
}

macro_rules! generate_range_quantifier_block {
    (eager, $min:expr, $consumer:expr) => {
        Ok(vec![$consumer; $min as usize]
            .into_iter()
            .chain(
                vec![
                    RelativeOpcode::Split(1, 3),
                    $consumer,
                    RelativeOpcode::Jmp(-2),
                ]
                .into_iter(),
            )
            .collect())
    };

    (lazy, $min:expr, $consumer:expr) => {
        Ok(vec![$consumer; $min as usize]
            .into_iter()
            .chain(
                vec![
                    RelativeOpcode::Split(3, 1),
                    $consumer,
                    RelativeOpcode::Jmp(-2),
                ]
                .into_iter(),
            )
            .collect())
    };

    (eager, $min:expr, $max:expr, $consumer:expr) => {
        Ok((0..($max - $min))
            .flat_map(|_| vec![$consumer, RelativeOpcode::Split(1, 2)])
            .chain(vec![$consumer; $min as usize].into_iter())
            .collect())
    };

    (lazy, $min:expr, $max:expr, $consumer:expr) => {
        Ok((0..($max - $min))
            .flat_map(|_| vec![$consumer, RelativeOpcode::Split(2, 1)])
            .chain(vec![$consumer; $min as usize].into_iter())
            .collect())
    };
}

fn match_item(m: ast::Match) -> Result<RelativeOpcodes, String> {
    use ast::{
        Char, Integer, Match, MatchCharacter, MatchCharacterClass, MatchItem, Quantifier,
        QuantifierType,
    };

    match m {
        // match exact
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(cnt))),
        } => Ok(vec![RelativeOpcode::Any; cnt as usize]),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(cnt))),
        } => Ok(vec![RelativeOpcode::Consume(c); cnt as usize]),

        // match at least
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => generate_range_quantifier_block!(eager, cnt, RelativeOpcode::Any),
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => generate_range_quantifier_block!(eager, cnt, RelativeOpcode::Consume(c)),

        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Lazy(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => generate_range_quantifier_block!(lazy, cnt, RelativeOpcode::Any),

        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Lazy(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => generate_range_quantifier_block!(lazy, cnt, RelativeOpcode::Consume(c)),

        // match between range
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier:
                Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => generate_range_quantifier_block!(eager, lower, upper, RelativeOpcode::Any),

        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier:
                Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => generate_range_quantifier_block!(eager, lower, upper, RelativeOpcode::Consume(c)),

        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier:
                Quantifier::Lazy(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => generate_range_quantifier_block!(lazy, lower, upper, RelativeOpcode::Any),

        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier:
                Quantifier::Lazy(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => generate_range_quantifier_block!(lazy, lower, upper, RelativeOpcode::Consume(c)),

        Match::WithoutQuantifier {
            item: MatchItem::MatchAnyCharacter,
        } => Ok(vec![RelativeOpcode::Any]),
        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
        } => Ok(vec![RelativeOpcode::Consume(c)]),

        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(cc)),
        } => {
            let set = match cc {
                ast::CharacterClass::AnyWord => {
                    InstConsumeSet::new(CharacterRangeSet::Ranges(vec![
                        'a'..='z',
                        'A'..='Z',
                        '0'..='9',
                        '_'..='_',
                    ]))
                }
                ast::CharacterClass::AnyWordInverted => todo!(),
                ast::CharacterClass::AnyDecimalDigit => {
                    InstConsumeSet::new(CharacterRangeSet::Ranges(vec!['0'..='9']))
                }
                ast::CharacterClass::AnyDecimalDigitInverted => todo!(),
            };

            Ok(vec![RelativeOpcode::ConsumeSet(set)])
        }

        // Catch-all todo
        Match::WithQuantifier {
            item: _,
            quantifier: _,
        } => todo!(),
        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacterClass(_),
        } => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_compile_unanchored_character_match() {
        use ast::*;
        use relex_runtime::*;

        // equivalent to `ab`
        let regex_ast = Regex::Unanchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
            }),
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('b'))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('b')),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_anchored_character_match() {
        use ast::*;
        use relex_runtime::*;

        // equivalent to `^ab`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
            }),
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('b'))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('b')),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_any_character_match() {
        use ast::*;
        use relex_runtime::*;

        // equivalent to `.`
        let regex_ast = Regex::Unanchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchAnyCharacter,
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
                Opcode::Any,
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_exact_match_quantified_item() {
        use ast::*;
        use relex_runtime::*;

        // equivalent to `^.{2}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(2))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Any,
                Opcode::Any,
                Opcode::Match,
            ])),
            compile(regex_ast)
        );

        // equivalent to `^a{2}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::MatchExactRange(Integer(2))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Match,
            ])),
            compile(regex_ast)
        )
    }

    #[test]
    fn should_compile_match_at_least_quantified_item() {
        use ast::*;
        use relex_runtime::*;

        // equivalent to `^.{2,}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(2))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Any,
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(5))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(2))),
                Opcode::Match
            ])),
            compile(regex_ast)
        );

        // equivalent to `^a{2,}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(2))),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(5))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Jmp(InstJmp::new(InstIndex::from(2))),
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_match_between_quantified_item() {
        use ast::*;
        use relex_runtime::*;

        // equivalent to `^.{2,4}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchAnyCharacter,
                quantifier: Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(2),
                    upper_bound: Integer(4),
                }),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(3))),
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(4), InstIndex::from(5))),
                Opcode::Any,
                Opcode::Any,
                Opcode::Match
            ])),
            compile(regex_ast)
        );

        // equivalent to `^a{2,4}`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithQuantifier {
                item: MatchItem::MatchCharacter(MatchCharacter(Char('a'))),
                quantifier: Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(2),
                    upper_bound: Integer(4),
                }),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(2), InstIndex::from(3))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(4), InstIndex::from(5))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }

    #[test]
    fn should_compile_character_classes() {
        use ast::*;
        use relex_runtime::*;

        // equivalent to `^\w`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(
                    CharacterClass::AnyWord,
                )),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::ConsumeSet(InstConsumeSet::new(CharacterRangeSet::Ranges(vec![
                    'a'..='z',
                    'A'..='Z',
                    '0'..='9',
                    '_'..='_',
                ]))),
                Opcode::Match,
            ])),
            compile(regex_ast)
        );

        // equivalent to `^\d`
        let regex_ast = Regex::StartOfStringAnchored(Expression(vec![SubExpression(vec![
            SubExpressionItem::Match(Match::WithoutQuantifier {
                item: MatchItem::MatchCharacterClass(MatchCharacterClass::CharacterClass(
                    CharacterClass::AnyDecimalDigit,
                )),
            }),
        ])]));

        assert_eq!(
            Ok(Instructions::new(vec![
                Opcode::ConsumeSet(InstConsumeSet::new(CharacterRangeSet::Ranges(vec![
                    '0'..='9',
                ]))),
                Opcode::Match,
            ])),
            compile(regex_ast)
        );
    }
}
