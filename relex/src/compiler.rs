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
    Split(isize, isize),
    Jmp(isize),
    StartSave(usize),
    EndSave(usize),
    Match,
}

impl RelativeOpcode {
    fn to_opcode_with_index(&self, idx: usize) -> Option<Opcode> {
        match self {
            RelativeOpcode::Any => Some(Opcode::Any),
            RelativeOpcode::Consume(c) => Some(Opcode::Consume(InstConsume::new(*c))),
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
            RelativeOpcode::StartSave(slot) => Some(Opcode::StartSave(InstStartSave::new(*slot))),
            RelativeOpcode::EndSave(slot) => Some(Opcode::EndSave(InstEndSave::new(*slot))),
            RelativeOpcode::Match => Some(Opcode::Match),
        }
    }

    fn to_opcode_with_index_unchecked(&self, idx: usize) -> Opcode {
        self.to_opcode_with_index(idx).unwrap()
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
                .map(|(idx, op)| op.to_opcode_with_index_unchecked(idx))
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

fn match_item(m: ast::Match) -> Result<RelativeOpcodes, String> {
    use ast::{Char, Integer, Match, MatchCharacter, MatchItem, Quantifier, QuantifierType};

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
        } => {
            let min_match = vec![RelativeOpcode::Any; cnt as usize];
            let optional = vec![
                // looping match case first (1) signifies eager consumption
                RelativeOpcode::Split(1, 3),
                RelativeOpcode::Any,
                RelativeOpcode::Jmp(-2),
            ];
            let joined_block = min_match.into_iter().chain(optional.into_iter()).collect();

            Ok(joined_block)
        }
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Eager(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => {
            let min_match = vec![RelativeOpcode::Consume(c); cnt as usize];
            let optional = vec![
                // looping match case first (1) signifies eager consumption
                RelativeOpcode::Split(1, 3),
                RelativeOpcode::Consume(c),
                RelativeOpcode::Jmp(-2),
            ];
            let joined_block = min_match.into_iter().chain(optional.into_iter()).collect();

            Ok(joined_block)
        }
        // match between range
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier:
                Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => {
            let optional_range_size = upper - lower;
            let min_match = vec![RelativeOpcode::Any; lower as usize];
            let optional = (0..optional_range_size).flat_map(|idx| {
                let split_to = 2 * (optional_range_size - idx);
                vec![
                    // Eagerly consume the next value or split to the end of the optionals.
                    RelativeOpcode::Split(1, split_to),
                    RelativeOpcode::Any,
                ]
            });

            let joined_block = min_match.into_iter().chain(optional).collect();

            Ok(joined_block)
        }
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier:
                Quantifier::Eager(QuantifierType::MatchBetweenRange {
                    lower_bound: Integer(lower),
                    upper_bound: Integer(upper),
                }),
        } => {
            let optional_range_size = upper - lower;
            let min_match = vec![RelativeOpcode::Consume(c); lower as usize];
            let optional = (0..optional_range_size).flat_map(|idx| {
                let split_to = 2 * (optional_range_size - idx);
                vec![
                    // Eagerly consume the next value or split to the end of the optionals.
                    RelativeOpcode::Split(1, split_to),
                    RelativeOpcode::Consume(c),
                ]
            });

            let joined_block = min_match.into_iter().chain(optional).collect();

            Ok(joined_block)
        }

        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Lazy(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => {
            let min_match = vec![RelativeOpcode::Any; cnt as usize];
            let optional = vec![
                // looping match case first (3) signifies lazy consumption
                RelativeOpcode::Split(3, 1),
                RelativeOpcode::Any,
                RelativeOpcode::Jmp(-2),
            ];
            let joined_block = min_match.into_iter().chain(optional.into_iter()).collect();

            Ok(joined_block)
        }
        Match::WithQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
            quantifier: Quantifier::Lazy(QuantifierType::MatchAtLeastRange(Integer(cnt))),
        } => {
            let min_match = vec![RelativeOpcode::Consume(c); cnt as usize];
            let optional = vec![
                // looping match case first (3) signifies lazy consumption
                RelativeOpcode::Split(3, 1),
                RelativeOpcode::Consume(c),
                RelativeOpcode::Jmp(-2),
            ];
            let joined_block = min_match.into_iter().chain(optional.into_iter()).collect();

            Ok(joined_block)
        }

        // Catch-all todo
        Match::WithQuantifier {
            item: _,
            quantifier: _,
        } => todo!(),
        Match::WithoutQuantifier {
            item: MatchItem::MatchAnyCharacter,
        } => Ok(vec![RelativeOpcode::Any]),
        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
        } => Ok(vec![RelativeOpcode::Consume(c)]),
        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacterClass(_mcc),
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
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(6))),
                Opcode::Any,
                Opcode::Split(InstSplit::new(InstIndex::from(5), InstIndex::from(6))),
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
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(6))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Split(InstSplit::new(InstIndex::from(5), InstIndex::from(6))),
                Opcode::Consume(InstConsume::new('a')),
                Opcode::Match
            ])),
            compile(regex_ast)
        );
    }
}
