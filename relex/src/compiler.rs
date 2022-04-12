use super::ast;
use relex_runtime::*;

/// A internal representation of the `relex_runtime::Opcode` type, with relative
/// addressing.
///
/// ## Note
/// This type is meant to exist only internally and should be
/// refined to the `relex_runtime::Opcode type
#[derive(Debug, Clone, PartialEq)]
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
    fn to_opcode_with_index_unchecked(&self, idx: usize) -> Opcode {
        match self {
            RelativeOpcode::Any => Opcode::Any,
            RelativeOpcode::Consume(c) => Opcode::Consume(InstConsume::new(*c)),
            RelativeOpcode::Split(rel_x, rel_y) => {
                let signed_idx = idx as isize;
                let x = signed_idx + rel_x;
                let y = signed_idx + rel_y;

                // this should be made safe.
                Opcode::Split(InstSplit::new(
                    InstIndex::from(x as usize),
                    InstIndex::from(y as usize),
                ))
            }
            RelativeOpcode::Jmp(rel_jmp_to) => {
                let signed_idx = idx as isize;
                let jmp_to = signed_idx + rel_jmp_to;

                // this should be made safe.
                Opcode::Jmp(InstJmp::new(InstIndex::from(jmp_to as usize)))
            }
            RelativeOpcode::StartSave(slot) => Opcode::StartSave(InstStartSave::new(*slot)),
            RelativeOpcode::EndSave(slot) => Opcode::EndSave(InstEndSave::new(*slot)),
            RelativeOpcode::Match => Opcode::Match,
        }
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
        // match atleast
        Match::WithQuantifier {
            item: MatchItem::MatchAnyCharacter,
            quantifier: Quantifier::Eager(QuantifierType::MatchAtleastRange(Integer(cnt))),
        } => {
            let _min_match = vec![RelativeOpcode::Any; cnt as usize];
            let _optional = vec![
                RelativeOpcode::Split(3, 1),
                RelativeOpcode::Any,
                RelativeOpcode::Jmp(-2),
            ];

            todo!()
        }

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
}
