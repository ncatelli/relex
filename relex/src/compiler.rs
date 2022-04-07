use super::ast;
use relex_runtime::*;

type Opcodes = Vec<Opcode>;

pub fn compile(regex_ast: ast::Regex) -> Result<Instructions, String> {
    let suffix = [Opcode::Match];

    match regex_ast {
        ast::Regex::StartOfStringAnchored(expr) => {
            expression(expr).map(|expr| expr.into_iter().chain(suffix.into_iter()).collect())
        }
        ast::Regex::Unanchored(expr) => expression(expr).map(|expr| {
            let prefix = [
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
            ];

            prefix
                .into_iter()
                .chain(expr.into_iter())
                .chain(suffix.into_iter())
                .collect()
        }),
    }
    .map(Instructions::new)
}

fn expression(expr: ast::Expression) -> Result<Opcodes, String> {
    let ast::Expression(subexprs) = expr;

    subexprs
        .into_iter()
        .map(subexpression)
        .collect::<Result<Vec<Opcodes>, _>>()
        .map(|opcodes| opcodes.into_iter().flatten().collect())
}

fn subexpression(subexpr: ast::SubExpression) -> Result<Opcodes, String> {
    let ast::SubExpression(items) = subexpr;

    items
        .into_iter()
        .map(|subexpr_item| match subexpr_item {
            ast::SubExpressionItem::Match(m) => match_item(m),
            ast::SubExpressionItem::Group(_) => todo!(),
            ast::SubExpressionItem::Anchor(_) => todo!(),
            ast::SubExpressionItem::Backreference(_) => todo!(),
        })
        .collect::<Result<Vec<Opcodes>, _>>()
        .map(|opcodes| opcodes.into_iter().flatten().collect())
}

fn match_item(m: ast::Match) -> Result<Opcodes, String> {
    use ast::{Char, Match, MatchCharacter, MatchItem};

    match m {
        Match::WithQuantifier {
            item: _,
            quantifier: _,
        } => todo!(),
        Match::WithoutQuantifier {
            item: MatchItem::MatchAnyCharacter,
        } => todo!(),
        Match::WithoutQuantifier {
            item: MatchItem::MatchCharacter(MatchCharacter(Char(c))),
        } => Ok(vec![Opcode::Consume(InstConsume::new(c))]),
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
}
