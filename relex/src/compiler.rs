use super::ast;
use relex_runtime::*;

type Opcodes = Vec<Opcode>;

pub fn compile(regex_ast: ast::Regex) -> Result<Opcodes, String> {
    match regex_ast {
        ast::Regex::StartOfStringAnchored(expr) => expression(expr),
        ast::Regex::Unanchored(expr) => {
            let prefix = [
                Opcode::Split(InstSplit::new(InstIndex::from(3), InstIndex::from(1))),
                Opcode::Any,
                Opcode::Jmp(InstJmp::new(InstIndex::from(0))),
            ];

            expression(expr).map(|expr| prefix.into_iter().chain(expr.into_iter()).collect())
        }
    }
}

fn expression(expr: ast::Expression) -> Result<Opcodes, String> {
    todo!()
}
