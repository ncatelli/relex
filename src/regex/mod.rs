pub mod ast;
pub mod parser;
mod state_machine;

pub use parser::{parse, ParseErr};
