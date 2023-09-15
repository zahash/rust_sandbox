mod expr;
mod stmt;

pub use expr::*;
pub use stmt::*;

use crate::Token;

pub fn parse<'text>(tokens: &[Token<'text>]) -> Result<Stmt<'text>, ParseError> {
    match tokens.is_empty() {
        true => Ok(Stmt::EmptyStmt),
        false => {
            let (stmt, pos) = parse_stmt(tokens, 0)?;
            match pos == tokens.len() {
                true => Ok(stmt),
                false => Err(ParseError::SyntaxError(pos)),
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(usize),
    MismatchedParentheses(usize),
    SyntaxError(usize),
    InvalidStatement(usize),
    ExpectedSemicolon(usize),
    ExpectedLParen(usize),
    ExpectedRParen(usize),
    ExpectedRCurly(usize),
    ExpectedKeyword(&'static str, usize),
    ExpectedIdentifier(usize),
}
