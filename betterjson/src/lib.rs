mod lex;
mod parse;

use lex::*;
use parse::*;

pub fn json(text: &str) -> Result<Expr, JsonError> {
    let tokens = lex(text)?;
    let expr = parse(&tokens)?;
    Ok(expr)
}

pub enum JsonError {
    LexError(InvalidToken),
    ParseError(ParseError),
}

impl From<InvalidToken> for JsonError {
    fn from(value: InvalidToken) -> Self {
        JsonError::LexError(value)
    }
}

impl From<ParseError> for JsonError {
    fn from(value: ParseError) -> Self {
        JsonError::ParseError(value)
    }
}
