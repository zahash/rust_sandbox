use super::ParseContext;
use crate::{ParseError, Token};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum StructOrUnion {
    Struct,
    Union,
}

pub fn parse_struct_or_union<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    _: &mut ParseContext<'text>,
) -> Result<(StructOrUnion, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Keyword("struct")) => Ok((StructOrUnion::Struct, pos + 1)),
        Some(Token::Keyword("union")) => Ok((StructOrUnion::Union, pos + 1)),
        _ => Err(ParseError::ExpectedOneOf(
            vec![Token::Keyword("struct"), Token::Keyword("union")],
            pos,
        )),
    }
}

impl Display for StructOrUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StructOrUnion::Struct => write!(f, "struct"),
            StructOrUnion::Union => write!(f, "union"),
        }
    }
}
