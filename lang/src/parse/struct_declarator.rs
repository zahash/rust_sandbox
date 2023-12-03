use super::{
    declarator::parse_declarator, expression::conditional::parse_conditional_expr, ParseContext,
};
use crate::{ConstantExpr, Declarator, ParseError, Token};
use chainchomp::ctx_sensitive::maybe;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum StructDeclarator<'text> {
    Declarator(Declarator<'text>),
    DeclaratorWithBitField(Declarator<'text>, ConstantExpr<'text>),
    BitField(ConstantExpr<'text>),
}

pub fn parse_struct_declarator<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(StructDeclarator<'text>, usize), ParseError> {
    let (declarator, pos) = maybe(tokens, pos, ctx, parse_declarator);

    let (bit_field, pos) = if let Some(Token::Symbol(":")) = tokens.get(pos) {
        let (bit_field, pos) = parse_conditional_expr(tokens, pos + 1, ctx)?;
        (Some(bit_field), pos)
    } else {
        (None, pos)
    };

    match (declarator, bit_field) {
        (None, None) => Err(ParseError::SyntaxError(
            pos,
            "cannot parse struct declarator. neither declarator nor bitfield found.",
        )),
        (None, Some(bit_field)) => Ok((StructDeclarator::BitField(bit_field), pos)),
        (Some(declarator), None) => Ok((StructDeclarator::Declarator(declarator), pos)),
        (Some(declarator), Some(bit_field)) => Ok((
            StructDeclarator::DeclaratorWithBitField(declarator, bit_field),
            pos,
        )),
    }
}

impl<'text> Display for StructDeclarator<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StructDeclarator::Declarator(d) => write!(f, "{}", d),
            StructDeclarator::DeclaratorWithBitField(d, e) => write!(f, "{} : {}", d, e),
            StructDeclarator::BitField(e) => write!(f, ": {}", e),
        }
    }
}
