use super::{
    specifier_qualifier::parse_specifier_qualifier, struct_declarator::parse_struct_declarator, write_arr, ParseContext,
};
use crate::{ParseError, SpecifierQualifier, StructDeclarator, Token};
use chainchomp::ctx_sensitive::{many, many_delimited};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclaration<'text> {
    pub specifier_qualifiers: Vec<SpecifierQualifier<'text>>,
    pub declarators: Vec<StructDeclarator<'text>>,
}

pub fn parse_struct_declaration<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(StructDeclaration<'text>, usize), ParseError> {
    let (sqs, pos) = many(tokens, pos, ctx, parse_specifier_qualifier);
    let (ds, pos) = many_delimited(
        tokens,
        pos,
        ctx,
        parse_struct_declarator,
        &Token::Symbol(","),
    );

    let Some(Token::Symbol(";")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos));
    };

    Ok((
        StructDeclaration {
            specifier_qualifiers: sqs,
            declarators: ds,
        },
        pos + 1,
    ))
}

impl<'text> Display for StructDeclaration<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_arr(f, &self.specifier_qualifiers, " ")?;
        write!(f, " ")?;
        write_arr(f, &self.declarators, ", ")?;
        write!(f, ";")
    }
}
