use self::assignment::parse_assignment_expr;
use super::ParseContext;
use crate::{AssignmentExpr, ParseError, Token};

pub mod additive;
pub mod assignment;
pub mod bitand;
pub mod bitor;
pub mod cast;
pub mod comparision;
pub mod conditional;
pub mod constant;
pub mod equality;
pub mod logicaland;
pub mod logicalor;
pub mod multiplicative;
pub mod postfix;
pub mod primary;
pub mod shift;
pub mod unary;
pub mod xor;

pub type Expr<'text> = AssignmentExpr<'text>;
pub fn parse_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(Expr<'text>, usize), ParseError> {
    parse_assignment_expr(tokens, pos, ctx)
}
