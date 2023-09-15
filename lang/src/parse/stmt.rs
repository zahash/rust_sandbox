use crate::lex::*;
use crate::parse::expr::*;
use crate::parse::ParseError;

#[derive(Debug)]
pub enum Stmt<'text> {
    Expr(ExprStmt<'text>),
    Selection(SelectionStmt<'text>),
}

#[derive(Debug)]
pub enum ExprStmt<'text> {
    Expr(Expr<'text>),
    EmptyStmt,
}

impl<'text> From<ExprStmt<'text>> for Stmt<'text> {
    fn from(value: ExprStmt<'text>) -> Self {
        Stmt::Expr(value)
    }
}

#[derive(Debug)]
pub enum SelectionStmt<'text> {
    If {
        test: Expr<'text>,
        pass: Box<Stmt<'text>>,
    },
    IfElse {
        test: Expr<'text>,
        pass: Box<Stmt<'text>>,
        fail: Box<Stmt<'text>>,
    },
}

impl<'text> From<SelectionStmt<'text>> for Stmt<'text> {
    fn from(value: SelectionStmt<'text>) -> Self {
        Stmt::Selection(value)
    }
}

pub fn parse_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParseError> {
    if let Some(Token::SemiColon) = tokens.get(pos) {
        return Ok((ExprStmt::EmptyStmt.into(), pos + 1));
    }

    if let Ok((expr, pos)) = parse_expr(tokens, pos) {
        return match tokens.get(pos) {
            Some(Token::SemiColon) => Ok((ExprStmt::Expr(expr).into(), pos + 1)),
            _ => Err(ParseError::ExpectedSemicolon(pos)),
        };
    }

    if let Some(Token::Keyword("if")) = tokens.get(pos) {
        return match tokens.get(pos + 1) {
            Some(Token::LParen) => {
                let (test, pos) = parse_expr(tokens, pos + 2)?;

                match tokens.get(pos) {
                    Some(Token::RParen) => {
                        let (pass, pos) = parse_stmt(tokens, pos + 1)?;

                        match tokens.get(pos) {
                            Some(Token::Keyword("else")) => {
                                let (fail, pos) = parse_stmt(tokens, pos + 1)?;

                                Ok((
                                    SelectionStmt::IfElse {
                                        test,
                                        pass: Box::new(pass),
                                        fail: Box::new(fail),
                                    }
                                    .into(),
                                    pos,
                                ))
                            }
                            _ => Ok((
                                SelectionStmt::If {
                                    test,
                                    pass: Box::new(pass),
                                }
                                .into(),
                                pos,
                            )),
                        }
                    }
                    _ => Err(ParseError::ExpectedRParen(pos)),
                }
            }
            _ => Err(ParseError::ExpectedLParen(pos + 1)),
        };
    }

    Err(ParseError::InvalidStatement(pos))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all() {
        let tokens = lex(r#"
            a;
        "#)
        .expect("** LEX ERROR");

        println!("{:?}", tokens);

        match parse_stmt(&tokens, 0) {
            Ok((expr, pos)) => println!("{} {} {:?}", tokens.len(), pos, expr),
            Err(e) => assert!(false, "{:?}", e),
        }
    }
}
