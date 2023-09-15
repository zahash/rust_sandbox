use crate::lex::*;
use crate::parse::expr::*;
use crate::parse::ParseError;

#[derive(Debug)]
pub enum Stmt<'text> {
    EmptyStmt,
    Expr(Expr<'text>),
    Labeled(LabeledStmt<'text>),
    Compound(Vec<Stmt<'text>>),
    Selection(SelectionStmt<'text>),
    Iteration(IterationStmt<'text>),
}

#[derive(Debug)]
pub enum LabeledStmt<'text> {
    Ident(&'text str, Box<Stmt<'text>>),
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

#[derive(Debug)]
pub enum IterationStmt<'text> {
    While {
        test: Expr<'text>,
        body: Box<Stmt<'text>>,
    },
    DoWhile {
        test: Expr<'text>,
        body: Box<Stmt<'text>>,
    },
    For {
        init: Option<Expr<'text>>,
        test: Option<Expr<'text>>,
        update: Option<Expr<'text>>,
        body: Box<Stmt<'text>>,
    },
}

pub fn parse_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParseError> {
    // Expty Statement
    if let Some(Token::SemiColon) = tokens.get(pos) {
        return Ok((Stmt::EmptyStmt, pos + 1));
    }

    // Expression Statement
    if let Ok((expr, pos)) = parse_expr(tokens, pos) {
        return match tokens.get(pos) {
            Some(Token::SemiColon) => Ok((Stmt::Expr(expr), pos + 1)),
            _ => Err(ParseError::ExpectedSemicolon(pos)),
        };
    }

    // Compound Statement
    if let Some(Token::LCurly) = tokens.get(pos) {
        let mut stmts = Vec::new();
        let mut pos = pos + 1;

        while let Some(token) = tokens.get(pos) {
            if token == &Token::RCurly {
                return Ok((Stmt::Compound(stmts), pos + 1));
            }

            let (stmt, next_pos) = parse_stmt(tokens, pos)?;
            pos = next_pos;

            stmts.push(stmt);
        }

        return Err(ParseError::ExpectedRCurly(pos));
    }

    // Selection Statement
    if let Some(Token::Keyword("if")) = tokens.get(pos) {
        let Some(Token::LParen) = tokens.get(pos + 1) else {
            return Err(ParseError::ExpectedLParen(pos + 1));
        };

        let (test, pos) = parse_expr(tokens, pos + 2)?;

        let Some(Token::RParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRParen(pos));
        };

        let (pass, pos) = parse_stmt(tokens, pos + 1)?;
        let pass = Box::new(pass);

        let Some(Token::Keyword("else")) = tokens.get(pos) else {
            return Ok((SelectionStmt::If { test, pass }.into(), pos));
        };

        let (fail, pos) = parse_stmt(tokens, pos + 1)?;
        let fail = Box::new(fail);

        return Ok((SelectionStmt::IfElse { test, pass, fail }.into(), pos));
    }

    // Iteration Statement -- For
    if let Some(Token::Keyword("for")) = tokens.get(pos) {
        let Some(Token::LParen) = tokens.get(pos + 1) else {
            return Err(ParseError::ExpectedLParen(pos + 1));
        };

        let (init, pos) = match tokens.get(pos + 2) {
            Some(Token::SemiColon) => (None, pos + 2),
            _ => {
                let (expr, pos) = parse_expr(tokens, pos + 2)?;
                (Some(expr), pos)
            }
        };

        let Some(Token::SemiColon) = tokens.get(pos) else {
            return Err(ParseError::ExpectedSemicolon(pos));
        };

        let (test, pos) = match tokens.get(pos + 1) {
            Some(Token::SemiColon) => (None, pos + 1),
            _ => {
                let (expr, pos) = parse_expr(tokens, pos + 1)?;
                (Some(expr), pos)
            }
        };

        let Some(Token::SemiColon) = tokens.get(pos) else {
            return Err(ParseError::ExpectedSemicolon(pos));
        };

        let (update, pos) = match tokens.get(pos + 1) {
            Some(Token::RParen) => (None, pos + 1),
            _ => {
                let (expr, pos) = parse_expr(tokens, pos + 1)?;
                (Some(expr), pos)
            }
        };

        let Some(Token::RParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRParen(pos));
        };

        let (body, pos) = parse_stmt(tokens, pos + 1)?;
        let body = Box::new(body);

        return Ok((
            IterationStmt::For {
                init,
                test,
                update,
                body,
            }
            .into(),
            pos,
        ));
    }

    Err(ParseError::InvalidStatement(pos))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all() {
        let tokens = lex(r#"
            {
                a++;
                b++;

                for (i = 0; i < 10; i++) {
                    a += 1;
                }

                if (a == 1) {
                    b++;
                } else if (a == 2) {
                    b--;
                } else {
                    b;
                }
            }
        "#)
        .expect("** LEX ERROR");

        println!("{:?}", tokens);

        match parse_stmt(&tokens, 0) {
            Ok((expr, pos)) => println!("{} {}\n{}", tokens.len(), pos, expr),
            Err(e) => assert!(false, "{:?}", e),
        }
    }
}
