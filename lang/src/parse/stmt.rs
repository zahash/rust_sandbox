use super::expr::{parse_expr, Expr};
use crate::{ParseError, Token};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug)]
pub enum Stmt<'text> {
    EmptyStmt,
    Expr(Expr<'text>),
    Labeled(LabeledStmt<'text>),
    Compound(Vec<Stmt<'text>>),
    Selection(SelectionStmt<'text>),
    Iteration(IterationStmt<'text>),
    Jump(JumpStmt<'text>),
}

pub fn parse_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParseError> {
    for parser in [
        parse_empty_stmt,
        parse_expr_stmt,
        parse_compound_stmt,
        parse_selection_stmt,
        parse_iteration_while_stmt,
        parse_iteration_do_while_stmt,
        parse_iteration_for_stmt,
        parse_jump_goto_stmt,
        parse_jump_continue_stmt,
        parse_jump_break_stmt,
        parse_jump_return_stmt,
    ] {
        match parser(tokens, pos) {
            Err(ParserCombinatorError::IncorrectParser) => continue,
            Err(ParserCombinatorError::ParseError(e)) => return Err(e),
            Ok((stmt, pos)) => return Ok((stmt, pos)),
        };
    }

    Err(ParseError::InvalidStatement(pos))
}

fn parse_empty_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    Ok((Stmt::EmptyStmt, pos + 1))
}

fn parse_expr_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Ok((expr, pos)) = parse_expr(tokens, pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    Ok((Stmt::Expr(expr), pos + 1))
}

#[derive(Debug)]
pub enum LabeledStmt<'text> {
    Ident(&'text str, Box<Stmt<'text>>),
}

fn parse_compound_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::LCurly) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let mut stmts = Vec::new();
    let mut pos = pos + 1;

    while let Some(token) = tokens.get(pos) {
        if token == &Token::RCurly {
            return Ok((Stmt::Compound(stmts), pos + 1));
        }

        match parse_stmt(tokens, pos) {
            Ok((stmt, next_pos)) => {
                pos = next_pos;
                stmts.push(stmt);
            }
            Err(e) => return Err(e.into()),
        };
    }

    Err(ParseError::ExpectedRCurly(pos).into())
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

fn parse_selection_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::Keyword("if")) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let Some(Token::LParen) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedLParen(pos + 1).into());
    };

    let (test, pos) = parse_expr(tokens, pos + 2)?;

    let Some(Token::RParen) = tokens.get(pos) else {
        return Err(ParseError::ExpectedRParen(pos).into());
    };

    let (pass, pos) = parse_stmt(tokens, pos + 1)?;
    let pass = Box::new(pass);

    let Some(Token::Keyword("else")) = tokens.get(pos) else {
        return Ok((SelectionStmt::If { test, pass }.into(), pos));
    };

    let (fail, pos) = parse_stmt(tokens, pos + 1)?;
    let fail = Box::new(fail);

    Ok((SelectionStmt::IfElse { test, pass, fail }.into(), pos))
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

fn parse_iteration_while_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::Keyword("while")) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let Some(Token::LParen) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedLParen(pos + 1).into());
    };

    let (test, pos) = parse_expr(tokens, pos + 2)?;

    let Some(Token::RParen) = tokens.get(pos) else {
        return Err(ParseError::ExpectedRParen(pos).into());
    };

    let (body, pos) = parse_stmt(tokens, pos + 1)?;
    let body = Box::new(body);

    Ok((IterationStmt::While { test, body }.into(), pos))
}

fn parse_iteration_do_while_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::Keyword("do")) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let (body, pos) = parse_stmt(tokens, pos + 1)?;
    let body = Box::new(body);

    let Some(Token::Keyword("while")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("while", pos).into());
    };

    let Some(Token::LParen) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedLParen(pos + 1).into());
    };

    let (test, pos) = parse_expr(tokens, pos + 2)?;

    let Some(Token::RParen) = tokens.get(pos) else {
        return Err(ParseError::ExpectedRParen(pos).into());
    };

    let Some(Token::SemiColon) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedSemicolon(pos + 1).into());
    };

    Ok((IterationStmt::DoWhile { test, body }.into(), pos + 2))
}

fn parse_iteration_for_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::Keyword("for")) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let Some(Token::LParen) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedLParen(pos + 1).into());
    };

    let (init, pos) = match tokens.get(pos + 2) {
        Some(Token::SemiColon) => (None, pos + 2),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 2)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    let (test, pos) = match tokens.get(pos + 1) {
        Some(Token::SemiColon) => (None, pos + 1),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    let (update, pos) = match tokens.get(pos + 1) {
        Some(Token::RParen) => (None, pos + 1),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::RParen) = tokens.get(pos) else {
        return Err(ParseError::ExpectedRParen(pos).into());
    };

    let (body, pos) = parse_stmt(tokens, pos + 1)?;
    let body = Box::new(body);

    Ok((
        IterationStmt::For {
            init,
            test,
            update,
            body,
        }
        .into(),
        pos,
    ))
}

#[derive(Debug)]
pub enum JumpStmt<'text> {
    Goto(&'text str),
    Continue,
    Break,
    Return(Option<Expr<'text>>),
}

fn parse_jump_goto_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::Keyword("goto")) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let Some(Token::Ident(ident)) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedIdentifier(pos + 1).into());
    };

    let Some(Token::SemiColon) = tokens.get(pos + 2) else {
        return Err(ParseError::ExpectedSemicolon(pos + 2).into());
    };

    Ok((JumpStmt::Goto(ident).into(), pos + 3))
}

fn parse_jump_continue_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::Keyword("continue")) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let Some(Token::SemiColon) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedSemicolon(pos + 1).into());
    };

    Ok((JumpStmt::Continue.into(), pos + 2))
}

fn parse_jump_break_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::Keyword("break")) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let Some(Token::SemiColon) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedSemicolon(pos + 1).into());
    };

    Ok((JumpStmt::Continue.into(), pos + 2))
}

fn parse_jump_return_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParserCombinatorError> {
    let Some(Token::Keyword("return")) = tokens.get(pos) else {
        return Err(ParserCombinatorError::IncorrectParser);
    };

    let (expr, pos) = match tokens.get(pos + 1) {
        Some(Token::SemiColon) => (None, pos + 1),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    return Ok((JumpStmt::Return(expr).into(), pos + 1));
}

enum ParserCombinatorError {
    ParseError(ParseError),
    IncorrectParser,
}

impl<'text> Display for Stmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::EmptyStmt => write!(f, ";"),
            Stmt::Expr(stmt) => write!(f, "{};", stmt),
            Stmt::Labeled(stmt) => write!(f, "{}", stmt),
            Stmt::Compound(stmts) => {
                write!(f, "{{ ")?;
                for stmt in stmts {
                    write!(f, "{} ", stmt)?;
                }
                write!(f, "}}")
            }
            Stmt::Selection(stmt) => write!(f, "{}", stmt),
            Stmt::Iteration(stmt) => write!(f, "{}", stmt),
            Stmt::Jump(stmt) => write!(f, "{}", stmt),
        }
    }
}

impl<'text> Display for LabeledStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LabeledStmt::Ident(ident, stmt) => write!(f, "{} : {}", ident, stmt),
        }
    }
}

impl<'text> Display for SelectionStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SelectionStmt::If { test, pass } => write!(f, "if ({}) {}", test, pass),
            SelectionStmt::IfElse { test, pass, fail } => {
                write!(f, "if ({}) {} else {}", test, pass, fail)
            }
        }
    }
}

impl<'text> Display for IterationStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IterationStmt::While { test, body } => write!(f, "while ({}) {}", test, body),
            IterationStmt::DoWhile { test, body } => write!(f, "do {} while ({});", body, test),
            IterationStmt::For {
                init,
                test,
                update,
                body,
            } => {
                write!(f, "for (")?;
                if let Some(expr) = init {
                    write!(f, "{}; ", expr)?;
                }
                if let Some(expr) = test {
                    write!(f, "{}; ", expr)?;
                }
                if let Some(expr) = update {
                    write!(f, "{}) ", expr)?;
                }
                write!(f, "{}", body)
            }
        }
    }
}

impl<'text> Display for JumpStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            JumpStmt::Goto(ident) => write!(f, "goto {};", ident),
            JumpStmt::Continue => write!(f, "continue;"),
            JumpStmt::Break => write!(f, "break;"),
            JumpStmt::Return(expr) => match expr {
                Some(expr) => write!(f, "return {};", expr),
                None => write!(f, "return;"),
            },
        }
    }
}

impl<'text> From<LabeledStmt<'text>> for Stmt<'text> {
    fn from(value: LabeledStmt<'text>) -> Self {
        Stmt::Labeled(value)
    }
}

impl<'text> From<SelectionStmt<'text>> for Stmt<'text> {
    fn from(value: SelectionStmt<'text>) -> Self {
        Stmt::Selection(value)
    }
}

impl<'text> From<IterationStmt<'text>> for Stmt<'text> {
    fn from(value: IterationStmt<'text>) -> Self {
        Stmt::Iteration(value)
    }
}

impl<'text> From<JumpStmt<'text>> for Stmt<'text> {
    fn from(value: JumpStmt<'text>) -> Self {
        Stmt::Jump(value)
    }
}

impl From<ParseError> for ParserCombinatorError {
    fn from(value: ParseError) -> Self {
        ParserCombinatorError::ParseError(value)
    }
}

#[cfg(test)]
mod tests {
    use super::parse_stmt;
    use crate::lex;

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
