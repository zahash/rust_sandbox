use crate::lex::*;

// ** Precedence Climbing **
// assignment-expr (right assoc)::= additive-expr | ident = assignment-expr
// additive-expr (left assoc) ::= multiplicative-expr
//                              | additive-expr + multiplicative-expr
//                              | additive-expr - multiplicative-expr
// multiplicative-expr (left assoc) ::= exponential-expr
//                                    | multiplicative-expr * exponential-expr
//                                    | multiplicative-expr / exponential-expr
// exponential-expr (right assoc) ::= primary | primary ^ exponential-expr
// primary ::= ( assignment-expr ) | num | ident
// ident ::= [A-Za-z][A-Za-z0-9]*
// num ::= [0-9]+

#[derive(Debug, PartialEq)]
pub enum AssignmentExpr<'ident> {
    Assign(Ident<'ident>, Box<AssignmentExpr<'ident>>),
    AdditiveExpr(AdditiveExpr<'ident>),
}

#[derive(Debug, PartialEq)]
pub struct Ident<'ident>(pub &'ident str);

#[derive(Debug, PartialEq)]
pub enum AdditiveExpr<'ident> {
    Add(Box<AdditiveExpr<'ident>>, MultiplicativeExpr<'ident>),
    Sub(Box<AdditiveExpr<'ident>>, MultiplicativeExpr<'ident>),
    MultiplicativeExpr(MultiplicativeExpr<'ident>),
}

#[derive(Debug, PartialEq)]
pub enum MultiplicativeExpr<'ident> {
    Mul(Box<MultiplicativeExpr<'ident>>, ExponentialExpr<'ident>),
    Div(Box<MultiplicativeExpr<'ident>>, ExponentialExpr<'ident>),
    ExponentialExpr(ExponentialExpr<'ident>),
}

#[derive(Debug, PartialEq)]
pub enum ExponentialExpr<'ident> {
    Pow(Primary<'ident>, Box<ExponentialExpr<'ident>>),
    Primary(Primary<'ident>),
}

#[derive(Debug, PartialEq)]
pub enum Primary<'ident> {
    Parens(Box<AssignmentExpr<'ident>>),
    Num(f64),
    Ident(&'ident str),
}

#[derive(Debug)]
pub enum ParseError {
    EmptyInput,
    UnexpectedToken(usize),
    MismatchedParentheses(usize),
    SyntaxError(usize),
}

pub fn parse<'ident>(tokens: &Tokens<'ident>) -> Result<AssignmentExpr<'ident>, ParseError> {
    let tokens = &tokens.0;
    match tokens.is_empty() {
        true => Err(ParseError::EmptyInput),
        false => {
            let (expr, pos) = parse_expr(&tokens, 0)?;
            match pos == tokens.len() {
                true => Ok(expr),
                false => Err(ParseError::SyntaxError(pos)),
            }
        }
    }
}

fn parse_expr<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(AssignmentExpr<'ident>, usize), ParseError> {
    parse_assignment(tokens, pos)
}

fn parse_assignment<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(AssignmentExpr<'ident>, usize), ParseError> {
    if let Some(&Token::Ident(ident)) = tokens.get(pos) {
        if let Some(&Token::Equals) = tokens.get(pos + 1) {
            let (rhs, pos) = parse_assignment(tokens, pos + 2)?;
            return Ok((AssignmentExpr::Assign(Ident(ident), Box::new(rhs)), pos));
        }
    }

    let (expr, pos) = parse_additive(tokens, pos)?;
    Ok((AssignmentExpr::AdditiveExpr(expr), pos))
}

fn parse_additive<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(AdditiveExpr<'ident>, usize), ParseError> {
    let (lhs, mut pos) = parse_multiplicative(tokens, pos)?;
    let mut lhs = AdditiveExpr::MultiplicativeExpr(lhs);
    while let Some(token) = tokens.get(pos) {
        match token {
            &Token::Plus => {
                let (rhs, next_pos) = parse_multiplicative(tokens, pos + 1)?;
                pos = next_pos;
                lhs = AdditiveExpr::Add(Box::new(lhs), rhs);
            }
            &Token::Hyphen => {
                let (rhs, next_pos) = parse_multiplicative(tokens, pos + 1)?;
                pos = next_pos;
                lhs = AdditiveExpr::Sub(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

fn parse_multiplicative<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(MultiplicativeExpr<'ident>, usize), ParseError> {
    let (lhs, mut pos) = parse_exponential(tokens, pos)?;
    let mut lhs = MultiplicativeExpr::ExponentialExpr(lhs);
    while let Some(token) = tokens.get(pos) {
        match token {
            &Token::Asterisk => {
                let (rhs, next_pos) = parse_exponential(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Mul(Box::new(lhs), rhs);
            }
            &Token::Slash => {
                let (rhs, next_pos) = parse_exponential(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Div(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

fn parse_exponential<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(ExponentialExpr<'ident>, usize), ParseError> {
    let (lhs, pos) = parse_primary(tokens, pos)?;
    if let Some(token) = tokens.get(pos) {
        if token == &Token::Caret {
            let (rhs, pos) = parse_exponential(tokens, pos + 1)?;
            return Ok((ExponentialExpr::Pow(lhs, Box::new(rhs)), pos));
        }
    }
    Ok((ExponentialExpr::Primary(lhs), pos))
}

fn parse_primary<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(Primary<'ident>, usize), ParseError> {
    match tokens.get(pos) {
        Some(&Token::LParen) => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            match tokens.get(pos) == Some(&Token::RParen) {
                true => Ok((Primary::Parens(Box::new(expr)), pos + 1)),
                false => Err(ParseError::MismatchedParentheses(pos)),
            }
        }
        Some(&Token::Ident(ident)) => Ok((Primary::Ident(ident), pos + 1)),
        Some(&Token::Num(num)) => Ok((Primary::Num(num), pos + 1)),
        None => Err(ParseError::SyntaxError(pos)),
        _ => Err(ParseError::UnexpectedToken(pos)),
    }
}

impl<'ident> std::fmt::Display for AssignmentExpr<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignmentExpr::Assign(lhs, rhs) => write!(f, "({}={})", lhs, rhs),
            AssignmentExpr::AdditiveExpr(expr) => write!(f, "{}", expr),
        }
    }
}

impl<'ident> std::fmt::Display for Ident<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'ident> std::fmt::Display for AdditiveExpr<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AdditiveExpr::Add(lhs, rhs) => write!(f, "({}+{})", lhs, rhs),
            AdditiveExpr::Sub(lhs, rhs) => write!(f, "({}-{})", lhs, rhs),
            AdditiveExpr::MultiplicativeExpr(expr) => write!(f, "{}", expr),
        }
    }
}

impl<'ident> std::fmt::Display for MultiplicativeExpr<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MultiplicativeExpr::Mul(lhs, rhs) => write!(f, "({}*{})", lhs, rhs),
            MultiplicativeExpr::Div(lhs, rhs) => write!(f, "({}/{})", lhs, rhs),
            MultiplicativeExpr::ExponentialExpr(expr) => write!(f, "{}", expr),
        }
    }
}

impl<'ident> std::fmt::Display for ExponentialExpr<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExponentialExpr::Pow(lhs, rhs) => write!(f, "({}^{})", lhs, rhs),
            ExponentialExpr::Primary(primary) => write!(f, "{}", primary),
        }
    }
}

impl<'ident> std::fmt::Display for Primary<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primary::Parens(expr) => write!(f, "({})", expr),
            Primary::Num(num) => write!(f, "{}", num),
            Primary::Ident(ident) => write!(f, "{}", ident),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_parse() {
        let tokens = lex("2 ^ 3 * 4").unwrap();
        println!("{:?}", tokens);
        let expr = parse(&tokens).unwrap();
        println!("{}", expr);
        println!("{:?}", expr);
    }

    #[test]
    fn test_parse_primary_ident() {
        let tokens = lex("aa 10").unwrap();

        println!("{:?}", tokens);
        println!("{:?}", parse_primary(&tokens.0, 0));
    }

    #[test]
    fn test_parse_primary_num() {
        let tokens = lex("10 aa").unwrap();

        println!("{:?}", tokens);
        println!("{:?}", parse_primary(&tokens.0, 0));
    }

    #[test]
    fn test_parse_primary_parens() {
        let tokens = lex("(((ab)))").unwrap();

        println!("{:?}", tokens);
        println!("{:?}", parse_primary(&tokens.0, 0));
    }

    #[test]
    fn test_parse_multiplicative() {
        let tokens = lex("a * b / c").unwrap();

        println!("{:?}", tokens);
        let expr = parse_multiplicative(&tokens.0, 0).unwrap();
        println!("{} {}", expr.0, expr.1);
    }

    #[test]
    fn test_parse_additive() {
        let tokens = lex("a - b - c").unwrap();

        println!("{:?}", tokens);
        let expr = parse_additive(&tokens.0, 0).unwrap();
        println!("{} {}", expr.0, expr.1);
    }
}
