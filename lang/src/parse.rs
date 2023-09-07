use crate::lex::*;

#[derive(Debug, PartialEq)]
pub enum AdditiveExpr<'ident> {
    Add(Box<AdditiveExpr<'ident>>, MultiplicativeExpr<'ident>),
    Sub(Box<AdditiveExpr<'ident>>, MultiplicativeExpr<'ident>),
    MultiplicativeExpr(MultiplicativeExpr<'ident>),
}

#[derive(Debug, PartialEq)]
pub enum MultiplicativeExpr<'ident> {
    Mul(Box<MultiplicativeExpr<'ident>>, Primary<'ident>),
    Div(Box<MultiplicativeExpr<'ident>>, Primary<'ident>),
    Primary(Primary<'ident>),
}

// #[derive(Debug, PartialEq)]
// pub enum ExponentialExpr<'ident> {
//     Pow(Primary<'ident>, Box<ExponentialExpr<'ident>>),
//     Primary(Primary<'ident>),
// }

#[derive(Debug, PartialEq)]
pub enum Primary<'ident> {
    Parens(Box<AdditiveExpr<'ident>>),
    Num(isize),
    Ident(&'ident str),
}

#[derive(Debug)]
pub enum ParseError<'ident> {
    EmptyInput,
    UnexpectedToken((usize, Token<'ident>)),
    MismatchedParentheses,
    SyntaxError(usize),
}

fn parse_additive<'ident, 'tokens>(
    tokens: &'tokens [Token<'ident>],
    pos: usize,
) -> Option<(AdditiveExpr<'ident>, usize)> {
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
    Some((lhs, pos))
}

fn parse_multiplicative<'ident, 'tokens>(
    tokens: &'tokens [Token<'ident>],
    pos: usize,
) -> Option<(MultiplicativeExpr<'ident>, usize)> {
    let (lhs, mut pos) = parse_primary(tokens, pos)?;
    let mut lhs = MultiplicativeExpr::Primary(lhs);
    while let Some(token) = tokens.get(pos) {
        match token {
            &Token::Asterisk => {
                let (rhs, next_pos) = parse_primary(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Mul(Box::new(lhs), rhs);
            }
            &Token::Slash => {
                let (rhs, next_pos) = parse_primary(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Div(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Some((lhs, pos))
}

fn parse_primary<'ident, 'tokens>(
    tokens: &'tokens [Token<'ident>],
    pos: usize,
) -> Option<(Primary<'ident>, usize)> {
    parse_primary_parens(tokens, pos)
        .or(parse_primary_ident(tokens, pos))
        .or(parse_primary_num(tokens, pos))
}

fn parse_primary_parens<'ident, 'tokens>(
    tokens: &'tokens [Token<'ident>],
    pos: usize,
) -> Option<(Primary<'ident>, usize)> {
    if let Some(&Token::LParen) = tokens.get(pos) {
        let (primary, pos) = parse_additive(tokens, pos + 1)?;
        if tokens.get(pos)? == &Token::RParen {
            return Some((Primary::Parens(Box::new(primary)), pos + 1));
        }
    }
    None
}

fn parse_primary_ident<'ident, 'tokens>(
    tokens: &'tokens [Token<'ident>],
    pos: usize,
) -> Option<(Primary<'ident>, usize)> {
    match tokens.get(pos) {
        Some(&Token::Ident(ident)) => Some((Primary::Ident(ident), pos + 1)),
        _ => None,
    }
}

fn parse_primary_num<'ident, 'tokens>(
    tokens: &'tokens [Token<'ident>],
    pos: usize,
) -> Option<(Primary<'ident>, usize)> {
    match tokens.get(pos) {
        Some(&Token::Num(n)) => Some((Primary::Num(n), pos + 1)),
        _ => None,
    }
}

impl<'ident> std::fmt::Display for AdditiveExpr<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AdditiveExpr::Add(lhs, rhs) => write!(f, "({}+{})", lhs, rhs),
            AdditiveExpr::Sub(lhs, rhs) => write!(f, "({}-{})", lhs, rhs),
            AdditiveExpr::MultiplicativeExpr(factor) => write!(f, "{}", factor),
        }
    }
}

impl<'ident> std::fmt::Display for MultiplicativeExpr<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MultiplicativeExpr::Mul(lhs, rhs) => write!(f, "({}*{})", lhs, rhs),
            MultiplicativeExpr::Div(lhs, rhs) => write!(f, "({}/{})", lhs, rhs),
            MultiplicativeExpr::Primary(factor) => write!(f, "{}", factor),
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
