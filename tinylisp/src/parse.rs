use crate::lex::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'text> {
    Num(f64),
    String(&'text str),
    Ident(&'text str),
    List(Vec<Expr<'text>>),
}

#[derive(Debug)]
pub enum ParseError {
    MismatchedParentheses(usize),
    SyntaxError(usize),
}

pub fn parse<'text>(tokens: &[Token<'text>]) -> Result<Expr<'text>, ParseError> {
    let (expr, pos) = parse_expr(&tokens, 0)?;
    match pos >= tokens.len() {
        true => Ok(expr),
        false => Err(ParseError::SyntaxError(pos)),
    }
}

fn parse_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Expr<'text>, usize), ParseError> {
    match tokens.get(pos) {
        Some(&Token::LParen) => {
            let (exprs, pos) = parse_list(tokens, pos + 1)?;
            match tokens.get(pos) == Some(&Token::RParen) {
                true => Ok((Expr::List(exprs), pos + 1)),
                false => Err(ParseError::MismatchedParentheses(pos)),
            }
        }
        Some(&Token::Ident(ident)) => Ok((Expr::Ident(ident), pos + 1)),
        Some(&Token::String(s)) => Ok((Expr::String(s), pos + 1)),
        Some(&Token::Num(n)) => Ok((Expr::Num(n), pos + 1)),
        _ => Err(ParseError::SyntaxError(pos)),
    }
}

fn parse_list<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Vec<Expr<'text>>, usize), ParseError> {
    let mut expressions = Vec::new();
    let mut new_pos = pos;
    while let Some(token) = tokens.get(new_pos) {
        match token {
            Token::RParen => break, // End of list
            _ => {
                let (expr, next_pos) = parse_expr(tokens, new_pos)?;
                expressions.push(expr);
                new_pos = next_pos;
            }
        }
    }
    Ok((expressions, new_pos))
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn can_parse() {
        let tokens = lex("(mul 10 (add 2 3) a)").unwrap();
        println!("{:?}", tokens);
        let expr = parse(&tokens).unwrap();
        println!("{:?}", expr);
    }
}
