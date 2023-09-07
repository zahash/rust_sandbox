use crate::lex::*;

#[derive(Debug, PartialEq)]
pub enum Primary<'ident> {
    Parens(Box<Primary<'ident>>),
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
        let (primary, pos) = parse_primary(tokens, pos + 1)?;
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
}
