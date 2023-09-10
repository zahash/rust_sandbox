use crate::lex::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Expr<'text> {
    Null,
    Bool(bool),
    Number(&'text str),
    String(&'text str),
    Array(Vec<Expr<'text>>),
    Object(HashMap<&'text str, Expr<'text>>),
}

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(usize),
    MismatchedSquareBrackets(usize),
    MismatchedCurlyBrackets(usize),
}

pub fn parse<'text>(tokens: &[Token<'text>]) -> Result<Expr<'text>, ParseError> {
    match tokens.is_empty() {
        true => Ok(Expr::Null),
        false => {
            let (expr, pos) = parse_expr(tokens, 0)?;
            match pos >= tokens.len() {
                true => Ok(expr),
                false => Err(ParseError::SyntaxError(pos)),
            }
        }
    }
}

fn parse_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Expr<'text>, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Null) => Ok((Expr::Null, pos + 1)),
        Some(Token::Bool(val)) => Ok((Expr::Bool(*val), pos + 1)),
        Some(Token::String(s)) => Ok((Expr::String(s), pos + 1)),
        Some(Token::Num(n)) => Ok((Expr::Number(n), pos + 1)),
        Some(Token::LSquare) => {
            let (exprs, pos) = parse_array(tokens, pos + 1)?;
            Ok((Expr::Array(exprs), pos))
        }
        Some(Token::LCurly) => {
            let (expr, pos) = parse_object(tokens, pos)?;
            Ok((Expr::Object(expr), pos))
        }
        _ => Err(ParseError::SyntaxError(pos)),
    }
}

fn parse_array<'text>(
    tokens: &[Token<'text>],
    mut pos: usize,
) -> Result<(Vec<Expr<'text>>, usize), ParseError> {
    if let Some(Token::LSquare) = tokens.get(pos) {
        pos += 1;
    }

    let mut array: Vec<Expr> = vec![];

    while let Some(token) = tokens.get(pos) {
        if token == &Token::RSquare {
            break;
        }

        let (item, next_pos) = parse_expr(tokens, pos)?;
        pos = next_pos;

        array.push(item);

        match tokens.get(pos) {
            Some(Token::Comma) => {
                pos += 1;
            }
            Some(Token::RSquare) => {
                break;
            }
            _ => return Err(ParseError::MismatchedSquareBrackets(pos)),
        }
    }

    match tokens.get(pos) {
        Some(Token::RSquare) => Ok((array, pos + 1)),
        _ => Err(ParseError::MismatchedSquareBrackets(pos)),
    }
}

fn parse_object<'text>(
    tokens: &[Token<'text>],
    mut pos: usize,
) -> Result<(HashMap<&'text str, Expr<'text>>, usize), ParseError> {
    if let Some(Token::LCurly) = tokens.get(pos) {
        pos += 1;
    }

    let mut object: HashMap<&str, Expr> = HashMap::new();

    while let Some(token) = tokens.get(pos) {
        if token == &Token::RCurly {
            break;
        }

        let (key, next_pos) = match tokens.get(pos) {
            Some(Token::String(key)) => (key, pos + 1),
            _ => return Err(ParseError::SyntaxError(pos)),
        };
        pos = next_pos;

        match tokens.get(pos) {
            Some(Token::Colon) => {
                pos += 1;
            }
            _ => return Err(ParseError::MismatchedSquareBrackets(pos)),
        }

        let (value, next_pos) = parse_expr(tokens, pos)?;
        pos = next_pos;

        object.insert(key, value);

        match tokens.get(pos) {
            Some(Token::Comma) => {
                pos += 1;
            }
            Some(Token::RCurly) => {
                break;
            }
            _ => return Err(ParseError::MismatchedSquareBrackets(pos)),
        }
    }

    match tokens.get(pos) {
        Some(Token::RCurly) => Ok((object, pos + 1)),
        _ => Err(ParseError::MismatchedSquareBrackets(pos)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let tokens = lex(r#" "#).unwrap();
        match parse(&tokens) {
            Ok(expr) => assert_eq!(expr, Expr::Null),
            Err(e) => assert!(false, "{:?}", e),
        }
    }

    #[test]
    fn array() {
        use Expr::*;

        let tokens = lex(r#" [1, 2, 3] "#).unwrap();
        match parse(&tokens) {
            Ok(expr) => assert_eq!(expr, Array(vec![Number("1"), Number("2"), Number("3"),])),
            Err(e) => assert!(false, "{:?}", e),
        }
    }

    #[test]
    fn array_trailing_comma() {
        use Expr::*;

        let tokens = lex(r#" [1, 2, 3,] "#).unwrap();
        match parse(&tokens) {
            Ok(expr) => assert_eq!(expr, Array(vec![Number("1"), Number("2"), Number("3"),])),
            Err(e) => assert!(false, "{:?}", e),
        }
    }

    #[test]
    fn invalid_array_missing_rsquare() {
        let tokens = lex(r#" [1, 2, 3 "#).unwrap();
        match parse(&tokens) {
            Ok(_) => assert!(false, "this shouldn't parse"),
            Err(ParseError::MismatchedSquareBrackets(_)) => assert!(true),
            Err(e) => assert!(false, "wrong error type {:?}", e),
        }
    }

    #[test]
    fn test_object() {
        use Expr::*;

        let tokens = lex(r#" {"name": "zahash", "age": 24} "#).unwrap();
        match parse(&tokens) {
            Ok(expr) => assert_eq!(
                expr,
                Object({
                    let mut map = HashMap::new();
                    map.insert("name", String("zahash"));
                    map.insert("age", Number("24"));
                    map
                })
            ),
            Err(e) => assert!(false, "{:?}", e),
        }
    }

    #[test]
    fn test_nested_object() {
        use Expr::*;

        let tokens = lex(r#"
        {
            "name": "zahash", 
            "age": 24,
            "l": ["🦀", 42, {"b": true}],
            "o": {
                "k": "v",
                "el": [],
            },
        }
        "#)
        .unwrap();
        match parse(&tokens) {
            Ok(expr) => assert_eq!(
                expr,
                Object({
                    let mut map = HashMap::new();
                    map.insert("name", String("zahash"));
                    map.insert("age", Number("24"));
                    map.insert(
                        "l",
                        Array(vec![
                            String("🦀"),
                            Number("42"),
                            Object({
                                let mut b = HashMap::new();
                                b.insert("b", Bool(true));
                                b
                            }),
                        ]),
                    );
                    map.insert(
                        "o",
                        Object({
                            let mut o = HashMap::new();
                            o.insert("k", String("v"));
                            o.insert("el", Array(vec![]));
                            o
                        }),
                    );
                    map
                })
            ),
            Err(e) => assert!(false, "{:?}", e),
        }
    }
}
