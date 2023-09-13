use std::fmt::Display;

use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Token<'text> {
    Ident(&'text str),
    String(&'text str),
    Whole(usize),
    Float(f64),
    Bool(bool),
    Null,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
    LParen,
    RParen,
    Comma,
    Colon,
    SemiColon,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    Caret,
    Equals,
}

lazy_static! {
    static ref IDENT_REGEX: Regex = Regex::new(r#"^[A-Za-z_][A-Za-z0-9_]*"#).unwrap();
    static ref STRING_REGEX: Regex = Regex::new(r#"^"[^"\n]+""#).unwrap();
    static ref WHOLE_REGEX: Regex = Regex::new(r"^[0-9]+").unwrap();
    static ref FLOAT_REGEX: Regex = Regex::new(r"^([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)").unwrap();
    static ref BOOL_REGEX: Regex = Regex::new(r"^(true|false)").unwrap();
}

#[derive(Debug)]
pub struct InvalidToken<'text> {
    pub text: &'text str,
    pub pos: usize,
}

impl<'text> Display for InvalidToken<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.text[self.pos..])
    }
}

pub fn lex(text: &str) -> Result<Vec<Token>, InvalidToken> {
    match text.is_empty() {
        true => Ok(vec![]),
        false => {
            let mut tokens = vec![];
            let mut pos = 0;

            loop {
                while let Some(" ") | Some("\n") = text.get(pos..pos + 1) {
                    pos += 1;
                }

                if pos >= text.len() {
                    break;
                }

                let (token, next_pos) = lex_token(text, pos)?;
                tokens.push(token);
                pos = next_pos;
            }

            Ok(tokens)
        }
    }
}

fn lex_token(text: &str, pos: usize) -> Result<(Token, usize), InvalidToken> {
    lex_bool(text, pos)
        .or(lex_null(text, pos))
        .or(lex_ident(text, pos))
        .or(lex_string(text, pos))
        .or(lex_float(text, pos))
        .or(lex_whole(text, pos))
        .or(lex_lcurly(text, pos))
        .or(lex_rcurly(text, pos))
        .or(lex_lsquare(text, pos))
        .or(lex_rsquare(text, pos))
        .or(lex_lparen(text, pos))
        .or(lex_rparen(text, pos))
        .or(lex_comma(text, pos))
        .or(lex_colon(text, pos))
        .or(lex_semicolon(text, pos))
        .or(lex_plus(text, pos))
        .or(lex_hyphen(text, pos))
        .or(lex_asterisk(text, pos))
        .or(lex_slash(text, pos))
        .or(lex_caret(text, pos))
        .or(lex_equals(text, pos))
        .ok_or(InvalidToken { text, pos })
}

fn lex_ident(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &IDENT_REGEX)?;
    Some((Token::Ident(token), pos))
}

fn lex_string(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &STRING_REGEX)?;
    let token = token
        .strip_prefix("\"")
        .unwrap()
        .strip_suffix("\"")
        .unwrap();
    Some((Token::String(token), pos))
}

fn lex_whole(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &WHOLE_REGEX)?;
    Some((Token::Whole(token.parse().ok()?), pos))
}

fn lex_float(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &FLOAT_REGEX)?;
    Some((Token::Float(token.parse().ok()?), pos))
}

fn lex_bool(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &BOOL_REGEX)?;
    Some((Token::Bool(token.parse().ok()?), pos))
}

fn lex_null(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Null, lex_with_prefix(text, pos, "NULL")?))
}

fn lex_lcurly(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::LCurly, lex_with_prefix(text, pos, "{")?))
}

fn lex_rcurly(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::RCurly, lex_with_prefix(text, pos, "}")?))
}

fn lex_lsquare(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::LSquare, lex_with_prefix(text, pos, "[")?))
}

fn lex_rsquare(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::RSquare, lex_with_prefix(text, pos, "]")?))
}

fn lex_lparen(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::LParen, lex_with_prefix(text, pos, "(")?))
}

fn lex_rparen(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::RParen, lex_with_prefix(text, pos, ")")?))
}

fn lex_comma(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Comma, lex_with_prefix(text, pos, ",")?))
}

fn lex_colon(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Colon, lex_with_prefix(text, pos, ":")?))
}

fn lex_semicolon(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::SemiColon, lex_with_prefix(text, pos, ";")?))
}

fn lex_plus(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Plus, lex_with_prefix(text, pos, "+")?))
}

fn lex_hyphen(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Hyphen, lex_with_prefix(text, pos, "-")?))
}

fn lex_asterisk(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Asterisk, lex_with_prefix(text, pos, "*")?))
}

fn lex_slash(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Slash, lex_with_prefix(text, pos, "/")?))
}

fn lex_caret(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Caret, lex_with_prefix(text, pos, "^")?))
}

fn lex_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Equals, lex_with_prefix(text, pos, "=")?))
}

fn lex_with_prefix<'text>(text: &'text str, pos: usize, prefix: &str) -> Option<usize> {
    match &text[pos..].starts_with(prefix) {
        true => Some(pos + prefix.len()),
        false => None,
    }
}

fn lex_with_pattern<'text>(
    text: &'text str,
    pos: usize,
    pat: &Regex,
) -> Option<(&'text str, usize)> {
    if let Some(slice) = text.get(pos..text.len()) {
        if let Some(m) = pat.find(slice) {
            assert!(
                m.start() == 0,
                "put carat ^ to match the text from the `pos` (text is sliced to start from pos)"
            );
            return Some((m.as_str(), pos + m.end()));
        }
    }

    None
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test() {
        let src = r#"
        idEnt_123"🦀"123 123. .123 123.123 true false NULL{}[](),:;+-*/^=
        "#;

        use Token::*;

        match lex(src) {
            Ok(tokens) => assert_eq!(
                tokens,
                vec![
                    Ident("idEnt_123"),
                    String("🦀"),
                    Whole(123),
                    Float(123.0),
                    Float(0.123),
                    Float(123.123),
                    Bool(true),
                    Bool(false),
                    Null,
                    LCurly,
                    RCurly,
                    LSquare,
                    RSquare,
                    LParen,
                    RParen,
                    Comma,
                    Colon,
                    SemiColon,
                    Plus,
                    Hyphen,
                    Asterisk,
                    Slash,
                    Caret,
                    Equals
                ]
            ),
            Err(e) => assert!(false, "{}", e),
        }
    }
}
