use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Token<'text> {
    String(&'text str),
    Num(&'text str),
    Bool(bool),
    Null,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
    Comma,
    Colon,
}

#[derive(Debug)]
pub struct InvalidToken {
    pub pos: usize,
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
    lex_string(text, pos)
        .or(lex_num(text, pos))
        .or(lex_bool(text, pos))
        .or(lex_null(text, pos))
        .or(lex_lcurly(text, pos))
        .or(lex_rcurly(text, pos))
        .or(lex_lsquare(text, pos))
        .or(lex_rsquare(text, pos))
        .or(lex_comma(text, pos))
        .or(lex_colon(text, pos))
        .ok_or(InvalidToken { pos })
}

fn lex_string(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r#"^"[^"\n]+""#).unwrap();
    let (token, pos) = lex_pattern(text, pos, &pat)?;
    let token = token
        .strip_prefix("\"")
        .unwrap()
        .strip_suffix("\"")
        .unwrap();
    Some((Token::String(token), pos))
}

fn lex_num(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))").unwrap();
    let (token, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::Num(token), pos))
}

fn lex_bool(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^(true|false)").unwrap();
    let (token, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::Bool(token.parse().ok()?), pos))
}

fn lex_null(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^null").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::Null, pos))
}

fn lex_lcurly(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^\{").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::LCurly, pos))
}

fn lex_rcurly(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^\}").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::RCurly, pos))
}

fn lex_lsquare(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^\[").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::LSquare, pos))
}

fn lex_rsquare(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^\]").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::RSquare, pos))
}

fn lex_comma(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^,").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::Comma, pos))
}

fn lex_colon(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^:").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::Colon, pos))
}

fn lex_pattern<'text>(text: &'text str, pos: usize, pat: &Regex) -> Option<(&'text str, usize)> {
    if let Some(slice) = text.get(pos..text.len()) {
        if let Some(m) = pat.find(slice) {
            assert!(
                m.start() == 0,
                "put carat ^ to match the text from the start"
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
    fn test() -> Result<(), InvalidToken> {
        println!(
            "{:?}",
            lex(r#"
        {
            "name": "zahash",
            "age": 24,
            "hobbies": ["🦀", 42, false],
            "nested": {
                "life": "42",
                "not_found": null,
            }
        }
        "#)?
        );

        Ok(())
    }
}
