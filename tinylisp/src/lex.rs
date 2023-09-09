use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Token<'ident> {
    Ident(&'ident str),
    Num(f64),
    LParen,
    RParen,
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
    lex_ident(text, pos)
        .or(lex_num(text, pos))
        .or(lex_lparen(text, pos))
        .or(lex_rparen(text, pos))
        .ok_or(InvalidToken { pos })
}

fn lex_ident(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^[A-Za-z_][A-Za-z0-9_]*").unwrap();
    let (token, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::Ident(token), pos))
}

fn lex_num(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^(([0-9]+(\.[0-9]*)?)|(\.[0-9]+))").unwrap();
    let (token, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::Num(token.parse().ok()?), pos))
}

fn lex_lparen(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^\(").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::LParen, pos))
}

fn lex_rparen(text: &str, pos: usize) -> Option<(Token, usize)> {
    let pat = Regex::new(r"^\)").unwrap();
    let (_, pos) = lex_pattern(text, pos, &pat)?;
    Some((Token::RParen, pos))
}

fn lex_pattern<'token>(text: &'token str, pos: usize, pat: &Regex) -> Option<(&'token str, usize)> {
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
    fn can_handle_blank_input() -> Result<(), InvalidToken> {
        assert!(lex("")?.is_empty());
        assert!(lex(" ")?.is_empty());
        assert!(lex("     ")?.is_empty());
        assert!(lex("\n")?.is_empty());
        assert!(lex(" \n ")?.is_empty());
        assert!(lex("\n\n")?.is_empty());
        assert!(lex("  \n    \n\n       ")?.is_empty());

        Ok(())
    }

    #[test]
    fn can_handle_whitespaces_and_new_lines() -> Result<(), InvalidToken> {
        assert_eq!(vec![Token::LParen], lex("     (         ")?);
        assert_eq!(vec![Token::LParen, Token::LParen], lex("(  (")?);
        assert_eq!(vec![Token::LParen, Token::LParen], lex("   ((    ")?);
        assert_eq!(vec![Token::LParen, Token::LParen], lex("  (    (       ")?);
        assert_eq!(vec![Token::LParen], lex("\n(")?);
        assert_eq!(vec![Token::LParen], lex("(\n")?);
        assert_eq!(vec![Token::LParen], lex("\n(\n")?);
        assert_eq!(vec![Token::LParen, Token::LParen], lex("(\n\n(")?);
        assert_eq!(vec![Token::LParen, Token::LParen], lex("\n\n\n((\n\n\n")?);

        assert_eq!(
            vec![Token::LParen, Token::LParen],
            lex("  \n\n  \n(    ( \n\n \n")?
        );

        Ok(())
    }

    #[test]
    fn can_lex_num() -> Result<(), InvalidToken> {
        assert_eq!(vec![Token::Num(12.0)], lex("12")?);
        assert_eq!(vec![Token::Num(12.3)], lex("12.3")?);
        assert_eq!(vec![Token::Num(12.0)], lex("12.")?);
        assert_eq!(vec![Token::Num(0.12)], lex(".12")?);

        Ok(())
    }
}
