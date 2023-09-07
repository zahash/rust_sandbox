#[derive(Debug)]
pub struct Tokens<'ident>(pub Vec<Token<'ident>>);

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'ident> {
    Ident(&'ident str),
    Num(isize),
    LParen,
    RParen,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
}

#[derive(Debug)]
pub enum LexError<'token> {
    EmptyInput,
    InvalidToken(&'token str),
}

pub fn lex<'token>(text: &'token str) -> Result<Tokens<'token>, LexError<'token>> {
    match lex_token(text) {
        Err(LexError::EmptyInput) => Ok(Tokens(vec![])),
        Ok((token, text)) => {
            let mut tokens = vec![token];
            tokens.extend(lex(text)?.0);
            Ok(Tokens(tokens))
        }
        Err(e) => Err(e),
    }
}

fn lex_token<'token>(text: &'token str) -> Result<(Token<'token>, &'token str), LexError<'token>> {
    match text.is_empty() {
        true => Err(LexError::EmptyInput),
        false => lex_ident(text)
            .or(lex_num(text))
            .or(lex_lparen(text))
            .or(lex_rparen(text))
            .or(lex_plus(text))
            .or(lex_hyphen(text))
            .or(lex_asterisk(text))
            .or(lex_slash(text))
            .ok_or(LexError::InvalidToken(text.lines().next().unwrap())),
    }
}

fn lex_ident<'token>(text: &'token str) -> Option<(Token<'token>, &'token str)> {
    let text = text.trim();
    match text.split_whitespace().next() {
        Some(word) => match word.chars().next() {
            Some(ch) if ch.is_alphabetic() => {
                let first_non_alnum_idx = word
                    .chars()
                    .position(|ch| !ch.is_alphanumeric())
                    .unwrap_or(word.len());

                Some((
                    Token::Ident(word.get(..first_non_alnum_idx).unwrap()),
                    &text[first_non_alnum_idx..text.len()],
                ))
            }
            _ => None,
        },
        _ => None,
    }
}

fn lex_num<'token>(text: &'token str) -> Option<(Token<'_>, &'token str)> {
    let text = text.trim();
    match text.split_whitespace().next() {
        Some(word) => {
            let first_non_digit_idx = word
                .chars()
                .position(|ch| !ch.is_digit(10))
                .unwrap_or(word.len());

            match word[..first_non_digit_idx].parse() {
                Ok(num) => Some((Token::Num(num), &text[first_non_digit_idx..text.len()])),
                Err(_) => None,
            }
        }
        _ => None,
    }
}

fn lex_lparen<'token>(text: &'token str) -> Option<(Token<'_>, &'token str)> {
    lex_single_char('(', Token::LParen, text)
}

fn lex_rparen<'token>(text: &'token str) -> Option<(Token<'_>, &'token str)> {
    lex_single_char(')', Token::RParen, text)
}

fn lex_plus<'token>(text: &'token str) -> Option<(Token<'_>, &'token str)> {
    lex_single_char('+', Token::Plus, text)
}

fn lex_hyphen<'token>(text: &'token str) -> Option<(Token<'_>, &'token str)> {
    lex_single_char('-', Token::Hyphen, text)
}

fn lex_asterisk<'token>(text: &'token str) -> Option<(Token<'_>, &'token str)> {
    lex_single_char('*', Token::Asterisk, text)
}

fn lex_slash<'token>(text: &'token str) -> Option<(Token<'_>, &'token str)> {
    lex_single_char('/', Token::Slash, text)
}

fn lex_single_char<'token>(
    ch: char,
    token: Token<'token>,
    text: &'token str,
) -> Option<(Token<'token>, &'token str)> {
    let mut chars = text.trim().chars();

    match chars.next() {
        Some(_ch) if _ch == ch => Some((token, chars.as_str())),
        _ => None,
    }
}

impl<'ident> std::fmt::Display for Tokens<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|token| token.to_string())
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

impl<'ident> std::fmt::Display for Token<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Plus => write!(f, "+"),
            Token::Hyphen => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Num(n) => write!(f, "{}", n),
        }
    }
}
