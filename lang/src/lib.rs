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

#[derive(Debug, PartialEq)]
pub enum Expr<'ident> {
    Add(Term<'ident>, Box<Expr<'ident>>),
    Sub(Term<'ident>, Box<Expr<'ident>>),
    Term(Term<'ident>),
}

#[derive(Debug, PartialEq)]
pub enum Term<'ident> {
    Mul(Factor<'ident>, Box<Term<'ident>>),
    Div(Factor<'ident>, Box<Term<'ident>>),
    Factor(Factor<'ident>),
}

#[derive(Debug, PartialEq)]
pub enum Factor<'ident> {
    Parens(Box<Expr<'ident>>),
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

#[derive(Debug)]
pub enum LexError<'token> {
    EmptyInput,
    InvalidToken(&'token str),
}

pub fn lex<'token>(text: &'token str) -> Result<Tokens<'token>, LexError<'token>> {
    match lex_token(text) {
        Err(LexError::EmptyInput) => Ok(Tokens(vec![])),
        Ok((token, text)) => {
            let mut tokens = vec![];
            tokens.push(token);
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
        Some(word) => {
            let mut chars = word.chars();
            match chars.next() {
                Some(ch) if ch.is_alphabetic() && chars.all(|ch| ch.is_alphanumeric()) => {
                    Some((Token::Ident(word), &text[word.len()..text.len()]))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn lex_num<'token>(text: &'token str) -> Option<(Token<'token>, &'token str)> {
    let text = text.trim();
    match text.split_whitespace().next() {
        Some(word) => match word.parse() {
            Ok(num) => Some((Token::Num(num), &text[word.len()..text.len()])),
            Err(_) => None,
        },
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

pub fn parse<'ident>(tokens: &[Token<'ident>]) -> Result<Expr<'ident>, ParseError<'ident>> {
    let (expr, pos) = parse_expr(tokens, 0)?;
    if pos == tokens.len() {
        Ok(expr)
    } else {
        Err(ParseError::UnexpectedToken((pos, tokens[pos].clone())))
    }
}

fn parse_expr<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(Expr<'ident>, usize), ParseError<'ident>> {
    let (lhs, pos) = parse_term(tokens, pos)?;
    if let Some(token) = tokens.get(pos) {
        if token == &Token::Plus {
            let (rhs, pos) = parse_expr(tokens, pos + 1)?;
            return Ok((Expr::Add(lhs, Box::new(rhs)), pos));
        }
        if token == &Token::Hyphen {
            let (rhs, pos) = parse_expr(tokens, pos + 1)?;
            return Ok((Expr::Sub(lhs, Box::new(rhs)), pos));
        }
    }

    Ok((Expr::Term(lhs), pos))
}

fn parse_term<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(Term<'ident>, usize), ParseError<'ident>> {
    let (lhs, pos) = parse_factor(tokens, pos)?;
    match tokens.get(pos) {
        Some(&Token::Asterisk) => {
            let (rhs, pos) = parse_term(tokens, pos + 1)?;
            Ok((Term::Mul(lhs, Box::new(rhs)), pos))
        }
        Some(&Token::Slash) => {
            let (rhs, pos) = parse_term(tokens, pos + 1)?;
            Ok((Term::Div(lhs, Box::new(rhs)), pos))
        }
        _ => Ok((Term::Factor(lhs), pos)),
    }
}

fn parse_factor<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(Factor<'ident>, usize), ParseError<'ident>> {
    match tokens.get(pos) {
        Some(&Token::LParen) => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            if tokens.get(pos) == Some(&Token::RParen) {
                Ok((Factor::Parens(Box::new(expr)), pos + 1))
            } else {
                Err(ParseError::MismatchedParentheses)
            }
        }
        Some(&Token::Ident(ident)) => Ok((Factor::Ident(ident), pos + 1)),
        Some(&Token::Num(num)) => Ok((Factor::Num(num), pos + 1)),
        None => Err(ParseError::SyntaxError(pos)),
        _ => Err(ParseError::UnexpectedToken((pos, tokens[pos].clone()))),
    }
}

impl<'ident> std::fmt::Display for Expr<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Add(lhs, rhs) => write!(f, "({}+{})", lhs, rhs),
            Expr::Sub(lhs, rhs) => write!(f, "({}-{})", lhs, rhs),
            Expr::Term(term) => write!(f, "{}", term),
        }
    }
}

impl<'ident> std::fmt::Display for Term<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Mul(lhs, rhs) => write!(f, "({}*{})", lhs, rhs),
            Term::Div(lhs, rhs) => write!(f, "({}/{})", lhs, rhs),
            Term::Factor(factor) => write!(f, "{}", factor),
        }
    }
}

impl<'ident> std::fmt::Display for Factor<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Factor::Parens(expr) => write!(f, "({})", expr),
            Factor::Num(num) => write!(f, "{}", num),
            Factor::Ident(ident) => write!(f, "{}", ident),
        }
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

pub fn run() {
    // a + b * c
    // let tokens = [
    //     Token::Ident("a"),
    //     Token::Plus,
    //     Token::Ident("b"),
    //     Token::Mul,
    //     Token::Ident("c"),
    // ];

    // a * b + c
    // let tokens = [
    //     Token::Ident("a"),
    //     Token::Mul,
    //     Token::Ident("b"),
    //     Token::Plus,
    //     Token::Ident("c"),
    // ];

    // 8 / 4 / 2
    // let tokens = [
    //     Token::Num(8),
    //     Token::Div,
    //     Token::Num(4),
    //     Token::Div,
    //     Token::Num(2),
    // ];

    let tokens = [Token::Ident("a"), Token::Asterisk];

    for token in &tokens {
        print!("{}", token);
    }
    println!("");

    println!("{:?}", tokens);

    match parse(&tokens) {
        Ok(expr) => println!("{}", expr),
        Err(e) => eprintln!("{:?}", e),
    }
}
