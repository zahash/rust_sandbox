#[derive(Debug, PartialEq, Clone)]
pub enum Token<'ident> {
    Ident(&'ident str),
    Num(isize),
    LParen,
    RParen,
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Expr<'ident> {
    Add(Term<'ident>, Box<Expr<'ident>>),
    Sub(Term<'ident>, Box<Expr<'ident>>),
    Term(Term<'ident>),
}

#[derive(Debug)]
pub enum Term<'ident> {
    Mul(Factor<'ident>, Box<Term<'ident>>),
    Div(Factor<'ident>, Box<Term<'ident>>),
    Factor(Factor<'ident>),
}

#[derive(Debug)]
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
    match tokens.get(pos) {
        Some(&Token::Plus) => {
            let (rhs, pos) = parse_expr(tokens, pos + 1)?;
            Ok((Expr::Add(lhs, Box::new(rhs)), pos))
        }
        Some(&Token::Minus) => {
            let (rhs, pos) = parse_expr(tokens, pos + 1)?;
            Ok((Expr::Sub(lhs, Box::new(rhs)), pos))
        }
        _ => Ok((Expr::Term(lhs), pos)),
    }
}

fn parse_term<'ident>(
    tokens: &[Token<'ident>],
    pos: usize,
) -> Result<(Term<'ident>, usize), ParseError<'ident>> {
    let (lhs, pos) = parse_factor(tokens, pos)?;
    match tokens.get(pos) {
        Some(&Token::Mul) => {
            let (rhs, pos) = parse_term(tokens, pos + 1)?;
            Ok((Term::Mul(lhs, Box::new(rhs)), pos))
        }
        Some(&Token::Div) => {
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
            Expr::Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Expr::Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Expr::Term(term) => write!(f, "{}", term),
        }
    }
}

impl<'ident> std::fmt::Display for Term<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Term::Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
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

impl<'ident> std::fmt::Display for Token<'ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
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

    let tokens = [Token::Ident("a"), Token::Mul];

    for token in &tokens {
        print!("{}", token);
    }
    println!("");

    println!("{:?}", tokens);

    let expr = parse(&tokens);

    match parse(&tokens) {
        Ok(expr) => println!("{}", expr),
        Err(e) => eprintln!("{:?}", e),
    }
}
