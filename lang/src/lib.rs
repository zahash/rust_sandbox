pub mod lex;
pub mod parse;

// expression ::= equality-expression
// equality-expression ::= additive-expression ( ( '==' | '!=' ) additive-expression ) *
// additive-expression ::= multiplicative-expression ( ( '+' | '-' ) multiplicative-expression ) *
// multiplicative-expression ::= primary ( ( '*' | '/' ) primary ) *
// primary ::= '(' expression ')' | NUMBER | VARIABLE | '-' primary

// #[derive(Debug, PartialEq)]
// pub enum Expr<'ident> {

// }

// #[derive(Debug)]
// pub enum Expr<'ident> {
//     Additive(Box<Expr<'ident>>, AdditiveExpr<'ident>),
//     Assignment(&'ident str, Box<Expr<'ident>>),
// }

// #[derive(Debug, PartialEq)]
// pub enum AdditiveExpr<'ident> {
//     Add(Box<AdditiveExpr<'ident>>, MultiplicativeExpr<'ident>),
//     Sub(Box<AdditiveExpr<'ident>>, MultiplicativeExpr<'ident>),
//     MultiplicativeExpr(MultiplicativeExpr<'ident>),
// }

// #[derive(Debug, PartialEq)]
// pub enum MultiplicativeExpr<'ident> {
//     Mul(Box<MultiplicativeExpr<'ident>>, Primary<'ident>),
//     Div(Box<MultiplicativeExpr<'ident>>, Primary<'ident>),
//     Primary(Primary<'ident>),
// }

// pub fn parse<'ident>(tokens: &[Token<'ident>]) -> Result<Expr<'ident>, ParseError<'ident>> {
//     let (expr, pos) = parse_expr(tokens, 0)?;
//     if pos == tokens.len() {
//         Ok(expr)
//     } else {
//         Err(ParseError::UnexpectedToken((pos, tokens[pos].clone())))
//     }
// }

// fn parse_expr<'ident>(
//     tokens: &[Token<'ident>],
//     pos: usize,
// ) -> Result<(Expr<'ident>, usize), ParseError<'ident>> {
//     let (lhs, pos) = parse_additive(tokens, pos)?;
//     if let Some(token) = tokens.get(pos) {
//         if token == &Token::Plus {
//             let (rhs, pos) = parse_expr(tokens, pos + 1)?;
//             return Ok((Expr::Add(lhs, Box::new(rhs)), pos));
//         }
//         if token == &Token::Hyphen {
//             let (rhs, pos) = parse_expr(tokens, pos + 1)?;
//             return Ok((Expr::Sub(lhs, Box::new(rhs)), pos));
//         }
//     }

//     Ok((Expr::MultiplicativeExpr(lhs), pos))
// }

// fn parse_additive<'ident>(
//     tokens: &[Token<'ident>],
//     pos: usize,
// ) -> Result<(AdditiveExpr<'ident>, usize), ParseError<'ident>> {
//     let (lhs, pos) = parse_factor(tokens, pos)?;
//     match tokens.get(pos) {
//         Some(&Token::Asterisk) => {
//             let (rhs, pos) = parse_term(tokens, pos + 1)?;
//             Ok((MultiplicativeExpr::Mul(lhs, Box::new(rhs)), pos))
//         }
//         Some(&Token::Slash) => {
//             let (rhs, pos) = parse_term(tokens, pos + 1)?;
//             Ok((MultiplicativeExpr::Div(lhs, Box::new(rhs)), pos))
//         }
//         _ => Ok((MultiplicativeExpr::Primary(lhs), pos)),
//     }
// }

// fn parse_primary<'ident>(
//     tokens: &[Token<'ident>],
// ) -> Result<(Primary<'ident>, usize), ParseError<'ident>> {
//     //     let (lhs, pos) = parse_additive(tokens, pos)?;
//     //     if let Some(token) = tokens.get(pos) {
//     //         if token == &Token::Plus {
//     //             let (rhs, pos) = parse_expr(tokens, pos + 1)?;
//     //             return Ok((Expr::Add(lhs, Box::new(rhs)), pos));
//     //         }
//     //         if token == &Token::Hyphen {
//     //             let (rhs, pos) = parse_expr(tokens, pos + 1)?;
//     //             return Ok((Expr::Sub(lhs, Box::new(rhs)), pos));
//     //         }
//     //     }

//     //     Ok((Expr::MultiplicativeExpr(lhs), pos))

//     match tokens.get(pos) {
//         Some(&Token::LParen) => {
//             let (expr, pos) = parse_expr(tokens, pos + 1)?;
//             if tokens.get(pos) == Some(&Token::RParen) {
//                 Ok((Primary::Parens(Box::new(expr)), pos + 1))
//             } else {
//                 Err(ParseError::MismatchedParentheses)
//             }
//         }
//         Some(&Token::Ident(ident)) => Ok((Primary::Ident(ident), pos + 1)),
//         Some(&Token::Num(num)) => Ok((Primary::Num(num), pos + 1)),
//         None => Err(ParseError::SyntaxError(pos)),
//         _ => Err(ParseError::UnexpectedToken((pos, tokens[pos].clone()))),
//     }
// }

// pub fn parse<'ident>(tokens: &[Token<'ident>]) -> Result<AdditiveExpr<'ident>, ParseError<'ident>> {
//     let (expr, pos) = parse_expr(tokens, 0)?;
//     if pos == tokens.len() {
//         Ok(expr)
//     } else {
//         Err(ParseError::UnexpectedToken((pos, tokens[pos].clone())))
//     }
// }

// fn parse_expr<'ident>(
//     tokens: &[Token<'ident>],
//     pos: usize,
// ) -> Result<(AdditiveExpr<'ident>, usize), ParseError<'ident>> {
//     let (lhs, pos) = parse_term(tokens, pos)?;
//     if let Some(token) = tokens.get(pos) {
//         if token == &Token::Plus {
//             let (rhs, pos) = parse_expr(tokens, pos + 1)?;
//             return Ok((AdditiveExpr::Add(lhs, Box::new(rhs)), pos));
//         }
//         if token == &Token::Hyphen {
//             let (rhs, pos) = parse_expr(tokens, pos + 1)?;
//             return Ok((AdditiveExpr::Sub(lhs, Box::new(rhs)), pos));
//         }
//     }

//     Ok((AdditiveExpr::MultiplicativeExpr(lhs), pos))
// }

// fn parse_term<'ident>(
//     tokens: &[Token<'ident>],
//     pos: usize,
// ) -> Result<(MultiplicativeExpr<'ident>, usize), ParseError<'ident>> {
//     let (lhs, pos) = parse_factor(tokens, pos)?;
//     match tokens.get(pos) {
//         Some(&Token::Asterisk) => {
//             let (rhs, pos) = parse_term(tokens, pos + 1)?;
//             Ok((MultiplicativeExpr::Mul(lhs, Box::new(rhs)), pos))
//         }
//         Some(&Token::Slash) => {
//             let (rhs, pos) = parse_term(tokens, pos + 1)?;
//             Ok((MultiplicativeExpr::Div(lhs, Box::new(rhs)), pos))
//         }
//         _ => Ok((MultiplicativeExpr::Primary(lhs), pos)),
//     }
// }

// fn parse_factor<'ident>(
//     tokens: &[Token<'ident>],
//     pos: usize,
// ) -> Result<(Primary<'ident>, usize), ParseError<'ident>> {
//     match tokens.get(pos) {
//         Some(&Token::LParen) => {
//             let (expr, pos) = parse_expr(tokens, pos + 1)?;
//             if tokens.get(pos) == Some(&Token::RParen) {
//                 Ok((Primary::Parens(Box::new(expr)), pos + 1))
//             } else {
//                 Err(ParseError::MismatchedParentheses)
//             }
//         }
//         Some(&Token::Ident(ident)) => Ok((Primary::Ident(ident), pos + 1)),
//         Some(&Token::Num(num)) => Ok((Primary::Num(num), pos + 1)),
//         None => Err(ParseError::SyntaxError(pos)),
//         _ => Err(ParseError::UnexpectedToken((pos, tokens[pos].clone()))),
//     }
// }

// impl<'ident> std::fmt::Display for AdditiveExpr<'ident> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             AdditiveExpr::Add(lhs, rhs) => write!(f, "({}+{})", lhs, rhs),
//             AdditiveExpr::Sub(lhs, rhs) => write!(f, "({}-{})", lhs, rhs),
//             AdditiveExpr::MultiplicativeExpr(term) => write!(f, "{}", term),
//         }
//     }
// }

// impl<'ident> std::fmt::Display for MultiplicativeExpr<'ident> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             MultiplicativeExpr::Mul(lhs, rhs) => write!(f, "({}*{})", lhs, rhs),
//             MultiplicativeExpr::Div(lhs, rhs) => write!(f, "({}/{})", lhs, rhs),
//             MultiplicativeExpr::Primary(factor) => write!(f, "{}", factor),
//         }
//     }
// }

// impl<'ident> std::fmt::Display for Primary<'ident> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Primary::Parens(expr) => write!(f, "({})", expr),
//             Primary::Num(num) => write!(f, "{}", num),
//             Primary::Ident(ident) => write!(f, "{}", ident),
//         }
//     }
// }

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

    use lex::*;

    let tokens = [Token::Ident("a"), Token::Asterisk];

    for token in &tokens {
        print!("{}", token);
    }
    println!("");

    println!("{:?}", tokens);

    // match parse(&tokens) {
    //     Ok(expr) => println!("{}", expr),
    //     Err(e) => eprintln!("{:?}", e),
    // }
}
