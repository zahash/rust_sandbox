#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(usize),
    MismatchedParentheses(usize),
    SyntaxError(usize),
    InvalidStatement(usize),
    ExpectedSemicolon(usize),
    ExpectedLParen(usize),
    ExpectedRParen(usize),
    ExpectedRCurly(usize),
}
