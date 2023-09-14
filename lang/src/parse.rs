use crate::lex::*;

pub fn parse<'ident>(tokens: &[Token<'ident>]) -> Result<Expr<'ident>, ParseError> {
    match tokens.is_empty() {
        true => Ok(Primary::Int(0).into()),
        false => {
            let (expr, pos) = parse_expr(&tokens, 0)?;
            match pos == tokens.len() {
                true => Ok(expr),
                false => Err(ParseError::SyntaxError(pos)),
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(usize),
    MismatchedParentheses(usize),
    SyntaxError(usize),
}

type Expr<'text> = AssignmentExpr<'text>;

pub fn parse_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Expr<'text>, usize), ParseError> {
    parse_assignment_expr(tokens, pos)
}

impl<'text> From<Primary<'text>> for Expr<'text> {
    fn from(value: Primary<'text>) -> Self {
        Expr::ConditionalExpr(ConditionalExpr::LogicalOrExpr(
            LogicalOrExpr::LogicalAndExpr(LogicalAndExpr::BitOrExpr(BitOrExpr::XORExpr(
                XORExpr::BitAndExpr(BitAndExpr::EqualityExpr(EqualityExpr::ComparisionExpr(
                    ComparisionExpr::ShiftExpr(ShiftExpr::AdditiveExpr(
                        AdditiveExpr::MultiplicativeExpr(MultiplicativeExpr::UnaryExpr(
                            UnaryExpr::PostfixExpr(PostfixExpr::Primary(value)),
                        )),
                    )),
                ))),
            ))),
        ))
    }
}

#[derive(Debug, PartialEq)]
pub enum AssignmentExpr<'text> {
    ConditionalExpr(ConditionalExpr<'text>),
    Assign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    MulAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    DivAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    ModAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    AddAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    SubAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    ShiftLeftAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    ShiftRightAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    BitAndAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    XORAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    BitOrAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
}

pub fn parse_assignment_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(AssignmentExpr<'text>, usize), ParseError> {
    todo!()
}

impl<'text> From<ConditionalExpr<'text>> for AssignmentExpr<'text> {
    fn from(value: ConditionalExpr<'text>) -> Self {
        AssignmentExpr::ConditionalExpr(value)
    }
}

impl<'text> std::fmt::Display for AssignmentExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignmentExpr::ConditionalExpr(expr) => write!(f, "{}", expr),
            AssignmentExpr::Assign(lhs, rhs) => write!(f, "({} = {})", lhs, rhs),
            AssignmentExpr::MulAssign(lhs, rhs) => write!(f, "({} *= {})", lhs, rhs),
            AssignmentExpr::DivAssign(lhs, rhs) => write!(f, "({} /= {})", lhs, rhs),
            AssignmentExpr::ModAssign(lhs, rhs) => write!(f, "({} %= {})", lhs, rhs),
            AssignmentExpr::AddAssign(lhs, rhs) => write!(f, "({} += {})", lhs, rhs),
            AssignmentExpr::SubAssign(lhs, rhs) => write!(f, "({} -= {})", lhs, rhs),
            AssignmentExpr::ShiftLeftAssign(lhs, rhs) => write!(f, "({} <<= {})", lhs, rhs),
            AssignmentExpr::ShiftRightAssign(lhs, rhs) => write!(f, "({} >>= {})", lhs, rhs),
            AssignmentExpr::BitAndAssign(lhs, rhs) => write!(f, "({} &= {})", lhs, rhs),
            AssignmentExpr::XORAssign(lhs, rhs) => write!(f, "({} ^= {})", lhs, rhs),
            AssignmentExpr::BitOrAssign(lhs, rhs) => write!(f, "({} |= {})", lhs, rhs),
        }
    }
}

// type ConstantExpr<'text> = ConditionalExpr<'text>;

#[derive(Debug, PartialEq)]
pub enum ConditionalExpr<'text> {
    LogicalOrExpr(LogicalOrExpr<'text>),
    Ternary {
        test: LogicalOrExpr<'text>,
        pass: Box<Expr<'text>>,
        fail: Box<ConditionalExpr<'text>>,
    },
}

pub fn parse_conditional_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ConditionalExpr<'text>, usize), ParseError> {
    todo!()
}

impl<'text> From<LogicalOrExpr<'text>> for ConditionalExpr<'text> {
    fn from(value: LogicalOrExpr<'text>) -> Self {
        ConditionalExpr::LogicalOrExpr(value)
    }
}

impl<'text> std::fmt::Display for ConditionalExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConditionalExpr::LogicalOrExpr(expr) => write!(f, "{}", expr),
            ConditionalExpr::Ternary { test, pass, fail } => {
                write!(f, "( {} ? {} : {} )", test, pass, fail)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LogicalOrExpr<'text> {
    LogicalAndExpr(LogicalAndExpr<'text>),
    LogicalOr(Box<LogicalOrExpr<'text>>, LogicalAndExpr<'text>),
}

pub fn parse_logicalor_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(LogicalOrExpr<'text>, usize), ParseError> {
    todo!()
}

impl<'text> From<LogicalAndExpr<'text>> for LogicalOrExpr<'text> {
    fn from(value: LogicalAndExpr<'text>) -> Self {
        LogicalOrExpr::LogicalAndExpr(value)
    }
}

impl<'text> std::fmt::Display for LogicalOrExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOrExpr::LogicalAndExpr(expr) => write!(f, "{}", expr),
            LogicalOrExpr::LogicalOr(lhs, rhs) => write!(f, "({} || {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LogicalAndExpr<'text> {
    BitOrExpr(BitOrExpr<'text>),
    LogicalAnd(Box<LogicalAndExpr<'text>>, BitOrExpr<'text>),
}

pub fn parse_logicaland_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(LogicalAndExpr<'text>, usize), ParseError> {
    todo!()
}

impl<'text> From<BitOrExpr<'text>> for LogicalAndExpr<'text> {
    fn from(value: BitOrExpr<'text>) -> Self {
        LogicalAndExpr::BitOrExpr(value)
    }
}

impl<'text> std::fmt::Display for LogicalAndExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalAndExpr::BitOrExpr(expr) => write!(f, "{}", expr),
            LogicalAndExpr::LogicalAnd(lhs, rhs) => write!(f, "({} && {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BitOrExpr<'text> {
    XORExpr(XORExpr<'text>),
    BitOr(Box<BitOrExpr<'text>>, XORExpr<'text>),
}

pub fn parse_bitor_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(BitOrExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_xor_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Pipe => {
                let (rhs, next_pos) = parse_xor_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = BitOrExpr::BitOr(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

impl<'text> From<XORExpr<'text>> for BitOrExpr<'text> {
    fn from(value: XORExpr<'text>) -> Self {
        BitOrExpr::XORExpr(value)
    }
}

impl<'text> std::fmt::Display for BitOrExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BitOrExpr::XORExpr(expr) => write!(f, "{}", expr),
            BitOrExpr::BitOr(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum XORExpr<'text> {
    BitAndExpr(BitAndExpr<'text>),
    XOR(Box<XORExpr<'text>>, BitAndExpr<'text>),
}

pub fn parse_xor_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(XORExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_bitand_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Caret => {
                let (rhs, next_pos) = parse_bitand_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = XORExpr::XOR(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

impl<'text> From<BitAndExpr<'text>> for XORExpr<'text> {
    fn from(value: BitAndExpr<'text>) -> Self {
        XORExpr::BitAndExpr(value)
    }
}

impl<'text> std::fmt::Display for XORExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            XORExpr::BitAndExpr(expr) => write!(f, "{}", expr),
            XORExpr::XOR(lhs, rhs) => write!(f, "({} ^ {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BitAndExpr<'text> {
    EqualityExpr(EqualityExpr<'text>),
    BitAnd(Box<BitAndExpr<'text>>, EqualityExpr<'text>),
}

pub fn parse_bitand_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(BitAndExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_equality_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Ampersand => {
                let (rhs, next_pos) = parse_equality_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = BitAndExpr::BitAnd(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

impl<'text> From<EqualityExpr<'text>> for BitAndExpr<'text> {
    fn from(value: EqualityExpr<'text>) -> Self {
        BitAndExpr::EqualityExpr(value)
    }
}

impl<'text> std::fmt::Display for BitAndExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BitAndExpr::EqualityExpr(expr) => write!(f, "{}", expr),
            BitAndExpr::BitAnd(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EqualityExpr<'text> {
    ComparisionExpr(ComparisionExpr<'text>),
    EQ(Box<EqualityExpr<'text>>, ComparisionExpr<'text>),
    NE(Box<EqualityExpr<'text>>, ComparisionExpr<'text>),
}

pub fn parse_equality_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(EqualityExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_comparision_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::EQ => {
                let (rhs, next_pos) = parse_comparision_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = EqualityExpr::EQ(Box::new(lhs), rhs);
            }
            Token::NE => {
                let (rhs, next_pos) = parse_comparision_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = EqualityExpr::NE(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

impl<'text> From<ComparisionExpr<'text>> for EqualityExpr<'text> {
    fn from(value: ComparisionExpr<'text>) -> Self {
        EqualityExpr::ComparisionExpr(value)
    }
}

impl<'text> std::fmt::Display for EqualityExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EqualityExpr::ComparisionExpr(expr) => write!(f, "{}", expr),
            EqualityExpr::EQ(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            EqualityExpr::NE(lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ComparisionExpr<'text> {
    ShiftExpr(ShiftExpr<'text>),
    LT(Box<ComparisionExpr<'text>>, ShiftExpr<'text>),
    GT(Box<ComparisionExpr<'text>>, ShiftExpr<'text>),
    LE(Box<ComparisionExpr<'text>>, ShiftExpr<'text>),
    GE(Box<ComparisionExpr<'text>>, ShiftExpr<'text>),
}

pub fn parse_comparision_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ComparisionExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_shift_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::LT => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ComparisionExpr::LT(Box::new(lhs), rhs);
            }
            Token::GT => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ComparisionExpr::GT(Box::new(lhs), rhs);
            }
            Token::LE => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ComparisionExpr::LE(Box::new(lhs), rhs);
            }
            Token::GE => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ComparisionExpr::GE(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

impl<'text> From<ShiftExpr<'text>> for ComparisionExpr<'text> {
    fn from(value: ShiftExpr<'text>) -> Self {
        ComparisionExpr::ShiftExpr(value)
    }
}

impl<'text> std::fmt::Display for ComparisionExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisionExpr::ShiftExpr(expr) => write!(f, "{}", expr),
            ComparisionExpr::LT(lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            ComparisionExpr::GT(lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            ComparisionExpr::LE(lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),
            ComparisionExpr::GE(lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ShiftExpr<'text> {
    AdditiveExpr(AdditiveExpr<'text>),
    ShiftLeft(Box<ShiftExpr<'text>>, AdditiveExpr<'text>),
    ShiftRight(Box<ShiftExpr<'text>>, AdditiveExpr<'text>),
}

pub fn parse_shift_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ShiftExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_additive_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::LTLT => {
                let (rhs, next_pos) = parse_additive_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ShiftExpr::ShiftLeft(Box::new(lhs), rhs);
            }
            Token::GTGT => {
                let (rhs, next_pos) = parse_additive_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ShiftExpr::ShiftRight(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

impl<'text> From<AdditiveExpr<'text>> for ShiftExpr<'text> {
    fn from(value: AdditiveExpr<'text>) -> Self {
        ShiftExpr::AdditiveExpr(value)
    }
}

impl<'text> std::fmt::Display for ShiftExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShiftExpr::AdditiveExpr(expr) => write!(f, "{}", expr),
            ShiftExpr::ShiftLeft(lhs, rhs) => write!(f, "({} << {})", lhs, rhs),
            ShiftExpr::ShiftRight(lhs, rhs) => write!(f, "({} >> {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AdditiveExpr<'text> {
    MultiplicativeExpr(MultiplicativeExpr<'text>),
    Add(Box<AdditiveExpr<'text>>, MultiplicativeExpr<'text>),
    Sub(Box<AdditiveExpr<'text>>, MultiplicativeExpr<'text>),
}

pub fn parse_additive_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(AdditiveExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_multiplicative_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Plus => {
                let (rhs, next_pos) = parse_multiplicative_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = AdditiveExpr::Add(Box::new(lhs), rhs);
            }
            Token::Hyphen => {
                let (rhs, next_pos) = parse_multiplicative_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = AdditiveExpr::Sub(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

impl<'text> From<MultiplicativeExpr<'text>> for AdditiveExpr<'text> {
    fn from(value: MultiplicativeExpr<'text>) -> Self {
        AdditiveExpr::MultiplicativeExpr(value)
    }
}

impl<'text> std::fmt::Display for AdditiveExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AdditiveExpr::MultiplicativeExpr(expr) => write!(f, "{}", expr),
            AdditiveExpr::Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            AdditiveExpr::Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MultiplicativeExpr<'text> {
    UnaryExpr(UnaryExpr<'text>),
    Mul(Box<MultiplicativeExpr<'text>>, UnaryExpr<'text>),
    Div(Box<MultiplicativeExpr<'text>>, UnaryExpr<'text>),
    Mod(Box<MultiplicativeExpr<'text>>, UnaryExpr<'text>),
}

pub fn parse_multiplicative_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(MultiplicativeExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_unary_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Asterisk => {
                let (rhs, next_pos) = parse_unary_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Mul(Box::new(lhs), rhs);
            }
            Token::Slash => {
                let (rhs, next_pos) = parse_unary_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Div(Box::new(lhs), rhs);
            }
            Token::Percent => {
                let (rhs, next_pos) = parse_unary_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Mod(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

impl<'text> From<UnaryExpr<'text>> for MultiplicativeExpr<'text> {
    fn from(value: UnaryExpr<'text>) -> Self {
        MultiplicativeExpr::UnaryExpr(value)
    }
}

impl<'text> std::fmt::Display for MultiplicativeExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MultiplicativeExpr::UnaryExpr(expr) => write!(f, "{}", expr),
            MultiplicativeExpr::Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            MultiplicativeExpr::Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            MultiplicativeExpr::Mod(lhs, rhs) => write!(f, "({} % {})", lhs, rhs),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryExpr<'text> {
    PostfixExpr(PostfixExpr<'text>),
    PreIncr(Box<UnaryExpr<'text>>),
    PreDecr(Box<UnaryExpr<'text>>),
    Ref(Box<UnaryExpr<'text>>),
    Deref(Box<UnaryExpr<'text>>),
    UnaryAdd(Box<UnaryExpr<'text>>),
    UnarySub(Box<UnaryExpr<'text>>),
    OnesComplement(Box<UnaryExpr<'text>>),
    Not(Box<UnaryExpr<'text>>),
}

pub fn parse_unary_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(UnaryExpr<'text>, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::PlusPlus) => {
            let (expr, pos) = parse_postfix_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::PreIncr(Box::new(expr.into())), pos))
        }
        Some(Token::HyphenHyphen) => {
            let (expr, pos) = parse_postfix_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::PreDecr(Box::new(expr.into())), pos))
        }
        Some(Token::Ampersand) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::Ref(Box::new(expr)), pos))
        }
        Some(Token::Asterisk) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::Deref(Box::new(expr)), pos))
        }
        Some(Token::Plus) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::UnaryAdd(Box::new(expr)), pos))
        }
        Some(Token::Hyphen) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::UnarySub(Box::new(expr)), pos))
        }
        Some(Token::Tilde) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::OnesComplement(Box::new(expr)), pos))
        }
        Some(Token::Exclamation) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::Not(Box::new(expr)), pos))
        }
        _ => {
            let (expr, pos) = parse_postfix_expr(tokens, pos)?;
            Ok((expr.into(), pos))
        }
    }
}

impl<'text> From<PostfixExpr<'text>> for UnaryExpr<'text> {
    fn from(value: PostfixExpr<'text>) -> Self {
        UnaryExpr::PostfixExpr(value)
    }
}

impl<'text> std::fmt::Display for UnaryExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryExpr::PostfixExpr(expr) => write!(f, "{}", expr),
            UnaryExpr::PreIncr(expr) => write!(f, "++{}", expr),
            UnaryExpr::PreDecr(expr) => write!(f, "--{}", expr),
            UnaryExpr::Ref(expr) => write!(f, "&{}", expr),
            UnaryExpr::Deref(expr) => write!(f, "*{}", expr),
            UnaryExpr::UnaryAdd(expr) => write!(f, "{}", expr),
            UnaryExpr::UnarySub(expr) => write!(f, "-{}", expr),
            UnaryExpr::OnesComplement(expr) => write!(f, "~{}", expr),
            UnaryExpr::Not(expr) => write!(f, "!{}", expr),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PostfixExpr<'text> {
    Primary(Primary<'text>),
    PostIncr(Box<PostfixExpr<'text>>),
    PostDecr(Box<PostfixExpr<'text>>),
}

pub fn parse_postfix_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(PostfixExpr<'text>, usize), ParseError> {
    // let (expr, mut pos) = parse_primary_expr(tokens, pos)?;
    // let mut expr = expr.into();

    // while let Some(token) = tokens.get(pos) {
    //     match token {
    //         Token::PlusPlus => {
    //             expr = PostfixExpr::PostIncr(Box::new(expr));
    //             pos += 1;
    //         }
    //         Token::HyphenHyphen => {
    //             expr = PostfixExpr::PostDecr(Box::new(expr));
    //             pos += 1;
    //         }
    //         _ => break,
    //     }
    // }

    // Ok((expr, pos))

    let (expr, pos) = parse_primary_expr(tokens, pos)?;

    match tokens.get(pos) {
        Some(Token::PlusPlus) => Ok((PostfixExpr::PostIncr(Box::new(expr.into())), pos + 1)),
        Some(Token::HyphenHyphen) => Ok((PostfixExpr::PostDecr(Box::new(expr.into())), pos + 1)),
        _ => Ok((expr.into(), pos)),
    }
}

impl<'text> From<Primary<'text>> for PostfixExpr<'text> {
    fn from(value: Primary<'text>) -> Self {
        PostfixExpr::Primary(value)
    }
}

impl<'text> std::fmt::Display for PostfixExpr<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PostfixExpr::Primary(expr) => write!(f, "{}", expr),
            PostfixExpr::PostIncr(expr) => write!(f, "{}++", expr),
            PostfixExpr::PostDecr(expr) => write!(f, "{}--", expr),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Primary<'text> {
    Ident(&'text str),
    Int(isize),
    Char(char),
    Float(f64),
    String(&'text str),
    Parens(Box<Expr<'text>>),
}

pub fn parse_primary_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Primary<'text>, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Ident(ident)) => Ok((Primary::Ident(ident), pos + 1)),
        Some(Token::Whole(n)) => Ok((Primary::Int(*n as isize), pos + 1)),
        Some(Token::Char(c)) => Ok((Primary::Char(*c), pos + 1)),
        Some(Token::Decimal(n)) => Ok((Primary::Float(*n), pos + 1)),
        Some(Token::String(s)) => Ok((Primary::String(s), pos + 1)),
        Some(Token::LParen) => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            match tokens.get(pos) {
                Some(Token::RParen) => Ok((Primary::Parens(Box::new(expr)), pos + 1)),
                _ => Err(ParseError::MismatchedParentheses(pos)),
            }
        }
        _ => Err(ParseError::SyntaxError(pos)),
    }
}

impl<'text> std::fmt::Display for Primary<'text> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primary::Ident(ident) => write!(f, "{}", ident),
            Primary::Int(n) => write!(f, "{}", n),
            Primary::Char(c) => write!(f, "'{}'", c),
            Primary::Float(n) => write!(f, "{}", n),
            Primary::String(s) => write!(f, "\"{}\"", s),
            Primary::Parens(expr) => write!(f, "({})", expr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all() {
        let tokens = lex(r#"
            --ab--
        "#)
        .expect("** LEX ERROR");

        println!("{:?}", tokens);

        match parse_unary_expr(&tokens, 0) {
            Ok((expr, pos)) => println!("{} {} {:?}", tokens.len(), pos, expr),
            Err(e) => assert!(false, "{:?}", e),
        }
    }
}

// ==================================================================================================================

// pub fn parse<'text>(tokens: &[Token<'text>]) -> Result<AssignmentExpr<'text>, ParseError> {
//     match tokens.is_empty() {
//         true => Err(ParseError::EmptyInput),
//         false => {
//             let (expr, pos) = parse_expr(&tokens, 0)?;
//             match pos == tokens.len() {
//                 true => Ok(expr),
//                 false => Err(ParseError::SyntaxError(pos)),
//             }
//         }
//     }
// }

// fn parse_expr<'text>(
//     tokens: &[Token<'text>],
//     pos: usize,
// ) -> Result<(AssignmentExpr<'text>, usize), ParseError> {
//     parse_assignment(tokens, pos)
// }

// fn parse_assignment<'text>(
//     tokens: &[Token<'text>],
//     pos: usize,
// ) -> Result<(AssignmentExpr<'text>, usize), ParseError> {
//     if let Some(&Token::Ident(ident)) = tokens.get(pos) {
//         if let Some(&Token::Equals) = tokens.get(pos + 1) {
//             let (rhs, pos) = parse_assignment(tokens, pos + 2)?;
//             return Ok((AssignmentExpr::Assign(Ident(ident), Box::new(rhs)), pos));
//         }
//     }

//     let (expr, pos) = parse_additive(tokens, pos)?;
//     Ok((AssignmentExpr::AdditiveExpr(expr), pos))
// }

// fn parse_additive<'text>(
//     tokens: &[Token<'text>],
//     pos: usize,
// ) -> Result<(AdditiveExpr<'text>, usize), ParseError> {
//     let (lhs, mut pos) = parse_multiplicative(tokens, pos)?;
//     let mut lhs = AdditiveExpr::MultiplicativeExpr(lhs);
//     while let Some(token) = tokens.get(pos) {
//         match token {
//             &Token::Plus => {
//                 let (rhs, next_pos) = parse_multiplicative(tokens, pos + 1)?;
//                 pos = next_pos;
//                 lhs = AdditiveExpr::Add(Box::new(lhs), rhs);
//             }
//             &Token::Hyphen => {
//                 let (rhs, next_pos) = parse_multiplicative(tokens, pos + 1)?;
//                 pos = next_pos;
//                 lhs = AdditiveExpr::Sub(Box::new(lhs), rhs);
//             }
//             _ => break,
//         }
//     }
//     Ok((lhs, pos))
// }

// fn parse_multiplicative<'text>(
//     tokens: &[Token<'text>],
//     pos: usize,
// ) -> Result<(MultiplicativeExpr<'text>, usize), ParseError> {
//     let (lhs, mut pos) = parse_exponential(tokens, pos)?;
//     let mut lhs = MultiplicativeExpr::ExponentialExpr(lhs);
//     while let Some(token) = tokens.get(pos) {
//         match token {
//             &Token::Asterisk => {
//                 let (rhs, next_pos) = parse_exponential(tokens, pos + 1)?;
//                 pos = next_pos;
//                 lhs = MultiplicativeExpr::Mul(Box::new(lhs), rhs);
//             }
//             &Token::Slash => {
//                 let (rhs, next_pos) = parse_exponential(tokens, pos + 1)?;
//                 pos = next_pos;
//                 lhs = MultiplicativeExpr::Div(Box::new(lhs), rhs);
//             }
//             _ => break,
//         }
//     }
//     Ok((lhs, pos))
// }

// fn parse_exponential<'text>(
//     tokens: &[Token<'text>],
//     pos: usize,
// ) -> Result<(ExponentialExpr<'text>, usize), ParseError> {
//     let (lhs, pos) = parse_primary(tokens, pos)?;
//     if let Some(token) = tokens.get(pos) {
//         if token == &Token::Caret {
//             let (rhs, pos) = parse_exponential(tokens, pos + 1)?;
//             return Ok((ExponentialExpr::Pow(lhs, Box::new(rhs)), pos));
//         }
//     }
//     Ok((ExponentialExpr::Primary(lhs), pos))
// }

// fn parse_primary<'text>(
//     tokens: &[Token<'text>],
//     pos: usize,
// ) -> Result<(Primary<'text>, usize), ParseError> {
//     match tokens.get(pos) {
//         Some(&Token::LParen) => {
//             let (expr, pos) = parse_expr(tokens, pos + 1)?;
//             match tokens.get(pos) == Some(&Token::RParen) {
//                 true => Ok((Primary::Parens(Box::new(expr)), pos + 1)),
//                 false => Err(ParseError::MismatchedParentheses(pos)),
//             }
//         }
//         Some(&Token::Ident(ident)) => Ok((Primary::Ident(ident), pos + 1)),
//         Some(&Token::Float(num)) => Ok((Primary::Num(num), pos + 1)),
//         Some(&Token::Hyphen) => {
//             let (expr, pos) = parse_primary(tokens, pos + 1)?;
//             Ok((Primary::UnarySub(Box::new(expr)), pos))
//         }
//         None => Err(ParseError::SyntaxError(pos)),
//         _ => Err(ParseError::UnexpectedToken(pos)),
//     }
// }

// impl<'text> std::fmt::Display for AssignmentExpr<'text> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             AssignmentExpr::Assign(lhs, rhs) => write!(f, "({}={})", lhs, rhs),
//             AssignmentExpr::AdditiveExpr(expr) => write!(f, "{}", expr),
//         }
//     }
// }

// impl<'text> std::fmt::Display for Ident<'text> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }

// impl<'text> std::fmt::Display for AdditiveExpr<'text> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             AdditiveExpr::Add(lhs, rhs) => write!(f, "({}+{})", lhs, rhs),
//             AdditiveExpr::Sub(lhs, rhs) => write!(f, "({}-{})", lhs, rhs),
//             AdditiveExpr::MultiplicativeExpr(expr) => write!(f, "{}", expr),
//         }
//     }
// }

// impl<'text> std::fmt::Display for MultiplicativeExpr<'text> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             MultiplicativeExpr::Mul(lhs, rhs) => write!(f, "({}*{})", lhs, rhs),
//             MultiplicativeExpr::Div(lhs, rhs) => write!(f, "({}/{})", lhs, rhs),
//             MultiplicativeExpr::ExponentialExpr(expr) => write!(f, "{}", expr),
//         }
//     }
// }

// impl<'text> std::fmt::Display for ExponentialExpr<'text> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             ExponentialExpr::Pow(lhs, rhs) => write!(f, "({}^{})", lhs, rhs),
//             ExponentialExpr::Primary(primary) => write!(f, "{}", primary),
//         }
//     }
// }

// impl<'text> std::fmt::Display for Primary<'text> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Primary::Parens(expr) => write!(f, "({})", expr),
//             Primary::Num(num) => write!(f, "{}", num),
//             Primary::Ident(ident) => write!(f, "{}", ident),
//             Primary::UnarySub(num) => write!(f, "{}", num),
//         }
//     }
// }
