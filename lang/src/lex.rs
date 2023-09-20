use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Token<'text> {
    Keyword(&'text str),
    Ident(&'text str),
    String(&'text str),
    Char(char),
    Whole(usize),
    Decimal(f64),
    Bool(bool),
    Null,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
    LParen,
    RParen,
    Dot,
    DotDotDot,
    Comma,
    Colon,
    SemiColon,
    Plus,
    PlusPlus,
    Hyphen,
    HyphenHyphen,
    Asterisk,
    Slash,
    Percent,
    Caret,
    Equals,
    AsteriskEquals,
    SlashEquals,
    PercentEquals,
    PlusEquals,
    HyphenEquals,
    LTLTEquals,
    GTGTEquals,
    AmpersandEquals,
    CaretEquals,
    PipeEquals,
    Ampersand,
    AmpersandAmpersand,
    Pipe,
    PipePipe,
    Exclamation,
    Question,
    Tilde,
    LT,
    GT,
    LE,
    GE,
    EQ,
    NE,
    LTLT,
    GTGT,
}

lazy_static! {
    static ref KEYWORD_REGEX: Regex = Regex::new(r#"^(auto|break|case|char|const|continue|default|do|double|else|enum|extern|float|for|goto|if|int|long|register|return|short|signed|sizeof|static|struct|switch|typedef|union|unsigned|void|volatile|while)\b"#).unwrap();
    static ref IDENT_REGEX: Regex = Regex::new(r#"^[A-Za-z_][A-Za-z0-9_]*"#).unwrap();
    static ref STRING_REGEX: Regex = Regex::new(r#"^"[^"\n]+""#).unwrap();
    static ref CHAR_REGEX: Regex = Regex::new(r#"^'.'"#).unwrap();
    static ref WHOLE_REGEX: Regex = Regex::new(r"^[0-9]+").unwrap();
    static ref FLOAT_REGEX: Regex = Regex::new(r"^([0-9]+\.[0-9]+|[0-9]+\.|\.[0-9]+)").unwrap();
    static ref BOOL_REGEX: Regex = Regex::new(r"^(true|false)").unwrap();
}

#[derive(Debug)]
pub enum LexError {
    InvalidToken { pos: usize },
}

pub fn lex(text: &str) -> Result<Vec<Token>, LexError> {
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

fn lex_token(text: &str, pos: usize) -> Result<(Token, usize), LexError> {
    lex_keyword(text, pos)
        .or(lex_bool(text, pos))
        .or(lex_null(text, pos))
        .or(lex_ident(text, pos))
        .or(lex_string(text, pos))
        .or(lex_char(text, pos))
        .or(lex_decimal(text, pos))
        .or(lex_whole(text, pos))
        .or(lex_lcurly(text, pos))
        .or(lex_rcurly(text, pos))
        .or(lex_lsquare(text, pos))
        .or(lex_rsquare(text, pos))
        .or(lex_lparen(text, pos))
        .or(lex_rparen(text, pos))
        .or(lex_dot_dot_dot(text, pos))
        .or(lex_dot(text, pos))
        .or(lex_comma(text, pos))
        .or(lex_colon(text, pos))
        .or(lex_semicolon(text, pos))
        .or(lex_plus_plus(text, pos))
        .or(lex_plus_equals(text, pos))
        .or(lex_plus(text, pos))
        .or(lex_hyphen_hyphen(text, pos))
        .or(lex_hyphen_equals(text, pos))
        .or(lex_hyphen(text, pos))
        .or(lex_asterisk_equals(text, pos))
        .or(lex_asterisk(text, pos))
        .or(lex_slash_equals(text, pos))
        .or(lex_slash(text, pos))
        .or(lex_percent_equals(text, pos))
        .or(lex_percent(text, pos))
        .or(lex_caret_equals(text, pos))
        .or(lex_caret(text, pos))
        .or(lex_eq(text, pos))
        .or(lex_ne(text, pos))
        .or(lex_equals(text, pos))
        .or(lex_ampersand_ampersand(text, pos))
        .or(lex_ampersand_equals(text, pos))
        .or(lex_ampersand(text, pos))
        .or(lex_pipe_pipe(text, pos))
        .or(lex_pipe_equals(text, pos))
        .or(lex_pipe(text, pos))
        .or(lex_exclamation(text, pos))
        .or(lex_question(text, pos))
        .or(lex_tilde(text, pos))
        .or(lex_ltlt_equals(text, pos))
        .or(lex_ltlt(text, pos))
        .or(lex_gtgt_equals(text, pos))
        .or(lex_gtgt(text, pos))
        .or(lex_le(text, pos))
        .or(lex_ge(text, pos))
        .or(lex_lt(text, pos))
        .or(lex_gt(text, pos))
        .ok_or(LexError::InvalidToken { pos })
}

fn lex_keyword(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &KEYWORD_REGEX)?;
    Some((Token::Keyword(token), pos))
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

fn lex_char(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &CHAR_REGEX)?;
    let token = token.strip_prefix("'").unwrap().strip_suffix("'").unwrap();
    Some((Token::Char(token.parse().ok()?), pos))
}

fn lex_whole(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &WHOLE_REGEX)?;
    Some((Token::Whole(token.parse().ok()?), pos))
}

fn lex_decimal(text: &str, pos: usize) -> Option<(Token, usize)> {
    let (token, pos) = lex_with_pattern(text, pos, &FLOAT_REGEX)?;
    Some((Token::Decimal(token.parse().ok()?), pos))
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

fn lex_dot_dot_dot(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::DotDotDot, lex_with_prefix(text, pos, "...")?))
}

fn lex_dot(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Dot, lex_with_prefix(text, pos, ".")?))
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

fn lex_plus_plus(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::PlusPlus, lex_with_prefix(text, pos, "++")?))
}

fn lex_hyphen(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Hyphen, lex_with_prefix(text, pos, "-")?))
}

fn lex_hyphen_hyphen(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::HyphenHyphen, lex_with_prefix(text, pos, "--")?))
}

fn lex_asterisk(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Asterisk, lex_with_prefix(text, pos, "*")?))
}

fn lex_slash(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Slash, lex_with_prefix(text, pos, "/")?))
}

fn lex_percent(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Percent, lex_with_prefix(text, pos, "%")?))
}

fn lex_caret(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Caret, lex_with_prefix(text, pos, "^")?))
}

fn lex_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Equals, lex_with_prefix(text, pos, "=")?))
}

fn lex_asterisk_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::AsteriskEquals, lex_with_prefix(text, pos, "*=")?))
}

fn lex_slash_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::SlashEquals, lex_with_prefix(text, pos, "/=")?))
}

fn lex_percent_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::PercentEquals, lex_with_prefix(text, pos, "%=")?))
}

fn lex_plus_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::PlusEquals, lex_with_prefix(text, pos, "+=")?))
}

fn lex_hyphen_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::HyphenEquals, lex_with_prefix(text, pos, "-=")?))
}

fn lex_ltlt_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::LTLTEquals, lex_with_prefix(text, pos, "<<=")?))
}

fn lex_gtgt_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::GTGTEquals, lex_with_prefix(text, pos, ">>=")?))
}

fn lex_ampersand_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::AmpersandEquals, lex_with_prefix(text, pos, "&=")?))
}

fn lex_caret_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::CaretEquals, lex_with_prefix(text, pos, "^=")?))
}

fn lex_pipe_equals(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::PipeEquals, lex_with_prefix(text, pos, "|=")?))
}

fn lex_ampersand(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Ampersand, lex_with_prefix(text, pos, "&")?))
}

fn lex_ampersand_ampersand(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::AmpersandAmpersand, lex_with_prefix(text, pos, "&&")?))
}

fn lex_pipe(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Pipe, lex_with_prefix(text, pos, "|")?))
}

fn lex_pipe_pipe(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::PipePipe, lex_with_prefix(text, pos, "||")?))
}

fn lex_exclamation(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Exclamation, lex_with_prefix(text, pos, "!")?))
}

fn lex_question(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Question, lex_with_prefix(text, pos, "?")?))
}

fn lex_tilde(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::Tilde, lex_with_prefix(text, pos, "~")?))
}

fn lex_lt(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::LT, lex_with_prefix(text, pos, "<")?))
}

fn lex_gt(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::GT, lex_with_prefix(text, pos, ">")?))
}

fn lex_le(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::LE, lex_with_prefix(text, pos, "<=")?))
}

fn lex_ge(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::GE, lex_with_prefix(text, pos, ">=")?))
}

fn lex_eq(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::EQ, lex_with_prefix(text, pos, "==")?))
}

fn lex_ne(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::NE, lex_with_prefix(text, pos, "!=")?))
}

fn lex_ltlt(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::LTLT, lex_with_prefix(text, pos, "<<")?))
}

fn lex_gtgt(text: &str, pos: usize) -> Option<(Token, usize)> {
    Some((Token::GTGT, lex_with_prefix(text, pos, ">>")?))
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
                "put caret ^ to match the text from the `pos` (text is sliced to start from pos)"
            );
            return Some((m.as_str(), pos + m.end()));
        }
    }

    None
}

#[cfg(test)]
mod tests {

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_all() {
        let src = r#"

        auto break case char const continue default do double 
        else enum extern float for goto if int long register 
        return short signed sizeof static struct switch typedef 
        union unsigned void volatile while

        idEnt_123"🦀"'c'123 123. .123 123.123 true false NULL{}[]()
        ....,:;+++---*/%^!===*=/=%=+=-=&=^=|==&&&|||!?~<<<<=<=<>>=>>>=>
        "#;

        use Token::*;

        match lex(src) {
            Ok(tokens) => assert_eq!(
                vec![
                    Keyword("auto"),
                    Keyword("break"),
                    Keyword("case"),
                    Keyword("char"),
                    Keyword("const"),
                    Keyword("continue"),
                    Keyword("default"),
                    Keyword("do"),
                    Keyword("double"),
                    Keyword("else"),
                    Keyword("enum"),
                    Keyword("extern"),
                    Keyword("float"),
                    Keyword("for"),
                    Keyword("goto"),
                    Keyword("if"),
                    Keyword("int"),
                    Keyword("long"),
                    Keyword("register"),
                    Keyword("return"),
                    Keyword("short"),
                    Keyword("signed"),
                    Keyword("sizeof"),
                    Keyword("static"),
                    Keyword("struct"),
                    Keyword("switch"),
                    Keyword("typedef"),
                    Keyword("union"),
                    Keyword("unsigned"),
                    Keyword("void"),
                    Keyword("volatile"),
                    Keyword("while"),
                    Ident("idEnt_123"),
                    String("🦀"),
                    Char('c'),
                    Whole(123),
                    Decimal(123.0),
                    Decimal(0.123),
                    Decimal(123.123),
                    Bool(true),
                    Bool(false),
                    Null,
                    LCurly,
                    RCurly,
                    LSquare,
                    RSquare,
                    LParen,
                    RParen,
                    DotDotDot,
                    Dot,
                    Comma,
                    Colon,
                    SemiColon,
                    PlusPlus,
                    Plus,
                    HyphenHyphen,
                    Hyphen,
                    Asterisk,
                    Slash,
                    Percent,
                    Caret,
                    NE,
                    EQ,
                    AsteriskEquals,
                    SlashEquals,
                    PercentEquals,
                    PlusEquals,
                    HyphenEquals,
                    AmpersandEquals,
                    CaretEquals,
                    PipeEquals,
                    Equals,
                    AmpersandAmpersand,
                    Ampersand,
                    PipePipe,
                    Pipe,
                    Exclamation,
                    Question,
                    Tilde,
                    LTLT,
                    LTLTEquals,
                    LE,
                    LT,
                    GTGTEquals,
                    GTGT,
                    GE,
                    GT
                ],
                tokens
            ),

            Err(LexError::InvalidToken { pos }) => assert!(false, "{}", &src[pos..]),
        }
    }

    #[test]
    fn test_c() {
        let src = r#"

        foreach

        int main() {
            char name[] = "zahash";
            int age = 42;
        }

        "#;
        match lex(src) {
            Ok(tokens) => println!("{:#?}", tokens),
            Err(LexError::InvalidToken { pos }) => assert!(false, "{}", &src[pos..]),
        }
    }
}
