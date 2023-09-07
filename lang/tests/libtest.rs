use lang::*;

#[test]
fn test_lexer() {
    let text = "  a  +  (b  * 10  )  ";
    let a = lex(text);
    println!("{}", text);
    println!("{:?}", a);
}

#[test]
fn operator_precedence() {
    // a + b * c
    assert(
        Tokens(vec![
            Token::Ident("a"),
            Token::Plus,
            Token::Ident("b"),
            Token::Asterisk,
            Token::Ident("c"),
        ]),
        "(a+(b*c))",
    );

    // a * b + c
    assert(
        Tokens(vec![
            Token::Ident("a"),
            Token::Asterisk,
            Token::Ident("b"),
            Token::Plus,
            Token::Ident("c"),
        ]),
        "((a*b)+c)",
    );
}

#[ignore]
#[test]
fn left_assoc() {
    assert(
        Tokens(vec![
            Token::Ident("a"),
            Token::Slash,
            Token::Ident("b"),
            Token::Slash,
            Token::Ident("c"),
        ]),
        "((a/b)/c)",
    );
}

fn assert(tokens: Tokens, expected_expr: &str) {
    match parse(&tokens.0) {
        Ok(expr) => assert_eq!(expected_expr, format!("{}", expr)),
        Err(e) => assert!(false, "{:?}", e),
    }
}
