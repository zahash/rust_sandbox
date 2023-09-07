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
    assert("a+b*c", "(a+(b*c))");
    // a * b + c
    assert("a*b+c", "((a*b)+c)");
}

#[ignore]
#[test]
fn left_assoc() {
    assert("a/b/c", "((a/b)/c)");
}

fn assert(text: &str, expected_expr: &str) {
    match lex(text) {
        Ok(tokens) => match parse(&tokens.0) {
            Ok(expr) => assert_eq!(expected_expr, format!("{}", expr)),
            Err(e) => assert!(false, "{:?}", e),
        },
        Err(e) => assert!(false, "{:?}", e),
    }
}
