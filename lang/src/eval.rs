use crate::lex::*;
use crate::parse::*;

pub fn eval(text: &str) -> f64 {
    let tokens = lex(text).expect("** lex error");
    let expr = parse(&tokens).expect("** parse error");
    expr.eval()
}

pub trait Eval {
    fn eval(&self) -> f64;
}

impl<'ident> Eval for AdditiveExpr<'ident> {
    fn eval(&self) -> f64 {
        match self {
            AdditiveExpr::Add(a, b) => a.eval() + b.eval(),
            AdditiveExpr::Sub(a, b) => a.eval() - b.eval(),
            AdditiveExpr::MultiplicativeExpr(a) => a.eval(),
        }
    }
}

impl<'ident> Eval for MultiplicativeExpr<'ident> {
    fn eval(&self) -> f64 {
        match self {
            MultiplicativeExpr::Mul(a, b) => a.eval() * b.eval(),
            MultiplicativeExpr::Div(a, b) => a.eval() / b.eval(),
            MultiplicativeExpr::ExponentialExpr(a) => a.eval(),
        }
    }
}

impl<'ident> Eval for ExponentialExpr<'ident> {
    fn eval(&self) -> f64 {
        match self {
            ExponentialExpr::Pow(a, b) => a.eval().powf(b.eval()),
            ExponentialExpr::Primary(a) => a.eval(),
        }
    }
}

impl<'ident> Eval for Primary<'ident> {
    fn eval(&self) -> f64 {
        match self {
            Primary::Parens(expr) => expr.eval(),
            Primary::Num(n) => *n as f64,
            Primary::Ident(_ident) => unimplemented!("variables are not yet implemented"),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_eval() {
        println!("{}", eval("8/a^2"));
    }
}
