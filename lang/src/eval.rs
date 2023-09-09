use std::collections::HashMap;

use crate::lex::*;
use crate::parse::*;

#[derive(Debug)]
pub struct State {
    variables: HashMap<String, f64>,
}

#[derive(Debug)]
pub enum EvaluatorError<'token> {
    LexError(LexError<'token>),
    ParseError(ParseError),
}

impl<'token> From<LexError<'token>> for EvaluatorError<'token> {
    fn from(value: LexError<'token>) -> Self {
        EvaluatorError::LexError(value)
    }
}

impl From<ParseError> for EvaluatorError<'_> {
    fn from(value: ParseError) -> Self {
        EvaluatorError::ParseError(value)
    }
}

impl State {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn value_of(&self, var: &str) -> Option<f64> {
        self.variables.get(var).cloned()
    }

    fn set_var(&mut self, var: &str, val: f64) {
        self.variables.insert(var.to_string(), val);
    }
}

pub trait Eval {
    fn eval(&self, state: &mut State) -> f64;
}

pub fn eval<'token>(text: &'token str, state: &mut State) -> Result<f64, EvaluatorError<'token>> {
    let tokens = lex(text)?;
    let expr = parse(&tokens)?;
    Ok(expr.eval(state))
}

impl<'ident> Eval for AssignmentExpr<'ident> {
    fn eval(&self, state: &mut State) -> f64 {
        match self {
            AssignmentExpr::Assign(a, b) => {
                let val = b.eval(state);
                state.set_var(a.0, val);
                val
            }
            AssignmentExpr::AdditiveExpr(a) => a.eval(state),
        }
    }
}

impl<'ident> Eval for AdditiveExpr<'ident> {
    fn eval(&self, state: &mut State) -> f64 {
        match self {
            AdditiveExpr::Add(a, b) => a.eval(state) + b.eval(state),
            AdditiveExpr::Sub(a, b) => a.eval(state) - b.eval(state),
            AdditiveExpr::MultiplicativeExpr(a) => a.eval(state),
        }
    }
}

impl<'ident> Eval for MultiplicativeExpr<'ident> {
    fn eval(&self, state: &mut State) -> f64 {
        match self {
            MultiplicativeExpr::Mul(a, b) => a.eval(state) * b.eval(state),
            MultiplicativeExpr::Div(a, b) => a.eval(state) / b.eval(state),
            MultiplicativeExpr::ExponentialExpr(a) => a.eval(state),
        }
    }
}

impl<'ident> Eval for ExponentialExpr<'ident> {
    fn eval(&self, state: &mut State) -> f64 {
        match self {
            ExponentialExpr::Pow(a, b) => a.eval(state).powf(b.eval(state)),
            ExponentialExpr::Primary(a) => a.eval(state),
        }
    }
}

impl<'ident> Eval for Primary<'ident> {
    fn eval(&self, state: &mut State) -> f64 {
        match self {
            Primary::Parens(expr) => expr.eval(state),
            Primary::Num(n) => *n,
            Primary::Ident(ident) => match state.value_of(ident) {
                Some(val) => val,
                None => panic!("** variable '{}' not found", ident),
            },
            Primary::UnarySub(n) => -n.eval(state),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_eval() {
        let mut state = State::new();

        let result = eval("a=3-4/7", &mut state).unwrap();
        println!("{}", result);
        println!("{:?}", state);
    }
}
