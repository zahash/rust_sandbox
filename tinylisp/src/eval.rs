use std::collections::HashMap;

use crate::parse::Expr;

pub struct State<'text> {
    vars: HashMap<&'text str, Expr<'text>>,
    fns: HashMap<&'text str, Box<dyn Fn(&Expr<'text>) -> Expr<'text>>>,
}

impl<'text> State<'text> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            fns: HashMap::new(),
        }
    }

    fn exec_fn(&self, name: &'text str, arg: &Expr<'text>) -> Option<Expr<'text>> {
        self.fns.get(name).map(|f| f(arg))
    }

    fn value_of(&self, var: &'text str) -> Option<Expr<'text>> {
        self.vars.get(var).cloned()
    }

    fn define_fn(&mut self, name: &'text str, f: impl Fn(&Expr<'text>) -> Expr<'text> + 'static) {
        self.fns.insert(name, Box::new(f));
    }

    fn set_var(&mut self, name: &'text str, val: Expr<'text>) {
        self.vars.insert(name, val);
    }
}

#[derive(Debug)]
pub enum EvalError<'text> {
    IdentNotFound(&'text str),
}

impl<'text> Expr<'text> {
    pub fn eval(self, state: &mut State<'text>) -> Result<Self, EvalError<'text>> {
        match self {
            Expr::Ident(ident) => state.value_of(ident).ok_or(EvalError::IdentNotFound(ident)),
            Expr::List(list) => match list.as_slice() {
                [Expr::Ident("lambda"), ] => todo!(),

                [Expr::Ident("define"), Expr::Ident(ident), val] => {
                    state.set_var(ident, val.clone());
                    Ok(val.clone())
                },

                [Expr::Ident(ident)] => {
                    state.value_of(ident).ok_or(EvalError::IdentNotFound(ident))
                }
                [Expr::Ident(ident), second] => match state.exec_fn(ident, second) {
                    Some(res) => Ok(res),
                    None => match state.value_of(ident) {
                        Some(val) => Ok(Expr::List(vec![val, second.clone()])),
                        None => Err(EvalError::IdentNotFound(ident)),
                    },
                },
                _ => todo!(),
            },
            _ => Ok(self),
        }
    }
}
