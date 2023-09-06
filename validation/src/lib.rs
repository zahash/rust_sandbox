use std::ops::AddAssign;

pub struct Validator<T, E> {
    rules: Vec<Box<dyn Fn(&T) -> Result<(), E>>>,
}

impl<T, E> Validator<T, E> {
    pub const fn new() -> Self {
        Validator { rules: vec![] }
    }

    pub fn add_rule(&mut self, rule: impl Fn(&T) -> Result<(), E> + 'static) {
        self.rules.push(Box::new(rule));
    }

    pub fn validate(&self, data: &T) -> Result<(), E> {
        for rule in &self.rules {
            rule(data)?
        }
        Ok(())
    }
}

impl<T, E, F> AddAssign<F> for Validator<T, E>
where
    F: Fn(&T) -> Result<(), E> + 'static,
{
    fn add_assign(&mut self, rhs: F) {
        self.add_rule(rhs);
    }
}

pub fn run() {
    #[derive(Debug)]
    enum UsernameErr {
        TooLong,
        TooShort,
    }

    let mut username_validator = Validator::<String, UsernameErr>::new();

    username_validator += |username| match username.len() >= 5 {
        true => Ok(()),
        false => Err(UsernameErr::TooShort),
    };

    username_validator += |username| match username.len() <= 20 {
        true => Ok(()),
        false => Err(UsernameErr::TooLong),
    };

    let username = String::from("joh");

    match username_validator.validate(&username) {
        Ok(_) => println!("Username is valid."),
        Err(e) => println!("Username is invalid : {:?}", e),
    }
}
