use std::io::Write;

use crate::eval::*;

pub fn run() {
    println!("** PadLock **");

    let mut state = State::new();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let text = ask();
        match eval(&text, &mut state) {
            Ok(_) => {},
            Err(e) => eprintln!("*** {:?}", e),
        }
    }
}

fn ask() -> String {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    input
}
