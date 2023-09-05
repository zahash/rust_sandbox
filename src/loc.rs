use std::error::Error;

use clap::Parser;

/// Count lines of code
#[derive(Parser)]
struct CLI {
    filepaths: Vec<String>,
}

struct LocError {
    filepath: String,
    error: Box<dyn Error>,
}

struct Loc {
    filepath: String,
    loc: usize,
}

pub fn run() {
    let args = CLI::parse();

    let filepaths = match args.filepaths.is_empty() {
        false => args.filepaths,
        true => std::io::stdin()
            .lines()
            .filter_map(|line| line.ok())
            .collect(),
    };

    let mut data = vec![];
    let mut errors = vec![];

    for fpath in filepaths {
        match std::fs::read_to_string(&fpath) {
            Ok(contents) => data.push(Loc {
                filepath: fpath,
                loc: contents
                    .lines()
                    .into_iter()
                    .map(|line| line.trim())
                    .filter(|line| !line.is_empty())
                    .count(),
            }),
            Err(e) => errors.push(LocError {
                filepath: fpath,
                error: Box::new(e),
            }),
        };
    }

    for LocError { filepath, error } in errors {
        eprintln!("** {} -- {:?}", filepath, error);
    }
    for Loc { filepath, loc } in data {
        println!("{} {}", loc, filepath);
    }
}
