use clap::Parser;
use rand::seq::SliceRandom;

use crate::util::read_json;

#[derive(Parser, Debug)]
struct RandomWordGeneratorArgs {
    /// words json filepath
    #[arg(short, long)]
    filepath: String,

    /// number of random words to generate
    #[arg(short, long, default_value_t = 1)]
    amount: usize,
}

pub fn print_random_words() {
    let args = RandomWordGeneratorArgs::parse();

    let all_words: Vec<String> = read_json(args.filepath);
    let chosen_words: Vec<_> = all_words
        .choose_multiple(&mut rand::thread_rng(), args.amount)
        .collect();

    for word in chosen_words {
        println!("{}", word);
    }
}
