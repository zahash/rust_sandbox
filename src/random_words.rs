use clap::Parser;
use rand::seq::SliceRandom;
use std::{fs::File, io::BufReader, path::Path};

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

    let all_words = read_words_json(args.filepath);
    let chosen_words: Vec<_> = all_words
        .choose_multiple(&mut rand::thread_rng(), args.amount)
        .collect();

    for word in chosen_words {
        println!("{}", word);
    }
}

pub fn read_words_json<P>(path: P) -> Vec<String>
where
    P: AsRef<Path>,
{
    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);
    serde_json::from_reader::<BufReader<File>, Vec<String>>(reader).unwrap()
}
