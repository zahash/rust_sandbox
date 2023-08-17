#![allow(dead_code, unused_variables)]

mod ai;
mod api;
mod chatgpt;
mod fraction;
mod game_of_life;
mod graph;
mod marker_invariants;
mod nes;
mod random_words;
mod sat;
mod static_assert;
mod sura;
mod symrs;
mod threadpool;
mod ui;
mod util;
mod validation;
mod vultus;
mod yavd;

#[tokio::main]
async fn main() {
    game_of_life::run();
}
