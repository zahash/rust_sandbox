#![allow(dead_code, unused_variables)]

mod api;
mod fraction;
mod graph;
mod nes;
mod random_words;
mod sat;
mod symrs;
mod threadpool;
mod ui;
mod util;

use reactivate::Reactive;

#[tokio::main]
async fn main() {
    let r = Reactive::new(0);
    r.update_unchecked(|_| 20);

    r.update_inplace_unchecked(|_| {});
}
