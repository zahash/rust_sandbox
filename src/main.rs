use sandbox::*;

use bitvec::prelude::*;

#[tokio::main]
async fn main() {
    let arr = bitarr![usize, Lsb0; 0; 80];

    game_of_life::run();
}
