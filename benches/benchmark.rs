use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sandbox::game_of_life::*;

const GRID_SIZE: usize = 10_000;

fn vec_benchmark(c: &mut Criterion) {
    let start = vec![vec![true; GRID_SIZE]; GRID_SIZE];

    let mut game: game_of_life_vec::Game<GRID_SIZE, GRID_SIZE> = game_of_life_vec::Game::new(start);

    c.bench_function("vec", |b| {
        b.iter(|| {
            game.update();
            black_box(&game);
        });
    });
}

fn vec_1d_benchmark(c: &mut Criterion) {
    let mut game: game_of_life_vec_1d::Game<GRID_SIZE, GRID_SIZE> =
        game_of_life_vec_1d::Game::empty();

    for r in 0..GRID_SIZE {
        for c in 0..GRID_SIZE {
            game.revive(r, c);
        }
    }

    c.bench_function("vec_1d", |b| {
        b.iter(|| {
            game.update();
            black_box(&game);
        });
    });
}

criterion_group!(benches, vec_benchmark, vec_1d_benchmark);
criterion_main!(benches);
