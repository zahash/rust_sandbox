use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sandbox::game_of_life::Game;

const GRID_SIZE: usize = 10_000;

fn sequential_benchmark(c: &mut Criterion) {
    let mut game = Game::new([[true; GRID_SIZE]; GRID_SIZE]);

    c.bench_function("sequential_update", |b| {
        b.iter(|| {
            game.update();
            black_box(&game);
        });
    });
}

fn parallel_benchmark(c: &mut Criterion) {
    let mut game = Game::new([[true; GRID_SIZE]; GRID_SIZE]);

    c.bench_function("parallel_update", |b| {
        b.iter(|| {
            game.par_update();
            black_box(&game);
        });
    });
}

criterion_group!(benches, sequential_benchmark, parallel_benchmark);
criterion_main!(benches);
