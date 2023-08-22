use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sandbox::game_of_life::*;

const GRID_SIZE: usize = 10_000;

fn stack_benchmark(c: &mut Criterion) {
    let start = [[true; GRID_SIZE]; GRID_SIZE];
    let mut game = game_of_life::Game::new(start);

    c.bench_function("stack", |b| {
        b.iter(|| {
            game.update();
            black_box(&game);
        });
    });
}

criterion_group!(benches, stack_benchmark);
criterion_main!(benches);
