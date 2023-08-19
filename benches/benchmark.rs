use bitvec::vec::BitVec;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sandbox::game_of_life::*;

const GRID_SIZE: usize = 2000;

fn bitvec_benchmark(c: &mut Criterion) {
    let start = {
        let mut start = BitVec::with_capacity(GRID_SIZE * GRID_SIZE);
        for _ in 0..GRID_SIZE * GRID_SIZE {
            start.push(false);
        }
        start
    };

    let mut game: game_of_life::Game<GRID_SIZE> = game_of_life::Game::new(start);

    let g = game.grid().as_raw_slice();
    let s = std::mem::size_of_val(g);

    println!("\n\n bitvec size {} \n\n", s);

    c.bench_function("bitvec", |b| {
        b.iter(|| {
            game.update();
            black_box(&game);
        });
    });
}

fn vec_benchmark(c: &mut Criterion) {
    let start = {
        let mut start = Vec::with_capacity(GRID_SIZE);
        for _ in 0..GRID_SIZE {
            let mut row = Vec::with_capacity(GRID_SIZE);
            for _ in 0..GRID_SIZE {
                row.push(false);
            }
            start.push(row);
        }
        start
    };

    let mut game: game_of_life_vec::Game<GRID_SIZE> = game_of_life_vec::Game::new(start);

    // let g = game.grid().as_slice();
    // let g0 = g[0].as_slice();

    // let s = std::mem::size_of_val(g0);

    // println!("\n\n vec size {} \n\n", s);

    c.bench_function("vec", |b| {
        b.iter(|| {
            game.update();
            black_box(&game);
        });
    });
}

fn stack_benchmark(c: &mut Criterion) {
    let mut game = game_of_life_stack::Game::new([[true; GRID_SIZE]; GRID_SIZE]);

    let g = game.grid();
    let s = std::mem::size_of_val(g);

    println!("\n\n stack size {} \n\n", s);

    c.bench_function("stack", |b| {
        b.iter(|| {
            game.par_update();
            black_box(&game);
        });
    });
}

criterion_group!(benches, bitvec_benchmark, vec_benchmark, stack_benchmark);
// criterion_group!(benches, bitvec_benchmark, vec_benchmark);
criterion_main!(benches);
