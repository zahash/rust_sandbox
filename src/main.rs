#![allow(dead_code, unused_variables)]

mod fraction;
mod graph;
mod observe;
mod sat;

use graph::{BinaryGrid, Graph};

fn main() {
    // let grid = BinaryGrid::grid_to_bool(vec![
    //     vec![1, 1, 0, 0, 0],
    //     vec![1, 1, 0, 0, 0],
    //     vec![0, 0, 1, 0, 0],
    //     vec![0, 0, 0, 1, 1],
    // ]);

    let grid = BinaryGrid::grid_to_bool(vec![
        vec![1, 1, 1, 1, 0],
        vec![1, 1, 0, 1, 0],
        vec![1, 1, 0, 0, 0],
        vec![0, 0, 0, 0, 0],
    ]);

    let graph = BinaryGrid::new_hv(grid);

    for path in graph.breadth_first_traverse(&(0, 0)) {
        println!("{:?}", path.into_list());
    }
}
