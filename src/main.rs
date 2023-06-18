#![allow(dead_code, unused_variables)]

mod fraction;
mod graph;
mod observe;
mod sat;

use sat::solve;

use crate::sat::{check, read_json_expr};

fn main() {
    // let expr = vec![vec![-1, 2], vec![-1, 3]];

    // https://www.borealisai.com/research-blogs/tutorial-10-sat-solvers-ii-algorithms/#:~:text=Worked%20example%3A%C2%A0This%20process%20is%20easiest%20to%20understand%20using%20a%20concrete%20example.%C2%A0Consider%20the%20following%202%2DSAT%20problem%20in%20four%20variables%3A
    // let expr = vec![
    //     vec![1, -2],
    //     vec![-1, -3],
    //     vec![2, 3],
    //     vec![-2, 4],
    //     vec![3, -4],
    // ];

    // let expr = vec![
    //     vec![1, 2],
    //     vec![1, -2, -3, 4],
    //     vec![1, -3, -4],
    //     vec![-1, 2, -3],
    //     vec![-1, 2, -4],
    //     vec![-1, 3, 4],
    //     vec![-2, 3],
    // ];

    let expr = read_json_expr(
        "/home/zahash/storage/sat/CBS_k3_n100_m449_b90_json/CBS_k3_n100_m449_b90_1.json",
    );
    let res = solve(&expr);

    println!("expr = {:?}", expr);
    println!("res  = {:?}", res);

    if let Some(sol) = res {
        println!("check = {}", check(&expr, &sol));
    }
}
