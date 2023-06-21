#![allow(dead_code, unused_variables)]

mod fraction;
mod graph;
mod observe;
mod sat;

use graph::AdjacencyList;
use std::collections::HashMap;

use crate::graph::algorithm::breadth_first_search;

fn main() {
    let graph = AdjacencyList::new(HashMap::from([
        ("ATL", vec!["BOS", "DFW", "MOB"]),
        ("BOS", vec!["ATL", "DFW"]),
        ("MOB", vec!["ATL"]),
        ("AUS", vec!["DFW", "HOU", "SAT"]),
        ("HOU", vec!["AUS", "DFW", "SAT"]),
        ("SAT", vec!["AUS", "HOU"]),
        ("LAX", vec!["DFW", "SFO"]),
        ("LIT", vec!["DFW"]),
        ("MSY", vec!["DFW"]),
        ("OKC", vec!["DFW"]),
        ("SHV", vec!["DFW"]),
        ("SFO", vec!["DFW", "LA"]),
        (
            "DFW",
            vec![
                "ATL", "AUS", "BOS", "HOU", "LAX", "LIT", "MSY", "OKC", "SHV", "SFO",
            ],
        ),
    ]));

    if let Some(path) = breadth_first_search(&graph, &"DFW", |airport| airport == &"LA") {
        println!("{:?}", Vec::<&str>::from(path));
    }
}
