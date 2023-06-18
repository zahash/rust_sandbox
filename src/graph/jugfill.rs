use crate::graph::Graph;
use std::vec;

use super::Planning;

const CONTAINER_SIZE: usize = 3;
type Containers = [i32; CONTAINER_SIZE];

pub struct JugFill {
    pub start: Containers,
    pub capacity: Containers,
    pub target: Containers,
}

impl Graph for JugFill {
    type Node = Containers;
    type Edge = (Containers, Containers);

    fn out_nodes(&self, node: &Containers) -> Vec<Containers> {
        let water = node;

        let pour = |from: usize, to: usize| -> Option<Containers> {
            let leftover_capacity = std::cmp::max(0, self.capacity[to] - water[to]);
            let amount_to_pour = std::cmp::min(leftover_capacity, water[from]);

            match amount_to_pour {
                0 => Option::None,
                _ => {
                    let mut new_water = *water;
                    new_water[from] -= amount_to_pour;
                    new_water[to] += amount_to_pour;
                    Option::Some(new_water)
                }
            }
        };

        let mut res = vec![];

        for i in 0..CONTAINER_SIZE {
            for j in 0..CONTAINER_SIZE {
                if i == j {
                    continue;
                }

                if let Some(new_water) = pour(i, j) {
                    res.push(new_water);
                }
            }
        }

        res
    }

    fn in_nodes(&self, node: &Self::Node) -> Vec<Self::Node> {
        self.out_nodes(node)
    }

    fn out_edges(&self, node: &Self::Node) -> Vec<Self::Edge> {
        let mut edges = vec![];

        for next_node in self.out_nodes(node) {
            edges.push((*node, next_node));
        }

        edges
    }

    fn in_edges(&self, node: &Self::Node) -> Vec<Self::Edge> {
        let mut edges = vec![];

        for next_node in self.in_nodes(node) {
            edges.push((next_node, *node));
        }

        edges
    }

    fn edges(&self, from: &Self::Node, to: &Self::Node) -> Vec<Self::Edge> {
        vec![(*from, *to)]
    }

    fn nodes(&self, edge: &Self::Edge) -> (Self::Node, Self::Node) {
        *edge
    }

    fn weight(&self, _edge: &Self::Edge) -> f64 {
        1.
    }
}

impl Planning for JugFill {
    type Solution = Vec<Containers>;

    fn solve(&self) -> Option<Vec<Containers>> {
        self.breadth_first_search(&self.start, |node| node == &self.target)
            .map(|path| path.into_list())
    }
}
