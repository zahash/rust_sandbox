use crate::graph::{algorithm::breadth_first_search, Graph, Planning};
use std::vec;

pub struct JugFill<const N: usize> {
    pub start: [i32; N],
    pub capacity: [i32; N],
    pub target: [i32; N],
}

impl<const N: usize> Graph for JugFill<N> {
    type V = [i32; N];
    type E = (Self::V, Self::V);

    fn out_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        let water = node;

        let pour = |from: usize, to: usize| -> Option<[i32; N]> {
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

        for i in 0..N {
            for j in 0..N {
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

    fn in_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        self.out_vertices(node)
    }

    fn out_edges(&self, node: &Self::V) -> Vec<Self::E> {
        let mut edges = vec![];

        for next_node in self.out_vertices(node) {
            edges.push((*node, next_node));
        }

        edges
    }

    fn in_edges(&self, node: &Self::V) -> Vec<Self::E> {
        let mut edges = vec![];

        for next_node in self.in_vertices(node) {
            edges.push((next_node, *node));
        }

        edges
    }

    fn edges(&self, from: &Self::V, to: &Self::V) -> Vec<Self::E> {
        match self.out_vertices(from).contains(to) {
            true => vec![(*from, *to)],
            false => vec![],
        }
    }

    fn vertices(&self, edge: &Self::E) -> (Self::V, Self::V) {
        *edge
    }

    fn all_vertices(&self) -> Vec<Self::V> {
        todo!()
    }

    fn all_edges(&self) -> Vec<Self::E> {
        todo!()
    }
}

impl<const N: usize> Planning for JugFill<N> {
    type Solution = Vec<[i32; N]>;

    fn solve(&self) -> Option<Vec<[i32; N]>> {
        breadth_first_search(self, &self.start, |node| node == &self.target).map(|path| path.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let jugfill = JugFill {
            start: [8, 0, 0],
            capacity: [8, 5, 3],
            target: [4, 4, 0],
        };

        let sol = jugfill.solve();

        if let Some(path) = jugfill.solve() {
            println!("{:?}", path);
        }
    }
}
