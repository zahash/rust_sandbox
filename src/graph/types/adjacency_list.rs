use crate::graph::Graph;
use std::{collections::HashMap, hash::Hash};

pub struct AdjacencyList<Node> {
    graph: HashMap<Node, Vec<Node>>,
}

impl<Node> AdjacencyList<Node> {
    pub fn new(graph: HashMap<Node, Vec<Node>>) -> Self {
        Self { graph }
    }
}

impl<'graph, Node: Hash + Eq> Graph for &'graph AdjacencyList<Node> {
    type V = &'graph Node;
    type E = (&'graph Node, &'graph Node);

    fn out_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        match self.graph.get(node) {
            Some(adj) => adj.iter().collect(),
            None => vec![],
        }
    }

    fn in_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        self.graph
            .iter()
            .filter(|(k, v)| v.contains(node))
            .map(|(k, v)| k)
            .collect()
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
        self.out_edges(from)
            .iter()
            .filter(|(_, t)| t == to)
            .map(|a| *a)
            .collect::<Vec<Self::E>>()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::algorithm::breadth_first_search;

    #[test]
    fn test() {
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

        if let Some(path) = breadth_first_search(&&graph, &&"DFW", |airport| airport == &&"LA") {
            println!("{:?}", Vec::<&&str>::from(path));
        }
    }
}
