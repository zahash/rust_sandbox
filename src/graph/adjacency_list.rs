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

impl<Node: Clone + Hash + Eq> Graph for AdjacencyList<Node> {
    type V = Node;
    type E = (Node, Node);

    fn out_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        match self.graph.get(node) {
            Some(adj) => adj.clone(),
            None => vec![],
        }
    }

    fn in_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        self.graph
            .iter()
            .filter(|(k, v)| v.contains(node))
            .map(|(k, v)| k.clone())
            .collect()
    }

    fn out_edges(&self, node: &Self::V) -> Vec<Self::E> {
        let mut edges = vec![];

        for next_node in self.out_vertices(node) {
            edges.push((node.clone(), next_node));
        }

        edges
    }

    fn in_edges(&self, node: &Self::V) -> Vec<Self::E> {
        let mut edges = vec![];

        for next_node in self.in_vertices(node) {
            edges.push((next_node, node.clone()));
        }

        edges
    }

    fn edges(&self, from: &Self::V, to: &Self::V) -> Vec<Self::E> {
        vec![(from.clone(), to.clone())]
    }

    fn vertices(&self, edge: &Self::E) -> (Self::V, Self::V) {
        edge.clone()
    }
}
