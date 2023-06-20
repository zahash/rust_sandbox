use crate::graph::Graph;
use std::{collections::HashMap, hash::Hash};

pub struct AdjacencyList<Node> {
    graph: HashMap<Node, Vec<Node>>,
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
        todo!()
    }

    fn out_edges(&self, node: &Self::V) -> Vec<Self::E> {
        todo!()
    }

    fn in_edges(&self, node: &Self::V) -> Vec<Self::E> {
        todo!()
    }

    fn edges(&self, from: &Self::V, to: &Self::V) -> Vec<Self::E> {
        todo!()
    }

    fn vertices(&self, edge: &Self::E) -> (Self::V, Self::V) {
        todo!()
    }
}
