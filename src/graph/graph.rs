use crate::graph::Path;
use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
};

pub trait Graph {
    type Node: Clone + Eq + Hash;
    type Edge;

    fn out_nodes(&self, node: &Self::Node) -> Vec<Self::Node>;

    fn in_nodes(&self, node: &Self::Node) -> Vec<Self::Node>;

    fn out_edges(&self, node: &Self::Node) -> Vec<Self::Edge>;

    fn in_edges(&self, node: &Self::Node) -> Vec<Self::Edge>;

    fn edges(&self, from: &Self::Node, to: &Self::Node) -> Vec<Self::Edge>;

    fn nodes(&self, edge: &Self::Edge) -> (Self::Node, Self::Node);

    fn weight(&self, edge: &Self::Edge) -> f64;

    fn breadth_first_traverse(&self, start: &Self::Node) -> Vec<Path<Self::Node>> {
        let mut frontier: VecDeque<Path<Self::Node>> = VecDeque::new();
        frontier.push_back(Path {
            node: start.clone(),
            parent: Box::default(),
        });

        let mut seen: HashSet<Self::Node> = HashSet::new();
        let mut paths: Vec<Path<Self::Node>> = vec![];

        while let Some(path) = frontier.pop_front() {
            if seen.contains(&path.node) {
                continue;
            }
            seen.insert(path.node.clone());

            paths.push(path.clone());

            for edge in self.out_edges(&path.node) {
                let (_, next_node) = self.nodes(&edge);

                frontier.push_back(Path {
                    node: next_node,
                    parent: Box::new(Some(path.clone())),
                })
            }
        }

        paths
    }

    fn depth_first_traverse(&self, start: &Self::Node) -> Vec<Path<Self::Node>> {
        let mut frontier: Vec<Path<Self::Node>> = Vec::new();
        frontier.push(Path {
            node: start.clone(),
            parent: Box::default(),
        });

        let mut seen: HashSet<Self::Node> = HashSet::new();
        let mut paths: Vec<Path<Self::Node>> = vec![];

        while let Some(path) = frontier.pop() {
            if seen.contains(&path.node) {
                continue;
            }
            seen.insert(path.node.clone());

            paths.push(path.clone());

            for edge in self.out_edges(&path.node) {
                let (_, next_node) = self.nodes(&edge);

                frontier.push(Path {
                    node: next_node,
                    parent: Box::new(Some(path.clone())),
                })
            }
        }

        paths
    }

    fn breadth_first_search(
        &self,
        start: &Self::Node,
        is_target: impl Fn(&Self::Node) -> bool,
    ) -> Option<Path<Self::Node>> {
        self.breadth_first_traverse(start)
            .into_iter()
            .find(|path| is_target(&path.node))
    }

    fn depth_first_search(
        &self,
        start: &Self::Node,
        is_target: impl Fn(&Self::Node) -> bool,
    ) -> Option<Path<Self::Node>> {
        self.depth_first_traverse(start)
            .into_iter()
            .find(|path| is_target(&path.node))
    }
}
