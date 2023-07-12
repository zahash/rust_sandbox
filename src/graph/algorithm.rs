use crate::graph::Graph;
use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path<Node> {
    pub node: Node,
    pub parent: Box<Option<Path<Node>>>,
}

impl<Node> From<Path<Node>> for Vec<Node> {
    fn from(path: Path<Node>) -> Self {
        let mut result = vec![];

        if let Some(parent) = *path.parent {
            result.append(&mut parent.into())
        }

        result.push(path.node);

        result
    }
}

pub fn breadth_first_traverse<Node: Clone + Eq + Hash>(
    graph: &impl Graph<V = Node>,
    start: &Node,
) -> Vec<Path<Node>> {
    let mut frontier: VecDeque<Path<Node>> = VecDeque::new();
    frontier.push_back(Path {
        node: start.clone(),
        parent: Box::default(),
    });

    let mut seen: HashSet<Node> = HashSet::new();
    let mut paths: Vec<Path<Node>> = vec![];

    while let Some(path) = frontier.pop_front() {
        if seen.contains(&path.node) {
            continue;
        }
        seen.insert(path.node.clone());

        paths.push(path.clone());

        for edge in graph.out_edges(&path.node) {
            let (_, next_node) = graph.vertices(&edge);

            frontier.push_back(Path {
                node: next_node,
                parent: Box::new(Some(path.clone())),
            })
        }
    }

    paths
}

pub fn depth_first_traverse<Node: Clone + Eq + Hash>(
    graph: &impl Graph<V = Node>,
    start: &Node,
) -> Vec<Path<Node>> {
    let mut frontier: Vec<Path<Node>> = Vec::new();
    frontier.push(Path {
        node: start.clone(),
        parent: Box::default(),
    });

    let mut seen: HashSet<Node> = HashSet::new();
    let mut paths: Vec<Path<Node>> = vec![];

    while let Some(path) = frontier.pop() {
        if seen.contains(&path.node) {
            continue;
        }
        seen.insert(path.node.clone());

        paths.push(path.clone());

        for edge in graph.out_edges(&path.node) {
            let (_, next_node) = graph.vertices(&edge);

            frontier.push(Path {
                node: next_node,
                parent: Box::new(Some(path.clone())),
            })
        }
    }

    paths
}

pub fn breadth_first_search<Node: Clone + Eq + Hash>(
    graph: &impl Graph<V = Node>,
    start: &Node,
    is_target: impl Fn(&Node) -> bool,
) -> Option<Path<Node>> {
    breadth_first_traverse(graph, start)
        .into_iter()
        .find(|path| is_target(&path.node))
}

pub fn depth_first_search<Node: Clone + Eq + Hash>(
    graph: &impl Graph<V = Node>,
    start: &Node,
    is_target: impl Fn(&Node) -> bool,
) -> Option<Path<Node>> {
    depth_first_traverse(graph, start)
        .into_iter()
        .find(|path| is_target(&path.node))
}
