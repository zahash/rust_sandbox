use crate::graph::Graph;

pub struct Classic<V> {
    vertices: Vec<V>,
    edges: Vec<(V, V)>,
}

impl<V: PartialEq + Clone> Graph for Classic<V> {
    type V = V;
    type E = (V, V);

    fn out_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        self.edges
            .iter()
            .filter(|(from, _)| from == node)
            .map(|(_, to)| to.clone())
            .collect()
    }

    fn in_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        self.edges
            .iter()
            .filter(|(_, to)| to == node)
            .map(|(from, _)| from.clone())
            .collect()
    }

    fn out_edges(&self, node: &Self::V) -> Vec<Self::E> {
        self.edges
            .iter()
            .filter(|(from, _)| from == node)
            .cloned()
            .collect()
    }

    fn in_edges(&self, node: &Self::V) -> Vec<Self::E> {
        self.edges
            .iter()
            .filter(|(_, to)| to == node)
            .cloned()
            .collect()
    }

    fn edges(&self, from: &Self::V, to: &Self::V) -> Vec<Self::E> {
        self.edges
            .iter()
            .filter(|(f, t)| f == from && t == to)
            .cloned()
            .collect()
    }

    fn vertices(&self, edge: &Self::E) -> (Self::V, Self::V) {
        edge.clone()
    }
}
