pub trait Graph {
    type V;
    type E;

    fn out_vertices(&self, node: &Self::V) -> Vec<Self::V>;

    fn in_vertices(&self, node: &Self::V) -> Vec<Self::V>;

    fn out_edges(&self, node: &Self::V) -> Vec<Self::E>;

    fn in_edges(&self, node: &Self::V) -> Vec<Self::E>;

    fn edges(&self, from: &Self::V, to: &Self::V) -> Vec<Self::E>;

    fn vertices(&self, edge: &Self::E) -> (Self::V, Self::V);
}
