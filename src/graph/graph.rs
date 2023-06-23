pub trait Graph {
    type V;
    type E;

    fn out_vertices(&self, v: &Self::V) -> Vec<Self::V>;

    fn in_vertices(&self, v: &Self::V) -> Vec<Self::V>;

    fn out_edges(&self, v: &Self::V) -> Vec<Self::E>;

    fn in_edges(&self, v: &Self::V) -> Vec<Self::E>;

    fn edges(&self, from: &Self::V, to: &Self::V) -> Vec<Self::E>;

    fn vertices(&self, e: &Self::E) -> (Self::V, Self::V);
}

pub trait Weighted
where
    Self: Graph,
{
    fn weight(&self, e: &Self::E) -> f64;
}
