pub trait Graph {
    type V;
    type E;

    fn out_vertices(&self, v: &Self::V) -> Vec<Self::V>;

    fn in_vertices(&self, v: &Self::V) -> Vec<Self::V>;

    fn out_edges(&self, v: &Self::V) -> Vec<Self::E>;

    fn in_edges(&self, v: &Self::V) -> Vec<Self::E>;

    fn edges(&self, from: &Self::V, to: &Self::V) -> Vec<Self::E>;

    fn vertices(&self, e: &Self::E) -> (Self::V, Self::V);

    fn all_vertices(&self) -> Vec<Self::V>;

    fn all_edges(&self) -> Vec<Self::E>;
}

pub trait Weighted {
    type E;

    fn weight(&self, e: &Self::E) -> f64;
}

pub trait Mutable {
    type V;
    type E;

    fn add_vertex(&mut self, v: Self::V);

    fn add_edge(&mut self, e: Self::E);
}
