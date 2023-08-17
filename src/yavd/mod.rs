#![allow(dead_code, unused_variables)]

mod cluster;
mod embedding_store;
mod params;

pub use cluster::Cluster;
pub use embedding_store::EmbeddingStore;
pub use params::Params;

use ordered_float::OrderedFloat;
use uuid::Uuid;

#[derive(Default)]
pub struct YAVD<V> {
    vectors: EmbeddingStore<V>,
    cluster_centres: EmbeddingStore<V>,
    cluster: Cluster<Uuid>,
}

fn foo() {
    type Point = (f64, f64);
    let mut points: Vec<Point> = vec![];

    points.sort_by_key(|(x, y)| OrderedFloat((x * x + y * y).sqrt()));
}

impl<V> YAVD<V> {
    pub fn new() -> Self {
        Self {
            vectors: EmbeddingStore::new(),
            cluster_centres: EmbeddingStore::new(),
            cluster: Cluster::new(),
        }
    }

    pub fn add(&mut self, vec: V) -> Uuid {
        Uuid::new_v4()
    }

    pub fn remove(&mut self, vec_id: &Uuid) {}

    pub fn closest(&self, vec: &V, params: Params<V>) -> Vec<&V> {
        let mut cluster_centres = self
            .cluster_centres
            .into_iter()
            .collect::<Vec<(&Uuid, &V)>>();

        cluster_centres.sort_by_key(|(_, center)| OrderedFloat(params.distance_fn()(vec, center)));

        // this is also fine
        // cluster_centres.sort_by(|(_, center1), (_, center2)| {
        //     params.distance_fn()(vec, center1).total_cmp(&params.distance_fn()(vec, center2))
        // });

        let mut k = params.k_closest();
        let threshold = params.threshold();

        let mut closest = vec![];

        let members = cluster_centres
            .iter()
            .map(|(cluster_id, _)| cluster_id)
            .flat_map(|cluster_id| self.cluster.members(cluster_id))
            .flatten()
            .map(|member_id| self.vectors.get(member_id))
            .flatten();

        for (id, _) in cluster_centres {
            for cluster_member in self
                .cluster
                .members(id)
                .iter()
                .flat_map(|set| set.iter())
                .map(|id| self.vectors.get(id))
                .flatten()
            {
                if k > 0 && params.distance_fn()(vec, cluster_member) <= threshold {
                    closest.push(cluster_member);
                    k -= 1;
                }
            }
        }

        closest
    }
}

#[cfg(test)]
mod tests {
    use crate::yavd::Params;

    #[test]
    fn test() {
        let p = Params::builder(|a: &[f64; 2], b| 0.6);
        let p = p.k_closest(10);
        let p = p.threshold(0.6);
        let type_b = p.build();
    }
}

// #[derive(Debug, Default)]
// pub struct Store {
//     data: Vec<EVec>,
//     clusters: Vec<EVec>,
//     index: Vec<(Id, Id)>,
//     auto_id: Id,
// }

// pub struct Store<const N: usize> {
//     data: EmbeddingStore<N>,
//     cluster_centers: EmbeddingStore<N>,
//     cluster_assoc: Cluster<String>,
// }

// impl<const N: usize> Store<N> {
//     pub fn new() -> Self {
//         Self {
//             data: EmbeddingStore::new(),
//             cluster_centers: EmbeddingStore::new(),
//             cluster_assoc: Cluster::new(),
//         }
//     }

//     pub fn get(&self, id: &Uuid) -> Option<&[f64; N]> {
//         self.data.get(id)
//     }

//     // pub fn cluster_id(&self, evec_id: &Uuid) -> Option<Uuid> {
//     //     self.cluster_assoc.data_id__cluster_id.get(evec_id).cloned()
//     // }

//     // pub fn members(&self, cluster_id: &Uuid) -> Vec<Uuid> {
//     //     match self.cluster_assoc.cluster_id__data_ids.get(cluster_id) {
//     //         Some(members) => members.iter().cloned().collect::<Vec<Uuid>>(),
//     //         None => vec![],
//     //     }
//     // }

//     pub fn add(&mut self, vec: [f64; N]) -> Uuid {
//         let id = self.data.add(vec);

//         // let cluster_id = { for (id, vec) in self.cluster_centers.data.iter() {} };

//         todo!()
//     }

//     pub fn remove(&mut self, id: &Uuid) {}

//     pub fn closest(&self, vec: &[f64; N], k: usize) -> Vec<Uuid> {
//         vec![]
//     }

//     //     fn assign_cluster(&mut self, evec_id: Id) {
//     //         let distances = self.cluster_distances(evec_id);
//     //         let (cluster_id, softmax) = self.min_softmax(&distances);

//     //         let assign_to_existing_cluster = rand::thread_rng().gen::<f64>() > softmax;
//     //         if assign_to_existing_cluster {
//     //             self.index.push((cluster_id, evec_id));
//     //         } else {
//     //             let id = self.auto_id();
//     //             let evec = self
//     //                 .data
//     //                 .iter()
//     //                 .find(|(id, _)| id == &evec_id)
//     //                 .map(|(_, evec)| evec)
//     //                 .cloned()
//     //                 .unwrap();
//     //             self.clusters.push((id, evec));
//     //         }
//     //     }

//     //     fn cluster_distances(&self, evec_id: Id) -> Vec<(Id, f64)> {
//     //         vec![]
//     //     }

//     //     fn min_softmax(&self, distances: &[(Id, f64)]) -> (Id, f64) {
//     //         (0, 0.)
//     //     }
// }

// impl Store {
//     pub fn add(&mut self, vec: Vec<f64>) -> Id {
//         let id = self.auto_id();

//         self.data.push((id, vec));
//         self.assign_cluster(id);
//         id
//     }

//     pub fn remove(&mut self, evec_id: Id) {
//         self.data.retain_mut(|(id, _)| id != &evec_id);
//     }

//     fn auto_id(&mut self) -> Id {
//         let id = self.auto_id;
//         self.auto_id += 1;
//         id
//     }

//     fn assign_cluster(&mut self, evec_id: Id) {
//         let distances = self.cluster_distances(evec_id);
//         let (cluster_id, softmax) = self.min_softmax(&distances);

//         let assign_to_existing_cluster = rand::thread_rng().gen::<f64>() > softmax;
//         if assign_to_existing_cluster {
//             self.index.push((cluster_id, evec_id));
//         } else {
//             let id = self.auto_id();
//             let evec = self
//                 .data
//                 .iter()
//                 .find(|(id, _)| id == &evec_id)
//                 .map(|(_, evec)| evec)
//                 .cloned()
//                 .unwrap();
//             self.clusters.push((id, evec));
//         }
//     }

//     fn cluster_distances(&self, evec_id: Id) -> Vec<(Id, f64)> {
//         vec![]
//     }

//     fn min_softmax(&self, distances: &[(Id, f64)]) -> (Id, f64) {
//         (0, 0.)
//     }
// }
