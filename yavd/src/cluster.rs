use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

#[derive(Default)]
pub struct Cluster<T> {
    cluster_data: HashMap<T, HashSet<T>>,
    data_cluster: HashMap<T, T>,
}

impl<T> Cluster<T> {
    pub fn new() -> Self {
        Self {
            cluster_data: HashMap::new(),
            data_cluster: HashMap::new(),
        }
    }

    pub fn assoc(&mut self, cluster_id: T, data_id: T)
    where
        T: Eq + Hash + Clone,
    {
        self.disassoc(&data_id);
        self.cluster_data
            .entry(cluster_id.clone())
            .or_default()
            .insert(data_id.clone());
        self.data_cluster.insert(data_id, cluster_id);
    }

    pub fn disassoc(&mut self, data_id: &T)
    where
        T: Eq + Hash,
    {
        if let Some(cluster_id) = self.data_cluster.remove(data_id) {
            self.cluster_data
                .get_mut(&cluster_id)
                .map(|members| members.remove(data_id));
            self.remove_cluster_if_empty(&cluster_id);
        }
    }

    pub fn members(&self, cluster_id: &T) -> Option<&HashSet<T>>
    where
        T: Eq + Hash,
    {
        self.cluster_data.get(cluster_id)
    }

    fn remove_cluster_if_empty(&mut self, cluster_id: &T)
    where
        T: Eq + Hash,
    {
        if let Some(len) = self
            .cluster_data
            .get(cluster_id)
            .map(|members| members.len())
        {
            if len == 0 {
                self.cluster_data.remove(cluster_id);
            }
        }
    }
}
