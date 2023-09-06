use std::collections::HashMap;
use uuid::Uuid;

#[derive(Default)]
pub struct EmbeddingStore<V> {
    data: HashMap<Uuid, V>,
}

impl<V> EmbeddingStore<V> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    pub fn get(&self, id: &Uuid) -> Option<&V> {
        self.data.get(id)
    }

    pub fn add(&mut self, vec: V) -> Uuid {
        let id = Uuid::new_v4();
        self.data.insert(id, vec);
        id
    }

    pub fn update(&mut self, id: &Uuid, f: impl FnOnce(&mut V)) {
        if let Some(evec) = self.data.get_mut(id) {
            f(evec);
        }
    }

    pub fn remove(&mut self, id: &Uuid) {
        self.data.remove(id);
    }
}

impl<'a, V> IntoIterator for &'a EmbeddingStore<V> {
    type Item = (&'a Uuid, &'a V);
    type IntoIter = std::collections::hash_map::Iter<'a, Uuid, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}
