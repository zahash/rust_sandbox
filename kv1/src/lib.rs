use std::{any::Any, collections::HashMap};

pub struct KV1 {
    store: HashMap<String, Box<dyn Any>>,
}

impl KV1 {
    pub fn get<T: 'static>(&self, k: &str) -> Option<&T> {
        self.store.get(k).and_then(|any| any.downcast_ref::<T>())
    }

    pub fn set<T: 'static>(&mut self, k: String, v: T) {
        self.store.insert(k, Box::new(v));
    }
}
