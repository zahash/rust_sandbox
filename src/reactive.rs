use std::{
    cell::RefCell,
    collections::hash_map::DefaultHasher,
    fmt::Debug,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Clone, Default)]
pub struct Reactive<T> {
    inner: Rc<RefCell<ReactiveInner<T>>>,
}

impl<T> Reactive<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: Rc::new(RefCell::new(ReactiveInner::new(value))),
        }
    }

    pub fn add_observer(&self, f: impl FnMut(&T) + 'static) {
        self.inner.borrow_mut().observers.push(Box::new(f));
    }
}

impl<T: Clone> Reactive<T> {
    pub fn value(&self) -> T {
        self.inner.borrow().value.clone()
    }

    pub fn derive<U: Default + Clone + PartialEq + 'static>(
        &self,
        f: impl Fn(&T) -> U + 'static,
    ) -> Reactive<U> {
        let derived_val = f(&self.value());
        let derived: Reactive<U> = Reactive::new(derived_val);

        self.add_observer({
            let derived = derived.clone();
            move |value| derived.update(|_| f(value))
        });

        derived
    }
}

impl<T: PartialEq> Reactive<T> {
    pub fn update(&self, f: impl Fn(&T) -> T) {
        self.inner.borrow_mut().update(f);
    }
}

impl<T: Hash> Reactive<T> {
    pub fn update_inplace(&self, f: impl Fn(&mut T)) {
        self.inner.borrow_mut().update_inplace(f);
    }
}

impl<T: Debug> Debug for Reactive<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Reactive")
            .field(&self.inner.borrow().value)
            .finish()
    }
}

#[derive(Default)]
struct ReactiveInner<T> {
    value: T,
    observers: Vec<Box<dyn FnMut(&T)>>,
}

impl<T> ReactiveInner<T> {
    fn new(value: T) -> Self {
        Self {
            value,
            observers: vec![],
        }
    }
}

impl<T: PartialEq> ReactiveInner<T> {
    fn update(&mut self, f: impl Fn(&T) -> T) {
        let new_value = f(&self.value);
        if new_value != self.value {
            self.value = new_value;
            for obs in &mut self.observers {
                obs(&self.value);
            }
        }
    }
}

impl<T: Hash> ReactiveInner<T> {
    fn update_inplace(&mut self, f: impl Fn(&mut T)) {
        let old_hash = {
            let mut hasher = DefaultHasher::new();
            self.value.hash(&mut hasher);
            hasher.finish()
        };

        f(&mut self.value);

        let new_hash = {
            let mut hasher = DefaultHasher::new();
            self.value.hash(&mut hasher);
            hasher.finish()
        };

        if old_hash != new_hash {
            for obs in &mut self.observers {
                obs(&self.value);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_derived_values_must_not_be_default() {
        let r = Reactive::new(10);
        let d = r.derive(|val| val + 5);

        assert_eq!(15, d.value());
    }

    #[test]
    fn can_update() {
        let r = Reactive::new(10);
        let d = r.derive(|val| val + 5);

        r.update(|_| 20);

        assert_eq!(25, d.value());
    }

    #[test]
    fn can_update_inplace() {
        let r = Reactive::new(vec![1, 2, 3]);
        let d = r.derive(|nums| nums.iter().sum::<i32>());

        r.update_inplace(|nums| {
            nums.push(4);
            nums.push(5);
            nums.push(6);
        });

        assert_eq!(21, d.value());
    }

    #[test]
    fn can_add_observers() {
        let r: Reactive<String> = Reactive::default();
        let changes: Rc<RefCell<Vec<String>>> = Default::default();

        r.add_observer({
            let changes = changes.clone();
            move |val| changes.borrow_mut().push(val.clone())
        });

        r.update(|_| String::from("a"));
        r.update_inplace(|s| {
            s.clear();
            s.push('b');
        });

        assert_eq!(
            vec![String::from("a"), String::from("b")],
            changes.borrow().clone()
        );
    }
}
