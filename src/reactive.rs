// use std::fmt::Debug;
use std::{cell::RefCell, fmt::Debug, rc::Rc};

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
}

impl<T: PartialEq> Reactive<T> {
    fn update(&self, f: impl FnMut(&T) -> T) {
        self.inner.borrow_mut().update(f);
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
    fn update(&mut self, mut f: impl FnMut(&T) -> T) {
        let new_value = f(&self.value);
        if new_value != self.value {
            self.value = new_value;
            for obs in &mut self.observers {
                obs(&self.value);
            }
        }
    }
}

// class Reactive<T> {
//     private _val: T;
//     private observers: ((value: T) => void)[] = [];

//     constructor(initVal: T) {
//       this._val = initVal;
//     }

//     val(): T {
//       return this._val;
//     }

//     update(fn: (value: T) => T): void {
//       let newValue = fn(this._val);
//       if (this._val !== newValue) {
//         this._val = newValue;
//         this.observers.forEach((obs) => obs(newValue));
//       }
//     }

//     addObserver(fn: (value: T) => void): void {
//       this.observers.push(fn);
//     }
//   }

// pub struct Derived<U> {
//     reactive: Reactive<U>
// }

// impl<U> Derived<U> {
//     pub fn new<T>(reactive: Reactive<T>, f: impl Fn(&T) -> U) -> Self {

//     }
// }

//   class Derived<T, U> {
//     private reactive: Reactive<U>;

//     constructor(reactive: Reactive<T>, fn: (val: T) => U) {
//       this.reactive = new Reactive(fn(reactive.val()));
//       reactive.addObserver((val) => this.reactive.update((_) => fn(val)));
//     }

//     val(): U {
//       return this.reactive.val();
//     }

//     addObserver(fn: (value: U) => void): void {
//       this.reactive.addObserver(fn);
//     }
//   }

//   const example1 = () => {
//     let nums = new Reactive([1, 2, 3]);
//     let total = new Derived(nums, (nums) => nums.reduce((acc, n) => acc + n, 0));

//     console.log(total.val());

//     nums.addObserver((val) => console.log(val));

//     nums.update((val) => [...val, 10, 11, 12]);
//     nums.update((val) => [...val, 20, 30, 40]);

//     console.log(total.val());
//   };

//   const example2 = () => {
//     let a = new Reactive([]);
//     let b = new Reactive(0);
//     let c = new Reactive(0);

//     a.addObserver((nums) => b.update((_) => nums.reduce((acc, n) => acc + n, 0)));
//     b.addObserver((val) => c.update((_) => val * 3));

//     a.update((_) => [1, 2, 3]);

//     console.log(a.val());
//     console.log(b.val());
//     console.log(c.val());
//   };

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reactive() {
        let a: Reactive<Vec<i32>> = Default::default();
        let sum_of_a: Reactive<i32> = Default::default();
        let three_times_sum_of_a: Reactive<i32> = Default::default();

        a.add_observer({
            let sum_of_a = sum_of_a.clone();
            move |nums| sum_of_a.update(|_| nums.iter().sum())
        });

        sum_of_a.add_observer({
            let three_times_sum_of_a = three_times_sum_of_a.clone();
            move |val| three_times_sum_of_a.update(|_| val * 3)
        });

        a.update(|_| vec![1, 2, 3]);

        println!("{:?}", a);
        println!("{:?}", sum_of_a);
        println!("{:?}", three_times_sum_of_a);
    }

    #[test]
    fn test_reactive_with_vec() {
        let x = Reactive::new(0);
        let changes: Rc<RefCell<Vec<i32>>> = Default::default();

        x.add_observer({
            let changes = changes.clone();
            move |val| changes.borrow_mut().push(val.clone())
        });

        x.update(|val| val + 10);
        x.update(|val| val + 10);
        x.update(|val| val + 10);

        println!("{:?}", x);
        println!("{:?}", changes);
    }
}
