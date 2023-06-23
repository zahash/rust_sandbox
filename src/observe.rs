use std::fmt::Debug;

#[derive(Default)]
pub struct Reactive<T> {
    val: T,
    observers: Vec<Box<dyn FnMut(&T)>>,
}

impl<T: PartialEq> Reactive<T> {
    pub fn new(val: T) -> Self {
        Self {
            val,
            observers: vec![],
        }
    }

    pub fn update(&mut self, f: impl Fn(&T) -> T) {
        let new_val = f(&self.val);
        if self.val != new_val {
            self.val = new_val;
            for obs in &mut self.observers {
                obs(&self.val);
            }
        }
    }

    pub fn add_observer(&mut self, f: impl FnMut(&T) + 'static) {
        self.observers.push(Box::new(f));
    }
}

impl<T: Debug> Debug for Reactive<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Reactive").field("val", &self.val).finish()
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
    use std::{cell::RefCell, rc::Rc};

    use super::*;

    #[test]
    fn test() {
        let a: Rc<RefCell<Reactive<Vec<i32>>>> = Default::default();
        let sum_of_a: Rc<RefCell<Reactive<i32>>> = Default::default();
        let three_times_sum_of_a: Rc<RefCell<Reactive<i32>>> = Default::default();

        a.borrow_mut().add_observer({
            let sum_of_a = sum_of_a.clone();
            move |nums| sum_of_a.borrow_mut().update(|_| nums.iter().sum())
        });

        sum_of_a.borrow_mut().add_observer({
            let three_times_sum_of_a = three_times_sum_of_a.clone();
            move |val| three_times_sum_of_a.borrow_mut().update(|_| val * 3)
        });

        a.borrow_mut().update(|_| vec![1, 2, 3]);

        println!("{:?}", a);
        println!("{:?}", sum_of_a);
        println!("{:?}", three_times_sum_of_a);
    }
}
