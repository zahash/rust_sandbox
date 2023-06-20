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
