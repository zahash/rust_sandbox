#![allow(dead_code, unused_variables)]

mod version1 {
    use core::marker::PhantomData;
    struct Unspecified;
    struct Specified;

    struct ParamsBuilder<V, A1, A2, A3> {
        a1: A1,
        a2: Option<f64>,
        a3: Option<usize>,
        _marker: PhantomData<(V, A2, A3)>,
    }

    pub struct Params<V, F> {
        distance_fn: F,
        threshold: Option<f64>,
        k_closest: Option<usize>,
        _marker: PhantomData<V>,
    }

    impl Params<Unspecified, Unspecified> {
        fn builder() -> ParamsBuilder<Unspecified, Unspecified, Unspecified, Unspecified> {
            ParamsBuilder {
                a1: Unspecified,
                a2: None,
                a3: None,
                _marker: PhantomData,
            }
        }
    }

    impl<V, A2, A3> ParamsBuilder<V, Unspecified, A2, A3> {
        fn distance_fn<F: Fn(V, V) -> f64>(self, f: F) -> ParamsBuilder<V, F, A2, A3> {
            let Self { a2, a3, .. } = self;
            ParamsBuilder {
                a1: f,
                a2,
                a3,
                _marker: PhantomData,
            }
        }
    }

    impl<V, A1> ParamsBuilder<V, A1, Unspecified, Unspecified> {
        fn threshold(self, x: f64) -> ParamsBuilder<V, A1, Specified, Unspecified> {
            let Self { a1, a3, .. } = self;
            ParamsBuilder {
                a1,
                a2: Some(x),
                a3,
                _marker: PhantomData,
            }
        }
    }

    impl<V, A1> ParamsBuilder<V, A1, Unspecified, Unspecified> {
        fn k_closest(self, k: usize) -> ParamsBuilder<V, A1, Unspecified, Specified> {
            let Self { a1, a2, .. } = self;
            ParamsBuilder {
                a1,
                a2,
                a3: Some(k),
                _marker: PhantomData,
            }
        }
    }

    impl<V, F: Fn(V, V) -> f64> ParamsBuilder<V, F, Specified, Unspecified> {
        fn build(self) -> Params<V, F> {
            let Self { a1, a2, a3, .. } = self;
            Params {
                distance_fn: a1,
                threshold: a2,
                k_closest: a3,
                _marker: PhantomData,
            }
        }
    }

    impl<V, F: Fn(V, V) -> f64> ParamsBuilder<V, F, Unspecified, Specified> {
        fn build(self) -> Params<V, F> {
            let Self { a1, a2, a3, .. } = self;
            Params {
                distance_fn: a1,
                threshold: a2,
                k_closest: a3,
                _marker: PhantomData,
            }
        }
    }

    fn main() {
        Params::builder()
            .distance_fn(|v, v2| 1.0)
            .threshold(1.0)
            .build();
        // Fail
        //Params::builder().distance_fn(|v, v2| 1.0).threshold(1.0).k_closest(0).build();
        Params::builder()
            .distance_fn(|v, v2| 1.0)
            .k_closest(0)
            .build();
        // Fail
        //Params::builder().build();
        //Params::builder().distance_fn(|v, v2| 1.0).build();
        //Params::builder().threshold(1.0).build();
        //Params::builder().k_closest(0).build();
    }
}

mod version2 {
    pub struct Valid;

    pub struct Params<V, Marker> {
        distance_fn: Box<dyn Fn(V, V) -> f64>,
        threshold: Option<f64>,
        k_closest: Option<usize>,
        _is_valid_invariant: std::marker::PhantomData<Marker>,
    }

    impl<V, Marker> Params<V, Marker> {
        pub fn threshold(mut self, x: f64) -> Params<V, Valid> {
            self.threshold = Some(x);
            Params::<V, Valid>::from_generic(self)
        }

        pub fn k_closest(mut self, k: usize) -> Params<V, Valid> {
            self.k_closest = Some(k);
            Params::<V, Valid>::from_generic(self)
        }
    }

    impl<V> Params<V, Valid> {
        pub fn new(distance_fn: impl Fn(V, V) -> f64 + 'static) -> Params<V, ()> {
            Params::<V, ()> {
                distance_fn: Box::new(distance_fn),
                threshold: None,
                k_closest: None,
                _is_valid_invariant: std::marker::PhantomData::default(),
            }
        }

        fn from_generic<Marker>(params: Params<V, Marker>) -> Self {
            Self {
                distance_fn: params.distance_fn,
                threshold: params.threshold,
                k_closest: params.k_closest,
                _is_valid_invariant: std::marker::PhantomData::default(),
            }
        }
    }

    fn main() {
        let type_a = Params::new(|a: u32, b| 0.6);
        let type_b = type_a.k_closest(5);
    }
}

mod version3 {
    #[allow(non_camel_case_types)]
    pub struct Missing_OneOf_Threshold_KClosest;

    pub struct Params<V, Marker> {
        distance_fn: Box<dyn Fn(V, V) -> f64>,
        threshold: Option<f64>,
        k_closest: Option<usize>,
        _marker: std::marker::PhantomData<Marker>,
    }

    impl<V, Marker> Params<V, Marker> {
        pub fn threshold(mut self, x: f64) -> Params<V, ()> {
            self.threshold = Some(x);
            Params::<V, ()>::from_generic(self)
        }

        pub fn k_closest(mut self, k: usize) -> Params<V, ()> {
            self.k_closest = Some(k);
            Params::<V, ()>::from_generic(self)
        }
    }

    impl<V> Params<V, ()> {
        pub fn new(
            distance_fn: impl Fn(V, V) -> f64 + 'static,
        ) -> Params<V, Missing_OneOf_Threshold_KClosest> {
            Params {
                distance_fn: Box::new(distance_fn),
                threshold: None,
                k_closest: None,
                _marker: std::marker::PhantomData,
            }
        }

        fn from_generic<Marker>(params: Params<V, Marker>) -> Self {
            Self {
                distance_fn: params.distance_fn,
                threshold: params.threshold,
                k_closest: params.k_closest,
                _marker: std::marker::PhantomData,
            }
        }
    }

    fn main() {
        let a = Params::new(|a: u32, b| 6f64);
        // let a = a.build();
        let a = a.k_closest(5).threshold(6f64);
    }
}
