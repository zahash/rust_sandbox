pub struct Params<V> {
    distance_fn: Box<dyn Fn(&V, &V) -> f64>,
    threshold: Option<f64>,
    k_closest: Option<usize>,
}

impl<V> Params<V> {
    pub const fn distance_fn(&self) -> &Box<dyn Fn(&V, &V) -> f64> {
        &self.distance_fn
    }

    pub const fn threshold(&self) -> f64 {
        match self.threshold {
            Some(x) => x,
            None => f64::INFINITY,
        }
    }

    pub const fn k_closest(&self) -> usize {
        match self.k_closest {
            Some(k) => k,
            None => usize::MAX,
        }
    }
}

impl<V> Params<V> {
    pub fn builder(
        distance_fn: impl Fn(&V, &V) -> f64 + 'static,
    ) -> ParamsBuilder<V, Missing_OneOf_Threshold_KClosest> {
        ParamsBuilder::new(distance_fn)
    }
}

#[allow(non_camel_case_types)]
pub struct Missing_OneOf_Threshold_KClosest;

pub struct ParamsBuilder<V, Marker> {
    distance_fn: Box<dyn Fn(&V, &V) -> f64>,
    threshold: Option<f64>,
    k_closest: Option<usize>,
    _marker: std::marker::PhantomData<Marker>,
}

impl<V, Marker> ParamsBuilder<V, Marker> {
    pub fn threshold(mut self, x: f64) -> ParamsBuilder<V, ()> {
        self.threshold = Some(x);
        ParamsBuilder::<V, ()>::from_generic(self)
    }

    pub fn k_closest(mut self, k: usize) -> ParamsBuilder<V, ()> {
        self.k_closest = Some(k);
        ParamsBuilder::<V, ()>::from_generic(self)
    }
}

impl<V> ParamsBuilder<V, ()> {
    pub fn new(
        distance_fn: impl Fn(&V, &V) -> f64 + 'static,
    ) -> ParamsBuilder<V, Missing_OneOf_Threshold_KClosest> {
        ParamsBuilder {
            distance_fn: Box::new(distance_fn),
            threshold: None,
            k_closest: None,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn build(self) -> Params<V> {
        Params {
            distance_fn: self.distance_fn,
            threshold: self.threshold,
            k_closest: self.k_closest,
        }
    }

    fn from_generic<Marker>(params: ParamsBuilder<V, Marker>) -> Self {
        Self {
            distance_fn: params.distance_fn,
            threshold: params.threshold,
            k_closest: params.k_closest,
            _marker: std::marker::PhantomData,
        }
    }
}

// pub struct Valid;

// pub struct ParamsBuilder<V, Marker> {
//     distance_fn: Box<dyn Fn(V, V) -> f64>,
//     threshold: Option<f64>,
//     k_closest: Option<usize>,
//     _is_valid_invariant: std::marker::PhantomData<Marker>,
// }

// impl<V, Marker> ParamsBuilder<V, Marker> {
//     pub fn threshold(mut self, x: f64) -> ParamsBuilder<V, Valid> {
//         self.threshold = Some(x);
//         ParamsBuilder::<V, Valid>::from_generic(self)
//     }

//     pub fn k_closest(mut self, k: usize) -> ParamsBuilder<V, Valid> {
//         self.k_closest = Some(k);
//         ParamsBuilder::<V, Valid>::from_generic(self)
//     }
// }

// impl<V> ParamsBuilder<V, Valid> {
//     pub fn new(distance_fn: impl Fn(V, V) -> f64 + 'static) -> ParamsBuilder<V, ()> {
//         ParamsBuilder::<V, ()> {
//             distance_fn: Box::new(distance_fn),
//             threshold: None,
//             k_closest: None,
//             _is_valid_invariant: std::marker::PhantomData,
//         }
//     }

//     pub fn build(self) -> Params<V> {
//         Params {
//             distance_fn: self.distance_fn,
//             threshold: self.threshold,
//             k_closest: self.k_closest,
//         }
//     }

//     fn from_generic<Marker>(params: ParamsBuilder<V, Marker>) -> Self {
//         Self {
//             distance_fn: params.distance_fn,
//             threshold: params.threshold,
//             k_closest: params.k_closest,
//             _is_valid_invariant: std::marker::PhantomData,
//         }
//     }
// }
