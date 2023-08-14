use ndarray::Array2;
use std::ops::{Deref, DerefMut};

pub struct Matrix<const M: usize, const N: usize> {
    data: Array2<f32>,
}

impl<const M: usize, const N: usize> Matrix<M, N> {
    pub fn new() -> Self {
        Matrix {
            data: Array2::zeros((M, N)),
        }
    }

    pub fn set(&mut self, data: Array2<f32>) -> Result<(), String> {
        match self.data.shape() == data.shape() {
            true => {
                self.data = data;
                Ok(())
            }
            false => Err(format!(
                "shape mismatch! expected {:?} but received {:?}",
                self.data.shape(),
                data.shape()
            )),
        }
    }

    pub fn dot<const K: usize>(&self, rhs: &Matrix<N, K>, out: &mut Matrix<M, K>) {
        out.set(self.data.dot(&rhs.data))
            .expect("never fails because shapes always match");
    }

    pub fn add(&mut self, rhs: &Matrix<M, N>) {
        self.set(&self.data + &rhs.data)
            .expect("never fails because shapes always match");
    }

    pub fn sub(&mut self, rhs: &Matrix<M, N>) {
        self.set(&self.data - &rhs.data)
            .expect("never fails because shapes always match");
    }
}

impl<const M: usize, const N: usize> Deref for Matrix<M, N> {
    type Target = Array2<f32>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<const M: usize, const N: usize> DerefMut for Matrix<M, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}
