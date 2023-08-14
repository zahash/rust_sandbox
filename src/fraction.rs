use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Frac(isize, isize);

impl Frac {
    pub const fn new(n: isize, d: isize) -> Self {
        let gcd = Self::gcd(n, d);
        Frac(n / gcd, d / gcd)
    }

    /// numerator
    pub const fn n(&self) -> isize {
        self.0
    }

    /// denominator
    pub const fn d(&self) -> isize {
        self.1
    }

    pub fn as_f64(&self) -> f64 {
        self.n() as f64 / self.d() as f64
    }

    const fn gcd(a: isize, b: isize) -> isize {
        match b {
            0 => a,
            _ => Self::gcd(b, a % b),
        }
    }
}

impl From<f64> for Frac {
    fn from(x: f64) -> Self {
        unimplemented!()
    }
}

impl From<(isize, isize)> for Frac {
    fn from((n, d): (isize, isize)) -> Self {
        Frac::new(n, d)
    }
}

impl From<isize> for Frac {
    fn from(value: isize) -> Self {
        Frac::new(value, 1)
    }
}

impl Add for Frac {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Frac::new(self.n() * rhs.d() + rhs.n() * self.d(), self.d() * rhs.d())
    }
}

impl Add<isize> for Frac {
    type Output = Self;

    fn add(self, rhs: isize) -> Self::Output {
        self + Frac::from(rhs)
    }
}

impl Add<Frac> for isize {
    type Output = Frac;

    fn add(self, rhs: Frac) -> Self::Output {
        Frac::from(self) + rhs
    }
}

impl Sub for Frac {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Frac::new(self.n() * rhs.d() - rhs.n() * self.d(), self.d() * rhs.d())
    }
}

impl Sub<isize> for Frac {
    type Output = Self;

    fn sub(self, rhs: isize) -> Self::Output {
        self - Frac::from(rhs)
    }
}

impl Sub<Frac> for isize {
    type Output = Frac;

    fn sub(self, rhs: Frac) -> Self::Output {
        Frac::from(self) - rhs
    }
}

impl Mul for Frac {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Frac::new(self.n() * rhs.n(), self.d() * rhs.d())
    }
}

impl Mul<isize> for Frac {
    type Output = Self;

    fn mul(self, rhs: isize) -> Self::Output {
        self * Frac::from(rhs)
    }
}

impl Mul<Frac> for isize {
    type Output = Frac;

    fn mul(self, rhs: Frac) -> Self::Output {
        Frac::from(self) * rhs
    }
}

impl Div for Frac {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Frac::new(self.n() * rhs.d(), self.d() * rhs.n())
    }
}

impl Div<isize> for Frac {
    type Output = Self;

    fn div(self, rhs: isize) -> Self::Output {
        self / Frac::from(rhs)
    }
}

impl Div<Frac> for isize {
    type Output = Frac;

    fn div(self, rhs: Frac) -> Self::Output {
        Frac::from(self) / rhs
    }
}

impl Display for Frac {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.0, self.1)
    }
}

#[macro_export]
macro_rules! fr {
    ($n:expr,$d:expr) => {
        Frac::new($n, $d)
    };
    ($x:expr) => {
        Frac::from($x)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn frac_arithmetic() {
        assert_eq!(fr!(5, 4), fr!(1, 2) + fr!(3, 4));
        assert_eq!(fr!(-1, 4), fr!(1, 2) - fr!(3, 4));
        assert_eq!(fr!(3, 8), fr!(1, 2) * fr!(3, 4));
        assert_eq!(fr!(2, 3), fr!(1, 2) / fr!(3, 4));
    }

    #[test]
    fn frac_number_arithmetic() {
        assert_eq!(fr!(3, 2), fr!(1, 2) + 1);
        assert_eq!(fr!(3, 2), 1 + fr!(1, 2));

        assert_eq!(fr!(-1, 2), fr!(1, 2) - 1);
        assert_eq!(fr!(1, 2), 1 - fr!(1, 2));

        assert_eq!(fr!(3, 2), fr!(1, 2) * 3);
        assert_eq!(fr!(3, 2), 3 * fr!(1, 2));

        assert_eq!(fr!(1, 6), fr!(1, 2) / 3);
        assert_eq!(fr!(6), 3 / fr!(1, 2));
    }
}
