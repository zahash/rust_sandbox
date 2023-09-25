use rayon::prelude::*;

pub struct Game<const R: usize, const C: usize> {
    grid: Box<[[bool; C]; R]>,
}

impl<const R: usize, const C: usize> Game<R, C> {
    pub fn new(start: Box<[[bool; C]; R]>) -> Self {
        Self { grid: start }
    }

    pub const fn grid(&self) -> &[[bool; C]; R] {
        &self.grid
    }

    pub fn update(&mut self) {
        let mut next = Box::new([[false; C]; R]);

        next.par_iter_mut().enumerate().for_each(|(r, row)| {
            row.par_iter_mut().enumerate().for_each(|(c, cell)| {
                *cell = self.next(r, c);
            });
        });

        std::mem::swap(&mut self.grid, &mut next);
    }

    #[inline]
    const fn next(&self, r: usize, c: usize) -> bool {
        match (self.grid[r][c], self.live_neighbors(r, c)) {
            (true, n) if n < 2 => false,
            (true, n) if n == 2 || n == 3 => true,
            (true, n) if n > 3 => false,
            (false, n) if n == 3 => true,
            _ => self.grid[r][c],
        }
    }

    #[inline]
    const fn live_neighbors(&self, r: usize, c: usize) -> u8 {
        self.grid[Self::diff::<R>(r, -1)][Self::diff::<C>(c, -1)] as u8
            + self.grid[Self::diff::<R>(r, -1)][c] as u8
            + self.grid[Self::diff::<R>(r, -1)][Self::diff::<C>(c, 1)] as u8
            + self.grid[r][Self::diff::<C>(c, -1)] as u8
            + self.grid[r][Self::diff::<C>(c, 1)] as u8
            + self.grid[Self::diff::<R>(r, 1)][Self::diff::<C>(c, -1)] as u8
            + self.grid[Self::diff::<R>(r, 1)][c] as u8
            + self.grid[Self::diff::<R>(r, 1)][Self::diff::<C>(c, 1)] as u8
    }

    #[inline]
    const fn diff<const I: usize>(i: usize, diff: isize) -> usize {
        match (i, diff) {
            (0, -1) => I - 1,
            (_, 1) if i == I - 1 => 0,
            (_, -1) => i - 1,
            (_, 1) => i + 1,
            _ => unreachable!(),
        }
    }
}
