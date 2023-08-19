use rayon::prelude::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

pub struct Game<const N: usize> {
    grid: [[bool; N]; N],
}

impl<const N: usize> Game<N> {
    pub const fn new(start: [[bool; N]; N]) -> Self {
        Self { grid: start }
    }

    pub const fn grid(&self) -> &[[bool; N]; N] {
        &self.grid
    }

    /// same as `update` but parallelized for large grids
    pub fn par_update(&mut self) {
        let mut next = [[false; N]; N];

        next.par_iter_mut().enumerate().for_each(|(r, row)| {
            row.par_iter_mut().enumerate().for_each(|(c, cell)| {
                *cell = self.next(r, c);
            });
        });

        self.grid = next;
    }

    pub fn update(&mut self) {
        let mut next = [[false; N]; N];

        for r in 0..N {
            for c in 0..N {
                next[r][c] = self.next(r, c);
            }
        }

        self.grid = next;
    }

    #[inline]
    fn next(&self, r: usize, c: usize) -> bool {
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
        self.grid[Self::wrap(r, -1)][Self::wrap(c, -1)] as u8
            + self.grid[Self::wrap(r, -1)][c] as u8
            + self.grid[Self::wrap(r, -1)][Self::wrap(c, 1)] as u8
            + self.grid[r][Self::wrap(c, -1)] as u8
            + self.grid[r][Self::wrap(c, 1)] as u8
            + self.grid[Self::wrap(r, 1)][Self::wrap(c, -1)] as u8
            + self.grid[Self::wrap(r, 1)][c] as u8
            + self.grid[Self::wrap(r, 1)][Self::wrap(c, 1)] as u8
    }

    #[inline]
    const fn wrap(i: usize, diff: isize) -> usize {
        match (i, diff) {
            (0, -1) => N - 1,
            (_, 1) if i == N - 1 => 0,
            (_, -1) => i - 1,
            (_, 1) => i + 1,
            _ => unreachable!(),
        }
    }
}
