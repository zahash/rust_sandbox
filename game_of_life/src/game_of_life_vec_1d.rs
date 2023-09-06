use rayon::prelude::*;

pub struct Game<const R: usize, const C: usize> {
    grid: Vec<bool>,
}

impl<const R: usize, const C: usize> Game<R, C> {
    pub fn empty() -> Self {
        Self {
            grid: vec![false; R * C],
        }
    }

    pub fn kill(&mut self, r: usize, c: usize) {
        self.grid[Self::i(r, c)] = false;
    }

    pub fn revive(&mut self, r: usize, c: usize) {
        self.grid[Self::i(r, c)] = true;
    }

    pub fn par_update(&mut self) {
        let mut next: Vec<bool> = vec![false; R * C];

        next.par_iter_mut().enumerate().for_each(|(i, cell)| {
            let (r, c) = Self::rc(i);
            *cell = self.next(r, c);
        });

        std::mem::swap(&mut self.grid, &mut next);
    }

    pub fn update(&mut self) {
        let mut next: Vec<bool> = Vec::with_capacity(R * C);

        for r in 0..R {
            for c in 0..C {
                next.push(self.next(r, c));
            }
        }

        std::mem::swap(&mut self.grid, &mut next);
    }

    #[inline]
    fn next(&self, r: usize, c: usize) -> bool {
        match (self.grid[Self::i(r, c)], self.live_neighbors(r, c)) {
            (true, n) if n < 2 => false,
            (true, n) if n == 2 || n == 3 => true,
            (true, n) if n > 3 => false,
            (false, n) if n == 3 => true,
            _ => self.grid[Self::i(r, c)],
        }
    }

    #[inline]
    fn live_neighbors(&self, r: usize, c: usize) -> u8 {
        let left_r = Self::diff::<R>(r, -1);
        let right_r = Self::diff::<R>(r, 1);
        let top_c = Self::diff::<C>(c, -1);
        let bottom_c = Self::diff::<C>(c, 1);

        let top_left = Self::i(left_r, top_c);
        let top = Self::i(r, top_c);
        let top_right = Self::i(right_r, top_c);
        let left = Self::i(left_r, c);
        let right = Self::i(right_r, c);
        let bottom_left = Self::i(left_r, bottom_c);
        let bottom = Self::i(r, bottom_c);
        let bottom_right = Self::i(right_r, bottom_c);

        self.grid[top_left] as u8
            + self.grid[top] as u8
            + self.grid[top_right] as u8
            + self.grid[left] as u8
            + self.grid[right] as u8
            + self.grid[bottom_left] as u8
            + self.grid[bottom] as u8
            + self.grid[bottom_right] as u8
    }

    #[inline]
    const fn diff<const I: usize>(i: usize, diff: i8) -> usize {
        match (i, diff) {
            (0, -1) => I - 1,
            (_, 1) if i == I - 1 => 0,
            (_, -1) => i - 1,
            (_, 1) => i + 1,
            _ => unreachable!(),
        }
    }

    #[inline]
    const fn i(r: usize, c: usize) -> usize {
        r * C + c
    }

    #[inline]
    const fn rc(i: usize) -> (usize, usize) {
        (i / C, i % C)
    }
}
