pub struct Game<const N: usize> {
    grid: Vec<Vec<bool>>,
}

impl<const N: usize> Game<N> {
    pub const fn new(start: Vec<Vec<bool>>) -> Self {
        Self { grid: start }
    }

    pub const fn grid(&self) -> &Vec<Vec<bool>> {
        &self.grid
    }

    pub fn update(&mut self) {
        let mut next: Vec<Vec<bool>> = Vec::with_capacity(N);

        for r in 0..N {
            let mut row: Vec<bool> = Vec::with_capacity(N);
            for c in 0..N {
                row.push(self.next(r, c));
            }
            next.push(row);
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
    fn live_neighbors(&self, r: usize, c: usize) -> u8 {
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
