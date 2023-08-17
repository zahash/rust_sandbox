pub struct Game<const N: usize> {
    grid: [[bool; N]; N],
    next: [[bool; N]; N],
}

impl<const N: usize> Game<N> {
    pub const fn new(start: [[bool; N]; N]) -> Self {
        Self {
            grid: start,
            next: [[false; N]; N],
        }
    }

    pub const fn grid(&self) -> &[[bool; N]; N] {
        &self.grid
    }

    pub fn update(&mut self) {
        // needs parallelization
        for r in 0..N {
            for c in 0..N {
                self.update_cell(r, c);
            }
        }
        self.grid = self.next;
    }

    /// updates the next state
    #[inline]
    fn update_cell(&mut self, r: usize, c: usize) {
        match (self.grid[r][c], self.live_neighbors(r, c)) {
            (true, n) if n < 2 => self.next[r][c] = false,
            (true, n) if n == 2 || n == 3 => self.next[r][c] = true,
            (true, n) if n > 3 => self.next[r][c] = false,
            (false, n) if n == 3 => self.next[r][c] = true,
            _ => self.next[r][c] = self.grid[r][c],
        };
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
            (_, -1) => i - 1,
            (_, 1) => (i + 1) % N,
            _ => unreachable!(),
        }
    }
}

const LIVE: char = '⬜'; // 🟩
const DEAD: char = '⬛';

impl<const N: usize> From<&str> for Game<N> {
    fn from(text: &str) -> Self {
        let mut grid = [[false; N]; N];

        let text = text.trim();

        if text.lines().count() != N {
            panic!(
                "number of rows mismatch. expected {} but got {}",
                N,
                text.lines().count()
            );
        }

        for (r, line) in text.lines().enumerate() {
            let line = line.trim();

            if line.chars().count() != N {
                panic!("col length mismatch. expected {} but got {}", N, line.len());
            }

            for (c, char) in line.chars().enumerate() {
                match char {
                    LIVE => grid[r][c] = true,
                    DEAD => grid[r][c] = false,
                    _ => panic!("only {} and {} allowed", LIVE, DEAD),
                }
            }
        }

        Game::new(grid)
    }
}

pub fn draw<const N: usize>(grid: &[[bool; N]; N]) {
    for r in 0..N {
        for c in 0..N {
            match grid[r][c] {
                true => print!("{}", LIVE),
                false => print!("{}", DEAD),
            }
        }
        println!("");
    }
}

pub fn run() {
    // let mut game: Game<10> = Game::from(
    //     "
    //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
    //     ⬛⬛⬛⬛⬛⬜⬛⬛⬛⬛
    //     ⬛⬛⬛⬜⬛⬜⬛⬛⬛⬛
    //     ⬛⬛⬛⬛⬜⬜⬛⬛⬛⬛
    //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
    //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
    //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
    //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
    //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
    //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
    //     ",
    // );

    let mut game: Game<10> = Game::from(
        "
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬜⬛⬛⬜⬛⬛⬛
        ⬛⬛⬜⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬜⬛⬛⬛⬜⬛⬛⬛
        ⬛⬛⬜⬜⬜⬜⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ",
    );

    loop {
        draw(game.grid());
        std::thread::sleep(std::time::Duration::from_millis(70));
        print!("\x1B[2J\x1B[1;1H"); // clear screen
        game.update();
    }
}
