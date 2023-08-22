use rayon::prelude::*;

pub struct Game<const R: usize, const C: usize> {
    grid: Vec<Vec<bool>>,
}

impl<const R: usize, const C: usize> Game<R, C> {
    pub const fn new(start: Vec<Vec<bool>>) -> Self {
        Self { grid: start }
    }

    pub const fn grid(&self) -> &Vec<Vec<bool>> {
        &self.grid
    }

    pub fn par_update(&mut self) {
        let mut next = vec![vec![false; C]; R];

        next.par_iter_mut().enumerate().for_each(|(r, row)| {
            row.par_iter_mut().enumerate().for_each(|(c, cell)| {
                *cell = self.next(r, c);
            });
        });

        std::mem::swap(&mut self.grid, &mut next);
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

const LIVE: char = '⬜';
const DEAD: char = '⬛';

impl<const R: usize, const C: usize> From<&str> for Game<R, C> {
    fn from(text: &str) -> Self {
        let mut grid = vec![vec![false; C]; R];

        let text = text.trim();

        if text.lines().count() != R {
            panic!(
                "number of rows mismatch. expected {} but got {}",
                R,
                text.lines().count()
            );
        }

        for (r, line) in text.lines().enumerate() {
            let line = line.trim();

            if line.chars().count() != C {
                panic!("col length mismatch. expected {} but got {}", C, line.len());
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

pub fn draw<const R: usize, const C: usize>(game: &Game<R, C>) {
    for r in 0..R {
        for c in 0..C {
            match game.grid()[r][c] {
                true => print!("{}", LIVE),
                false => print!("{}", DEAD),
            }
        }
        println!("");
    }
}

pub fn run() {
    // let mut game = Game::new([[true; 10_000]; 10_000]);

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

    let mut game: Game<16, 16> = Game::from(
        "
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬜⬛⬛⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬜⬛⬛⬛⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬜⬜⬜⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬜⬛⬛⬜⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬜⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬜⬛⬛⬛⬜⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬜⬜⬜⬜⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
        ",
    );

    loop {
        draw(&game);
        std::thread::sleep(std::time::Duration::from_millis(70));
        print!("\x1B[2J\x1B[1;1H"); // clear screen
        game.par_update();
    }
}
