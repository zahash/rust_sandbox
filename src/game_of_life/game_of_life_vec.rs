use std::collections::HashSet;

use rayon::prelude::*;

pub struct Game {
    grid: Vec<Vec<bool>>,
}

impl Game {
    pub fn empty(r: usize, c: usize) -> Self {
        Self {
            grid: vec![vec![false; c]; r],
        }
    }

    pub fn nr(&self) -> usize {
        self.grid.len()
    }

    pub fn nc(&self) -> usize {
        self.grid[0].len()
    }

    pub const fn new(start: Vec<Vec<bool>>) -> Self {
        Self { grid: start }
    }

    pub const fn grid(&self) -> &Vec<Vec<bool>> {
        &self.grid
    }

    pub fn update(&mut self) {
        let mut next = vec![vec![false; self.grid[0].len()]; self.grid.len()];

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
        self.grid[Self::diff(self.nr(), r, -1)][Self::diff(self.nc(), c, -1)] as u8
            + self.grid[Self::diff(self.nr(), r, -1)][c] as u8
            + self.grid[Self::diff(self.nr(), r, -1)][Self::diff(self.nc(), c, 1)] as u8
            + self.grid[r][Self::diff(self.nc(), c, -1)] as u8
            + self.grid[r][Self::diff(self.nc(), c, 1)] as u8
            + self.grid[Self::diff(self.nr(), r, 1)][Self::diff(self.nc(), c, -1)] as u8
            + self.grid[Self::diff(self.nr(), r, 1)][c] as u8
            + self.grid[Self::diff(self.nr(), r, 1)][Self::diff(self.nc(), c, 1)] as u8
    }

    #[inline]
    const fn diff(max_i: usize, i: usize, diff: isize) -> usize {
        match (i, diff) {
            (0, -1) => max_i - 1,
            (_, 1) if i == max_i - 1 => 0,
            (_, -1) => i - 1,
            (_, 1) => i + 1,
            _ => unreachable!(),
        }
    }
}

const LIVE: char = '⬜';
const DEAD: char = '⬛';

impl From<&str> for Game {
    fn from(text: &str) -> Self {
        let mut grid: Vec<Vec<bool>> = Vec::new();

        for (r, line) in text.trim().lines().enumerate() {
            let line = line.trim();

            let mut row = vec![];
            for (c, char) in line.chars().enumerate() {
                match char {
                    LIVE => row.push(true),
                    DEAD => row.push(false),
                    _ => panic!("only {} and {} allowed", LIVE, DEAD),
                }
            }
            grid.push(row);
        }

        if grid
            .iter()
            .map(|row| row.len())
            .collect::<HashSet<usize>>()
            .len()
            != 1
        {
            panic!("variable length columns!");
        }

        Game::new(grid)
    }
}

pub fn draw(game: &Game) {
    for row in game.grid() {
        for val in row {
            match val {
                true => print!("{}", LIVE),
                false => print!("{}", DEAD),
            }
        }
        println!("");
    }
}

pub fn run() {
    // let mut game = Game::from(
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

    let mut game: Game = Game::from(
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
        game.update();
    }
}
