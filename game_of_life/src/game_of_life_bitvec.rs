use bitvec::prelude::*;

pub struct Game<const N: usize> {
    grid: BitVec,
}

impl<const N: usize> Game<N> {
    pub const fn new(start: BitVec) -> Self {
        Self { grid: start }
    }

    pub const fn grid(&self) -> &BitVec {
        &self.grid
    }

    pub fn update(&mut self) {
        let mut next = BitVec::with_capacity(N * N);

        for r in 0..N {
            for c in 0..N {
                next.push(self.next(r, c));
            }
        }

        self.grid = next;
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
        self.grid[Self::i(Self::wrap(r, -1), Self::wrap(c, -1))] as u8
            + self.grid[Self::i(Self::wrap(r, -1), c)] as u8
            + self.grid[Self::i(Self::wrap(r, -1), Self::wrap(c, 1))] as u8
            + self.grid[Self::i(r, Self::wrap(c, -1))] as u8
            + self.grid[Self::i(r, Self::wrap(c, 1))] as u8
            + self.grid[Self::i(Self::wrap(r, 1), Self::wrap(c, -1))] as u8
            + self.grid[Self::i(Self::wrap(r, 1), c)] as u8
            + self.grid[Self::i(Self::wrap(r, 1), Self::wrap(c, 1))] as u8
    }

    #[inline]
    const fn i(r: usize, c: usize) -> usize {
        r * N + c
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

// const LIVE: char = '⬜';
// const DEAD: char = '⬛';

// impl<const N: usize> From<&str> for Game<N> {
//     fn from(text: &str) -> Self {
//         let mut grid = BitVec::with_capacity(N * N);

//         let text = text.trim();

//         if text.lines().count() != N {
//             panic!(
//                 "number of rows mismatch. expected {} but got {}",
//                 N,
//                 text.lines().count()
//             );
//         }

//         for (r, line) in text.lines().enumerate() {
//             let line = line.trim();

//             if line.chars().count() != N {
//                 panic!("col length mismatch. expected {} but got {}", N, line.len());
//             }

//             for (c, char) in line.chars().enumerate() {
//                 match char {
//                     LIVE => grid.push(true),
//                     DEAD => grid.push(false),
//                     _ => panic!("only {} and {} allowed", LIVE, DEAD),
//                 }
//             }
//         }

//         Game::new(grid)
//     }
// }

// pub fn draw<const N: usize>(game: &Game<N>) {
//     for r in 0..N {
//         for c in 0..N {
//             match game.grid[Game::<N>::i(r, c)] {
//                 true => print!("{}", LIVE),
//                 false => print!("{}", DEAD),
//             }
//         }
//         println!("");
//     }
// }

// pub fn run() {
//     // let mut game = Game::new([[true; 10_000]; 10_000]);

//     // let mut game: Game<10> = Game::from(
//     //     "
//     //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//     //     ⬛⬛⬛⬛⬛⬜⬛⬛⬛⬛
//     //     ⬛⬛⬛⬜⬛⬜⬛⬛⬛⬛
//     //     ⬛⬛⬛⬛⬜⬜⬛⬛⬛⬛
//     //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//     //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//     //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//     //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//     //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//     //     ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//     //     ",
//     // );

//     let mut game: Game<16> = Game::from(
//         "
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬜⬛⬛⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬜⬛⬛⬛⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬜⬜⬜⬜⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬜⬛⬛⬜⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬜⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬜⬛⬛⬛⬜⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬜⬜⬜⬜⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛
//         ",
//     );

//     loop {
//         draw(&game);
//         std::thread::sleep(std::time::Duration::from_millis(70));
//         print!("\x1B[2J\x1B[1;1H"); // clear screen
//         game.update();
//     }
// }
