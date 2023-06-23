use crate::graph::Graph;

#[derive(Debug)]
pub struct BinaryGrid {
    grid: Vec<Vec<bool>>,
    directions: [[bool; 3]; 3],
}

impl BinaryGrid {
    /// traversable horizontally and vertically
    pub fn new_hv(grid: Vec<Vec<bool>>) -> Self {
        Self {
            grid,
            directions: [
                [false, true, false],
                [true, false, true],
                [false, true, false],
            ],
        }
    }

    /// traversable horizontally, vertically and diagonally
    pub fn new_hvd(grid: Vec<Vec<bool>>) -> Self {
        Self {
            grid,
            directions: [[true, true, true], [true, false, true], [true, true, true]],
        }
    }

    /// directions: traversable directions represented by matrix
    /// eg: traversable only horizontally and vertically [
    ///   [false, true,  false],
    ///   [true,  false, true],
    ///   [false, true,  false]
    /// ]
    pub fn new(grid: Vec<Vec<bool>>, directions: [[bool; 3]; 3]) -> Self {
        Self { grid, directions }
    }

    /// converts binary number grid to boolean grid
    pub fn grid_to_bool(grid: Vec<Vec<u8>>) -> Vec<Vec<bool>> {
        let to_bool = |val| match val {
            0 => false,
            _ => true,
        };

        grid.into_iter()
            .map(|row| row.into_iter().map(|val| to_bool(val)).collect())
            .collect()
    }

    fn nrc(&self, (r, c): &<BinaryGrid as Graph>::V) -> Vec<<BinaryGrid as Graph>::V> {
        let mut nrc = vec![];

        let r = *r;
        let c = *c;

        if r >= self.grid.len() || c >= self.grid[r].len() {
            return vec![];
        }

        for dr in 0..self.directions.len() {
            for dc in 0..self.directions.len() {
                if self.directions[dr][dc] {
                    // dr, dc = 0 | 1 | 2
                    // delta = -1 | 0 | 1
                    let delta_r: i8 = (dr as i8) - 1;
                    let delta_c: i8 = (dc as i8) - 1;

                    if (r == 0 && delta_r == -1)
                        || (c == 0 && delta_c == -1)
                        || (r == self.grid.len() - 1 && delta_r == 1)
                        || (c == self.grid[r].len() - 1 && delta_c == 1)
                    {
                        continue;
                    }

                    let nr: usize = match delta_r {
                        -1 => r - 1,
                        1 => r + 1,
                        _ => r,
                    };
                    let nc: usize = match delta_c {
                        -1 => c - 1,
                        1 => c + 1,
                        _ => c,
                    };

                    if self.grid[nr][nc] {
                        nrc.push((nr, nc));
                    }
                }
            }
        }

        nrc
    }
}

impl Graph for BinaryGrid {
    type V = (usize, usize);
    type E = (Self::V, Self::V);

    fn out_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        self.nrc(node)
    }

    fn in_vertices(&self, node: &Self::V) -> Vec<Self::V> {
        self.out_vertices(node)
    }

    fn out_edges(&self, node: &Self::V) -> Vec<Self::E> {
        let mut edges = vec![];

        for next_node in self.out_vertices(node) {
            edges.push((*node, next_node));
        }

        edges
    }

    fn in_edges(&self, node: &Self::V) -> Vec<Self::E> {
        let mut edges = vec![];

        for next_node in self.in_vertices(node) {
            edges.push((next_node, *node));
        }

        edges
    }

    fn edges(&self, from: &Self::V, to: &Self::V) -> Vec<Self::E> {
        match self.out_vertices(from).contains(to) {
            true => vec![(*from, *to)],
            false => vec![],
        }
    }

    fn vertices(&self, edge: &Self::E) -> (Self::V, Self::V) {
        *edge
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // let grid = BinaryGrid::grid_to_bool(vec![
        //     vec![1, 1, 0, 0, 0],
        //     vec![1, 1, 0, 0, 0],
        //     vec![0, 0, 1, 0, 0],
        //     vec![0, 0, 0, 1, 1],
        // ]);

        let grid = BinaryGrid::grid_to_bool(vec![
            vec![1, 1, 1, 1, 0],
            vec![1, 1, 0, 1, 0],
            vec![1, 1, 0, 0, 0],
            vec![0, 0, 0, 0, 0],
        ]);

        let graph = BinaryGrid::new_hv(grid);
    }
}
