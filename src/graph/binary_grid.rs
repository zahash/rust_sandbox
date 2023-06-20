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

    fn nrc(&self, (r, c): &<BinaryGrid as Graph>::Node) -> Vec<<BinaryGrid as Graph>::Node> {
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
    type Node = (usize, usize);
    type Edge = (Self::Node, Self::Node);

    fn out_nodes(&self, node: &Self::Node) -> Vec<Self::Node> {
        self.nrc(node)
    }

    fn in_nodes(&self, node: &Self::Node) -> Vec<Self::Node> {
        self.out_nodes(node)
    }

    fn out_edges(&self, node: &Self::Node) -> Vec<Self::Edge> {
        let mut edges = vec![];

        for next_node in self.out_nodes(node) {
            edges.push((*node, next_node));
        }

        edges
    }

    fn in_edges(&self, node: &Self::Node) -> Vec<Self::Edge> {
        let mut edges = vec![];

        for next_node in self.in_nodes(node) {
            edges.push((next_node, *node));
        }

        edges
    }

    fn edges(&self, from: &Self::Node, to: &Self::Node) -> Vec<Self::Edge> {
        vec![(*from, *to)]
    }

    fn nodes(&self, edge: &Self::Edge) -> (Self::Node, Self::Node) {
        *edge
    }

    fn weight(&self, edge: &Self::Edge) -> f64 {
        1.
    }
}
