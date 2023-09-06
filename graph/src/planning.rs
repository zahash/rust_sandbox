pub trait Planning {
    type Solution;

    fn solve(&self) -> Option<Self::Solution>;
}
