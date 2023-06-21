#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path<Node> {
    pub node: Node,
    pub parent: Box<Option<Path<Node>>>,
}

impl<Node> From<Path<Node>> for Vec<Node> {
    fn from(path: Path<Node>) -> Self {
        let mut result = vec![];

        if let Some(parent) = *path.parent {
            result.append(&mut parent.into())
        }

        result.push(path.node);

        result
    }
}
