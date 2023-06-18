#[derive(Clone, PartialEq, Eq)]
pub struct Path<Node> {
    pub node: Node,
    pub parent: Box<Option<Path<Node>>>,
}

impl<Node> Path<Node> {
    pub fn into_list(self) -> Vec<Node> {
        let mut result = vec![];

        if let Some(parent) = *self.parent {
            result.append(&mut parent.into_list())
        }

        result.push(self.node);

        result
    }
}

