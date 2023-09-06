#[derive(Debug, PartialEq, Clone)]
pub enum ComponentKind {
    Resistor(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Component {
    pub kind: ComponentKind,
    pub name: String,
    pub nodes: Vec<usize>,
}
