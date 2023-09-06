use crate::Graph;

pub struct Query<'graph, V, E> {
    graph: Box<&'graph dyn Graph<V = V, E = E>>,
}

impl<'graph, V, E> Query<'graph, V, E> {
    pub fn new(graph: &'graph impl Graph<V = V, E = E>) -> Self {
        Self {
            graph: Box::new(graph),
        }
    }

    pub fn vertices(&self) -> QueryIter<V> {
        QueryIter {
            items: self.graph.all_vertices(),
        }
    }

    pub fn edges(&self) -> QueryIter<E> {
        QueryIter {
            items: self.graph.all_edges(),
        }
    }
}

#[allow(dead_code)]
pub struct VertexIter<V> {
    vertices: Vec<V>,
}

#[allow(dead_code)]
pub struct EdgeIter<E> {
    edges: Vec<E>,
}

pub struct QueryIter<T> {
    items: Vec<T>,
}

impl<T> Iterator for QueryIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.items.pop()
    }
}

impl<T> QueryIter<T> {
    // pub fn in_edges<E>(&self) -> QueryIter<E> {
    //     QueryIter { items: todo!() }
    // }
}

#[cfg(test)]
mod tests {
    // use super::*;
    use crate::AdjacencyList;
    use std::collections::HashMap;

    #[test]
    fn test_query() {
        let _graph = AdjacencyList::new(HashMap::from([
            ("ATL", vec!["BOS", "DFW", "MOB"]),
            ("BOS", vec!["ATL", "DFW"]),
            ("MOB", vec!["ATL"]),
            ("AUS", vec!["DFW", "HOU", "SAT"]),
            ("HOU", vec!["AUS", "DFW", "SAT"]),
            ("SAT", vec!["AUS", "HOU"]),
            ("LAX", vec!["DFW", "SFO"]),
            ("LIT", vec!["DFW"]),
            ("MSY", vec!["DFW"]),
            ("OKC", vec!["DFW"]),
            ("SHV", vec!["DFW"]),
            ("SFO", vec!["DFW", "LA"]),
            (
                "DFW",
                vec![
                    "ATL", "AUS", "BOS", "HOU", "LAX", "LIT", "MSY", "OKC", "SHV", "SFO",
                ],
            ),
        ]));

        // let query = Query::new(&graph)
        //     .vertices()
        //     .filter(|v| v == &"asdf")
        //     .in_edges()
        //     .filter(|e| e.r_type = "parent")
        //     .map(|e| e.from());

        // let query = Query::new(&graph)
        //     .edges()
        //     .filter(|(from, to)| from == &"asdf")
        //     .in_vertices()
        //     .filter(|v| v == &"blah")
        //     ;
    }
}
