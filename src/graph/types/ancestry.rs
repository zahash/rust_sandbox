// use std::collections::HashSet;

// use crate::graph::classic::{Classic, Edge};
// use crate::graph::{Graph, Mutable};

// #[derive(Debug, Clone, PartialEq, Hash, Eq)]
// pub struct Person {
//     pub name: String,
// }

// #[derive(Debug, Clone)]
// pub struct Relation {
//     pub r_type: RelationType,
//     pub from: Person,
//     pub to: Person,
// }

// impl Edge for Relation {
//     type V = Person;

//     fn from(&self) -> &Self::V {
//         &self.from
//     }

//     fn to(&self) -> &Self::V {
//         &self.to
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum RelationType {
//     Parent,
//     Child,
//     Sibling,
// }

// pub struct Ancestry {
//     graph: Classic<Person, Relation>,
// }

// impl<'graph> Ancestry {
//     pub fn new(people: Vec<Person>, relations: Vec<Relation>) -> Self {
//         Self {
//             graph: Classic::new(people, relations),
//         }
//     }

//     pub fn add_person(&mut self, person: Person) {
//         self.graph.add_vertex(person);
//     }

//     pub fn add_relation(&mut self, relation: Relation) {
//         self.graph.add_edge(relation);
//     }

//     pub fn parents(&'graph self, person: &'graph Person) -> HashSet<&'graph Person> {
//         self.in_relation(|relation| relation.r_type == RelationType::Parent, person)
//     }

//     pub fn siblings(&'graph self, person: &'graph Person) -> HashSet<&Person> {
//         self.in_relation(|relation| relation.r_type == RelationType::Sibling, person)
//     }

//     pub fn children(&'graph self, person: &'graph Person) -> HashSet<&Person> {
//         self.in_relation(|e| e.r_type == RelationType::Child, person)
//     }

//     fn in_relation(
//         &'graph self,
//         f: impl Fn(&Relation) -> bool,
//         person: &'graph Person,
//     ) -> HashSet<&Person> {
//         let a = &self.graph;
//         a.in_edges(&person)
//             .iter()
//             .filter(|relation| f(relation))
//             .map(|relation| relation.from())
//             .collect()
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test() {
//         let a: std::slice::Iter<'_, i32> = vec![1].iter();

//         let person = |name: &str| Person { name: name.into() };
//         let parent_child = |parent: &str, child: &str| {
//             vec![
//                 Relation {
//                     r_type: RelationType::Parent,
//                     from: person(parent),
//                     to: person(child),
//                 },
//                 Relation {
//                     r_type: RelationType::Child,
//                     from: person(child),
//                     to: person(parent),
//                 },
//             ]
//         };
//         let siblings = |p1: &str, p2: &str| {
//             vec![
//                 Relation {
//                     r_type: RelationType::Sibling,
//                     from: person(p1),
//                     to: person(p2),
//                 },
//                 Relation {
//                     r_type: RelationType::Sibling,
//                     from: person(p2),
//                     to: person(p1),
//                 },
//             ]
//         };

//         let graph = Ancestry::new(
//             vec![
//                 person("zahid"),
//                 person("reshma"),
//                 person("zahash"),
//                 person("hazash"),
//                 person("munwar"),
//                 person("bani"),
//                 person("arhan"),
//                 person("amreen"),
//                 person("mansoor"),
//                 person("ms. mansoor"),
//                 person("zaveria"),
//             ],
//             {
//                 let mut relations = vec![];

//                 relations.extend(parent_child("zahid", "zahash"));
//                 relations.extend(parent_child("reshma", "zahash"));

//                 relations.extend(parent_child("zahid", "hazash"));
//                 relations.extend(parent_child("reshma", "hazash"));

//                 relations.extend(parent_child("munwar", "arhan"));
//                 relations.extend(parent_child("bani", "arhan"));

//                 relations.extend(parent_child("munwar", "amreen"));
//                 relations.extend(parent_child("bani", "amreen"));

//                 relations.extend(parent_child("mansoor", "zaveria"));
//                 relations.extend(parent_child("ms. mansoor", "zaveria"));

//                 relations.extend(parent_child("mansoor", "zaveria"));
//                 relations.extend(parent_child("ms. mansoor", "zaveria"));

//                 relations.extend(siblings("zahid", "munwar"));
//                 relations.extend(siblings("munwar", "mansoor"));
//                 relations.extend(siblings("mansoor", "zahid"));

//                 relations
//             },
//         );

//         let a = vec![person("zahash")]
//             .into_iter()
//             .flat_map(|person| graph.parents(&person))
//             .flat_map(|parent| graph.siblings(&parent))
//             .flat_map(|sibling| graph.children(&sibling))
//             .collect::<Vec<&Person>>();

//         println!("{:?}", a);

//         // a.out_vertices();
//     }
// }
