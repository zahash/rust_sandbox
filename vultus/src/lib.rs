use rayon::prelude::*;
use std::{fs::read_to_string, path::Path};

pub fn corpus<P>(fpaths: &[P]) -> Vec<String>
where
    P: AsRef<Path>,
{
    fpaths
        .iter()
        .map(|fpath| {
            read_to_string(fpath)
                .unwrap()
                .split_whitespace()
                .collect::<Vec<&str>>()
                .join(" ")
        })
        .collect()
}

pub fn tf(term: &str, doc: &str) -> f64 {
    let nmatches = doc.matches(term).count();
    let nwords = doc.split_whitespace().count();

    match nwords {
        0 => 0.,
        _ => f64_div(nmatches, nwords),
    }
}

pub fn df(term: &str, docs: &[String]) -> usize {
    docs.par_iter().filter(|doc| doc.contains(term)).count()
}

pub fn idf(term: &str, docs: &[String]) -> f64 {
    f64_div(docs.len(), 1 + df(term, docs)).log2()
}

fn f64_div(n: usize, d: usize) -> f64 {
    let gcd = gcd(n, d);
    let n = n / gcd;
    let d = d / gcd;
    n as f64 / d as f64
}

const fn gcd(a: usize, b: usize) -> usize {
    match b {
        0 => a,
        _ => gcd(b, a % b),
    }
}

// let dir = Path::new("assets/vultus");

// let corpus = vultus::corpus(&[dir.join("a.txt"), dir.join("b.txt"), dir.join("c.txt")]);

// println!("{:?}", corpus);

// let term = "asdf";
// let idf = vultus::idf(term, &corpus);

// let tf_idf = |doc| vultus::tf(term, doc) * idf;

// println!("{}", vultus::idf("asdf", &corpus));
