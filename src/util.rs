use serde::Deserialize;
use std::{fs::File, io::BufReader, path::Path};

pub fn read_json<P, T>(path: P) -> T
where
    P: AsRef<Path>,
    T: for<'de> Deserialize<'de>,
{
    let file = File::open(path).expect("File not found");
    let reader = BufReader::new(file);
    serde_json::from_reader::<BufReader<File>, T>(reader)
        .expect("structure of the json input does not match the structure expected by `T`")
}
