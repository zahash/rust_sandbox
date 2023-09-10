use betterjson::json;

#[test]
fn test() {
    let text = std::fs::read_to_string("sample.json").expect("file not found");
    let data = json(&text).expect("invalid json");
    println!("{:#?}", data);
}
