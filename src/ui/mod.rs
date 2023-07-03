mod todo;

pub fn run() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::Renderer::<todo::Todo>::new().render();
}
