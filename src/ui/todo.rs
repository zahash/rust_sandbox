use log::info;
use web_sys::HtmlInputElement;
use yew::prelude::*;

#[function_component]
pub fn Todo() -> Html {
    let new_todo_ref = NodeRef::default();

    let onclick = {
        let new_todo_ref = new_todo_ref.clone();

        move |_| {
            let todo = new_todo_ref
                .cast::<HtmlInputElement>()
                .map(|input_element| input_element.value())
                .unwrap();

            info!("{}", todo);
        }
    };

    html! {
        <>
        <div>
            <label for="todo-text">{"New Item"}</label>
            <input type="text" ref={new_todo_ref.clone()} />
            <button {onclick}>{"Add"}</button>
        </div>
        </>
    }
}
