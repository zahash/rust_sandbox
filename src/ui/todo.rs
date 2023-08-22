use std::ops::Deref;

use web_sys::HtmlInputElement;
use yew::prelude::*;

#[derive(Properties, PartialEq, Clone, Debug)]
pub struct TodoItemProps {
    pub id: u32,
    pub text: String,
    pub completed: bool,
}

#[function_component]
pub fn TodoItem(props: &TodoItemProps) -> Html {
    let props = props.clone();

    html! {
        <>

        if props.completed {
            <input type="checkbox" checked=true/>
        } else {
            <input type="checkbox"/>
        }

        {format!("{} {}", props.id, props.text)}

        <button>{"delete"}</button>
        </>
    }
}

#[function_component]
pub fn Todo() -> Html {
    let todos: UseStateHandle<Vec<TodoItemProps>> = use_state(|| vec![]);
    let new_todo_ref = NodeRef::default();

    let onclick = {
        let new_todo_ref = new_todo_ref.clone();
        let todos = todos.clone();

        move |_| {
            let todo = new_todo_ref
                .cast::<HtmlInputElement>()
                .map(|input_element| input_element.value())
                .unwrap();

            todos.set({
                let mut todos = Vec::from_iter(todos.deref().clone());
                todos.push(TodoItemProps {
                    id: rand::random::<u32>(),
                    text: todo,
                    completed: false,
                });
                todos
            });
        }
    };

    html! {
        <>
        <div>
            <label for="todo-text">{"New Item"}</label>
            <input type="text" ref={new_todo_ref.clone()} />
            <button {onclick}>{"Add"}</button>
        </div>

        <ul>
        {
            todos.iter().map(|todo| {
                html!{
                    <li key={todo.id}>
                        <TodoItem ..todo.clone() />
                    </li>
                }
            }).collect::<Html>()
        }
        </ul>
        </>
    }
}

pub fn run() {
    wasm_logger::init(wasm_logger::Config::default());
    yew::Renderer::<Todo>::new().render();
}
