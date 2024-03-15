// use std::fmt::Display;

// use floem::id::Id;
// use floem::reactive::create_signal;
// use floem::view::View;
// use floem::views::{h_stack, label, v_stack, Decorators, Label};
// use floem::widgets::button;
// use reactivate::Reactive;

// // pub fn rlabel<S: Display + Clone + 'static>(label: Reactive<S>) -> Label {
// //     let id = Id::next();

// //     label.add_observer(|val| id.update_state(val, false));

// //     // let initial_label = create_updater(
// //     //     move || label().to_string(),
// //     //     move |new_label| id.update_state(new_label, false),
// //     // );
// //     // Label::new(id, label.value().to_string())
// // }

// fn app_view() -> impl View {
//     // Create a reactive signal with a counter value, defaulting to 0
//     let (counter, set_counter) = create_signal(0);

//     let count = Reactive::new(0);

//     // // Create a vertical layout
//     // v_stack((
//     //     // The counter value updates automatically, thanks to reactivity
//     //     label(move || format!("Value: {}", counter.get())),
//     //     // Create a horizontal layout
//     //     h_stack((
//     //         button(|| "Increment").on_click_stop(move |_| {
//     //             set_counter.update(|value| *value += 1);
//     //         }),
//     //         button(|| "Decrement").on_click_stop(move |_| {
//     //             set_counter.update(|value| *value -= 1);
//     //         }),
//     //     )),
//     // ))

//     // v_stack((
//     //     // The counter value updates automatically, thanks to reactivity
//     //     label(count.clone()),
//     //     // Create a horizontal layout
//     //     h_stack((
//     //         button(|| "Increment").on_click_stop({
//     //             let count = count.clone();
//     //             move |_| count.update(|c| c + 1)
//     //         }),
//     //         button(|| "Decrement").on_click_stop({
//     //             let count = count.clone();
//     //             move |_| count.update(|c| c - 1)
//     //         }),
//     //     )),
//     // ))
// }

// pub fn run() {
//     floem::launch(app_view);
// }

// struct Foo;

// impl View for Foo {
//     fn view_data(&self) -> &floem::view::ViewData {
//         todo!()
//     }

//     fn view_data_mut(&mut self) -> &mut floem::view::ViewData {
//         todo!()
//     }
// }
