use std::{cell::RefCell, rc::Rc};

use life::Game;
use rand::Rng;
use wasm_bindgen::prelude::*;

const R: usize = 70;
const C: usize = 150;
const CELL_SIZE: usize = 10;

#[wasm_bindgen(start)]
fn run() -> Result<(), JsValue> {
    let document = web_sys::window().unwrap().document().unwrap();

    let canvas_height = R * CELL_SIZE;
    let canvas_width = C * CELL_SIZE;

    let grid = {
        let mut grid = Box::new([[false; C]; R]);
        let mut rng = rand::thread_rng();

        for r in 0..R {
            for c in 0..C {
                grid[r][c] = rng.gen::<bool>();
            }
        }

        grid
    };

    let mut game = Game::new(grid);

    let canvas = document
        .get_element_by_id("canvas")
        .unwrap()
        .unchecked_into::<web_sys::HtmlCanvasElement>();

    canvas.set_height(canvas_height as u32);
    canvas.set_width(canvas_width as u32);

    let context = canvas
        .get_context("2d")?
        .unwrap()
        .unchecked_into::<web_sys::CanvasRenderingContext2d>();

    let f = Rc::new(RefCell::new(None));
    let g = f.clone();
    *g.borrow_mut() = Some(Closure::new(move || {
        render(game.grid(), &context);
        game.update();
        request_animation_frame(f.borrow().as_ref().unwrap());
    }));

    request_animation_frame(g.borrow().as_ref().unwrap());

    Ok(())
}

fn render(grid: &[[bool; C]; R], ctx: &web_sys::CanvasRenderingContext2d) {
    let live = JsValue::from("white");
    let dead = JsValue::from("#00112c");

    for r in 0..R {
        for c in 0..C {
            ctx.begin_path();
            ctx.rect(
                (c * CELL_SIZE) as f64,
                (r * CELL_SIZE) as f64,
                CELL_SIZE as f64,
                CELL_SIZE as f64,
            );

            ctx.set_fill_style(match grid[r][c] {
                true => &live,
                false => &dead,
            });

            ctx.fill();
        }
    }
}

fn request_animation_frame(f: &Closure<dyn FnMut()>) {
    web_sys::window()
        .unwrap()
        .request_animation_frame(f.as_ref().unchecked_ref())
        .expect("should register `requestAnimationFrame` OK");
}
