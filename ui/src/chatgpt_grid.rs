#![allow(unused_variables)]

use eframe::egui;
use egui::{Color32, Pos2, Vec2, Window};

struct GridApp {
    rows: usize,
    cols: usize,
    cell_size: f32,
    grid: Vec<Vec<bool>>,
}

impl Default for GridApp {
    fn default() -> Self {
        Self {
            rows: 20,
            cols: 20,
            cell_size: 10.,
            grid: vec![vec![false; 20]; 20],
        }
    }
}

impl eframe::App for GridApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        let screen_rect = ctx.screen_rect();

        egui::CentralPanel::default().show(ctx, |ui| {
            Window::new("Boolean Grid")
                .collapsible(false)
                .scroll2([false, false])
                .resizable(false)
                .show(ctx, |ui| {
                    ui.label("Click on cells to toggle the boolean value:");
                    ui.separator();

                    // let canvas = ui.canvas(&screen_rect);
                    let (response, painter) = ui.allocate_painter(
                        Vec2 {
                            x: screen_rect.height(),
                            y: screen_rect.width(),
                        },
                        egui::Sense::click(),
                    );
                    painter.rect_filled(screen_rect, 0.0, Color32::from_black_alpha(64));

                    let mut row_height = self.cell_size;
                    let mut x = 0.0;
                    let mut y = 0.0;
                    for row in &mut self.grid {
                        let mut col_width = self.cell_size;
                        for cell in row {
                            let rect = egui::Rect::from_min_size(
                                Pos2::new(x, y),
                                Vec2::new(col_width, row_height),
                            );
                            let mut color = Color32::WHITE;
                            if *cell {
                                color = Color32::from_black_alpha(128);
                            }
                            painter.rect_filled(rect, 0.0, color);

                            if let Some(pos) = response.hover_pos() {
                                if rect.contains(pos) {
                                    if response.clicked() {
                                        *cell = true;
                                    }
                                    if response.secondary_clicked() {
                                        *cell = false;
                                    }

                                    painter.rect(rect, 0.0, Color32::WHITE, (1., Color32::WHITE));
                                }
                            }

                            x += col_width;
                            col_width = self.cell_size;
                        }
                        x = 0.0;
                        y += row_height;
                        row_height = self.cell_size;
                    }
                });
        });

        self.rows = ((screen_rect.height() / self.cell_size) as usize).max(1);
        self.cols = ((screen_rect.width() / self.cell_size) as usize).max(1);

        if self.grid.len() != self.rows {
            self.grid.resize(self.rows, vec![false; self.cols]);
        }
        for row in &mut self.grid {
            row.resize(self.cols, false);
        }
    }
}

pub fn run() {
    let options = eframe::NativeOptions {
        drag_and_drop_support: false,
        initial_window_size: Some(egui::vec2(600.0, 400.0)),
        ..Default::default()
    };
    eframe::run_native(
        "Egui Grid Example",
        options,
        Box::new(|_| Box::new(GridApp::default())),
    )
    .unwrap();
}
