use eframe::egui;

struct BinaryGrid<const R: usize, const C: usize> {
    data: Box<[[bool; C]; R]>,
}

impl<const R: usize, const C: usize> eframe::App for BinaryGrid<R, C> {
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {

            ui.vertical(|ui| {
                
            });

        });
    }
}
