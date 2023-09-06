#![allow(dead_code, unused_variables)]

// import os
// import math
// import json
// import enum
// import random
// from PIL import Image, ImageDraw

// class Shape(enum.Enum):
//     POLY = 1
//     RECTANGLE = 2

// class Point:
//     def __init__(self, x, y):
//         self.x = x
//         self.y = y

//     def totuple(self):
//         return (self.x, self.y)

struct Point {
    pub x: f64,
    pub y: f64,
}

// class Settings:
//     def __init__(self, resh, resv, darkmode):
//         self.resh = resh
//         self.resv = resv
//         self.darkmode = darkmode
//         self.h_repeat = random.randint(int(resh ** 0.25), int(resh ** 0.5))
//         self.v_repeat = random.randint(int(resv ** 0.25), int(resv ** 0.5))
//         self.n_shapes = random.randint(1, 10)
//         self.line_width = 5
//         self.break_down = {Shape.POLY: 0.5, Shape.RECTANGLE: 0.5}
//         self.palette = tuple(map(tuple, rnd_palette()))
//         max_luminous_color = max(self.palette, key=luminosity)
//         min_luminous_color = min(self.palette, key=luminosity)
//         if darkmode:
//             self.back_color, self.line_color = min_luminous_color, max_luminous_color
//         else:
//             self.back_color, self.line_color = max_luminous_color, min_luminous_color

type RGB = (u8, u8, u8);

pub struct Settings {
    pub resh: usize,
    pub resv: usize,
    pub darkmode: bool,
    pub h_repeat: u8,
    pub v_repeat: u8,
    pub n_shapes: u8,
    pub line_width: u8,
    pub palette: Vec<RGB>,
}

impl Settings {
    // pub const fn max_luminous_color(&self) -> Color {
    //     self.palette.sort_by_key(f)
    // }
}

pub fn gen(settings: Settings) -> image::RgbImage {
    image::open("tests/images/jpg/progressive/cat.jpg")
        .unwrap()
        .into_rgb8()
}

// def generate(resh, resv, darkmode, seed=None):
//     random.seed(seed)
//     settings = Settings(resh, resv, darkmode)
//     repeater_resh = math.ceil(resh / settings.h_repeat)
//     repeater_resv = math.ceil(resv / settings.v_repeat)
//     repeater = Image.new("RGB", (repeater_resh, repeater_resv), settings.back_color)
//     draw = ImageDraw.Draw(repeater)

//     rnd_shapes = random.choices(
//         list(settings.break_down.keys()),
//         weights=list(settings.break_down.values()),
//         k=settings.n_shapes,
//     )
//     for shape in rnd_shapes:
//         if shape == Shape.POLY:
//             points = rnd_points(repeater_resh, repeater_resv, random.randint(3, 10))
//             points = convex_hull(points)
//             points.append(points[0])
//         elif shape == Shape.RECTANGLE:
//             diag_p1, diag_p2 = rnd_points(repeater_resh, repeater_resv, 2)
//             points = [
//                 diag_p1,
//                 Point(diag_p2.x, diag_p1.y),
//                 diag_p2,
//                 Point(diag_p1.x, diag_p2.y),
//             ]
//             points = rotate_rect(points, random.randint(0, 180))
//             points.append(points[0])

//         draw.line(
//             [p.totuple() for p in points],
//             fill=settings.line_color,
//             width=settings.line_width,
//         )
//         for p in points:
//             draw.ellipse(
//                 (
//                     p.x - (settings.line_width // 2 - 1),
//                     p.y - (settings.line_width // 2 - 1),
//                     p.x + (settings.line_width // 2 - 1),
//                     p.y + (settings.line_width // 2 - 1),
//                 ),
//                 fill=settings.line_color,
//             )
//         draw.polygon(
//             [p.totuple() for p in points], fill=random.choice(settings.palette)
//         )

//     repeater = repeater.resize((repeater_resh, repeater_resv), Image.ANTIALIAS)
//     img = Image.new(
//         "RGB",
//         (settings.h_repeat * repeater_resh, settings.v_repeat * repeater_resv),
//         settings.back_color,
//     )
//     for h_offset in range(0, img.size[0] + 1, repeater_resh):
//         for v_offset in range(0, img.size[1] + 1, repeater_resv):
//             img.paste(repeater, (h_offset, v_offset))
//     img = img.crop((0, 0, settings.resh, settings.resv))
//     return img

// def rotate_rect(points, degree):
//     return points

// def convex_hull(points):
//     if len(points) < 3:
//         return points

//     left_idx = points.index(min(points, key=lambda p: p.x))

//     hull = []
//     p1_idx = left_idx
//     while True:
//         hull.append(p1_idx)
//         p2_idx = (p1_idx + 1) % len(points)
//         for i in range(len(points)):
//             if (points[i].y - points[p1_idx].y) * (points[p2_idx].x - points[i].x) - (
//                 points[i].x - points[p1_idx].x
//             ) * (points[p2_idx].y - points[i].y) < 0:
//                 p2_idx = i
//         p1_idx = p2_idx
//         if p1_idx == left_idx:
//             break

//     return [points[idx] for idx in hull]

// def rnd_points(max_x, max_y, n_points):
//     ranges = [
//         [Point(0, 0), Point(max_x, max_y)],
//         [Point(0, 0), Point(max_x // 2, max_y // 2)],
//         [Point(max_x // 2, max_y // 2), Point(max_x, max_y)],
//         [Point(0, max_y // 2), Point(max_x // 2, max_y)],
//         [Point(max_x // 2, 0), Point(max_x, max_y // 2)],
//         [
//             Point(int(max_x ** 0.05), int(max_y * 0.05)),
//             Point(int(max_x * 0.95), int(max_y * 0.95)),
//         ],
//         [
//             Point(-int(max_x * 0.15), -int(max_y * 0.15)),
//             Point(int(max_x * 1.15), int(max_y * 1.15)),
//         ],
//     ]

//     p1, p2 = random.choice(ranges)

//     points = []
//     for _ in range(n_points):
//         rn_x = random.randint(p1.x, p2.x)
//         rn_y = random.randint(p1.y, p2.y)
//         points.append(Point(rn_x, rn_y))

//     return points

// def rnd_palette():
//     with open(os.path.join(os.path.dirname(__file__), "palettes.json"), "r") as f:
//         return random.choice(json.load(f))

// def luminosity(rgb):
//     rgb = [val / 255 for val in rgb]
//     luminosity = 0.5 * (max(rgb) + min(rgb))
//     return round(luminosity, 3)
