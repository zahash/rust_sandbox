use std::ffi::{c_char, c_double, c_int, CStr};

#[repr(C)]
pub struct Coords {
    pub x: c_double,
    pub y: c_double,
}

#[repr(C)]
pub struct Person {
    pub name: *const c_char,
    pub age: c_int,
}

#[no_mangle]
pub extern "C" fn use_person(_p: &Person) {}

#[no_mangle]
pub extern "C" fn use_coords(_c: &Coords) {}

#[no_mangle]
pub extern "C" fn plus_one(a: c_int) -> c_int {
    a + 1
}

#[no_mangle]
pub extern "C" fn greet(s: *const c_char) {
    let s =
        unsafe { CStr::from_ptr(s).to_str() }.expect("Failed to convert C string to Rust string");
    println!("Hello, {}!", s);
}
