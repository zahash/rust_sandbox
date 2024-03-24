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

#[repr(C)]
pub enum MyEnum {
    A(u32),
    B(f32, u64),
    C { x: u32, y: u8 },
    D,
}

#[no_mangle]
pub extern "C" fn use_person(_p: &Person) {
    // do something with person
}

#[no_mangle]
pub extern "C" fn use_coords(_c: &Coords) {
    // do something with coords
}

#[no_mangle]
pub extern "C" fn use_my_enum(_e: &MyEnum) {
    // do something with my enum
}

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
