use std::ffi::{c_char, c_int, CStr};

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
