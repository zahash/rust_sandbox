use std::ffi::c_int;

#[no_mangle]
pub extern "C" fn plus_one(a: c_int) -> c_int {
    a + 1
}
