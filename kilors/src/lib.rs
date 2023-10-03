use std::ffi::{c_char, c_int};

// #[no_mangle]
// pub extern "C" fn meme() -> c_long {
//     69420
// }

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct editorSyntax {
    filematch: *mut *mut c_char,
    keywords: *mut *mut c_char,
    singleline_comment_start: [c_char; 2],
    multiline_comment_start: [c_char; 3],
    multiline_comment_end: [c_char; 3],
    flags: c_int,
}

#[no_mangle]
pub extern "C" fn use_editorSyntax(_: &editorSyntax) {}
