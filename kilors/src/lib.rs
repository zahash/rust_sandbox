use std::ffi::*;

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

#[repr(C)]
#[allow(non_camel_case_types)]
/// This structure represents a single line of the file we are editing.
pub struct erow {
    /// Row index in the file, zero-based.
    idx: c_int,

    /// Size of the row, excluding the null term.
    size: c_int,

    /// Size of the rendered row.
    rsize: c_int,

    /// Row content.
    chars: *mut c_char,

    /// Row content "rendered" for screen (for TABs).
    render: *mut c_char,

    /// Syntax highlight type for each character in render.
    hl: *mut c_uchar,

    /// Row had open comment at end in last syntax highlight check.
    hl_oc: c_int,
}

#[no_mangle]
pub extern "C" fn use_erow(_: &erow) {}
