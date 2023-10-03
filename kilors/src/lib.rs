use libc::time_t;
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

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct hlcolor {
    r: c_int,
    g: c_int,
    b: c_int,
}

#[no_mangle]
pub extern "C" fn use_hlcolor(_: &hlcolor) {}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct editorConfig {
    /// Cursor x position in characters
    cx: c_int,

    /// Cursor y position in characters
    cy: c_int,

    /// Offset of row displayed.
    rowoff: c_int,

    /// Offset of column displayed.
    coloff: c_int,

    /// Number of rows that we can show
    screenrows: c_int,

    /// Number of cols that we can show
    screencols: c_int,

    /// Number of rows
    numrows: c_int,

    /// Is terminal raw mode enabled?
    rawmode: c_int,

    /// Rows
    row: *mut erow,

    ///  File modified but not saved.
    dirty: c_int,

    /// Currently open filename
    filename: *mut c_char,

    statusmsg: [c_char; 80],

    statusmsg_time: time_t,

    /// Current syntax highlight, or NULL.
    syntax: *mut editorSyntax,
}

#[no_mangle]
pub extern "C" fn use_editorConfig(_: &editorConfig) {}
