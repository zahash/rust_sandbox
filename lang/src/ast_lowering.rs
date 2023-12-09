// use crate::ast;

pub struct LoweringError;

pub enum Ty {
    Void,

    Char,
    SignedChar,
    UnSignedChar,

    Short,
    UnSignedShort,

    Int,
    UnSigned,

    Long,
    UnSignedLong,
    LongLong,
    UnSignedLongLong,

    Float,
    Double,
    LongDouble,

    String,

    P(Box<Ty>),
    // Arr(Box<Ty<'text>>, usize),

    // Fn {
    //     return_ty: Box<Ty<'text>>,
    //     param_tys: Vec<Ty<'text>>,
    // },
    // Struct {
    //     name: &'text str,
    //     members: Vec<(&'text str, Ty<'text>)>,
    // },
    // TypeDef {
    //     name: &'text str,
    //     ty: Box<Ty<'text>>,
    // },
}
