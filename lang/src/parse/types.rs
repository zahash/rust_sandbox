use crate::EnumSpecifier;

pub enum SpecifierQualifier<'text> {
    TypeSpecifier(TypeSpecifier<'text>),
    TypeQualifier(TypeQualifier),
}

pub enum TypeSpecifier<'text> {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    UnSigned,
    EnumSpecifier(EnumSpecifier<'text>),
    TypeDefName(&'text str),
}

pub enum TypeQualifier {
    Const,
    Volatile,
}
