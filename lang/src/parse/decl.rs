// <declaration-specifier> ::= <storage-class-specifier>
//                           | <type-specifier>
//                           | <type-qualifier>

use crate::{TypeQualifier, TypeSpecifier};

pub enum DeclarationSpecifier<'text> {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier<'text>),
    TypeQualifier(TypeQualifier),
}

pub enum StorageClassSpecifier {
    Auto,
    Register,
    Static,
    Extern,
    TypeDef,
}
