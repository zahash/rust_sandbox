use crate::ConstantExpr;

pub enum EnumSpecifier<'text> {
    NamedEnum(&'text str, EnumeratorList<'text>),
    AnonymousEnum(EnumeratorList<'text>),
    EnumDeclarator(&'text str),
}

pub enum EnumeratorList<'text> {
    Single(Enumerator<'text>),
    Multiple(Box<EnumeratorList<'text>>, Enumerator<'text>),
}

pub enum Enumerator<'text> {
    Implicit(&'text str),
    Explicit(&'text str, ConstantExpr<'text>),
}
