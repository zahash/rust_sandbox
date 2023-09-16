// <struct-or-union-specifier> ::=
//                      <struct-or-union> <identifier> { {<struct-declaration>}+ }
//                    | <struct-or-union> { {<struct-declaration>}+ }
//                    | <struct-or-union> <identifier>

// <struct-or-union> ::= struct
//                     | union

// <struct-declaration> ::= {<specifier-qualifier>}* <struct-declarator-list>

pub enum StructOrUnionSpecifier {}

pub enum StructOrUnion {
    Struct,
    Union,
}

pub enum StructDeclaration {}
