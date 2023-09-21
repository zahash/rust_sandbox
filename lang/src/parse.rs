use std::{
    fmt::{self, Display, Formatter},
    ops::Deref,
};

use crate::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct TranslationUnit<'text>(pub Vec<ExternalDeclaration<'text>>);

fn parse_translation_unit<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(TranslationUnit<'text>, usize), ParseError> {
    todo!()
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExternalDeclaration<'text> {
    FunctionDefinition(FunctionDefinition<'text>),
    Declaration(Declaration<'text>),
}

fn parse_external_declaration<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ExternalDeclaration<'text>, usize), ParseError> {
    todo!()
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition<'text> {
    pub return_type: Vec<DeclarationSpecifier<'text>>,
    pub declarator: Declarator<'text>,
    pub body: CompoundStmt<'text>,
}

fn parse_function_definition<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(FunctionDefinition<'text>, usize), ParseError> {
    todo!()
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructOrUnionSpecifier<'text> {
    Named(StructOrUnion, &'text str, Vec<StructDeclaration<'text>>),
    Anonymous(Vec<StructDeclaration<'text>>),
    ForwardDeclaration(&'text str),
}

fn parse_struct_or_union_specifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(StructOrUnionSpecifier<'text>, usize), ParseError> {
    todo!()
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructOrUnion {
    Struct,
    Union,
}

fn parse_struct_or_union<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(StructOrUnion, usize), ParseError> {
    todo!()
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclaration<'text> {
    pub specifier_qualifiers: Vec<SpecifierQualifier<'text>>,
    pub declarators: Vec<StructDeclarator<'text>>,
}

fn parse_struct_declaration<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(StructDeclaration<'text>, usize), ParseError> {
    todo!()
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructDeclarator<'text> {
    Declarator(Declarator<'text>),
    DeclaratorWithBitField(Declarator<'text>, ConstantExpr<'text>),
    BitField(ConstantExpr<'text>),
}

fn parse_struct_declarator<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(StructDeclarator<'text>, usize), ParseError> {
    todo!()
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declarator<'text> {
    pub pointer: Option<Pointer>,
    pub declarator: DirectDeclarator<'text>,
}

fn parse_declarator<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Declarator<'text>, usize), ParseError> {
    let (pointer, pos) = maybe(tokens, pos, parse_pointer);
    let (dd, pos) = parse_direct_declarator(tokens, pos)?;

    Ok((
        Declarator {
            pointer,
            declarator: dd,
        },
        pos,
    ))
}

#[derive(Debug, PartialEq, Clone)]
pub enum DirectDeclarator<'text> {
    Ident(&'text str, Option<DirectDeclaratorTail<'text>>),
    Parens(Box<Declarator<'text>>, Option<DirectDeclaratorTail<'text>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum DirectDeclaratorTail<'text> {
    Array(
        Option<ConstantExpr<'text>>,
        Box<Option<DirectDeclaratorTail<'text>>>,
    ),
    Function(
        ParameterTypeList<'text>,
        Box<Option<DirectDeclaratorTail<'text>>>,
    ),
    Parameters(Vec<&'text str>, Box<Option<DirectDeclaratorTail<'text>>>),
}

fn parse_direct_declarator<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(DirectDeclarator<'text>, usize), ParseError> {
    fn parse_ident<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectDeclarator<'text>, usize), ParseError> {
        let Some(Token::Ident(ident)) = tokens.get(pos) else {
            return Err(ParseError::ExpectedIdentifier(pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, parse_direct_declarator_tail);

        Ok((DirectDeclarator::Ident(&ident, dd_tail), pos))
    }

    fn parse_parens<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectDeclarator<'text>, usize), ParseError> {
        let Some(Token::LParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLParen(pos));
        };

        let (declarator, pos) = parse_declarator(tokens, pos + 1)?;

        let Some(Token::RParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRParen(pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, parse_direct_declarator_tail);

        Ok((DirectDeclarator::Parens(Box::new(declarator), dd_tail), pos))
    }

    combine_parsers(
        tokens,
        pos,
        &[Box::new(parse_ident), Box::new(parse_parens)],
        "cannot parse direct declarator",
    )
}

fn parse_direct_declarator_tail<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(DirectDeclaratorTail<'text>, usize), ParseError> {
    fn parse_array<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectDeclaratorTail<'text>, usize), ParseError> {
        let Some(Token::LSquare) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLSquare(pos));
        };

        let (expr, pos) = maybe(tokens, pos + 1, parse_constant_expr);

        let Some(Token::RSquare) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRSquare(pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, parse_direct_declarator_tail);

        Ok((DirectDeclaratorTail::Array(expr, Box::new(dd_tail)), pos))
    }

    fn parse_function<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectDeclaratorTail<'text>, usize), ParseError> {
        let Some(Token::LParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLParen(pos));
        };

        let (list, pos) = parse_parameter_type_list(tokens, pos + 1)?;

        let Some(Token::RParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRParen(pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, parse_direct_declarator_tail);

        Ok((DirectDeclaratorTail::Function(list, Box::new(dd_tail)), pos))
    }

    fn parse_parameters<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectDeclaratorTail<'text>, usize), ParseError> {
        let Some(Token::LParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLParen(pos));
        };

        let mut idents = vec![];
        let mut pos = pos + 1;
        while let Some(Token::Ident(ident)) = tokens.get(pos) {
            pos += 1;
            idents.push(*ident);
        }

        let Some(Token::RParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRParen(pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, parse_direct_declarator_tail);

        Ok((
            DirectDeclaratorTail::Parameters(idents, Box::new(dd_tail)),
            pos,
        ))
    }

    combine_parsers(
        tokens,
        pos,
        &[
            Box::new(parse_array),
            Box::new(parse_function),
            Box::new(parse_parameters),
        ],
        "cannot parse direct declarator tail",
    )
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeName<'text> {
    pub specifier_qualifiers: Vec<SpecifierQualifier<'text>>,
    pub abstract_declarator: Option<AbstractDeclarator<'text>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParameterTypeList<'text> {
    ParameterList(Vec<ParameterDeclaration<'text>>),
    VariadicParameterList(Vec<ParameterDeclaration<'text>>),
}

fn parse_parameter_type_list<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ParameterTypeList<'text>, usize), ParseError> {
    let (declarations, pos) = many(tokens, pos, parse_parameter_declaration, Some(Token::Comma));

    if let Some(Token::DotDotDot) = tokens.get(pos) {
        return Ok((
            ParameterTypeList::VariadicParameterList(declarations),
            pos + 1,
        ));
    }

    Ok((ParameterTypeList::ParameterList(declarations), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParameterDeclaration<'text> {
    WithDeclarator(Vec<DeclarationSpecifier<'text>>, Declarator<'text>),
    WithAbstractDeclarator(Vec<DeclarationSpecifier<'text>>, AbstractDeclarator<'text>),
    OnlySpecifiers(Vec<DeclarationSpecifier<'text>>),
}

fn parse_parameter_declaration<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ParameterDeclaration<'text>, usize), ParseError> {
    let (dss, pos) = many(tokens, pos, parse_declaration_specifier, None);
    if dss.is_empty() {
        return Err(ParseError::SyntaxError(
            pos,
            "parse_declaration: expected atleast one declaration specifier",
        ));
    }

    let (d, pos) = maybe(tokens, pos, parse_declarator);
    let (ad, pos) = maybe(tokens, pos, parse_abstract_declarator);

    match (d, ad) {
        (None, None) => Ok((ParameterDeclaration::OnlySpecifiers(dss), pos)),
        (None, Some(ad)) => Ok((ParameterDeclaration::WithAbstractDeclarator(dss, ad), pos)),
        (Some(d), None) => Ok((ParameterDeclaration::WithDeclarator(dss, d), pos)),
        (Some(_), Some(_)) => Err(ParseError::SyntaxError(
            pos,
            "cannot parse parameter declaration",
        )),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AbstractDeclarator<'text> {
    Pointer(Pointer),
    PointerWithDirect(Pointer, DirectAbstractDeclarator<'text>),
    Direct(DirectAbstractDeclarator<'text>),
}

fn parse_abstract_declarator<'text>(
    tokens: &[Token<'text>],
    mut pos: usize,
) -> Result<(AbstractDeclarator<'text>, usize), ParseError> {
    let (pointer, pos) = maybe(tokens, pos, parse_pointer);
    let (dad, pos) = maybe(tokens, pos, parse_direct_abstract_declarator);

    match (pointer, dad) {
        (None, None) => Err(ParseError::SyntaxError(
            pos,
            "cannot parse abstract declarator",
        )),
        (None, Some(d)) => Ok((AbstractDeclarator::Direct(d), pos)),
        (Some(p), None) => Ok((AbstractDeclarator::Pointer(p), pos)),
        (Some(p), Some(d)) => Ok((AbstractDeclarator::PointerWithDirect(p, d), pos)),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DirectAbstractDeclarator<'text> {
    Parens(
        Box<AbstractDeclarator<'text>>,
        Option<DirectAbstractDeclaratorTail<'text>>,
    ),
    Array(
        Option<ConstantExpr<'text>>,
        Option<DirectAbstractDeclaratorTail<'text>>,
    ),
    Function(
        Option<ParameterTypeList<'text>>,
        Option<DirectAbstractDeclaratorTail<'text>>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
pub enum DirectAbstractDeclaratorTail<'text> {
    Array(
        Option<ConstantExpr<'text>>,
        Box<Option<DirectAbstractDeclaratorTail<'text>>>,
    ),
    Function(
        Option<ParameterTypeList<'text>>,
        Box<Option<DirectAbstractDeclaratorTail<'text>>>,
    ),
}

fn parse_direct_abstract_declarator<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(DirectAbstractDeclarator<'text>, usize), ParseError> {
    fn parse_parens<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectAbstractDeclarator<'text>, usize), ParseError> {
        let Some(Token::LParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLParen(pos));
        };

        let (declarator, pos) = parse_abstract_declarator(tokens, pos + 1)?;

        let Some(Token::RParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRParen(pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, parse_direct_abstract_declarator_tail);

        Ok((
            DirectAbstractDeclarator::Parens(Box::new(declarator), dad_tail),
            pos,
        ))
    }

    fn parse_array<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectAbstractDeclarator<'text>, usize), ParseError> {
        let Some(Token::LSquare) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLSquare(pos));
        };

        let (expr, pos) = maybe(tokens, pos + 1, parse_constant_expr);

        let Some(Token::RSquare) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRSquare(pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, parse_direct_abstract_declarator_tail);

        Ok((DirectAbstractDeclarator::Array(expr, dad_tail), pos))
    }

    fn parse_function<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectAbstractDeclarator<'text>, usize), ParseError> {
        let Some(Token::LParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLParen(pos));
        };

        let (parameter_type_list, pos) = maybe(tokens, pos + 1, parse_parameter_type_list);

        let Some(Token::RParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRParen(pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, parse_direct_abstract_declarator_tail);

        Ok((
            DirectAbstractDeclarator::Function(parameter_type_list, dad_tail),
            pos,
        ))
    }

    combine_parsers(
        tokens,
        pos,
        &[
            Box::new(parse_parens),
            Box::new(parse_array),
            Box::new(parse_function),
        ],
        "cannot parse direct abstract declarator",
    )
}

fn parse_direct_abstract_declarator_tail<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(DirectAbstractDeclaratorTail<'text>, usize), ParseError> {
    fn parse_array<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectAbstractDeclaratorTail<'text>, usize), ParseError> {
        // let (dad, pos) = maybe(tokens, pos, parse_direct_abstract_declarator);

        let Some(Token::LSquare) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLSquare(pos));
        };

        let (expr, pos) = maybe(tokens, pos + 1, parse_constant_expr);

        let Some(Token::RSquare) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRSquare(pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, parse_direct_abstract_declarator_tail);

        Ok((
            DirectAbstractDeclaratorTail::Array(expr, Box::new(dad_tail)),
            pos,
        ))
    }

    fn parse_function<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(DirectAbstractDeclaratorTail<'text>, usize), ParseError> {
        // let (dad, pos) = maybe(tokens, pos, parse_direct_abstract_declarator);

        let Some(Token::LParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLParen(pos));
        };

        let (parameter_type_list, pos) = maybe(tokens, pos + 1, parse_parameter_type_list);

        let Some(Token::RParen) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRParen(pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, parse_direct_abstract_declarator_tail);

        Ok((
            DirectAbstractDeclaratorTail::Function(parameter_type_list, Box::new(dad_tail)),
            pos,
        ))
    }

    combine_parsers(
        tokens,
        pos,
        &[Box::new(parse_array), Box::new(parse_function)],
        "cannot parse direct abstract declarator tail",
    )
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration<'text> {
    pub declaration_specifiers: Vec<DeclarationSpecifier<'text>>,
    pub init_declarators: Vec<InitDeclarator<'text>>,
}

fn parse_declaration<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Declaration<'text>, usize), ParseError> {
    let (declaration_specifiers, pos) = many(tokens, pos, parse_declaration_specifier, None);
    if declaration_specifiers.is_empty() {
        return Err(ParseError::SyntaxError(
            pos,
            "parse_declaration: expected atleast one declaration specifier",
        ));
    }

    let (init_declarators, pos) = many(tokens, pos, parse_init_declarator, Some(Token::Comma));

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    Ok((
        Declaration {
            declaration_specifiers,
            init_declarators,
        },
        pos + 1,
    ))
}

#[derive(Debug, PartialEq, Clone)]
pub enum InitDeclarator<'text> {
    Declared(Declarator<'text>),
    Initialized(Declarator<'text>, Initializer<'text>),
}

fn parse_init_declarator<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(InitDeclarator<'text>, usize), ParseError> {
    let (declarator, pos) = parse_declarator(tokens, pos)?;

    let Some(Token::Equals) = tokens.get(pos) else {
        return Ok((InitDeclarator::Declared(declarator), pos));
    };

    let (initializer, pos) = parse_initializer(tokens, pos + 1)?;
    Ok((InitDeclarator::Initialized(declarator, initializer), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Initializer<'text> {
    Assignment(AssignmentExpr<'text>),
    InitializerList(Vec<Initializer<'text>>),
}

fn parse_initializer<'text>(
    tokens: &[Token<'text>],
    mut pos: usize,
) -> Result<(Initializer<'text>, usize), ParseError> {
    if let Some(Token::LCurly) = tokens.get(pos) {
        let mut initializers = Vec::new();
        pos += 1;

        while let Some(token) = tokens.get(pos) {
            if token == &Token::RCurly {
                pos += 1;
                break;
            }

            let (initializer, next_pos) = parse_initializer(tokens, pos)?;
            pos = next_pos;

            initializers.push(initializer);

            match tokens.get(pos) {
                Some(Token::Comma) => pos += 1,
                Some(Token::RCurly) => {
                    pos += 1;
                    break;
                }
                _ => return Err(ParseError::ExpectedRCurly(pos)),
            }
        }

        return Ok((Initializer::InitializerList(initializers), pos));
    }

    let (expr, pos) = parse_assignment_expr(tokens, pos)?;
    Ok((Initializer::Assignment(expr), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationSpecifier<'text> {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier<'text>),
    TypeQualifier(TypeQualifier),
}

fn parse_declaration_specifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(DeclarationSpecifier<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        &[
            Box::new(parse_storage_class_specifier),
            Box::new(parse_type_specifier),
            Box::new(parse_type_qualifier),
        ],
        "cannot parse declaration specifier",
    )
}

#[derive(Debug, PartialEq, Clone)]
pub enum StorageClassSpecifier {
    Auto,
    Register,
    Static,
    Extern,
    TypeDef,
}

fn parse_storage_class_specifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(StorageClassSpecifier, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Keyword("auto")) => Ok((StorageClassSpecifier::Auto, pos + 1)),
        Some(Token::Keyword("register")) => Ok((StorageClassSpecifier::Register, pos + 1)),
        Some(Token::Keyword("static")) => Ok((StorageClassSpecifier::Static, pos + 1)),
        Some(Token::Keyword("extern")) => Ok((StorageClassSpecifier::Extern, pos + 1)),
        Some(Token::Keyword("typedef")) => Ok((StorageClassSpecifier::TypeDef, pos + 1)),
        _ => Err(ParseError::ExpectedKeyword(
            "expected `auto` or `register` or `static` or `extern` or `typedef`",
            pos,
        )),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SpecifierQualifier<'text> {
    TypeSpecifier(TypeSpecifier<'text>),
    TypeQualifier(TypeQualifier),
}

fn parse_specifier_qualifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(SpecifierQualifier<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        &[
            Box::new(parse_type_specifier),
            Box::new(parse_type_qualifier),
        ],
        "cannot parse specifier qualifier",
    )
}

#[derive(Debug, PartialEq, Clone)]
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

fn parse_type_specifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(TypeSpecifier<'text>, usize), ParseError> {
    fn parse_basic_type_specifier<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(TypeSpecifier<'text>, usize), ParseError> {
        match tokens.get(pos) {
            Some(Token::Keyword("void")) => Ok((TypeSpecifier::Void, pos + 1)),
            Some(Token::Keyword("char")) => Ok((TypeSpecifier::Char, pos + 1)),
            Some(Token::Keyword("short")) => Ok((TypeSpecifier::Short, pos + 1)),
            Some(Token::Keyword("int")) => Ok((TypeSpecifier::Int, pos + 1)),
            Some(Token::Keyword("long")) => Ok((TypeSpecifier::Long, pos + 1)),
            Some(Token::Keyword("float")) => Ok((TypeSpecifier::Float, pos + 1)),
            Some(Token::Keyword("double")) => Ok((TypeSpecifier::Double, pos + 1)),
            Some(Token::Keyword("signed")) => Ok((TypeSpecifier::Signed, pos + 1)),
            Some(Token::Keyword("unsigned")) => Ok((TypeSpecifier::UnSigned, pos + 1)),
            _ => Err(ParseError::ExpectedKeyword(
                "expected `void` or `char` or `short` or `int` or `long` or `float` or `double` or `signed` or `unsigned`",
                pos,
            )),
        }
    }

    fn parse_typedef_name<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(TypeSpecifier<'text>, usize), ParseError> {
        match tokens.get(pos) {
            Some(Token::Ident(ident)) => Ok((TypeSpecifier::TypeDefName(ident), pos + 1)),
            _ => Err(ParseError::ExpectedIdentifier(pos)),
        }
    }

    combine_parsers(
        tokens,
        pos,
        &[
            Box::new(parse_basic_type_specifier),
            Box::new(parse_enum_specifier),
            // Box::new(parse_typedef_name),
        ],
        "cannot parse type specifier",
    )
}

#[derive(Debug, PartialEq, Clone)]
pub struct Pointer {
    pub qualifiers: Vec<TypeQualifier>,
    pub next: Option<Box<Pointer>>,
}

pub struct PointerIter<'pointer> {
    pointer: Option<&'pointer Pointer>,
}

impl<'pointer> IntoIterator for &'pointer Pointer {
    type Item = &'pointer [TypeQualifier];
    type IntoIter = PointerIter<'pointer>;

    fn into_iter(self) -> Self::IntoIter {
        PointerIter {
            pointer: Some(self),
        }
    }
}

impl<'pointer> Iterator for PointerIter<'pointer> {
    type Item = &'pointer [TypeQualifier];

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(pointer) = &self.pointer {
            let qualifiers = pointer.qualifiers.as_slice();
            self.pointer = pointer.next.as_ref().map(|boxed| boxed.deref());
            return Some(qualifiers);
        }

        None
    }
}

fn parse_pointer<'text>(
    tokens: &[Token<'text>],
    mut pos: usize,
) -> Result<(Pointer, usize), ParseError> {
    let Some(Token::Asterisk) = tokens.get(pos) else {
        return Err(ParseError::SyntaxError(pos, "parse_pointer: expected `*`"));
    };
    pos += 1;

    let mut pointer = Pointer {
        qualifiers: vec![],
        next: None,
    };

    loop {
        match parse_type_qualifier(tokens, pos) {
            Err(_) => break,
            Ok((qualifier, next_pos)) => {
                pointer.qualifiers.push(qualifier);
                pos = next_pos;
            }
        }
    }

    match parse_pointer(tokens, pos) {
        Ok((next_pointer, next_pos)) => {
            pos = next_pos;
            pointer.next = Some(Box::new(next_pointer));
            Ok((pointer, pos))
        }
        Err(_) => Ok((pointer, pos)),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeQualifier {
    Const,
    Volatile,
}

fn parse_type_qualifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(TypeQualifier, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Keyword("const")) => Ok((TypeQualifier::Const, pos + 1)),
        Some(Token::Keyword("volatile")) => Ok((TypeQualifier::Volatile, pos + 1)),
        _ => Err(ParseError::ExpectedKeyword("`const` or `volatile`", pos)),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum EnumSpecifier<'text> {
    Named(&'text str, Vec<Enumerator<'text>>),
    Anonymous(Vec<Enumerator<'text>>),
    ForwardDeclaration(&'text str),
}

fn parse_enum_specifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(EnumSpecifier<'text>, usize), ParseError> {
    let Some(Token::Keyword("enum")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("enum", pos));
    };

    fn parse_enum_body<'text>(
        tokens: &[Token<'text>],
        pos: usize,
    ) -> Result<(Vec<Enumerator<'text>>, usize), ParseError> {
        let Some(Token::LCurly) = tokens.get(pos) else {
            return Err(ParseError::ExpectedLCurly(pos));
        };

        let (list, pos) = many(tokens, pos + 1, parse_enumerator, Some(Token::Comma));

        let Some(Token::RCurly) = tokens.get(pos) else {
            return Err(ParseError::ExpectedRCurly(pos).into());
        };

        return Ok((list, pos + 1));
    }

    if let Some(Token::Ident(ident)) = tokens.get(pos + 1) {
        return match parse_enum_body(tokens, pos + 2) {
            Ok((enumerators, pos)) => Ok((EnumSpecifier::Named(ident, enumerators), pos)),
            Err(_) => Ok((EnumSpecifier::ForwardDeclaration(ident), pos + 2)),
        };
    }

    let (list, pos) = parse_enum_body(tokens, pos + 1)?;
    Ok((EnumSpecifier::Anonymous(list), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Enumerator<'text> {
    Implicit(&'text str),
    Explicit(&'text str, ConstantExpr<'text>),
}

fn parse_enumerator<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Enumerator<'text>, usize), ParseError> {
    let Some(Token::Ident(ident)) = tokens.get(pos) else {
        return Err(ParseError::ExpectedIdentifier(pos));
    };

    let Some(Token::Equals) = tokens.get(pos + 1) else {
        return Ok((Enumerator::Implicit(ident), pos + 1));
    };

    let (expr, pos) = parse_constant_expr(tokens, pos + 2)?;

    Ok((Enumerator::Explicit(ident, expr), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'text> {
    Labeled(LabeledStmt<'text>),
    EmptyStmt,
    Expr(Expr<'text>),
    Compound(CompoundStmt<'text>),
    Selection(SelectionStmt<'text>),
    Iteration(IterationStmt<'text>),
    Jump(JumpStmt<'text>),
}

fn parse_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        &[
            Box::new(parse_labeled_ident_stmt),
            Box::new(parse_empty_stmt),
            Box::new(parse_expr_stmt),
            Box::new(parse_compound_stmt),
            Box::new(parse_selection_stmt),
            Box::new(parse_iteration_while_stmt),
            Box::new(parse_iteration_do_while_stmt),
            Box::new(parse_iteration_for_stmt),
            Box::new(parse_jump_goto_stmt),
            Box::new(parse_jump_continue_stmt),
            Box::new(parse_jump_break_stmt),
            Box::new(parse_jump_return_stmt),
        ],
        "cannot parse statement",
    )
}

#[derive(Debug, PartialEq, Clone)]
pub enum LabeledStmt<'text> {
    Ident(&'text str, Box<Stmt<'text>>),
}

fn parse_labeled_ident_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(LabeledStmt<'text>, usize), ParseError> {
    let Some(Token::Ident(ident)) = tokens.get(pos) else {
        return Err(ParseError::ExpectedIdentifier(pos));
    };

    let Some(Token::Colon) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedColon(pos));
    };

    let (stmt, pos) = parse_stmt(tokens, pos + 2)?;
    Ok((LabeledStmt::Ident(ident, Box::new(stmt)), pos))
}

fn parse_empty_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParseError> {
    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos));
    };

    Ok((Stmt::EmptyStmt, pos + 1))
}

fn parse_expr_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Stmt<'text>, usize), ParseError> {
    let (expr, pos) = parse_expr(tokens, pos)?;

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    Ok((Stmt::Expr(expr), pos + 1))
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompoundStmt<'text>(pub Vec<Stmt<'text>>);

fn parse_compound_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(CompoundStmt<'text>, usize), ParseError> {
    let Some(Token::LCurly) = tokens.get(pos) else {
        return Err(ParseError::ExpectedLCurly(pos));
    };

    let mut stmts = Vec::new();
    let mut pos = pos + 1;

    while let Some(token) = tokens.get(pos) {
        if token == &Token::RCurly {
            return Ok((CompoundStmt(stmts), pos + 1));
        }

        match parse_stmt(tokens, pos) {
            Ok((stmt, next_pos)) => {
                pos = next_pos;
                stmts.push(stmt);
            }
            Err(e) => return Err(e.into()),
        };
    }

    Err(ParseError::ExpectedRCurly(pos).into())
}

#[derive(Debug, PartialEq, Clone)]
pub enum SelectionStmt<'text> {
    If {
        test: Expr<'text>,
        pass: Box<Stmt<'text>>,
    },
    IfElse {
        test: Expr<'text>,
        pass: Box<Stmt<'text>>,
        fail: Box<Stmt<'text>>,
    },
}

fn parse_selection_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(SelectionStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("if")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("if", pos));
    };

    let Some(Token::LParen) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedLParen(pos + 1).into());
    };

    let (test, pos) = parse_expr(tokens, pos + 2)?;

    let Some(Token::RParen) = tokens.get(pos) else {
        return Err(ParseError::ExpectedRParen(pos).into());
    };

    let (pass, pos) = parse_stmt(tokens, pos + 1)?;
    let pass = Box::new(pass);

    let Some(Token::Keyword("else")) = tokens.get(pos) else {
        return Ok((SelectionStmt::If { test, pass }, pos));
    };

    let (fail, pos) = parse_stmt(tokens, pos + 1)?;
    let fail = Box::new(fail);

    Ok((SelectionStmt::IfElse { test, pass, fail }, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum IterationStmt<'text> {
    While {
        test: Expr<'text>,
        body: Box<Stmt<'text>>,
    },
    DoWhile {
        test: Expr<'text>,
        body: Box<Stmt<'text>>,
    },
    For {
        init: Option<Expr<'text>>,
        test: Option<Expr<'text>>,
        update: Option<Expr<'text>>,
        body: Box<Stmt<'text>>,
    },
}

fn parse_iteration_while_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(IterationStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("while")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("while", pos));
    };

    let Some(Token::LParen) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedLParen(pos + 1).into());
    };

    let (test, pos) = parse_expr(tokens, pos + 2)?;

    let Some(Token::RParen) = tokens.get(pos) else {
        return Err(ParseError::ExpectedRParen(pos).into());
    };

    let (body, pos) = parse_stmt(tokens, pos + 1)?;
    let body = Box::new(body);

    Ok((IterationStmt::While { test, body }, pos))
}

fn parse_iteration_do_while_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(IterationStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("do")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("do", pos));
    };

    let (body, pos) = parse_stmt(tokens, pos + 1)?;
    let body = Box::new(body);

    let Some(Token::Keyword("while")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("while", pos).into());
    };

    let Some(Token::LParen) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedLParen(pos + 1).into());
    };

    let (test, pos) = parse_expr(tokens, pos + 2)?;

    let Some(Token::RParen) = tokens.get(pos) else {
        return Err(ParseError::ExpectedRParen(pos).into());
    };

    let Some(Token::SemiColon) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedSemicolon(pos + 1).into());
    };

    Ok((IterationStmt::DoWhile { test, body }, pos + 2))
}

fn parse_iteration_for_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(IterationStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("for")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("for", pos));
    };

    let Some(Token::LParen) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedLParen(pos + 1).into());
    };

    let (init, pos) = match tokens.get(pos + 2) {
        Some(Token::SemiColon) => (None, pos + 2),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 2)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    let (test, pos) = match tokens.get(pos + 1) {
        Some(Token::SemiColon) => (None, pos + 1),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    let (update, pos) = match tokens.get(pos + 1) {
        Some(Token::RParen) => (None, pos + 1),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::RParen) = tokens.get(pos) else {
        return Err(ParseError::ExpectedRParen(pos).into());
    };

    let (body, pos) = parse_stmt(tokens, pos + 1)?;
    let body = Box::new(body);

    Ok((
        IterationStmt::For {
            init,
            test,
            update,
            body,
        },
        pos,
    ))
}

#[derive(Debug, PartialEq, Clone)]
pub enum JumpStmt<'text> {
    Goto(&'text str),
    Continue,
    Break,
    Return(Option<Expr<'text>>),
}

fn parse_jump_goto_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("goto")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("goto", pos));
    };

    let Some(Token::Ident(ident)) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedIdentifier(pos + 1).into());
    };

    let Some(Token::SemiColon) = tokens.get(pos + 2) else {
        return Err(ParseError::ExpectedSemicolon(pos + 2).into());
    };

    Ok((JumpStmt::Goto(ident), pos + 3))
}

fn parse_jump_continue_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("continue")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("continue", pos));
    };

    let Some(Token::SemiColon) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedSemicolon(pos + 1).into());
    };

    Ok((JumpStmt::Continue, pos + 2))
}

fn parse_jump_break_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("break")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("break", pos));
    };

    let Some(Token::SemiColon) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedSemicolon(pos + 1).into());
    };

    Ok((JumpStmt::Break, pos + 2))
}

fn parse_jump_return_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("return")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedKeyword("return", pos));
    };

    let (expr, pos) = maybe(tokens, pos + 1, parse_expr);

    let Some(Token::SemiColon) = tokens.get(pos) else {
        return Err(ParseError::ExpectedSemicolon(pos).into());
    };

    return Ok((JumpStmt::Return(expr), pos + 1));
}

fn many<'text, Ast>(
    tokens: &[Token<'text>],
    mut pos: usize,
    parser: impl Fn(&[Token<'text>], usize) -> Result<(Ast, usize), ParseError>,
    delimiter: Option<Token>,
) -> (Vec<Ast>, usize) {
    let mut list = vec![];

    while let Ok((ast, next_pos)) = parser(tokens, pos) {
        list.push(ast);
        pos = next_pos;

        if let Some(delimiter) = &delimiter {
            match tokens.get(pos) {
                Some(token) if token == delimiter => {
                    pos += 1;
                }
                _ => break,
            };
        }
    }

    (list, pos)
}

fn maybe<'text, Ast>(
    tokens: &[Token<'text>],
    pos: usize,
    parser: impl Fn(&[Token<'text>], usize) -> Result<(Ast, usize), ParseError>,
) -> (Option<Ast>, usize) {
    match parser(tokens, pos) {
        Ok((ast, pos)) => (Some(ast), pos),
        Err(_) => (None, pos),
    }
}

trait Parser<'text, Ast> {
    fn parse(&self, tokens: &[Token<'text>], pos: usize) -> Result<(Ast, usize), ParseError>;
}

fn combine_parsers<'text, Ast>(
    tokens: &[Token<'text>],
    pos: usize,
    parsers: &[Box<dyn Parser<'text, Ast>>],
    msg: &'static str,
) -> Result<(Ast, usize), ParseError> {
    for parser in parsers {
        match parser.parse(tokens, pos) {
            Ok((ast, pos)) => return Ok((ast, pos)),
            Err(_) => continue,
        };
    }

    Err(ParseError::SyntaxError(pos, msg))
}

impl<'text, ParsedValue, F, Ast> Parser<'text, Ast> for F
where
    ParsedValue: Into<Ast>,
    F: Fn(&[Token<'text>], usize) -> Result<(ParsedValue, usize), ParseError>,
{
    fn parse(&self, tokens: &[Token<'text>], pos: usize) -> Result<(Ast, usize), ParseError> {
        match self(tokens, pos) {
            Ok((val, pos)) => Ok((val.into(), pos)),
            Err(e) => Err(e),
        }
    }
}

// #[derive(Debug)]
// enum ParserCombinatorError {
//     ParseError(ParseError),
//     IncorrectParser,
// }

pub type Expr<'text> = AssignmentExpr<'text>;

fn parse_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Expr<'text>, usize), ParseError> {
    parse_assignment_expr(tokens, pos)
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignmentExpr<'text> {
    ConditionalExpr(ConditionalExpr<'text>),
    Assign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    MulAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    DivAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    ModAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    AddAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    SubAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    ShiftLeftAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    ShiftRightAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    BitAndAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    XORAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
    BitOrAssign(UnaryExpr<'text>, Box<AssignmentExpr<'text>>),
}

fn parse_assignment_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(AssignmentExpr<'text>, usize), ParseError> {
    if let Ok((unary, pos)) = parse_unary_expr(tokens, pos) {
        if let Some(op) = tokens.get(pos) {
            if op == &Token::Equals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::Assign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::AsteriskEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::MulAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::SlashEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::DivAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::PercentEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::ModAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::PlusEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::AddAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::HyphenEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::SubAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::LTLTEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::ShiftLeftAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::GTGTEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::ShiftRightAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::AmpersandEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::BitAndAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::CaretEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::XORAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::PipeEquals {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1)?;
                return Ok((AssignmentExpr::BitOrAssign(unary, Box::new(rhs)), pos));
            }
        }
    }

    let (expr, pos) = parse_conditional_expr(tokens, pos)?;
    Ok((expr.into(), pos))
}

pub type ConstantExpr<'text> = ConditionalExpr<'text>;

fn parse_constant_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ConstantExpr<'text>, usize), ParseError> {
    parse_conditional_expr(tokens, pos)
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConditionalExpr<'text> {
    LogicalOrExpr(LogicalOrExpr<'text>),
    Ternary {
        test: LogicalOrExpr<'text>,
        pass: Box<Expr<'text>>,
        fail: Box<ConditionalExpr<'text>>,
    },
}

fn parse_conditional_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ConditionalExpr<'text>, usize), ParseError> {
    let (test, mut pos) = parse_logicalor_expr(tokens, pos)?;

    if let Some(Token::Question) = tokens.get(pos) {
        let (pass, next_pos) = parse_expr(tokens, pos + 1)?;
        pos = next_pos;

        if let Some(Token::Colon) = tokens.get(pos) {
            let (fail, next_pos) = parse_conditional_expr(tokens, pos + 1)?;
            pos = next_pos;

            return Ok((
                ConditionalExpr::Ternary {
                    test,
                    pass: Box::new(pass),
                    fail: Box::new(fail),
                },
                pos,
            ));
        }
    }

    Ok((test.into(), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum LogicalOrExpr<'text> {
    LogicalAndExpr(LogicalAndExpr<'text>),
    LogicalOr(Box<LogicalOrExpr<'text>>, LogicalAndExpr<'text>),
}

fn parse_logicalor_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(LogicalOrExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_logicaland_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::PipePipe => {
                let (rhs, next_pos) = parse_logicaland_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = LogicalOrExpr::LogicalOr(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum LogicalAndExpr<'text> {
    BitOrExpr(BitOrExpr<'text>),
    LogicalAnd(Box<LogicalAndExpr<'text>>, BitOrExpr<'text>),
}

fn parse_logicaland_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(LogicalAndExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_bitor_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::AmpersandAmpersand => {
                let (rhs, next_pos) = parse_bitor_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = LogicalAndExpr::LogicalAnd(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum BitOrExpr<'text> {
    XORExpr(XORExpr<'text>),
    BitOr(Box<BitOrExpr<'text>>, XORExpr<'text>),
}

fn parse_bitor_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(BitOrExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_xor_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Pipe => {
                let (rhs, next_pos) = parse_xor_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = BitOrExpr::BitOr(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum XORExpr<'text> {
    BitAndExpr(BitAndExpr<'text>),
    XOR(Box<XORExpr<'text>>, BitAndExpr<'text>),
}

fn parse_xor_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(XORExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_bitand_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Caret => {
                let (rhs, next_pos) = parse_bitand_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = XORExpr::XOR(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum BitAndExpr<'text> {
    EqualityExpr(EqualityExpr<'text>),
    BitAnd(Box<BitAndExpr<'text>>, EqualityExpr<'text>),
}

fn parse_bitand_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(BitAndExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_equality_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Ampersand => {
                let (rhs, next_pos) = parse_equality_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = BitAndExpr::BitAnd(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum EqualityExpr<'text> {
    ComparisionExpr(ComparisionExpr<'text>),
    EQ(Box<EqualityExpr<'text>>, ComparisionExpr<'text>),
    NE(Box<EqualityExpr<'text>>, ComparisionExpr<'text>),
}

fn parse_equality_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(EqualityExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_comparision_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::EQ => {
                let (rhs, next_pos) = parse_comparision_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = EqualityExpr::EQ(Box::new(lhs), rhs);
            }
            Token::NE => {
                let (rhs, next_pos) = parse_comparision_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = EqualityExpr::NE(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ComparisionExpr<'text> {
    ShiftExpr(ShiftExpr<'text>),
    LT(Box<ComparisionExpr<'text>>, ShiftExpr<'text>),
    GT(Box<ComparisionExpr<'text>>, ShiftExpr<'text>),
    LE(Box<ComparisionExpr<'text>>, ShiftExpr<'text>),
    GE(Box<ComparisionExpr<'text>>, ShiftExpr<'text>),
}

fn parse_comparision_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ComparisionExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_shift_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::LT => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ComparisionExpr::LT(Box::new(lhs), rhs);
            }
            Token::GT => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ComparisionExpr::GT(Box::new(lhs), rhs);
            }
            Token::LE => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ComparisionExpr::LE(Box::new(lhs), rhs);
            }
            Token::GE => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ComparisionExpr::GE(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ShiftExpr<'text> {
    AdditiveExpr(AdditiveExpr<'text>),
    ShiftLeft(Box<ShiftExpr<'text>>, AdditiveExpr<'text>),
    ShiftRight(Box<ShiftExpr<'text>>, AdditiveExpr<'text>),
}

fn parse_shift_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(ShiftExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_additive_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::LTLT => {
                let (rhs, next_pos) = parse_additive_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ShiftExpr::ShiftLeft(Box::new(lhs), rhs);
            }
            Token::GTGT => {
                let (rhs, next_pos) = parse_additive_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = ShiftExpr::ShiftRight(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum AdditiveExpr<'text> {
    MultiplicativeExpr(MultiplicativeExpr<'text>),
    Add(Box<AdditiveExpr<'text>>, MultiplicativeExpr<'text>),
    Sub(Box<AdditiveExpr<'text>>, MultiplicativeExpr<'text>),
}

fn parse_additive_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(AdditiveExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_multiplicative_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Plus => {
                let (rhs, next_pos) = parse_multiplicative_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = AdditiveExpr::Add(Box::new(lhs), rhs);
            }
            Token::Hyphen => {
                let (rhs, next_pos) = parse_multiplicative_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = AdditiveExpr::Sub(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum MultiplicativeExpr<'text> {
    UnaryExpr(UnaryExpr<'text>),
    Mul(Box<MultiplicativeExpr<'text>>, UnaryExpr<'text>),
    Div(Box<MultiplicativeExpr<'text>>, UnaryExpr<'text>),
    Mod(Box<MultiplicativeExpr<'text>>, UnaryExpr<'text>),
}

fn parse_multiplicative_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(MultiplicativeExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_unary_expr(tokens, pos)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Asterisk => {
                let (rhs, next_pos) = parse_unary_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Mul(Box::new(lhs), rhs);
            }
            Token::Slash => {
                let (rhs, next_pos) = parse_unary_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Div(Box::new(lhs), rhs);
            }
            Token::Percent => {
                let (rhs, next_pos) = parse_unary_expr(tokens, pos + 1)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Mod(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryExpr<'text> {
    PostfixExpr(PostfixExpr<'text>),
    PreIncr(Box<UnaryExpr<'text>>),
    PreDecr(Box<UnaryExpr<'text>>),
    Ref(Box<UnaryExpr<'text>>),
    Deref(Box<UnaryExpr<'text>>),
    UnaryAdd(Box<UnaryExpr<'text>>),
    UnarySub(Box<UnaryExpr<'text>>),
    OnesComplement(Box<UnaryExpr<'text>>),
    Not(Box<UnaryExpr<'text>>),
}

fn parse_unary_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(UnaryExpr<'text>, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::PlusPlus) => {
            let (expr, pos) = parse_postfix_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::PreIncr(Box::new(expr.into())), pos))
        }
        Some(Token::HyphenHyphen) => {
            let (expr, pos) = parse_postfix_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::PreDecr(Box::new(expr.into())), pos))
        }
        Some(Token::Ampersand) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::Ref(Box::new(expr)), pos))
        }
        Some(Token::Asterisk) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::Deref(Box::new(expr)), pos))
        }
        Some(Token::Plus) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::UnaryAdd(Box::new(expr)), pos))
        }
        Some(Token::Hyphen) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::UnarySub(Box::new(expr)), pos))
        }
        Some(Token::Tilde) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::OnesComplement(Box::new(expr)), pos))
        }
        Some(Token::Exclamation) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1)?;
            Ok((UnaryExpr::Not(Box::new(expr)), pos))
        }
        _ => {
            let (expr, pos) = parse_postfix_expr(tokens, pos)?;
            Ok((expr.into(), pos))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PostfixExpr<'text> {
    Primary(Primary<'text>),
    PostIncr(Box<PostfixExpr<'text>>),
    PostDecr(Box<PostfixExpr<'text>>),
}

fn parse_postfix_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(PostfixExpr<'text>, usize), ParseError> {
    let (expr, pos) = parse_primary_expr(tokens, pos)?;

    match tokens.get(pos) {
        Some(Token::PlusPlus) => Ok((PostfixExpr::PostIncr(Box::new(expr.into())), pos + 1)),
        Some(Token::HyphenHyphen) => Ok((PostfixExpr::PostDecr(Box::new(expr.into())), pos + 1)),
        _ => Ok((expr.into(), pos)),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Primary<'text> {
    Ident(&'text str),
    Int(isize),
    Char(char),
    Float(f64),
    String(&'text str),
    Parens(Box<Expr<'text>>),
}

fn parse_primary_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
) -> Result<(Primary<'text>, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Ident(ident)) => Ok((Primary::Ident(ident), pos + 1)),
        Some(Token::Whole(n)) => Ok((Primary::Int(*n as isize), pos + 1)),
        Some(Token::Char(c)) => Ok((Primary::Char(*c), pos + 1)),
        Some(Token::Decimal(n)) => Ok((Primary::Float(*n), pos + 1)),
        Some(Token::String(s)) => Ok((Primary::String(s), pos + 1)),
        Some(Token::LParen) => {
            let (expr, pos) = parse_expr(tokens, pos + 1)?;
            match tokens.get(pos) {
                Some(Token::RParen) => Ok((Primary::Parens(Box::new(expr)), pos + 1)),
                _ => Err(ParseError::MismatchedParentheses(pos)),
            }
        }
        _ => Err(ParseError::SyntaxError(
            pos,
            "parse_primary_expr: expected <identifier> or `int` or `char` or `float` or `string` or ( <expression> ) ",
        )),
    }
}

impl<'text> Display for EnumSpecifier<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EnumSpecifier::Named(ident, list) => {
                write!(f, "enum {} {{ ", ident)?;
                write_arr(f, list, ", ")?;
                write!(f, "}}")
            }
            EnumSpecifier::Anonymous(list) => {
                write!(f, "enum {{ ")?;
                write_arr(f, list, ", ")?;
                write!(f, "}}")
            }
            EnumSpecifier::ForwardDeclaration(ident) => write!(f, "enum {}", ident),
        }
    }
}

impl<'text> Display for Enumerator<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Enumerator::Implicit(ident) => write!(f, "{}", ident),
            Enumerator::Explicit(ident, value) => write!(f, "{} = {}", ident, value),
        }
    }
}

impl<'text> Display for DeclarationSpecifier<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DeclarationSpecifier::StorageClassSpecifier(x) => write!(f, "{}", x),
            DeclarationSpecifier::TypeSpecifier(x) => write!(f, "{}", x),
            DeclarationSpecifier::TypeQualifier(x) => write!(f, "{}", x),
        }
    }
}

impl Display for StorageClassSpecifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StorageClassSpecifier::Auto => write!(f, "auto"),
            StorageClassSpecifier::Register => write!(f, "register"),
            StorageClassSpecifier::Static => write!(f, "static"),
            StorageClassSpecifier::Extern => write!(f, "extern"),
            StorageClassSpecifier::TypeDef => write!(f, "typedef"),
        }
    }
}

impl<'text> Display for SpecifierQualifier<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SpecifierQualifier::TypeSpecifier(specifier) => write!(f, "{}", specifier),
            SpecifierQualifier::TypeQualifier(qualifier) => write!(f, "{}", qualifier),
        }
    }
}

impl<'text> Display for TypeSpecifier<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeSpecifier::Void => write!(f, "void"),
            TypeSpecifier::Char => write!(f, "char"),
            TypeSpecifier::Short => write!(f, "short"),
            TypeSpecifier::Int => write!(f, "int"),
            TypeSpecifier::Long => write!(f, "long"),
            TypeSpecifier::Float => write!(f, "float"),
            TypeSpecifier::Double => write!(f, "double"),
            TypeSpecifier::Signed => write!(f, "signed"),
            TypeSpecifier::UnSigned => write!(f, "unsigned"),
            TypeSpecifier::EnumSpecifier(specifier) => write!(f, "{}", specifier),
            TypeSpecifier::TypeDefName(ident) => write!(f, "{}", ident),
        }
    }
}

impl Display for Pointer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "*")?;
        for qualifier in &self.qualifiers {
            write!(f, "{} ", qualifier)?;
        }

        if let Some(next_pointer) = &self.next {
            write!(f, "{}", next_pointer)?;
        }

        Ok(())
    }
}

impl Display for TypeQualifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeQualifier::Const => write!(f, "const"),
            TypeQualifier::Volatile => write!(f, "volatile"),
        }
    }
}

impl<'text> Display for Declaration<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_arr(f, &self.declaration_specifiers, " ")?;
        write!(f, " ")?;
        write_arr(f, &self.init_declarators, " ")?;
        write!(f, ";")
    }
}

impl<'text> Display for Declarator<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(pointer) = &self.pointer {
            write!(f, "{}", pointer)?;
        }
        write!(f, "{}", self.declarator)
    }
}

impl<'text> Display for DirectDeclarator<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DirectDeclarator::Ident(ident, tail) => {
                write!(f, "{}", ident)?;
                if let Some(tail) = tail {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
            DirectDeclarator::Parens(d, tail) => {
                write!(f, "({})", d)?;
                if let Some(tail) = tail {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
        }
    }
}

impl<'text> Display for DirectDeclaratorTail<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DirectDeclaratorTail::Array(e, tail) => {
                match e {
                    Some(e) => write!(f, "[{}]", e),
                    None => write!(f, "[]"),
                }?;
                if let Some(tail) = tail.deref() {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
            DirectDeclaratorTail::Function(p, tail) => {
                write!(f, "({})", p)?;
                if let Some(tail) = tail.deref() {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
            DirectDeclaratorTail::Parameters(idents, tail) => {
                write!(f, "(")?;
                write_arr(f, idents, " ")?;
                write!(f, ")")?;
                if let Some(tail) = tail.deref() {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
        }
    }
}

impl<'text> Display for ParameterTypeList<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ParameterTypeList::ParameterList(l) => write_arr(f, l, ", "),
            ParameterTypeList::VariadicParameterList(l) => {
                write_arr(f, l, ", ")?;
                write!(f, ", ...")
            }
        }
    }
}

impl<'text> Display for ParameterDeclaration<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ParameterDeclaration::WithDeclarator(dss, d) => {
                write_arr(f, &dss, " ")?;
                write!(f, " {}", d)
            }
            ParameterDeclaration::WithAbstractDeclarator(dss, ad) => {
                write_arr(f, &dss, " ")?;
                write!(f, " {}", ad)
            }
            ParameterDeclaration::OnlySpecifiers(dss) => write_arr(f, &dss, " "),
        }
    }
}

impl<'text> Display for AbstractDeclarator<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AbstractDeclarator::Pointer(p) => write!(f, "{}", p),
            AbstractDeclarator::PointerWithDirect(p, dad) => write!(f, "{}{}", p, dad),
            AbstractDeclarator::Direct(dad) => write!(f, "{}", dad),
        }
    }
}

impl<'text> Display for DirectAbstractDeclarator<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DirectAbstractDeclarator::Parens(ad, tail) => {
                write!(f, "({})", ad)?;
                if let Some(tail) = tail {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
            DirectAbstractDeclarator::Array(e, tail) => {
                match e {
                    Some(e) => write!(f, "[{}]", e),
                    None => write!(f, "[]"),
                }?;
                if let Some(tail) = tail.deref() {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
            DirectAbstractDeclarator::Function(p, tail) => {
                match p {
                    Some(p) => write!(f, "({})", p),
                    None => write!(f, "[]"),
                }?;
                if let Some(tail) = tail.deref() {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
        }
    }
}

impl<'text> Display for DirectAbstractDeclaratorTail<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DirectAbstractDeclaratorTail::Array(e, tail) => {
                match e {
                    Some(e) => write!(f, "[{}]", e),
                    None => write!(f, "[]"),
                }?;
                if let Some(tail) = tail.deref() {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
            DirectAbstractDeclaratorTail::Function(p, tail) => {
                match p {
                    Some(p) => write!(f, "({})", p),
                    None => write!(f, "[]"),
                }?;
                if let Some(tail) = tail.deref() {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
        }
    }
}

impl<'text> Display for InitDeclarator<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InitDeclarator::Declared(d) => write!(f, "{}", d),
            InitDeclarator::Initialized(d, val) => write!(f, "{} = {}", d, val),
        }
    }
}

impl<'text> Display for Initializer<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Initializer::Assignment(expr) => write!(f, "{}", expr),
            Initializer::InitializerList(list) => {
                write!(f, "{{ ")?;
                for expr in list {
                    write!(f, "{}, ", expr)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl<'text> Display for Stmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::EmptyStmt => write!(f, ";"),
            Stmt::Expr(stmt) => write!(f, "{};", stmt),
            Stmt::Labeled(stmt) => write!(f, "{}", stmt),
            Stmt::Compound(stmt) => write!(f, "{}", stmt),
            Stmt::Selection(stmt) => write!(f, "{}", stmt),
            Stmt::Iteration(stmt) => write!(f, "{}", stmt),
            Stmt::Jump(stmt) => write!(f, "{}", stmt),
        }
    }
}

impl<'text> Display for LabeledStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LabeledStmt::Ident(ident, stmt) => write!(f, "{} : {}", ident, stmt),
        }
    }
}

impl<'text> Display for CompoundStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{ ")?;
        for stmt in &self.0 {
            write!(f, "{} ", stmt)?;
        }
        write!(f, "}}")
    }
}

impl<'text> Display for SelectionStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SelectionStmt::If { test, pass } => write!(f, "if ({}) {}", test, pass),
            SelectionStmt::IfElse { test, pass, fail } => {
                write!(f, "if ({}) {} else {}", test, pass, fail)
            }
        }
    }
}

impl<'text> Display for IterationStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IterationStmt::While { test, body } => write!(f, "while ({}) {}", test, body),
            IterationStmt::DoWhile { test, body } => write!(f, "do {} while ({});", body, test),
            IterationStmt::For {
                init,
                test,
                update,
                body,
            } => {
                write!(f, "for (")?;
                if let Some(expr) = init {
                    write!(f, "{}", expr)?;
                }
                write!(f, ";")?;
                if let Some(expr) = test {
                    write!(f, " {}", expr)?;
                }
                write!(f, ";")?;
                if let Some(expr) = update {
                    write!(f, " {}", expr)?;
                }
                write!(f, ")")?;
                write!(f, " {}", body)
            }
        }
    }
}

impl<'text> Display for JumpStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            JumpStmt::Goto(ident) => write!(f, "goto {};", ident),
            JumpStmt::Continue => write!(f, "continue;"),
            JumpStmt::Break => write!(f, "break;"),
            JumpStmt::Return(expr) => match expr {
                Some(expr) => write!(f, "return {};", expr),
                None => write!(f, "return;"),
            },
        }
    }
}

impl<'text> Display for AssignmentExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AssignmentExpr::ConditionalExpr(expr) => write!(f, "{}", expr),
            AssignmentExpr::Assign(lhs, rhs) => write!(f, "({} = {})", lhs, rhs),
            AssignmentExpr::MulAssign(lhs, rhs) => write!(f, "({} *= {})", lhs, rhs),
            AssignmentExpr::DivAssign(lhs, rhs) => write!(f, "({} /= {})", lhs, rhs),
            AssignmentExpr::ModAssign(lhs, rhs) => write!(f, "({} %= {})", lhs, rhs),
            AssignmentExpr::AddAssign(lhs, rhs) => write!(f, "({} += {})", lhs, rhs),
            AssignmentExpr::SubAssign(lhs, rhs) => write!(f, "({} -= {})", lhs, rhs),
            AssignmentExpr::ShiftLeftAssign(lhs, rhs) => write!(f, "({} <<= {})", lhs, rhs),
            AssignmentExpr::ShiftRightAssign(lhs, rhs) => write!(f, "({} >>= {})", lhs, rhs),
            AssignmentExpr::BitAndAssign(lhs, rhs) => write!(f, "({} &= {})", lhs, rhs),
            AssignmentExpr::XORAssign(lhs, rhs) => write!(f, "({} ^= {})", lhs, rhs),
            AssignmentExpr::BitOrAssign(lhs, rhs) => write!(f, "({} |= {})", lhs, rhs),
        }
    }
}

impl<'text> Display for ConditionalExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ConditionalExpr::LogicalOrExpr(expr) => write!(f, "{}", expr),
            ConditionalExpr::Ternary { test, pass, fail } => {
                write!(f, "({} ? {} : {})", test, pass, fail)
            }
        }
    }
}

impl<'text> Display for LogicalOrExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LogicalOrExpr::LogicalAndExpr(expr) => write!(f, "{}", expr),
            LogicalOrExpr::LogicalOr(lhs, rhs) => write!(f, "({} || {})", lhs, rhs),
        }
    }
}

impl<'text> Display for LogicalAndExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LogicalAndExpr::BitOrExpr(expr) => write!(f, "{}", expr),
            LogicalAndExpr::LogicalAnd(lhs, rhs) => write!(f, "({} && {})", lhs, rhs),
        }
    }
}

impl<'text> Display for BitOrExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BitOrExpr::XORExpr(expr) => write!(f, "{}", expr),
            BitOrExpr::BitOr(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
        }
    }
}

impl<'text> Display for XORExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            XORExpr::BitAndExpr(expr) => write!(f, "{}", expr),
            XORExpr::XOR(lhs, rhs) => write!(f, "({} ^ {})", lhs, rhs),
        }
    }
}

impl<'text> Display for BitAndExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BitAndExpr::EqualityExpr(expr) => write!(f, "{}", expr),
            BitAndExpr::BitAnd(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
        }
    }
}

impl<'text> Display for EqualityExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EqualityExpr::ComparisionExpr(expr) => write!(f, "{}", expr),
            EqualityExpr::EQ(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            EqualityExpr::NE(lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
        }
    }
}

impl<'text> Display for ComparisionExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ComparisionExpr::ShiftExpr(expr) => write!(f, "{}", expr),
            ComparisionExpr::LT(lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            ComparisionExpr::GT(lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            ComparisionExpr::LE(lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),
            ComparisionExpr::GE(lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
        }
    }
}

impl<'text> Display for ShiftExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ShiftExpr::AdditiveExpr(expr) => write!(f, "{}", expr),
            ShiftExpr::ShiftLeft(lhs, rhs) => write!(f, "({} << {})", lhs, rhs),
            ShiftExpr::ShiftRight(lhs, rhs) => write!(f, "({} >> {})", lhs, rhs),
        }
    }
}

impl<'text> Display for AdditiveExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AdditiveExpr::MultiplicativeExpr(expr) => write!(f, "{}", expr),
            AdditiveExpr::Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            AdditiveExpr::Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
        }
    }
}

impl<'text> Display for MultiplicativeExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MultiplicativeExpr::UnaryExpr(expr) => write!(f, "{}", expr),
            MultiplicativeExpr::Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            MultiplicativeExpr::Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            MultiplicativeExpr::Mod(lhs, rhs) => write!(f, "({} % {})", lhs, rhs),
        }
    }
}

impl<'text> Display for UnaryExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnaryExpr::PostfixExpr(expr) => write!(f, "{}", expr),
            UnaryExpr::PreIncr(expr) => write!(f, "++{}", expr),
            UnaryExpr::PreDecr(expr) => write!(f, "--{}", expr),
            UnaryExpr::Ref(expr) => write!(f, "&{}", expr),
            UnaryExpr::Deref(expr) => write!(f, "*{}", expr),
            UnaryExpr::UnaryAdd(expr) => write!(f, "{}", expr),
            UnaryExpr::UnarySub(expr) => write!(f, "-{}", expr),
            UnaryExpr::OnesComplement(expr) => write!(f, "~{}", expr),
            UnaryExpr::Not(expr) => write!(f, "!{}", expr),
        }
    }
}

impl<'text> Display for PostfixExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PostfixExpr::Primary(expr) => write!(f, "{}", expr),
            PostfixExpr::PostIncr(expr) => write!(f, "{}++", expr),
            PostfixExpr::PostDecr(expr) => write!(f, "{}--", expr),
        }
    }
}

impl<'text> Display for Primary<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Primary::Ident(ident) => write!(f, "{}", ident),
            Primary::Int(n) => write!(f, "{}", n),
            Primary::Char(c) => write!(f, "'{}'", c),
            Primary::Float(n) => write!(f, "{}", n),
            Primary::String(s) => write!(f, "\"{}\"", s),
            Primary::Parens(expr) => write!(f, "({})", expr),
        }
    }
}

fn write_arr<T>(f: &mut Formatter<'_>, arr: &[T], sep: &str) -> fmt::Result
where
    T: Display,
{
    if let Some(item) = arr.get(0) {
        write!(f, "{}", item)?;
        for item in &arr[1..] {
            write!(f, "{}{}", sep, item)?;
        }
    }

    Ok(())
}

impl<'text> From<EnumSpecifier<'text>> for TypeSpecifier<'text> {
    fn from(value: EnumSpecifier<'text>) -> Self {
        TypeSpecifier::EnumSpecifier(value)
    }
}

impl<'text> From<StorageClassSpecifier> for DeclarationSpecifier<'text> {
    fn from(value: StorageClassSpecifier) -> Self {
        DeclarationSpecifier::StorageClassSpecifier(value)
    }
}

impl<'text> From<TypeSpecifier<'text>> for DeclarationSpecifier<'text> {
    fn from(value: TypeSpecifier<'text>) -> Self {
        DeclarationSpecifier::TypeSpecifier(value)
    }
}

impl<'text> From<TypeQualifier> for DeclarationSpecifier<'text> {
    fn from(value: TypeQualifier) -> Self {
        DeclarationSpecifier::TypeQualifier(value)
    }
}

impl<'text> From<TypeSpecifier<'text>> for SpecifierQualifier<'text> {
    fn from(value: TypeSpecifier<'text>) -> Self {
        SpecifierQualifier::TypeSpecifier(value)
    }
}

impl<'text> From<TypeQualifier> for SpecifierQualifier<'text> {
    fn from(value: TypeQualifier) -> Self {
        SpecifierQualifier::TypeQualifier(value)
    }
}

impl<'text> From<AssignmentExpr<'text>> for Initializer<'text> {
    fn from(value: AssignmentExpr<'text>) -> Self {
        Initializer::Assignment(value)
    }
}

impl<'text> From<LabeledStmt<'text>> for Stmt<'text> {
    fn from(value: LabeledStmt<'text>) -> Self {
        Stmt::Labeled(value)
    }
}

impl<'text> From<CompoundStmt<'text>> for Stmt<'text> {
    fn from(value: CompoundStmt<'text>) -> Self {
        Stmt::Compound(value)
    }
}

impl<'text> From<SelectionStmt<'text>> for Stmt<'text> {
    fn from(value: SelectionStmt<'text>) -> Self {
        Stmt::Selection(value)
    }
}

impl<'text> From<IterationStmt<'text>> for Stmt<'text> {
    fn from(value: IterationStmt<'text>) -> Self {
        Stmt::Iteration(value)
    }
}

impl<'text> From<JumpStmt<'text>> for Stmt<'text> {
    fn from(value: JumpStmt<'text>) -> Self {
        Stmt::Jump(value)
    }
}

impl<'text> From<ConditionalExpr<'text>> for AssignmentExpr<'text> {
    fn from(value: ConditionalExpr<'text>) -> Self {
        AssignmentExpr::ConditionalExpr(value)
    }
}

impl<'text> From<LogicalOrExpr<'text>> for ConditionalExpr<'text> {
    fn from(value: LogicalOrExpr<'text>) -> Self {
        ConditionalExpr::LogicalOrExpr(value)
    }
}
impl<'text> From<LogicalAndExpr<'text>> for LogicalOrExpr<'text> {
    fn from(value: LogicalAndExpr<'text>) -> Self {
        LogicalOrExpr::LogicalAndExpr(value)
    }
}

impl<'text> From<BitOrExpr<'text>> for LogicalAndExpr<'text> {
    fn from(value: BitOrExpr<'text>) -> Self {
        LogicalAndExpr::BitOrExpr(value)
    }
}

impl<'text> From<XORExpr<'text>> for BitOrExpr<'text> {
    fn from(value: XORExpr<'text>) -> Self {
        BitOrExpr::XORExpr(value)
    }
}

impl<'text> From<BitAndExpr<'text>> for XORExpr<'text> {
    fn from(value: BitAndExpr<'text>) -> Self {
        XORExpr::BitAndExpr(value)
    }
}

impl<'text> From<EqualityExpr<'text>> for BitAndExpr<'text> {
    fn from(value: EqualityExpr<'text>) -> Self {
        BitAndExpr::EqualityExpr(value)
    }
}

impl<'text> From<ComparisionExpr<'text>> for EqualityExpr<'text> {
    fn from(value: ComparisionExpr<'text>) -> Self {
        EqualityExpr::ComparisionExpr(value)
    }
}

impl<'text> From<ShiftExpr<'text>> for ComparisionExpr<'text> {
    fn from(value: ShiftExpr<'text>) -> Self {
        ComparisionExpr::ShiftExpr(value)
    }
}

impl<'text> From<AdditiveExpr<'text>> for ShiftExpr<'text> {
    fn from(value: AdditiveExpr<'text>) -> Self {
        ShiftExpr::AdditiveExpr(value)
    }
}

impl<'text> From<MultiplicativeExpr<'text>> for AdditiveExpr<'text> {
    fn from(value: MultiplicativeExpr<'text>) -> Self {
        AdditiveExpr::MultiplicativeExpr(value)
    }
}

impl<'text> From<UnaryExpr<'text>> for MultiplicativeExpr<'text> {
    fn from(value: UnaryExpr<'text>) -> Self {
        MultiplicativeExpr::UnaryExpr(value)
    }
}

impl<'text> From<PostfixExpr<'text>> for UnaryExpr<'text> {
    fn from(value: PostfixExpr<'text>) -> Self {
        UnaryExpr::PostfixExpr(value)
    }
}

impl<'text> From<Primary<'text>> for PostfixExpr<'text> {
    fn from(value: Primary<'text>) -> Self {
        PostfixExpr::Primary(value)
    }
}

#[derive(Debug)]
pub enum ParseError {
    MismatchedParentheses(usize),
    SyntaxError(usize, &'static str),
    InvalidStatement(usize),
    ExpectedSemicolon(usize),
    ExpectedColon(usize),
    ExpectedLParen(usize),
    ExpectedRParen(usize),
    ExpectedLCurly(usize),
    ExpectedRCurly(usize),
    ExpectedLSquare(usize),
    ExpectedRSquare(usize),
    ExpectedKeyword(&'static str, usize),
    ExpectedIdentifier(usize),
}

macro_rules! check {
    ($f:ident, $src:expr, $expected:expr) => {
        let tokens = lex($src).expect("** LEX ERROR");
        let (stmt, pos) = $f(&tokens, 0).expect("** Unable to parse statement");
        assert_eq!(pos, tokens.len(), "** Unable to parse all Tokens\n{}", stmt);
        let stmt = format!("{}", stmt);
        assert_eq!($expected, stmt);
    };
    ($f:ident, $src:expr) => {
        check!($f, $src, $src)
    };
}

macro_rules! check_raw {
    ($f:ident, $src:expr, $expected:expr) => {
        let tokens = lex($src).expect("** LEX ERROR");
        let (stmt, pos) = $f(&tokens, 0).expect("** Unable to parse statement");
        assert_eq!(pos, tokens.len());
        assert_eq!($expected, stmt);
    };
}

macro_rules! ast {
    ($f:ident, $src:expr) => {{
        let tokens = lex($src).expect("** LEX ERROR");
        let (stmt, pos) = $f(&tokens, 0).expect("** Unable to parse statement");
        assert_eq!(pos, tokens.len());
        stmt
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex;

    use pretty_assertions::assert_eq;

    const ENUM: [(&'static str, &'static str); 3] = [
        ("enum Color", "enum Color"),
        (
            r#"
            enum Color { 
                RED, 
                GREEN ="00FF00", 
                BLUE = 7 
            }
            "#,
            r#"enum Color { RED, GREEN = "00FF00", BLUE = 7}"#,
        ),
        (
            r#"
            enum { 
                RED, 
                GREEN = "00FF00", 
                BLUE = 7 
            }
            "#,
            r#"enum { RED, GREEN = "00FF00", BLUE = 7}"#,
        ),
    ];

    const STORAGE_CLASS_SPECIFIER: [(&'static str, StorageClassSpecifier); 5] = [
        ("auto", StorageClassSpecifier::Auto),
        ("register", StorageClassSpecifier::Register),
        ("static", StorageClassSpecifier::Static),
        ("extern", StorageClassSpecifier::Extern),
        ("typedef", StorageClassSpecifier::TypeDef),
    ];

    const TYPE_SPECIFIER: [(&'static str, TypeSpecifier); 9] = [
        ("void", TypeSpecifier::Void),
        ("char", TypeSpecifier::Char),
        ("short", TypeSpecifier::Short),
        ("int", TypeSpecifier::Int),
        ("long", TypeSpecifier::Long),
        ("float", TypeSpecifier::Float),
        ("double", TypeSpecifier::Double),
        ("signed", TypeSpecifier::Signed),
        ("unsigned", TypeSpecifier::UnSigned),
        // ("a", TypeSpecifier::TypeDefName("a")),
    ];

    const TYPE_QUALIFIER: [(&'static str, TypeQualifier); 2] = [
        ("const", TypeQualifier::Const),
        ("volatile", TypeQualifier::Volatile),
    ];

    #[test]
    fn test_declarator() {
        check!(parse_declarator, "x");
        check!(parse_declarator, "(y)");
        check!(parse_declarator, "*a");
        check!(parse_declarator, "*****a[]");
        check!(parse_declarator, "*a()");
        check!(parse_declarator, "*(*a)");
        check!(parse_declarator, "arr[]");
        check!(parse_declarator, "arr[SIZE]");
        check!(parse_declarator, "mat[3][4]");
        check!(parse_declarator, "f()");
        check!(parse_declarator, "add(int a, int b)");
        check!(parse_declarator, "(*func_ptr)()");
        check!(parse_declarator, "(*sqrt)(double x, double y)");
        check!(parse_declarator, "sum(int n, ...)");
        check!(parse_declarator, "print(const char *fmt, ...)");
        check!(parse_declarator, "log(const char *message, ...)");
        check!(parse_declarator, "f(a b c)");
    }

    #[test]
    fn test_abstract_declarator() {
        check!(parse_abstract_declarator, "*");
        check!(parse_abstract_declarator, "(*)");
        check!(parse_abstract_declarator, "*****[]");
        check!(parse_abstract_declarator, "*()");
        check!(parse_abstract_declarator, "*(*)");
        check!(parse_abstract_declarator, "[]");
        check!(parse_abstract_declarator, "[SIZE]");
        check!(parse_abstract_declarator, "[3][4]");
        check!(parse_abstract_declarator, "()");
        check!(parse_abstract_declarator, "(int a, int b)");
        check!(parse_abstract_declarator, "(*)()");
        check!(parse_abstract_declarator, "(*)(double x, double y)");
        check!(parse_abstract_declarator, "(int n, ...)");
        check!(parse_abstract_declarator, "(const char *fmt, ...)");
        check!(parse_abstract_declarator, "(const char *message, ...)");
    }

    #[test]
    fn test_declaration() {
        check!(parse_declaration, "int x = 10;");
        check!(parse_declaration, "int nums[] = { 1, 2, 3, };");
        check!(
            parse_declaration,
            "float mat[2][3] = { { 3.1, 0.6, 2.7, }, { 1.4, 4.7, 0.6, }, };"
        );
        check!(parse_declaration, r#"const char *name = "zahash";"#);
        // Need a proper way to recognize and parse typedefs (custom types/type names)
        // if typedef parsing is enabled, it thinks `origin` is a type name instead of just an ident
        // if typedef parsing is disabled, it cannot recognize Point as a type and not just an ident
        // check!(parse_declaration, "const Point origin = { 0, 0, }");
        // check!(parse_declaration, r#"Person p = { name = "zahash", age = 24, }"#);
    }

    #[test]
    fn test_init_declarator() {
        check!(parse_init_declarator, "x = 10");
        check!(parse_init_declarator, "nums[] = { 1, 2, 3, }");
        check!(
            parse_init_declarator,
            "mat[2][3] = { { 1, 2, 3, }, { 4, 5, 6, }, }"
        );
        check!(parse_init_declarator, "obj = { a, b, c, }");
        check!(parse_init_declarator, r#"*name = "zahash""#);
    }

    #[test]
    fn test_initializer() {
        check!(parse_initializer, "expr");
        check!(parse_initializer, "{a, b, c}", "{ a, b, c, }");
        check!(parse_initializer, "{a, b, c,}", "{ a, b, c, }");
        check!(
            parse_initializer,
            "{a, {b, c,}, {d,}}",
            "{ a, { b, c, }, { d, }, }"
        );
    }

    #[test]
    fn test_declaration_specifier() {
        for (src, expected) in STORAGE_CLASS_SPECIFIER {
            check_raw!(
                parse_declaration_specifier,
                src,
                DeclarationSpecifier::from(expected)
            );
        }

        for (src, expected) in TYPE_SPECIFIER {
            check_raw!(
                parse_declaration_specifier,
                src,
                DeclarationSpecifier::from(expected)
            );
        }

        for (src, expected) in ENUM {
            check!(parse_declaration_specifier, src, expected);
        }

        for (src, expected) in TYPE_QUALIFIER {
            check_raw!(
                parse_declaration_specifier,
                src,
                DeclarationSpecifier::from(expected)
            );
        }
    }

    #[test]
    fn test_storage_class_specifier() {
        for (src, expected) in STORAGE_CLASS_SPECIFIER {
            check_raw!(parse_storage_class_specifier, src, expected);
        }
    }

    #[test]
    fn test_specifier_qualifier() {
        for (src, expected) in TYPE_SPECIFIER {
            check_raw!(
                parse_specifier_qualifier,
                src,
                SpecifierQualifier::from(expected)
            );
        }

        for (src, expected) in ENUM {
            check!(parse_specifier_qualifier, src, expected);
        }

        for (src, expected) in TYPE_QUALIFIER {
            check_raw!(
                parse_specifier_qualifier,
                src,
                SpecifierQualifier::from(expected)
            );
        }
    }

    #[test]
    fn test_type_specifier() {
        for (src, expected) in TYPE_SPECIFIER {
            check_raw!(parse_type_specifier, src, expected);
        }

        for (src, expected) in ENUM {
            check!(parse_type_specifier, src, expected);
        }
    }

    #[test]
    fn test_pointer() {
        check!(parse_pointer, "*");
        check!(parse_pointer, "**");
        check!(parse_pointer, "***");
        check!(parse_pointer, "*const ");
        check!(
            parse_pointer,
            "*const volatile const volatile volatile const "
        );
        check!(
            parse_pointer,
            "*volatile const volatile *const const volatile "
        );
        check!(
            parse_pointer,
            "**volatile *******const ***const volatile ******"
        );

        let pointer = ast!(parse_pointer, "* *const volatile *volatile * *");
        let qualifiers: Vec<Vec<TypeQualifier>> = pointer.into_iter().map(|q| q.into()).collect();

        assert_eq!(
            qualifiers,
            vec![
                vec![],
                vec![TypeQualifier::Const, TypeQualifier::Volatile],
                vec![TypeQualifier::Volatile],
                vec![],
                vec![],
            ]
        );
    }

    #[test]
    fn test_type_qualifier() {
        for (src, expected) in TYPE_QUALIFIER {
            check_raw!(parse_type_qualifier, src, expected);
        }
    }

    #[test]
    fn test_enum() {
        for (src, expected) in ENUM {
            check!(parse_enum_specifier, src, expected);
        }
    }

    #[test]
    fn test_simple_stmt() {
        check_raw!(parse_stmt, ";", Stmt::EmptyStmt);
        check_raw!(parse_stmt, "{ }", Stmt::Compound(CompoundStmt(vec![])));
        check!(parse_stmt, "a++;");
        check!(parse_stmt, "{ a++; }");
    }

    #[test]
    fn test_labeled_stmt() {
        check!(parse_stmt, "a : ;");
        check!(parse_stmt, "a : { }");
        check!(parse_stmt, "a : b;");
        check!(parse_stmt, "a : { b; }");
    }

    #[test]
    fn test_if_stmt() {
        check!(parse_stmt, "if (a) ;");
        check!(parse_stmt, "if (a) b;");
        check!(parse_stmt, "if (a) b; else c;");
        check!(parse_stmt, "if (a) b; else if (c) d;");
        check!(parse_stmt, "if (a) b; else if (c) d; else e;");
        check!(parse_stmt, "if (a) { }");
        check!(parse_stmt, "if (a) { b; }");
        check!(parse_stmt, "if (a) { b; } else { c; }");
        check!(parse_stmt, "if (a) { b; } else if (c) { d; }");
        check!(parse_stmt, "if (a) { b; } else if (c) { d; } else { e; }");
        check!(
            parse_stmt,
            r#"
            if (a == 1) {
                b++;
            } else if (a == 2) {
                b--;
            } else {
                b;
            }
            "#,
            "if ((a == 1)) { b++; } else if ((a == 2)) { b--; } else { b; }"
        );
    }

    #[test]
    fn test_while_stmt() {
        check!(parse_stmt, "while (a) ;");
        check!(parse_stmt, "while (a) { }");
        check!(parse_stmt, "while (a) { b; }");
        check!(
            parse_stmt,
            "while (a <= 10) { a++; }",
            "while ((a <= 10)) { a++; }"
        );
    }

    #[test]
    fn test_do_while_stmt() {
        check!(parse_stmt, "do ; while (a);");
        check!(parse_stmt, "do { } while (a);");
        check!(parse_stmt, "do { b; } while (a);");
        check!(
            parse_stmt,
            "do { a++; } while (a <= 10);",
            "do { a++; } while ((a <= 10));"
        );
    }

    #[test]
    fn test_for_stmt() {
        check!(parse_stmt, "for (;;) ;");
        check!(parse_stmt, "for (;;) { }");
        check!(parse_stmt, "for (a;;) ;");
        check!(parse_stmt, "for (; a;) ;");
        check!(parse_stmt, "for (;; a) ;");
        check!(parse_stmt, "for (a; a; a) ;");
        check!(parse_stmt, "for (a; a; a) { }");
        check!(parse_stmt, "for (a; a; a) { b; }");
        check!(
            parse_stmt,
            "for (i=0; i<10; i++) { a++; }",
            "for ((i = 0); (i < 10); i++) { a++; }"
        );
    }

    #[test]
    fn test_jump_stmt() {
        check!(parse_stmt, "goto a;");
        check!(parse_stmt, "continue;");
        check!(parse_stmt, "break;");
        check!(parse_stmt, "return;");
        check!(parse_stmt, "return a;");
    }

    #[test]
    fn test_primary() {
        check!(parse_expr, "ident");
        check!(parse_expr, "123");
        check!(parse_expr, "'c'");
        check!(parse_expr, "123.123");
        check!(parse_expr, r#""string""#);
        check!(parse_expr, r#"(a)"#);
    }

    #[test]
    fn test_postfix_expr() {
        check!(parse_expr, "a++");
        check!(parse_expr, "a--");
    }

    #[test]
    fn test_unary_expr() {
        check!(parse_expr, "++a");
        check!(parse_expr, "--a");
        check!(parse_expr, "&a");
        check!(parse_expr, "*a");
        check!(parse_expr, "+a", "a");
        check!(parse_expr, "-a");
        check!(parse_expr, "~a");
    }

    #[test]
    fn test_multiplicative_expr() {
        check!(parse_expr, "a * b", "(a * b)");
        check!(parse_expr, "a / b", "(a / b)");
        check!(parse_expr, "a % b", "(a % b)");
        check!(parse_expr, "a * b / c % d", "(((a * b) / c) % d)");
    }

    #[test]
    fn test_additive_expr() {
        check!(parse_expr, "a + b", "(a + b)");
        check!(parse_expr, "a - b", "(a - b)");
        check!(parse_expr, "a + b - c", "((a + b) - c)");
    }

    #[test]
    fn test_shift_expr() {
        check!(parse_expr, "a << b", "(a << b)");
        check!(parse_expr, "a >> b", "(a >> b)");
        check!(parse_expr, "a << b >> c", "((a << b) >> c)");
    }

    #[test]
    fn test_comparision_expr() {
        check!(parse_expr, "a < b", "(a < b)");
        check!(parse_expr, "a > b", "(a > b)");
        check!(parse_expr, "a <= b", "(a <= b)");
        check!(parse_expr, "a >= b", "(a >= b)");
        check!(
            parse_expr,
            "a < b > c <= d >= e",
            "((((a < b) > c) <= d) >= e)"
        );
    }

    #[test]
    fn test_equality_expr() {
        check!(parse_expr, "a == b", "(a == b)");
        check!(parse_expr, "a != b", "(a != b)");
        check!(parse_expr, "a == b != c", "((a == b) != c)");
    }

    #[test]
    fn test_bit_and_expr() {
        check!(parse_expr, "a & b", "(a & b)");
        check!(parse_expr, "a & b & c", "((a & b) & c)");
    }

    #[test]
    fn test_xor_expr() {
        check!(parse_expr, "a ^ b", "(a ^ b)");
        check!(parse_expr, "a ^ b ^ c", "((a ^ b) ^ c)");
    }

    #[test]
    fn test_bit_or_expr() {
        check!(parse_expr, "a | b", "(a | b)");
        check!(parse_expr, "a | b | c", "((a | b) | c)");
    }

    #[test]
    fn test_logical_and_expr() {
        check!(parse_expr, "a && b", "(a && b)");
        check!(parse_expr, "a && b && c", "((a && b) && c)");
    }

    #[test]
    fn test_logical_or_expr() {
        check!(parse_expr, "a || b", "(a || b)");
        check!(parse_expr, "a || b || c", "((a || b) || c)");
    }

    #[test]
    fn test_conditional_expr() {
        check!(parse_expr, "a ? b : c", "(a ? b : c)");
        check!(parse_expr, "a ? b ? c : d : e", "(a ? (b ? c : d) : e)");
    }

    #[test]
    fn test_assignment_expr() {
        check!(parse_expr, "a = b", "(a = b)");
        check!(parse_expr, "a *= b", "(a *= b)");
        check!(parse_expr, "a /= b", "(a /= b)");
        check!(parse_expr, "a %= b", "(a %= b)");
        check!(parse_expr, "a += b", "(a += b)");
        check!(parse_expr, "a -= b", "(a -= b)");
        check!(parse_expr, "a <<= b", "(a <<= b)");
        check!(parse_expr, "a >>= b", "(a >>= b)");
        check!(parse_expr, "a &= b", "(a &= b)");
        check!(parse_expr, "a ^= b", "(a ^= b)");
        check!(parse_expr, "a |= b", "(a |= b)");

        check!(parse_expr, "a -= b &= c", "(a -= (b &= c))");
    }
}
