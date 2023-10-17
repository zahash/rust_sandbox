use std::{
    fmt::{self, Display, Formatter},
    ops::Deref,
};

use chainchomp::ctx_sensitive::{combine_parsers, maybe};

use crate::Token;

pub struct ParseContext<'text> {
    typedefs: Vec<&'text str>,
    enum_consts: Vec<&'text str>,
}

#[allow(dead_code)]
impl<'text> ParseContext<'text> {
    fn new() -> Self {
        Self {
            typedefs: vec![],
            enum_consts: vec![],
        }
    }

    fn set_typedef(&mut self, name: &'text str) {
        self.typedefs.push(name);
    }

    fn is_typedef(&self, name: &str) -> bool {
        self.typedefs.contains(&name)
    }

    fn set_enum_constant(&mut self, name: &'text str) {
        self.enum_consts.push(name);
    }

    fn is_enum_constant(&self, name: &str) -> bool {
        self.enum_consts.contains(&name)
    }
}

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(usize, &'static str),
    ExpectedIdent(usize),
    Expected(Token<'static>, usize),
    ExpectedOneOf(Vec<Token<'static>>, usize),
    InvalidDeclarationSpecifiers(usize, String),
}

pub fn parse<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(TranslationUnit<'text>, usize), ParseError> {
    parse_translation_unit(tokens, pos, ctx)
}

#[derive(Debug, PartialEq, Clone)]
pub struct TranslationUnit<'text>(pub Vec<ExternalDeclaration<'text>>);

fn parse_translation_unit<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(TranslationUnit<'text>, usize), ParseError> {
    let (eds, pos) = many(tokens, pos, ctx, parse_external_declaration, None);
    Ok((TranslationUnit(eds), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExternalDeclaration<'text> {
    FunctionDefinition(FunctionDefinition<'text>),
    Declaration(Declaration<'text>),
}

fn parse_external_declaration<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(ExternalDeclaration<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_function_definition),
            Box::new(parse_declaration),
        ],
        ParseError::SyntaxError(pos, "cannot parse external declaration"),
    )
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition<'text> {
    pub return_type: Vec<DeclarationSpecifier<'text>>,
    pub declarator: Declarator<'text>,
    pub declarations: Vec<Declaration<'text>>,
    pub body: CompoundStmt<'text>,
}

fn parse_function_definition<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(FunctionDefinition<'text>, usize), ParseError> {
    let (dss, pos) = many(tokens, pos, ctx, parse_declaration_specifier, None);
    let (declarator, pos) = parse_declarator(tokens, pos, ctx)?;
    let (declarations, pos) = many(tokens, pos, ctx, parse_declaration, None);
    let (body, pos) = parse_compound_stmt(tokens, pos, ctx)?;

    Ok((
        FunctionDefinition {
            return_type: dss,
            declarator,
            declarations,
            body,
        },
        pos,
    ))
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructOrUnionSpecifier<'text> {
    Named(StructOrUnion, &'text str, Vec<StructDeclaration<'text>>),
    Anonymous(StructOrUnion, Vec<StructDeclaration<'text>>),
    ForwardDeclaration(StructOrUnion, &'text str),
}

fn parse_struct_or_union_specifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(StructOrUnionSpecifier<'text>, usize), ParseError> {
    let (sou, pos) = parse_struct_or_union(tokens, pos, ctx)?;

    fn parse_struct_body<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(Vec<StructDeclaration<'text>>, usize), ParseError> {
        let Some(Token::Symbol("{")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("{"), pos));
        };

        let (sds, pos) = many(tokens, pos + 1, ctx, parse_struct_declaration, None);

        let Some(Token::Symbol("}")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("}"), pos));
        };

        return Ok((sds, pos + 1));
    }

    if let Some(Token::Ident(ident)) = tokens.get(pos) {
        return match parse_struct_body(tokens, pos + 1, ctx) {
            Ok((sds, pos)) => Ok((StructOrUnionSpecifier::Named(sou, ident, sds), pos)),
            Err(_) => Ok((
                StructOrUnionSpecifier::ForwardDeclaration(sou, ident),
                pos + 1,
            )),
        };
    }

    let (sds, pos) = parse_struct_body(tokens, pos, ctx)?;
    Ok((StructOrUnionSpecifier::Anonymous(sou, sds), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructOrUnion {
    Struct,
    Union,
}

fn parse_struct_or_union<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    _: &mut ParseContext<'text>,
) -> Result<(StructOrUnion, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Keyword("struct")) => Ok((StructOrUnion::Struct, pos + 1)),
        Some(Token::Keyword("union")) => Ok((StructOrUnion::Union, pos + 1)),
        _ => Err(ParseError::ExpectedOneOf(
            vec![Token::Keyword("struct"), Token::Keyword("union")],
            pos,
        )),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructDeclaration<'text> {
    pub specifier_qualifiers: Vec<SpecifierQualifier<'text>>,
    pub declarators: Vec<StructDeclarator<'text>>,
}

fn parse_struct_declaration<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(StructDeclaration<'text>, usize), ParseError> {
    let (sqs, pos) = many(tokens, pos, ctx, parse_specifier_qualifier, None);
    let (ds, pos) = many(
        tokens,
        pos,
        ctx,
        parse_struct_declarator,
        Some(Token::Symbol(",")),
    );

    let Some(Token::Symbol(";")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos));
    };

    Ok((
        StructDeclaration {
            specifier_qualifiers: sqs,
            declarators: ds,
        },
        pos + 1,
    ))
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
    ctx: &mut ParseContext<'text>,
) -> Result<(StructDeclarator<'text>, usize), ParseError> {
    let (declarator, pos) = maybe(tokens, pos, ctx, parse_declarator);

    let (bit_field, pos) = if let Some(Token::Symbol(":")) = tokens.get(pos) {
        let (bit_field, pos) = parse_conditional_expr(tokens, pos + 1, ctx)?;
        (Some(bit_field), pos)
    } else {
        (None, pos)
    };

    match (declarator, bit_field) {
        (None, None) => Err(ParseError::SyntaxError(
            pos,
            "cannot parse struct declarator. neither declarator nor bitfield found.",
        )),
        (None, Some(bit_field)) => Ok((StructDeclarator::BitField(bit_field), pos)),
        (Some(declarator), None) => Ok((StructDeclarator::Declarator(declarator), pos)),
        (Some(declarator), Some(bit_field)) => Ok((
            StructDeclarator::DeclaratorWithBitField(declarator, bit_field),
            pos,
        )),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declarator<'text> {
    pub pointer: Option<Pointer>,
    pub declarator: DirectDeclarator<'text>,
}

fn parse_declarator<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(Declarator<'text>, usize), ParseError> {
    let (pointer, pos) = maybe(tokens, pos, ctx, parse_pointer);
    let (dd, pos) = parse_direct_declarator(tokens, pos, ctx)?;

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
    ctx: &mut ParseContext<'text>,
) -> Result<(DirectDeclarator<'text>, usize), ParseError> {
    fn parse_ident<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectDeclarator<'text>, usize), ParseError> {
        let Some(Token::Ident(ident)) = tokens.get(pos) else {
            return Err(ParseError::ExpectedIdent(pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_declarator_tail);

        Ok((DirectDeclarator::Ident(&ident, dd_tail), pos))
    }

    fn parse_parens<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectDeclarator<'text>, usize), ParseError> {
        let Some(Token::Symbol("(")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("("), pos));
        };

        let (declarator, pos) = parse_declarator(tokens, pos + 1, ctx)?;

        let Some(Token::Symbol(")")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol(")"), pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_declarator_tail);

        Ok((DirectDeclarator::Parens(Box::new(declarator), dd_tail), pos))
    }

    combine_parsers(
        tokens,
        pos,
        ctx,
        &[Box::new(parse_ident), Box::new(parse_parens)],
        ParseError::SyntaxError(pos, "cannot parse direct declarator"),
    )
}

fn parse_direct_declarator_tail<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(DirectDeclaratorTail<'text>, usize), ParseError> {
    fn parse_array<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectDeclaratorTail<'text>, usize), ParseError> {
        let Some(Token::Symbol("[")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("["), pos));
        };

        let (expr, pos) = maybe(tokens, pos + 1, ctx, parse_constant_expr);

        let Some(Token::Symbol("]")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("]"), pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_declarator_tail);

        Ok((DirectDeclaratorTail::Array(expr, Box::new(dd_tail)), pos))
    }

    fn parse_function<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectDeclaratorTail<'text>, usize), ParseError> {
        let Some(Token::Symbol("(")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("("), pos));
        };

        let (list, pos) = parse_parameter_type_list(tokens, pos + 1, ctx)?;

        let Some(Token::Symbol(")")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol(")"), pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_declarator_tail);

        Ok((DirectDeclaratorTail::Function(list, Box::new(dd_tail)), pos))
    }

    fn parse_parameters<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectDeclaratorTail<'text>, usize), ParseError> {
        let Some(Token::Symbol("(")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("("), pos));
        };

        fn parse_ident<'text>(
            tokens: &[Token<'text>],
            pos: usize,
            _: &mut ParseContext<'text>,
        ) -> Result<(&'text str, usize), ParseError> {
            match tokens.get(pos) {
                Some(Token::Ident(ident)) => Ok((ident, pos + 1)),
                _ => Err(ParseError::ExpectedIdent(pos)),
            }
        }

        let (idents, pos) = many(tokens, pos + 1, ctx, parse_ident, Some(Token::Symbol(",")));

        let Some(Token::Symbol(")")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol(")"), pos));
        };

        let (dd_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_declarator_tail);

        Ok((
            DirectDeclaratorTail::Parameters(idents, Box::new(dd_tail)),
            pos,
        ))
    }

    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_array),
            Box::new(parse_function),
            Box::new(parse_parameters),
        ],
        ParseError::SyntaxError(pos, "cannot parse direct declarator tail"),
    )
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeName<'text> {
    pub specifier_qualifiers: Vec<SpecifierQualifier<'text>>,
    pub abstract_declarator: Box<Option<AbstractDeclarator<'text>>>,
}

fn parse_type_name<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(TypeName<'text>, usize), ParseError> {
    let (sqs, pos) = many(tokens, pos, ctx, parse_specifier_qualifier, None);
    if sqs.is_empty() {
        return Err(ParseError::SyntaxError(
            pos,
            "parse_type_name: expected atleast one specifier qualifier",
        ));
    }

    let (ad, pos) = maybe(tokens, pos, ctx, parse_abstract_declarator);

    Ok((
        TypeName {
            specifier_qualifiers: sqs,
            abstract_declarator: Box::new(ad),
        },
        pos,
    ))
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParameterTypeList<'text> {
    ParameterList(Vec<ParameterDeclaration<'text>>),
    VariadicParameterList(Vec<ParameterDeclaration<'text>>),
}

fn parse_parameter_type_list<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(ParameterTypeList<'text>, usize), ParseError> {
    let (declarations, pos) = many(
        tokens,
        pos,
        ctx,
        parse_parameter_declaration,
        Some(Token::Symbol(",")),
    );

    if let Some(Token::Symbol("...")) = tokens.get(pos) {
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
    ctx: &mut ParseContext<'text>,
) -> Result<(ParameterDeclaration<'text>, usize), ParseError> {
    let (dss, pos) = many(tokens, pos, ctx, parse_declaration_specifier, None);
    if dss.is_empty() {
        return Err(ParseError::SyntaxError(
            pos,
            "parse_declaration: expected atleast one declaration specifier",
        ));
    }

    let (d, pos) = maybe(tokens, pos, ctx, parse_declarator);
    let (ad, pos) = maybe(tokens, pos, ctx, parse_abstract_declarator);

    match (d, ad) {
        (None, None) => Ok((ParameterDeclaration::OnlySpecifiers(dss), pos)),
        (None, Some(ad)) => Ok((ParameterDeclaration::WithAbstractDeclarator(dss, ad), pos)),
        (Some(d), None) => Ok((ParameterDeclaration::WithDeclarator(dss, d), pos)),
        (Some(_), Some(_)) => Err(ParseError::SyntaxError(
            pos,
            "cannot parse parameter declaration. can have either declarator or abstract declarator but not both.",
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
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(AbstractDeclarator<'text>, usize), ParseError> {
    let (pointer, pos) = maybe(tokens, pos, ctx, parse_pointer);
    let (dad, pos) = maybe(tokens, pos, ctx, parse_direct_abstract_declarator);

    match (pointer, dad) {
        (None, None) => Err(ParseError::SyntaxError(
            pos,
            "cannot parse abstract declarator. neither pointer nor abstract direct declarator found.",
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
    ctx: &mut ParseContext<'text>,
) -> Result<(DirectAbstractDeclarator<'text>, usize), ParseError> {
    fn parse_parens<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectAbstractDeclarator<'text>, usize), ParseError> {
        let Some(Token::Symbol("(")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("("), pos));
        };

        let (declarator, pos) = parse_abstract_declarator(tokens, pos + 1, ctx)?;

        let Some(Token::Symbol(")")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol(")"), pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_abstract_declarator_tail);

        Ok((
            DirectAbstractDeclarator::Parens(Box::new(declarator), dad_tail),
            pos,
        ))
    }

    fn parse_array<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectAbstractDeclarator<'text>, usize), ParseError> {
        let Some(Token::Symbol("[")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("["), pos));
        };

        let (expr, pos) = maybe(tokens, pos + 1, ctx, parse_constant_expr);

        let Some(Token::Symbol("]")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("]"), pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_abstract_declarator_tail);

        Ok((DirectAbstractDeclarator::Array(expr, dad_tail), pos))
    }

    fn parse_function<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectAbstractDeclarator<'text>, usize), ParseError> {
        let Some(Token::Symbol("(")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("("), pos));
        };

        let (parameter_type_list, pos) = maybe(tokens, pos + 1, ctx, parse_parameter_type_list);

        let Some(Token::Symbol(")")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol(")"), pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_abstract_declarator_tail);

        Ok((
            DirectAbstractDeclarator::Function(parameter_type_list, dad_tail),
            pos,
        ))
    }

    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_parens),
            Box::new(parse_array),
            Box::new(parse_function),
        ],
        ParseError::SyntaxError(pos, "cannot parse direct abstract declarator"),
    )
}

fn parse_direct_abstract_declarator_tail<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(DirectAbstractDeclaratorTail<'text>, usize), ParseError> {
    fn parse_array<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectAbstractDeclaratorTail<'text>, usize), ParseError> {
        let Some(Token::Symbol("[")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("["), pos));
        };

        let (expr, pos) = maybe(tokens, pos + 1, ctx, parse_constant_expr);

        let Some(Token::Symbol("]")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("]"), pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_abstract_declarator_tail);

        Ok((
            DirectAbstractDeclaratorTail::Array(expr, Box::new(dad_tail)),
            pos,
        ))
    }

    fn parse_function<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(DirectAbstractDeclaratorTail<'text>, usize), ParseError> {
        let Some(Token::Symbol("(")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("("), pos));
        };

        let (parameter_type_list, pos) = maybe(tokens, pos + 1, ctx, parse_parameter_type_list);

        let Some(Token::Symbol(")")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol(")"), pos));
        };

        let (dad_tail, pos) = maybe(tokens, pos + 1, ctx, parse_direct_abstract_declarator_tail);

        Ok((
            DirectAbstractDeclaratorTail::Function(parameter_type_list, Box::new(dad_tail)),
            pos,
        ))
    }

    combine_parsers(
        tokens,
        pos,
        ctx,
        &[Box::new(parse_array), Box::new(parse_function)],
        ParseError::SyntaxError(pos, "cannot parse direct abstract declarator tail"),
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
    ctx: &mut ParseContext<'text>,
) -> Result<(Declaration<'text>, usize), ParseError> {
    let (dss, pos) = many(tokens, pos, ctx, parse_declaration_specifier, None);
    if dss.is_empty() {
        return Err(ParseError::SyntaxError(
            pos,
            "parse_declaration: expected atleast one declaration specifier",
        ));
    }

    let (init_declarators, pos) = many(
        tokens,
        pos,
        ctx,
        parse_init_declarator,
        Some(Token::Symbol(",")),
    );

    let Some(Token::Symbol(";")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos));
    };

    let validated_dss: ValidatedDeclarationSpecifiers<'text> = dss
        .try_into()
        .map_err(|err| ParseError::InvalidDeclarationSpecifiers(pos, err))?;

    if validated_dss.storage_class_specifier == Some(StorageClassSpecifier::TypeDef) {
        if let Some(InitDeclarator::Declared(Declarator {
            pointer: None,
            declarator: DirectDeclarator::Ident(ident, None),
        })) = init_declarators.get(0)
        {
            ctx.set_typedef(ident);
        }
    }

    Ok((
        Declaration {
            declaration_specifiers: validated_dss.into(),
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
    ctx: &mut ParseContext<'text>,
) -> Result<(InitDeclarator<'text>, usize), ParseError> {
    let (declarator, pos) = parse_declarator(tokens, pos, ctx)?;

    let Some(Token::Symbol("=")) = tokens.get(pos) else {
        return Ok((InitDeclarator::Declared(declarator), pos));
    };

    let (initializer, pos) = parse_initializer(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(Initializer<'text>, usize), ParseError> {
    if let Some(Token::Symbol("{")) = tokens.get(pos) {
        let mut initializers = Vec::new();
        pos += 1;

        while let Some(token) = tokens.get(pos) {
            if token == &Token::Symbol("}") {
                pos += 1;
                break;
            }

            let (initializer, next_pos) = parse_initializer(tokens, pos, ctx)?;
            pos = next_pos;

            initializers.push(initializer);

            match tokens.get(pos) {
                Some(Token::Symbol(",")) => pos += 1,
                Some(Token::Symbol("}")) => {
                    pos += 1;
                    break;
                }
                _ => return Err(ParseError::Expected(Token::Symbol("}"), pos)),
            }
        }

        return Ok((Initializer::InitializerList(initializers), pos));
    }

    let (expr, pos) = parse_assignment_expr(tokens, pos, ctx)?;
    Ok((Initializer::Assignment(expr), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationSpecifier<'text> {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier<'text>),
    TypeQualifier(TypeQualifier),
}

struct ValidatedDeclarationSpecifiers<'text> {
    storage_class_specifier: Option<StorageClassSpecifier>,
    type_qualifiers: Vec<TypeQualifier>,
    type_specifiers: Vec<TypeSpecifier<'text>>,
}

impl<'text> From<ValidatedDeclarationSpecifiers<'text>> for Vec<DeclarationSpecifier<'text>> {
    fn from(validated_dss: ValidatedDeclarationSpecifiers<'text>) -> Self {
        let mut dss = vec![];

        if let Some(scs) = validated_dss.storage_class_specifier {
            dss.push(DeclarationSpecifier::StorageClassSpecifier(scs));
        }
        dss.extend(
            validated_dss
                .type_qualifiers
                .into_iter()
                .map(|tq| DeclarationSpecifier::TypeQualifier(tq)),
        );
        dss.extend(
            validated_dss
                .type_specifiers
                .into_iter()
                .map(|ts| DeclarationSpecifier::TypeSpecifier(ts)),
        );
        dss
    }
}

impl<'text> TryFrom<Vec<DeclarationSpecifier<'text>>> for ValidatedDeclarationSpecifiers<'text> {
    type Error = String;

    fn try_from(dss: Vec<DeclarationSpecifier<'text>>) -> Result<Self, Self::Error> {
        let dss_len = dss.len();

        let mut storage_class_specifiers = vec![];
        let mut type_specifiers = vec![];
        let mut type_qualifiers = vec![];

        for ds in dss {
            match ds {
                DeclarationSpecifier::StorageClassSpecifier(scs) => {
                    storage_class_specifiers.push(scs)
                }
                DeclarationSpecifier::TypeSpecifier(ts) => type_specifiers.push(ts),
                DeclarationSpecifier::TypeQualifier(tq) => type_qualifiers.push(tq),
            }
        }

        if storage_class_specifiers.len() > 1 {
            return Err(format!(
                "cannot have more than 1 storage class specifier. Found {}. {:?}",
                storage_class_specifiers.len(),
                storage_class_specifiers
            ));
        }
        let storage_class_specifier = storage_class_specifiers.into_iter().next();

        let mut simple_type_specifiers = vec![];
        let mut struct_or_union_specifiers = vec![];
        let mut enum_specifiers = vec![];
        let mut typedef_names = vec![];
        for ts in &type_specifiers {
            match ts {
                TypeSpecifier::StructOrUnionSpecifier(sus) => struct_or_union_specifiers.push(sus),
                TypeSpecifier::EnumSpecifier(es) => enum_specifiers.push(es),
                TypeSpecifier::TypeDefName(name) => typedef_names.push(name),
                _ => simple_type_specifiers.push(ts),
            }
        }

        let simple_type_specifiers_present = !simple_type_specifiers.is_empty();
        let struct_or_union_specifiers_present = !struct_or_union_specifiers.is_empty();
        let enum_specifiers_present = !enum_specifiers.is_empty();
        let typedef_names_present = !typedef_names.is_empty();

        let exactly_one_type_is_present = simple_type_specifiers_present
            ^ struct_or_union_specifiers_present
            ^ enum_specifiers_present
            ^ typedef_names_present;

        if !exactly_one_type_is_present {
            return Err(format!(
            "either enum specifier or struct specifier or typedef name or simple specifier (void, int, ...) must be present. \
            Eg: `long long unsigned` or `enum {{ A, B }}` or `struct {{ int a; }}` or `Person` are allowed \
            but not `long enum {{ A, B }}` because it contains both simple specifier (long) and enum specifier. \
            Right now, this contains {}{}{}{}
            ",
            if simple_type_specifiers_present {"simple type specifiers, "} else {""},
            if struct_or_union_specifiers_present {"struct or union specifiers, "} else {""},
            if enum_specifiers_present {"enum specifiers, "} else {""},
            if typedef_names_present {"typedef names"} else {""},
        ));
        }

        // just a sanity check
        assert_eq!(
            dss_len,
            storage_class_specifier.is_some() as usize
                + type_qualifiers.len()
                + type_specifiers.len(),
            "declaration specifiers length mismatch after validation"
        );

        Ok(ValidatedDeclarationSpecifiers {
            storage_class_specifier,
            type_qualifiers,
            type_specifiers,
        })
    }
}

fn parse_declaration_specifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(DeclarationSpecifier<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_storage_class_specifier),
            Box::new(parse_type_specifier),
            Box::new(parse_type_qualifier),
        ],
        ParseError::SyntaxError(pos, "cannot parse declaration specifier"),
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
    _: &mut ParseContext<'text>,
) -> Result<(StorageClassSpecifier, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Keyword("auto")) => Ok((StorageClassSpecifier::Auto, pos + 1)),
        Some(Token::Keyword("register")) => Ok((StorageClassSpecifier::Register, pos + 1)),
        Some(Token::Keyword("static")) => Ok((StorageClassSpecifier::Static, pos + 1)),
        Some(Token::Keyword("extern")) => Ok((StorageClassSpecifier::Extern, pos + 1)),
        Some(Token::Keyword("typedef")) => Ok((StorageClassSpecifier::TypeDef, pos + 1)),
        _ => Err(ParseError::ExpectedOneOf(
            vec![
                Token::Keyword("auto"),
                Token::Keyword("register"),
                Token::Keyword("static"),
                Token::Keyword("extern"),
                Token::Keyword("typedef"),
            ],
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
    ctx: &mut ParseContext<'text>,
) -> Result<(SpecifierQualifier<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_type_specifier),
            Box::new(parse_type_qualifier),
        ],
        ParseError::SyntaxError(pos, "cannot parse specifier qualifier"),
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
    StructOrUnionSpecifier(StructOrUnionSpecifier<'text>),
    EnumSpecifier(EnumSpecifier<'text>),
    TypeDefName(&'text str),
}

fn parse_type_specifier<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(TypeSpecifier<'text>, usize), ParseError> {
    fn parse_basic_type_specifier<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        _: &mut ParseContext<'text>,
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
            _ => Err(ParseError::ExpectedOneOf(
                vec![
                    Token::Keyword("void"),
                    Token::Keyword("char"),
                    Token::Keyword("short"),
                    Token::Keyword("int"),
                    Token::Keyword("long"),
                    Token::Keyword("float"),
                    Token::Keyword("double"),
                    Token::Keyword("signed"),
                    Token::Keyword("unsigned"),
                ],
                pos,
            )),
        }
    }

    fn parse_typedef_name<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(TypeSpecifier<'text>, usize), ParseError> {
        match tokens.get(pos) {
            Some(Token::Ident(ident)) if ctx.is_typedef(ident) => {
                Ok((TypeSpecifier::TypeDefName(ident), pos + 1))
            }
            _ => Err(ParseError::ExpectedIdent(pos)),
        }
    }

    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_basic_type_specifier),
            Box::new(parse_struct_or_union_specifier),
            Box::new(parse_enum_specifier),
            Box::new(parse_typedef_name),
        ],
        ParseError::SyntaxError(pos, "cannot parse type specifier"),
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
    ctx: &mut ParseContext<'text>,
) -> Result<(Pointer, usize), ParseError> {
    let Some(Token::Symbol("*")) = tokens.get(pos) else {
        return Err(ParseError::SyntaxError(pos, "parse_pointer: expected `*`"));
    };
    pos += 1;

    let mut pointer = Pointer {
        qualifiers: vec![],
        next: None,
    };

    loop {
        match parse_type_qualifier(tokens, pos, ctx) {
            Err(_) => break,
            Ok((qualifier, next_pos)) => {
                pointer.qualifiers.push(qualifier);
                pos = next_pos;
            }
        }
    }

    match parse_pointer(tokens, pos, ctx) {
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
    _: &mut ParseContext<'text>,
) -> Result<(TypeQualifier, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Keyword("const")) => Ok((TypeQualifier::Const, pos + 1)),
        Some(Token::Keyword("volatile")) => Ok((TypeQualifier::Volatile, pos + 1)),
        _ => Err(ParseError::ExpectedOneOf(
            vec![Token::Keyword("const"), Token::Keyword("volatile")],
            pos,
        )),
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
    ctx: &mut ParseContext<'text>,
) -> Result<(EnumSpecifier<'text>, usize), ParseError> {
    let Some(Token::Keyword("enum")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("enum"), pos));
    };

    fn parse_enum_body<'text>(
        tokens: &[Token<'text>],
        pos: usize,
        ctx: &mut ParseContext<'text>,
    ) -> Result<(Vec<Enumerator<'text>>, usize), ParseError> {
        let Some(Token::Symbol("{")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("{"), pos));
        };

        let (enum_constants, pos) = many(
            tokens,
            pos + 1,
            ctx,
            parse_enumerator,
            Some(Token::Symbol(",")),
        );

        for Enumerator::Implicit(c) | Enumerator::Explicit(c, _) in &enum_constants {
            ctx.set_enum_constant(c);
        }

        let Some(Token::Symbol("}")) = tokens.get(pos) else {
            return Err(ParseError::Expected(Token::Symbol("}"), pos));
        };

        return Ok((enum_constants, pos + 1));
    }

    if let Some(Token::Ident(ident)) = tokens.get(pos + 1) {
        return match parse_enum_body(tokens, pos + 2, ctx) {
            Ok((enumerators, pos)) => Ok((EnumSpecifier::Named(ident, enumerators), pos)),
            Err(_) => Ok((EnumSpecifier::ForwardDeclaration(ident), pos + 2)),
        };
    }

    let (list, pos) = parse_enum_body(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(Enumerator<'text>, usize), ParseError> {
    let Some(Token::Ident(ident)) = tokens.get(pos) else {
        return Err(ParseError::ExpectedIdent(pos));
    };

    let Some(Token::Symbol("=")) = tokens.get(pos + 1) else {
        return Ok((Enumerator::Implicit(ident), pos + 1));
    };

    let (expr, pos) = parse_constant_expr(tokens, pos + 2, ctx)?;

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
    ctx: &mut ParseContext<'text>,
) -> Result<(Stmt<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_labeled_stmt),
            Box::new(parse_empty_stmt),
            Box::new(parse_expr_stmt),
            Box::new(parse_compound_stmt),
            Box::new(parse_selection_stmt),
            Box::new(parse_iteration_stmt),
            Box::new(parse_jump_stmt),
        ],
        ParseError::SyntaxError(pos, "cannot parse statement"),
    )
}

#[derive(Debug, PartialEq, Clone)]
pub enum LabeledStmt<'text> {
    Ident(&'text str, Box<Stmt<'text>>),
    Case(ConstantExpr<'text>, Box<Stmt<'text>>),
    Default(Box<Stmt<'text>>),
}

fn parse_labeled_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(LabeledStmt<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_labeled_ident_stmt),
            Box::new(parse_labeled_case_stmt),
            Box::new(parse_labeled_default_stmt),
        ],
        ParseError::SyntaxError(pos, "cannot parse labeled statement"),
    )
}

fn parse_labeled_ident_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(LabeledStmt<'text>, usize), ParseError> {
    let Some(Token::Ident(ident)) = tokens.get(pos) else {
        return Err(ParseError::ExpectedIdent(pos));
    };

    let Some(Token::Symbol(":")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol(":"), pos));
    };

    let (stmt, pos) = parse_stmt(tokens, pos + 2, ctx)?;
    Ok((LabeledStmt::Ident(ident, Box::new(stmt)), pos))
}

fn parse_labeled_case_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(LabeledStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("case")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedIdent(pos));
    };

    let (expr, pos) = parse_constant_expr(tokens, pos + 1, ctx)?;

    let Some(Token::Symbol(":")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(":"), pos));
    };

    let (stmt, pos) = parse_stmt(tokens, pos + 1, ctx)?;
    Ok((LabeledStmt::Case(expr, Box::new(stmt)), pos))
}

fn parse_labeled_default_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(LabeledStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("default")) = tokens.get(pos) else {
        return Err(ParseError::ExpectedIdent(pos));
    };

    let Some(Token::Symbol(":")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol(":"), pos));
    };

    let (stmt, pos) = parse_stmt(tokens, pos + 2, ctx)?;
    Ok((LabeledStmt::Default(Box::new(stmt)), pos))
}

fn parse_empty_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    _: &mut ParseContext<'text>,
) -> Result<(Stmt<'text>, usize), ParseError> {
    let Some(Token::Symbol(";")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos));
    };

    Ok((Stmt::EmptyStmt, pos + 1))
}

fn parse_expr_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(Stmt<'text>, usize), ParseError> {
    let (expr, pos) = parse_expr(tokens, pos, ctx)?;

    let Some(Token::Symbol(";")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos));
    };

    Ok((Stmt::Expr(expr), pos + 1))
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompoundStmt<'text>(pub Vec<BlockItem<'text>>);

fn parse_compound_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(CompoundStmt<'text>, usize), ParseError> {
    let Some(Token::Symbol("{")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol("{"), pos));
    };

    let (items, pos) = many(tokens, pos + 1, ctx, parse_block_item, None);

    let Some(Token::Symbol("}")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol("}"), pos));
    };

    Ok((CompoundStmt(items), pos + 1))
}

#[derive(Debug, PartialEq, Clone)]
pub enum BlockItem<'text> {
    Declaration(Declaration<'text>),
    Statement(Stmt<'text>),
}

fn parse_block_item<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(BlockItem<'text>, usize), ParseError> {
    if let (Some(d), pos) = maybe(tokens, pos, ctx, parse_declaration) {
        return Ok((BlockItem::Declaration(d), pos));
    }

    if let (Some(s), pos) = maybe(tokens, pos, ctx, parse_stmt) {
        return Ok((BlockItem::Statement(s), pos));
    }

    Err(ParseError::SyntaxError(
        pos,
        "parse_block_item: expected declaration or statement",
    ))
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
    Switch {
        test: Expr<'text>,
        pass: Box<Stmt<'text>>,
    },
}

fn parse_selection_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(SelectionStmt<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_selection_if_else_stmt),
            Box::new(parse_selection_switch_stmt),
        ],
        ParseError::SyntaxError(pos, "cannot parse selection statement"),
    )
}

fn parse_selection_if_else_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(SelectionStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("if")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("if"), pos));
    };

    let Some(Token::Symbol("(")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol("("), pos + 1));
    };

    let (test, pos) = parse_expr(tokens, pos + 2, ctx)?;

    let Some(Token::Symbol(")")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(")"), pos));
    };

    let (pass, pos) = parse_stmt(tokens, pos + 1, ctx)?;
    let pass = Box::new(pass);

    let Some(Token::Keyword("else")) = tokens.get(pos) else {
        return Ok((SelectionStmt::If { test, pass }, pos));
    };

    let (fail, pos) = parse_stmt(tokens, pos + 1, ctx)?;
    let fail = Box::new(fail);

    Ok((SelectionStmt::IfElse { test, pass, fail }, pos))
}

fn parse_selection_switch_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(SelectionStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("switch")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("switch"), pos));
    };

    let Some(Token::Symbol("(")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol("("), pos + 1));
    };

    let (test, pos) = parse_expr(tokens, pos + 2, ctx)?;

    let Some(Token::Symbol(")")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(")"), pos));
    };

    let (pass, pos) = parse_stmt(tokens, pos + 1, ctx)?;
    let pass = Box::new(pass);

    Ok((SelectionStmt::Switch { test, pass }, pos))
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

fn parse_iteration_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(IterationStmt<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_iteration_while_stmt),
            Box::new(parse_iteration_do_while_stmt),
            Box::new(parse_iteration_for_stmt),
        ],
        ParseError::SyntaxError(pos, "cannot parse iteration statement"),
    )
}

fn parse_iteration_while_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(IterationStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("while")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("while"), pos));
    };

    let Some(Token::Symbol("(")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol("("), pos + 1));
    };

    let (test, pos) = parse_expr(tokens, pos + 2, ctx)?;

    let Some(Token::Symbol(")")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(")"), pos));
    };

    let (body, pos) = parse_stmt(tokens, pos + 1, ctx)?;
    let body = Box::new(body);

    Ok((IterationStmt::While { test, body }, pos))
}

fn parse_iteration_do_while_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(IterationStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("do")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("do"), pos));
    };

    let (body, pos) = parse_stmt(tokens, pos + 1, ctx)?;
    let body = Box::new(body);

    let Some(Token::Keyword("while")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("while"), pos));
    };

    let Some(Token::Symbol("(")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol("("), pos + 1));
    };

    let (test, pos) = parse_expr(tokens, pos + 2, ctx)?;

    let Some(Token::Symbol(")")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(")"), pos));
    };

    let Some(Token::Symbol(";")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos + 1));
    };

    Ok((IterationStmt::DoWhile { test, body }, pos + 2))
}

fn parse_iteration_for_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(IterationStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("for")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("for"), pos));
    };

    let Some(Token::Symbol("(")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol("("), pos + 1));
    };

    let (init, pos) = match tokens.get(pos + 2) {
        Some(Token::Symbol(";")) => (None, pos + 2),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 2, ctx)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::Symbol(";")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos));
    };

    let (test, pos) = match tokens.get(pos + 1) {
        Some(Token::Symbol(";")) => (None, pos + 1),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 1, ctx)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::Symbol(";")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos));
    };

    let (update, pos) = match tokens.get(pos + 1) {
        Some(Token::Symbol(")")) => (None, pos + 1),
        _ => {
            let (expr, pos) = parse_expr(tokens, pos + 1, ctx)?;
            (Some(expr), pos)
        }
    };

    let Some(Token::Symbol(")")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(")"), pos));
    };

    let (body, pos) = parse_stmt(tokens, pos + 1, ctx)?;
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

fn parse_jump_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    combine_parsers(
        tokens,
        pos,
        ctx,
        &[
            Box::new(parse_jump_goto_stmt),
            Box::new(parse_jump_continue_stmt),
            Box::new(parse_jump_break_stmt),
            Box::new(parse_jump_return_stmt),
        ],
        ParseError::SyntaxError(pos, "cannot parse jump statement"),
    )
}

fn parse_jump_goto_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    _: &mut ParseContext<'text>,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("goto")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("goto"), pos));
    };

    let Some(Token::Ident(ident)) = tokens.get(pos + 1) else {
        return Err(ParseError::ExpectedIdent(pos + 1));
    };

    let Some(Token::Symbol(";")) = tokens.get(pos + 2) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos + 2));
    };

    Ok((JumpStmt::Goto(ident), pos + 3))
}

fn parse_jump_continue_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    _: &mut ParseContext<'text>,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("continue")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("continue"), pos));
    };

    let Some(Token::Symbol(";")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos + 1));
    };

    Ok((JumpStmt::Continue, pos + 2))
}

fn parse_jump_break_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    _: &mut ParseContext<'text>,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("break")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("break"), pos));
    };

    let Some(Token::Symbol(";")) = tokens.get(pos + 1) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos + 1));
    };

    Ok((JumpStmt::Break, pos + 2))
}

fn parse_jump_return_stmt<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(JumpStmt<'text>, usize), ParseError> {
    let Some(Token::Keyword("return")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Keyword("return"), pos));
    };

    let (expr, pos) = maybe(tokens, pos + 1, ctx, parse_expr);

    let Some(Token::Symbol(";")) = tokens.get(pos) else {
        return Err(ParseError::Expected(Token::Symbol(";"), pos));
    };

    return Ok((JumpStmt::Return(expr), pos + 1));
}

fn many<'text, Ast>(
    tokens: &[Token<'text>],
    mut pos: usize,
    ctx: &mut ParseContext<'text>,
    parser: impl Fn(
        &[Token<'text>],
        usize,
        &mut ParseContext<'text>,
    ) -> Result<(Ast, usize), ParseError>,
    delimiter: Option<Token>,
) -> (Vec<Ast>, usize) {
    let mut list = vec![];

    while let Ok((ast, next_pos)) = parser(tokens, pos, ctx) {
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

pub type Expr<'text> = AssignmentExpr<'text>;

fn parse_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(Expr<'text>, usize), ParseError> {
    parse_assignment_expr(tokens, pos, ctx)
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
    ctx: &mut ParseContext<'text>,
) -> Result<(AssignmentExpr<'text>, usize), ParseError> {
    if let Ok((unary, pos)) = parse_unary_expr(tokens, pos, ctx) {
        if let Some(op) = tokens.get(pos) {
            if op == &Token::Symbol("=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::Assign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("*=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::MulAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("/=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::DivAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("%=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::ModAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("+=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::AddAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("-=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::SubAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("<<=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::ShiftLeftAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol(">>=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::ShiftRightAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("&=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::BitAndAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("^=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::XORAssign(unary, Box::new(rhs)), pos));
            }

            if op == &Token::Symbol("|=") {
                let (rhs, pos) = parse_assignment_expr(tokens, pos + 1, ctx)?;
                return Ok((AssignmentExpr::BitOrAssign(unary, Box::new(rhs)), pos));
            }
        }
    }

    let (expr, pos) = parse_conditional_expr(tokens, pos, ctx)?;
    Ok((expr.into(), pos))
}

pub type ConstantExpr<'text> = ConditionalExpr<'text>;

fn parse_constant_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(ConstantExpr<'text>, usize), ParseError> {
    parse_conditional_expr(tokens, pos, ctx)
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
    ctx: &mut ParseContext<'text>,
) -> Result<(ConditionalExpr<'text>, usize), ParseError> {
    let (test, mut pos) = parse_logicalor_expr(tokens, pos, ctx)?;

    if let Some(Token::Symbol("?")) = tokens.get(pos) {
        let (pass, next_pos) = parse_expr(tokens, pos + 1, ctx)?;
        pos = next_pos;

        if let Some(Token::Symbol(":")) = tokens.get(pos) {
            let (fail, next_pos) = parse_conditional_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(LogicalOrExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_logicaland_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("||") => {
                let (rhs, next_pos) = parse_logicaland_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(LogicalAndExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_bitor_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("&&") => {
                let (rhs, next_pos) = parse_bitor_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(BitOrExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_xor_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("|") => {
                let (rhs, next_pos) = parse_xor_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(XORExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_bitand_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("^") => {
                let (rhs, next_pos) = parse_bitand_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(BitAndExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_equality_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("&") => {
                let (rhs, next_pos) = parse_equality_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(EqualityExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_comparision_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("==") => {
                let (rhs, next_pos) = parse_comparision_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = EqualityExpr::EQ(Box::new(lhs), rhs);
            }
            Token::Symbol("!=") => {
                let (rhs, next_pos) = parse_comparision_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(ComparisionExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_shift_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("<") => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = ComparisionExpr::LT(Box::new(lhs), rhs);
            }
            Token::Symbol(">") => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = ComparisionExpr::GT(Box::new(lhs), rhs);
            }
            Token::Symbol("<=") => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = ComparisionExpr::LE(Box::new(lhs), rhs);
            }
            Token::Symbol(">=") => {
                let (rhs, next_pos) = parse_shift_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(ShiftExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_additive_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("<<") => {
                let (rhs, next_pos) = parse_additive_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = ShiftExpr::ShiftLeft(Box::new(lhs), rhs);
            }
            Token::Symbol(">>") => {
                let (rhs, next_pos) = parse_additive_expr(tokens, pos + 1, ctx)?;
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
    ctx: &mut ParseContext<'text>,
) -> Result<(AdditiveExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_multiplicative_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("+") => {
                let (rhs, next_pos) = parse_multiplicative_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = AdditiveExpr::Add(Box::new(lhs), rhs);
            }
            Token::Symbol("-") => {
                let (rhs, next_pos) = parse_multiplicative_expr(tokens, pos + 1, ctx)?;
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
    CastExpr(CastExpr<'text>),
    Mul(Box<MultiplicativeExpr<'text>>, CastExpr<'text>),
    Div(Box<MultiplicativeExpr<'text>>, CastExpr<'text>),
    Mod(Box<MultiplicativeExpr<'text>>, CastExpr<'text>),
}

fn parse_multiplicative_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(MultiplicativeExpr<'text>, usize), ParseError> {
    let (lhs, mut pos) = parse_cast_expr(tokens, pos, ctx)?;
    let mut lhs = lhs.into();
    while let Some(token) = tokens.get(pos) {
        match token {
            Token::Symbol("*") => {
                let (rhs, next_pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Mul(Box::new(lhs), rhs);
            }
            Token::Symbol("/") => {
                let (rhs, next_pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Div(Box::new(lhs), rhs);
            }
            Token::Symbol("%") => {
                let (rhs, next_pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
                pos = next_pos;
                lhs = MultiplicativeExpr::Mod(Box::new(lhs), rhs);
            }
            _ => break,
        }
    }
    Ok((lhs, pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum CastExpr<'text> {
    UnaryExpr(UnaryExpr<'text>),
    Cast(TypeName<'text>, Box<CastExpr<'text>>),
}

fn parse_cast_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(CastExpr<'text>, usize), ParseError> {
    if let Some(Token::Symbol("(")) = tokens.get(pos) {
        // if its not a TypeName in parens, it must've been just a normal <primary-expression> ::= ( <expression> )
        if let Ok((type_name, pos)) = parse_type_name(tokens, pos + 1, ctx) {
            let Some(Token::Symbol(")")) = tokens.get(pos) else {
                return Err(ParseError::Expected(Token::Symbol(")"), pos));
            };
            let (expr, pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
            return Ok((CastExpr::Cast(type_name, Box::new(expr)), pos));
        }
    }

    let (expr, pos) = parse_unary_expr(tokens, pos, ctx)?;
    Ok((CastExpr::UnaryExpr(expr), pos))
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryExpr<'text> {
    PostfixExpr(PostfixExpr<'text>),
    PreIncr(Box<UnaryExpr<'text>>),
    PreDecr(Box<UnaryExpr<'text>>),
    Ref(Box<CastExpr<'text>>),
    Deref(Box<CastExpr<'text>>),
    UnaryAdd(Box<CastExpr<'text>>),
    UnarySub(Box<CastExpr<'text>>),
    OnesComplement(Box<CastExpr<'text>>),
    Not(Box<CastExpr<'text>>),
    SizeofExpr(Box<UnaryExpr<'text>>),
    SizeofTypeName(TypeName<'text>),
}

fn parse_unary_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(UnaryExpr<'text>, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Symbol("++")) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::PreIncr(Box::new(expr)), pos))
        }
        Some(Token::Symbol("--")) => {
            let (expr, pos) = parse_unary_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::PreDecr(Box::new(expr)), pos))
        }
        Some(Token::Symbol("&")) => {
            let (expr, pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::Ref(Box::new(expr)), pos))
        }
        Some(Token::Symbol("*")) => {
            let (expr, pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::Deref(Box::new(expr)), pos))
        }
        Some(Token::Symbol("+")) => {
            let (expr, pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::UnaryAdd(Box::new(expr)), pos))
        }
        Some(Token::Symbol("-")) => {
            let (expr, pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::UnarySub(Box::new(expr)), pos))
        }
        Some(Token::Symbol("~")) => {
            let (expr, pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::OnesComplement(Box::new(expr)), pos))
        }
        Some(Token::Symbol("!")) => {
            let (expr, pos) = parse_cast_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::Not(Box::new(expr)), pos))
        }
        Some(Token::Keyword("sizeof")) => {
            if let Some(Token::Symbol("(")) = tokens.get(pos + 1) {
                // if its not a TypeName in parens, it must've been just a normal <primary-expression> ::= ( <expression> )
                if let Ok((type_name, pos)) = parse_type_name(tokens, pos + 2, ctx) {
                    let Some(Token::Symbol(")")) = tokens.get(pos) else {
                        return Err(ParseError::Expected(Token::Symbol(")"), pos));
                    };
                    return Ok((UnaryExpr::SizeofTypeName(type_name), pos + 1));
                }
            }

            let (expr, pos) = parse_unary_expr(tokens, pos + 1, ctx)?;
            Ok((UnaryExpr::SizeofExpr(Box::new(expr)), pos))
        }
        _ => {
            let (expr, pos) = parse_postfix_expr(tokens, pos, ctx)?;
            Ok((expr.into(), pos))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PostfixExpr<'text> {
    Primary(Primary<'text>),
    ArrayAccess(Box<PostfixExpr<'text>>, Box<Expr<'text>>),
    FunctionCall(Box<PostfixExpr<'text>>, Vec<AssignmentExpr<'text>>),
    MemberAccess(Box<PostfixExpr<'text>>, &'text str),
    PointerMemberAccess(Box<PostfixExpr<'text>>, &'text str),
    PostIncr(Box<PostfixExpr<'text>>),
    PostDecr(Box<PostfixExpr<'text>>),
}

fn parse_postfix_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(PostfixExpr<'text>, usize), ParseError> {
    let (expr, pos) = parse_primary_expr(tokens, pos, ctx)?;

    match tokens.get(pos) {
        Some(Token::Symbol("[")) => {
            let (access, pos) = parse_expr(tokens, pos + 1, ctx)?;
            match tokens.get(pos) {
                Some(Token::Symbol("]")) => Ok((
                    PostfixExpr::ArrayAccess(Box::new(expr.into()), Box::new(access)),
                    pos + 1,
                )),
                _ => Err(ParseError::Expected(Token::Symbol("]"), pos)),
            }
        }
        Some(Token::Symbol("(")) => {
            let (args, pos) = many(
                tokens,
                pos + 1,
                ctx,
                parse_assignment_expr,
                Some(Token::Symbol(",")),
            );
            match tokens.get(pos) {
                Some(Token::Symbol(")")) => Ok((
                    PostfixExpr::FunctionCall(Box::new(expr.into()), args),
                    pos + 1,
                )),
                _ => Err(ParseError::Expected(Token::Symbol(")"), pos)),
            }
        }
        Some(Token::Symbol(".")) => match tokens.get(pos + 1) {
            Some(Token::Ident(ident)) => Ok((
                PostfixExpr::MemberAccess(Box::new(expr.into()), ident),
                pos + 2,
            )),
            _ => Err(ParseError::ExpectedIdent(pos + 1)),
        },
        Some(Token::Symbol("->")) => match tokens.get(pos + 1) {
            Some(Token::Ident(ident)) => Ok((
                PostfixExpr::PointerMemberAccess(Box::new(expr.into()), ident),
                pos + 2,
            )),
            _ => Err(ParseError::ExpectedIdent(pos + 1)),
        },
        Some(Token::Symbol("++")) => Ok((PostfixExpr::PostIncr(Box::new(expr.into())), pos + 1)),
        Some(Token::Symbol("--")) => Ok((PostfixExpr::PostDecr(Box::new(expr.into())), pos + 1)),
        _ => Ok((expr.into(), pos)),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Primary<'text> {
    Ident(&'text str),
    Int(isize),
    Char(char),
    Float(f64),
    EnumConstant(&'text str),
    String(&'text str),
    Parens(Box<Expr<'text>>),
}

fn parse_primary_expr<'text>(
    tokens: &[Token<'text>],
    pos: usize,
    ctx: &mut ParseContext<'text>,
) -> Result<(Primary<'text>, usize), ParseError> {
    match tokens.get(pos) {
        Some(Token::Ident(ident)) => match ctx.is_enum_constant(ident) {
            true => Ok((Primary::EnumConstant(ident), pos + 1)),
            false => Ok((Primary::Ident(ident), pos + 1)),
        },
        Some(Token::Whole(n)) => Ok((Primary::Int(*n as isize), pos + 1)),
        Some(Token::Char(c)) => Ok((Primary::Char(*c), pos + 1)),
        Some(Token::Decimal(n)) => Ok((Primary::Float(*n), pos + 1)),
        Some(Token::String(s)) => Ok((Primary::String(s), pos + 1)),
        Some(Token::Symbol("(")) => {
            let (expr, pos) = parse_expr(tokens, pos + 1, ctx)?;
            match tokens.get(pos) {
                Some(Token::Symbol(")")) => Ok((Primary::Parens(Box::new(expr)), pos + 1)),
                _ => Err(ParseError::Expected(Token::Symbol(")"), pos)),
            }
        }
        _ => Err(ParseError::SyntaxError(
            pos,
            "parse_primary_expr: expected <identifier> or `int` or `char` or `float` or `string` or ( <expression> ) ",
        )),
    }
}

impl<'text> Display for FunctionDefinition<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.return_type.is_empty() {
            write_arr(f, &self.return_type, " ")?;
            write!(f, " ")?;
        }

        write!(f, "{}", self.declarator)?;
        write!(f, " ")?;

        if !self.declarations.is_empty() {
            write_arr(f, &self.declarations, " ")?;
            write!(f, " ")?;
        }

        write!(f, "{}", self.body)
    }
}

impl<'text> Display for StructOrUnionSpecifier<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StructOrUnionSpecifier::Named(sou, ident, ds) => {
                write!(f, "{} {} {{ ", sou, ident)?;
                write_arr(f, ds, " ")?;
                write!(f, " }}")
            }
            StructOrUnionSpecifier::Anonymous(sou, ds) => {
                write!(f, "{} {{ ", sou)?;
                write_arr(f, ds, " ")?;
                write!(f, " }}")
            }
            StructOrUnionSpecifier::ForwardDeclaration(sou, ident) => {
                write!(f, "{} {}", sou, ident)
            }
        }
    }
}

impl Display for StructOrUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StructOrUnion::Struct => write!(f, "struct"),
            StructOrUnion::Union => write!(f, "union"),
        }
    }
}

impl<'text> Display for StructDeclaration<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_arr(f, &self.specifier_qualifiers, " ")?;
        write!(f, " ")?;
        write_arr(f, &self.declarators, ", ")?;
        write!(f, ";")
    }
}

impl<'text> Display for StructDeclarator<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StructDeclarator::Declarator(d) => write!(f, "{}", d),
            StructDeclarator::DeclaratorWithBitField(d, e) => write!(f, "{} : {}", d, e),
            StructDeclarator::BitField(e) => write!(f, ": {}", e),
        }
    }
}

impl<'text> Display for EnumSpecifier<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            EnumSpecifier::Named(ident, members) => {
                write!(f, "enum {} {{ ", ident)?;
                write_arr(f, members, ", ")?;
                write!(f, " }}")
            }
            EnumSpecifier::Anonymous(members) => {
                write!(f, "enum {{ ")?;
                write_arr(f, members, ", ")?;
                write!(f, " }}")
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
            TypeSpecifier::StructOrUnionSpecifier(specifier) => write!(f, "{}", specifier),
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
                write_arr(f, idents, ", ")?;
                write!(f, ")")?;
                if let Some(tail) = tail.deref() {
                    write!(f, "{}", tail)?;
                }
                Ok(())
            }
        }
    }
}

impl<'text> Display for TypeName<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_arr(f, &self.specifier_qualifiers, " ")?;
        if let Some(ad) = &self.abstract_declarator.deref() {
            write!(f, "{}", ad)?;
        }
        Ok(())
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
                write!(f, "{}", ad)
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
            LabeledStmt::Case(expr, stmt) => write!(f, "case {} : {}", expr, stmt),
            LabeledStmt::Default(stmt) => write!(f, "default : {}", stmt),
        }
    }
}

impl<'text> Display for CompoundStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{ ")?;
        for item in &self.0 {
            write!(f, "{} ", item)?;
        }
        write!(f, "}}")
    }
}

impl<'text> Display for BlockItem<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BlockItem::Declaration(d) => write!(f, "{}", d),
            BlockItem::Statement(s) => write!(f, "{}", s),
        }
    }
}

impl<'text> Display for SelectionStmt<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SelectionStmt::If { test, pass } => write!(f, "if ({}) {}", test, pass),
            SelectionStmt::IfElse { test, pass, fail } => {
                write!(f, "if ({}) {} else {}", test, pass, fail)
            }
            SelectionStmt::Switch { test, pass } => write!(f, "switch ({}) {}", test, pass),
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
            MultiplicativeExpr::CastExpr(expr) => write!(f, "{}", expr),
            MultiplicativeExpr::Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            MultiplicativeExpr::Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            MultiplicativeExpr::Mod(lhs, rhs) => write!(f, "({} % {})", lhs, rhs),
        }
    }
}

impl<'text> Display for CastExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CastExpr::UnaryExpr(expr) => write!(f, "{}", expr),
            CastExpr::Cast(type_name, expr) => write!(f, "({}){}", type_name, expr),
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
            UnaryExpr::SizeofExpr(expr) => write!(f, "sizeof {}", expr),
            UnaryExpr::SizeofTypeName(type_name) => write!(f, "sizeof ({})", type_name),
        }
    }
}

impl<'text> Display for PostfixExpr<'text> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PostfixExpr::Primary(expr) => write!(f, "{}", expr),
            PostfixExpr::ArrayAccess(expr, access) => write!(f, "{}[{}]", expr, access),
            PostfixExpr::FunctionCall(expr, args) => {
                write!(f, "{}", expr)?;
                write!(f, "(")?;
                write_arr(f, args, ", ")?;
                write!(f, ")")
            }
            PostfixExpr::MemberAccess(expr, ident) => write!(f, "{}.{}", expr, ident),
            PostfixExpr::PointerMemberAccess(expr, ident) => write!(f, "{}->{}", expr, ident),
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
            Primary::EnumConstant(e) => write!(f, "{}", e),
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

impl<'text> From<FunctionDefinition<'text>> for ExternalDeclaration<'text> {
    fn from(value: FunctionDefinition<'text>) -> Self {
        ExternalDeclaration::FunctionDefinition(value)
    }
}

impl<'text> From<Declaration<'text>> for ExternalDeclaration<'text> {
    fn from(value: Declaration<'text>) -> Self {
        ExternalDeclaration::Declaration(value)
    }
}

impl<'text> From<StructOrUnionSpecifier<'text>> for TypeSpecifier<'text> {
    fn from(value: StructOrUnionSpecifier<'text>) -> Self {
        TypeSpecifier::StructOrUnionSpecifier(value)
    }
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

impl<'text> From<CastExpr<'text>> for MultiplicativeExpr<'text> {
    fn from(value: CastExpr<'text>) -> Self {
        MultiplicativeExpr::CastExpr(value)
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

impl<'text> From<Primary<'text>> for Expr<'text> {
    fn from(value: Primary<'text>) -> Self {
        Expr::ConditionalExpr(ConditionalExpr::LogicalOrExpr(
            LogicalOrExpr::LogicalAndExpr(LogicalAndExpr::BitOrExpr(BitOrExpr::XORExpr(
                XORExpr::BitAndExpr(BitAndExpr::EqualityExpr(EqualityExpr::ComparisionExpr(
                    ComparisionExpr::ShiftExpr(ShiftExpr::AdditiveExpr(
                        AdditiveExpr::MultiplicativeExpr(MultiplicativeExpr::CastExpr(
                            CastExpr::UnaryExpr(UnaryExpr::PostfixExpr(PostfixExpr::Primary(
                                value,
                            ))),
                        )),
                    )),
                ))),
            ))),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex;

    use pretty_assertions::assert_eq;

    macro_rules! check {
        ($f:ident, $ctx:expr, $src:expr, $expected:expr) => {
            let tokens = lex($src).expect("** LEX ERROR");
            let (stmt, pos) = $f(&tokens, 0, $ctx).expect("** Unable to parse statement");
            assert_eq!(pos, tokens.len(), "** Unable to parse all Tokens\n{}", stmt);
            let stmt = format!("{}", stmt);
            assert_eq!($expected, stmt);
        };
        ($f:ident, $ctx:expr, $src:expr) => {
            check!($f, $ctx, $src, $src)
        };
    }

    macro_rules! check_ast {
        ($f:ident, $ctx:expr, $src:expr, $expected:expr) => {
            let tokens = lex($src).expect("** LEX ERROR");
            let (stmt, pos) = $f(&tokens, 0, $ctx).expect("** Unable to parse statement");
            assert_eq!(pos, tokens.len());
            assert_eq!($expected, stmt);
        };
    }

    macro_rules! ast {
        ($f:ident, $ctx:expr, $src:expr) => {{
            let tokens = lex($src).expect("** LEX ERROR");
            let (stmt, pos) = $f(&tokens, 0, $ctx).expect("** Unable to parse statement");
            assert_eq!(pos, tokens.len());
            stmt
        }};
    }

    const STRUCT_UNION: [&'static str; 6] = [
        "struct Point",
        r#"struct Point { float x; float y; }"#,
        r#"struct { float x; float y; }"#,
        "union Data",
        r#"union Data { int i; float f; char str[20]; }"#,
        r#"union { int i; float f; char str[20]; }"#,
    ];

    const ENUM: [&'static str; 3] = [
        "enum Color",
        r#"enum Color { RED, GREEN = "00FF00", BLUE = 7 }"#,
        r#"enum { RED, GREEN = "00FF00", BLUE = 7 }"#,
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
    ];

    const TYPE_QUALIFIER: [(&'static str, TypeQualifier); 2] = [
        ("const", TypeQualifier::Const),
        ("volatile", TypeQualifier::Volatile),
    ];

    #[test]
    fn test_function_definition() {
        let mut ctx = ParseContext::new();

        check!(
            parse_function_definition,
            &mut ctx,
            "int add1(int a) { return ++a; }"
        );

        check!(
            parse_function_definition,
            &mut ctx,
            "static double calculateAverage(int values[], int size) { return 0; }"
        );

        check!(
            parse_function_definition,
            &mut ctx,
            r#"const char *getMessage() { return "Hello, World!"; }"#
        );

        check!(
            parse_function_definition,
            &mut ctx,
            r#"unsigned long long factorial(int n) { }"#
        );

        check!(
            parse_function_definition,
            &mut ctx,
            "float add(a, b) int a; float b; { return ++a; }"
        );

        check!(
            parse_function_definition,
            &mut ctx,
            "int add(int a, int b) { return a + b; }",
            "int add(int a, int b) { return (a + b); }"
        );

        check!(
            parse_function_definition,
            &mut ctx,
            "int euc(int x, int y) { return sqrt(x*x + y*y); }",
            "int euc(int x, int y) { return sqrt(((x * x) + (y * y))); }"
        );
    }

    #[test]
    fn test_struct_or_union() {
        let mut ctx = ParseContext::new();

        for src in STRUCT_UNION {
            check!(parse_struct_or_union_specifier, &mut ctx, src);
        }
    }

    #[test]
    fn test_type_name() {
        let mut ctx = ParseContext::new();

        check!(parse_type_name, &mut ctx, "int");
        check!(parse_type_name, &mut ctx, "const char volatile");
        check!(parse_type_name, &mut ctx, "int*");
        check!(parse_type_name, &mut ctx, "double[10]");
        check!(parse_type_name, &mut ctx, "int(*)(int, char)");
        check!(parse_type_name, &mut ctx, "const int*(*)(int[5])");
        check!(parse_type_name, &mut ctx, "int(*(*[10])())[5]");
        check!(
            parse_type_name,
            &mut ctx,
            "unsigned long long(*)(char*, int)"
        );
    }

    #[test]
    fn test_declarator() {
        let mut ctx = ParseContext::new();

        check!(parse_declarator, &mut ctx, "x");
        check!(parse_declarator, &mut ctx, "(y)");
        check!(parse_declarator, &mut ctx, "*a");
        check!(parse_declarator, &mut ctx, "*****a[]");
        check!(parse_declarator, &mut ctx, "*a()");
        check!(parse_declarator, &mut ctx, "*(*a)");
        check!(parse_declarator, &mut ctx, "arr[]");
        check!(parse_declarator, &mut ctx, "arr[SIZE]");
        check!(parse_declarator, &mut ctx, "mat[3][4]");
        check!(parse_declarator, &mut ctx, "f()");
        check!(parse_declarator, &mut ctx, "add(int a, int b)");
        check!(parse_declarator, &mut ctx, "(*func_ptr)()");
        check!(parse_declarator, &mut ctx, "(*sqrt)(double x, double y)");
        check!(parse_declarator, &mut ctx, "sum(int n, ...)");
        check!(parse_declarator, &mut ctx, "print(const char *fmt, ...)");
        check!(parse_declarator, &mut ctx, "log(const char *message, ...)");
        check!(parse_declarator, &mut ctx, "f(a, b, c)");
    }

    #[test]
    fn test_abstract_declarator() {
        let mut ctx = ParseContext::new();

        check!(parse_abstract_declarator, &mut ctx, "*");
        check!(parse_abstract_declarator, &mut ctx, "(*)");
        check!(parse_abstract_declarator, &mut ctx, "*****[]");
        check!(parse_abstract_declarator, &mut ctx, "*()");
        check!(parse_abstract_declarator, &mut ctx, "*(*)");
        check!(parse_abstract_declarator, &mut ctx, "[]");
        check!(parse_abstract_declarator, &mut ctx, "[SIZE]");
        check!(parse_abstract_declarator, &mut ctx, "[3][4]");
        check!(parse_abstract_declarator, &mut ctx, "()");
        check!(parse_abstract_declarator, &mut ctx, "(int a, int b)");
        check!(parse_abstract_declarator, &mut ctx, "(*)()");
        check!(
            parse_abstract_declarator,
            &mut ctx,
            "(*)(double x, double y)"
        );
        check!(parse_abstract_declarator, &mut ctx, "(int n, ...)");
        check!(
            parse_abstract_declarator,
            &mut ctx,
            "(const char *fmt, ...)"
        );
        check!(
            parse_abstract_declarator,
            &mut ctx,
            "(const char *message, ...)"
        );
    }

    #[test]
    fn test_declaration() {
        let mut ctx = ParseContext::new();

        check!(parse_declaration, &mut ctx, "int x;");
        check!(parse_declaration, &mut ctx, "int x = 10;");
        check!(parse_declaration, &mut ctx, "int nums[] = { 1, 2, 3, };");
        check!(
            parse_declaration,
            &mut ctx,
            "float mat[2][3] = { { 3.1, 0.6, 2.7, }, { 1.4, 4.7, 0.6, }, };"
        );
        check!(
            parse_declaration,
            &mut ctx,
            r#"const char *name = "zahash";"#
        );

        check!(parse_stmt, &mut ctx, "{ typedef long long ll; ll a = 10; }");

        check!(
            parse_stmt,
            &mut ctx,
            "{ typedef struct { float x; float y; } Point; const Point origin = { 0, 0, }; }"
        );

        check!(
            parse_declaration,
            &mut ctx,
            "typedef struct { const char *name; int age; } Person;"
        );
        check!(
            parse_declaration,
            &mut ctx,
            r#"Person p = { name = "zahash", age = 24, };"#,
            r#"Person p = { (name = "zahash"), (age = 24), };"#
        );

        check!(
            parse_declaration,
            &mut ctx,
            r#"typedef enum { RED, GREEN, BLUE } Color;"#
        );
        check!(parse_declaration, &mut ctx, r#"Color c = RED;"#);
    }

    #[test]
    fn test_init_declarator() {
        let mut ctx = ParseContext::new();

        check!(parse_init_declarator, &mut ctx, "x = 10");
        check!(parse_init_declarator, &mut ctx, "nums[] = { 1, 2, 3, }");
        check!(
            parse_init_declarator,
            &mut ctx,
            "mat[2][3] = { { 1, 2, 3, }, { 4, 5, 6, }, }"
        );
        check!(parse_init_declarator, &mut ctx, "obj = { a, b, c, }");
        check!(parse_init_declarator, &mut ctx, r#"*name = "zahash""#);
    }

    #[test]
    fn test_initializer() {
        let mut ctx = ParseContext::new();

        check!(parse_initializer, &mut ctx, "expr");
        check!(parse_initializer, &mut ctx, "{a, b, c}", "{ a, b, c, }");
        check!(parse_initializer, &mut ctx, "{a, b, c,}", "{ a, b, c, }");
        check!(
            parse_initializer,
            &mut ctx,
            "{a, {b, c,}, {d,}}",
            "{ a, { b, c, }, { d, }, }"
        );
    }

    #[test]
    fn test_declaration_specifier() {
        let mut ctx = ParseContext::new();

        for (src, expected) in STORAGE_CLASS_SPECIFIER {
            check_ast!(
                parse_declaration_specifier,
                &mut ctx,
                src,
                DeclarationSpecifier::from(expected)
            );
        }

        for (src, expected) in TYPE_SPECIFIER {
            check_ast!(
                parse_declaration_specifier,
                &mut ctx,
                src,
                DeclarationSpecifier::from(expected)
            );
        }

        for src in ENUM {
            check!(parse_declaration_specifier, &mut ctx, src);
        }

        for (src, expected) in TYPE_QUALIFIER {
            check_ast!(
                parse_declaration_specifier,
                &mut ctx,
                src,
                DeclarationSpecifier::from(expected)
            );
        }
    }

    #[test]
    fn test_storage_class_specifier() {
        let mut ctx = ParseContext::new();

        for (src, expected) in STORAGE_CLASS_SPECIFIER {
            check_ast!(parse_storage_class_specifier, &mut ctx, src, expected);
        }
    }

    #[test]
    fn test_specifier_qualifier() {
        let mut ctx = ParseContext::new();

        for (src, expected) in TYPE_SPECIFIER {
            check_ast!(
                parse_specifier_qualifier,
                &mut ctx,
                src,
                SpecifierQualifier::from(expected)
            );
        }

        for src in ENUM {
            check!(parse_specifier_qualifier, &mut ctx, src);
        }

        for (src, expected) in TYPE_QUALIFIER {
            check_ast!(
                parse_specifier_qualifier,
                &mut ctx,
                src,
                SpecifierQualifier::from(expected)
            );
        }
    }

    #[test]
    fn test_type_specifier() {
        let mut ctx = ParseContext::new();

        check!(parse_declaration, &mut ctx, "typedef int A;");
        check_ast!(
            parse_type_specifier,
            &mut ctx,
            "A",
            TypeSpecifier::TypeDefName("A")
        );

        for (src, expected) in TYPE_SPECIFIER {
            check_ast!(parse_type_specifier, &mut ctx, src, expected);
        }

        for src in STRUCT_UNION {
            check!(parse_type_specifier, &mut ctx, src);
        }

        for src in ENUM {
            check!(parse_type_specifier, &mut ctx, src);
        }
    }

    #[test]
    fn test_pointer() {
        let mut ctx = ParseContext::new();

        check!(parse_pointer, &mut ctx, "*");
        check!(parse_pointer, &mut ctx, "**");
        check!(parse_pointer, &mut ctx, "***");
        check!(parse_pointer, &mut ctx, "*const ");
        check!(
            parse_pointer,
            &mut ctx,
            "*const volatile const volatile volatile const "
        );
        check!(
            parse_pointer,
            &mut ctx,
            "*volatile const volatile *const const volatile "
        );
        check!(
            parse_pointer,
            &mut ctx,
            "**volatile *******const ***const volatile ******"
        );

        let pointer = ast!(parse_pointer, &mut ctx, "* *const volatile *volatile * *");
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
        let mut ctx = ParseContext::new();

        for (src, expected) in TYPE_QUALIFIER {
            check_ast!(parse_type_qualifier, &mut ctx, src, expected);
        }
    }

    #[test]
    fn test_enum() {
        let mut ctx = ParseContext::new();

        for src in ENUM {
            check!(parse_enum_specifier, &mut ctx, src);
        }
    }

    #[test]
    fn test_simple_stmt() {
        let mut ctx = ParseContext::new();

        check_ast!(parse_stmt, &mut ctx, ";", Stmt::EmptyStmt);
        check_ast!(
            parse_stmt,
            &mut ctx,
            "{ }",
            Stmt::Compound(CompoundStmt(vec![]))
        );
        check!(parse_stmt, &mut ctx, "a++;");
        check!(parse_stmt, &mut ctx, "{ a++; }");
        check!(parse_stmt, &mut ctx, "{ int a = 0; a++; }");
    }

    #[test]
    fn test_labeled_stmt() {
        let mut ctx = ParseContext::new();

        check!(parse_stmt, &mut ctx, "a : ;");
        check!(parse_stmt, &mut ctx, "a : { }");
        check!(parse_stmt, &mut ctx, "a : b;");
        check!(parse_stmt, &mut ctx, "a : { b; }");
        check!(parse_stmt, &mut ctx, "case 10 : { b; }");
        check!(parse_stmt, &mut ctx, "default : { b; }");
    }

    #[test]
    fn test_if_else_stmt() {
        let mut ctx = ParseContext::new();

        check!(parse_stmt, &mut ctx, "if (a) ;");
        check!(parse_stmt, &mut ctx, "if (a) b;");
        check!(parse_stmt, &mut ctx, "if (a) b; else c;");
        check!(parse_stmt, &mut ctx, "if (a) b; else if (c) d;");
        check!(parse_stmt, &mut ctx, "if (a) b; else if (c) d; else e;");
        check!(parse_stmt, &mut ctx, "if (a) { }");
        check!(parse_stmt, &mut ctx, "if (a) { b; }");
        check!(parse_stmt, &mut ctx, "if (a) { b; } else { c; }");
        check!(parse_stmt, &mut ctx, "if (a) { b; } else if (c) { d; }");
        check!(
            parse_stmt,
            &mut ctx,
            "if (a) { b; } else if (c) { d; } else { e; }"
        );
        check!(
            parse_stmt,
            &mut ctx,
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
    fn test_switch_stmt() {
        let mut ctx = ParseContext::new();

        check!(
            parse_stmt,
            &mut ctx,
            r#"
            switch (a) {
                case 1:
                    printf("C1");
                    break;

                case 2:
                    printf("C2");
                    break;

                default:
                    printf("D");
                    break;
            }
            "#,
            r#"switch (a) { case 1 : printf("C1"); break; case 2 : printf("C2"); break; default : printf("D"); break; }"#
        );
    }

    #[test]
    fn test_while_stmt() {
        let mut ctx = ParseContext::new();

        check!(parse_stmt, &mut ctx, "while (a) ;");
        check!(parse_stmt, &mut ctx, "while (a) { }");
        check!(parse_stmt, &mut ctx, "while (a) { b; }");
        check!(
            parse_stmt,
            &mut ctx,
            "while (a <= 10) { a++; }",
            "while ((a <= 10)) { a++; }"
        );
    }

    #[test]
    fn test_do_while_stmt() {
        let mut ctx = ParseContext::new();

        check!(parse_stmt, &mut ctx, "do ; while (a);");
        check!(parse_stmt, &mut ctx, "do { } while (a);");
        check!(parse_stmt, &mut ctx, "do { b; } while (a);");
        check!(
            parse_stmt,
            &mut ctx,
            "do { a++; } while (a <= 10);",
            "do { a++; } while ((a <= 10));"
        );
    }

    #[test]
    fn test_for_stmt() {
        let mut ctx = ParseContext::new();

        check!(parse_stmt, &mut ctx, "for (;;) ;");
        check!(parse_stmt, &mut ctx, "for (;;) { }");
        check!(parse_stmt, &mut ctx, "for (a;;) ;");
        check!(parse_stmt, &mut ctx, "for (; a;) ;");
        check!(parse_stmt, &mut ctx, "for (;; a) ;");
        check!(parse_stmt, &mut ctx, "for (a; a; a) ;");
        check!(parse_stmt, &mut ctx, "for (a; a; a) { }");
        check!(parse_stmt, &mut ctx, "for (a; a; a) { b; }");
        check!(
            parse_stmt,
            &mut ctx,
            "for (i=0; i<10; i++) { a++; }",
            "for ((i = 0); (i < 10); i++) { a++; }"
        );
    }

    #[test]
    fn test_jump_stmt() {
        let mut ctx = ParseContext::new();

        check!(parse_stmt, &mut ctx, "goto a;");
        check!(parse_stmt, &mut ctx, "continue;");
        check!(parse_stmt, &mut ctx, "break;");
        check!(parse_stmt, &mut ctx, "return;");
        check!(parse_stmt, &mut ctx, "return a;");
    }

    #[test]
    fn test_primary() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "ident");
        check!(parse_expr, &mut ctx, "123");
        check!(parse_expr, &mut ctx, "'c'");
        check!(parse_expr, &mut ctx, "123.123");
        check!(parse_expr, &mut ctx, r#""string""#);
        check!(parse_expr, &mut ctx, "(a)");
        check!(parse_expr, &mut ctx, "(add(a, b))");

        check!(
            parse_declaration,
            &mut ctx,
            "typedef enum { RED, GREEN, BLUE } Color;"
        );
        check_ast!(
            parse_expr,
            &mut ctx,
            "BLUE",
            Expr::from(Primary::EnumConstant("BLUE"))
        );
    }

    #[test]
    fn test_postfix_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a++");
        check!(parse_expr, &mut ctx, "a--");
        check!(parse_expr, &mut ctx, "add(a, b)");
        check!(parse_expr, &mut ctx, "arr[10]");
        check!(parse_expr, &mut ctx, "person.name");
        check!(parse_expr, &mut ctx, "person->name");
    }

    #[test]
    fn test_cast_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "(float)10");
        check!(parse_expr, &mut ctx, "(const int*(*)(int[5]))my_var");
    }

    #[test]
    fn test_unary_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "++a");
        check!(parse_expr, &mut ctx, "--a");
        check!(parse_expr, &mut ctx, "&a");
        check!(parse_expr, &mut ctx, "*a");
        check!(parse_expr, &mut ctx, "+a", "a");
        check!(parse_expr, &mut ctx, "-a");
        check!(parse_expr, &mut ctx, "~a");

        check!(parse_expr, &mut ctx, "sizeof a");
        check!(parse_expr, &mut ctx, "sizeof (int)");
        check!(parse_expr, &mut ctx, "sizeof *ptr");
        check!(parse_expr, &mut ctx, "sizeof (struct Person)");
        check!(parse_expr, &mut ctx, "sizeof (double[10])");
        check!(parse_expr, &mut ctx, "sizeof (struct Point*)");
    }

    #[test]
    fn test_multiplicative_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a * b", "(a * b)");
        check!(parse_expr, &mut ctx, "a / b", "(a / b)");
        check!(parse_expr, &mut ctx, "a % b", "(a % b)");
        check!(parse_expr, &mut ctx, "a * b / c % d", "(((a * b) / c) % d)");
    }

    #[test]
    fn test_additive_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a + b", "(a + b)");
        check!(parse_expr, &mut ctx, "a - b", "(a - b)");
        check!(parse_expr, &mut ctx, "a + b - c", "((a + b) - c)");
    }

    #[test]
    fn test_shift_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a << b", "(a << b)");
        check!(parse_expr, &mut ctx, "a >> b", "(a >> b)");
        check!(parse_expr, &mut ctx, "a << b >> c", "((a << b) >> c)");
    }

    #[test]
    fn test_comparision_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a < b", "(a < b)");
        check!(parse_expr, &mut ctx, "a > b", "(a > b)");
        check!(parse_expr, &mut ctx, "a <= b", "(a <= b)");
        check!(parse_expr, &mut ctx, "a >= b", "(a >= b)");
        check!(
            parse_expr,
            &mut ctx,
            "a < b > c <= d >= e",
            "((((a < b) > c) <= d) >= e)"
        );
    }

    #[test]
    fn test_equality_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a == b", "(a == b)");
        check!(parse_expr, &mut ctx, "a != b", "(a != b)");
        check!(parse_expr, &mut ctx, "a == b != c", "((a == b) != c)");
    }

    #[test]
    fn test_bit_and_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a & b", "(a & b)");
        check!(parse_expr, &mut ctx, "a & b & c", "((a & b) & c)");
    }

    #[test]
    fn test_xor_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a ^ b", "(a ^ b)");
        check!(parse_expr, &mut ctx, "a ^ b ^ c", "((a ^ b) ^ c)");
    }

    #[test]
    fn test_bit_or_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a | b", "(a | b)");
        check!(parse_expr, &mut ctx, "a | b | c", "((a | b) | c)");
    }

    #[test]
    fn test_logical_and_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a && b", "(a && b)");
        check!(parse_expr, &mut ctx, "a && b && c", "((a && b) && c)");
    }

    #[test]
    fn test_logical_or_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a || b", "(a || b)");
        check!(parse_expr, &mut ctx, "a || b || c", "((a || b) || c)");
    }

    #[test]
    fn test_conditional_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a ? b : c", "(a ? b : c)");
        check!(
            parse_expr,
            &mut ctx,
            "a ? b ? c : d : e",
            "(a ? (b ? c : d) : e)"
        );
    }

    #[test]
    fn test_assignment_expr() {
        let mut ctx = ParseContext::new();

        check!(parse_expr, &mut ctx, "a = b", "(a = b)");
        check!(parse_expr, &mut ctx, "a *= b", "(a *= b)");
        check!(parse_expr, &mut ctx, "a /= b", "(a /= b)");
        check!(parse_expr, &mut ctx, "a %= b", "(a %= b)");
        check!(parse_expr, &mut ctx, "a += b", "(a += b)");
        check!(parse_expr, &mut ctx, "a -= b", "(a -= b)");
        check!(parse_expr, &mut ctx, "a <<= b", "(a <<= b)");
        check!(parse_expr, &mut ctx, "a >>= b", "(a >>= b)");
        check!(parse_expr, &mut ctx, "a &= b", "(a &= b)");
        check!(parse_expr, &mut ctx, "a ^= b", "(a ^= b)");
        check!(parse_expr, &mut ctx, "a |= b", "(a |= b)");

        check!(parse_expr, &mut ctx, "a -= b &= c", "(a -= (b &= c))");
    }
}
