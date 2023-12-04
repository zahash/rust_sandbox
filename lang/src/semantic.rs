#![allow(dead_code, unused_variables)]

use std::fmt::{Debug, Display};

use crate::*;

#[derive(Debug, PartialEq, Clone)]
enum Type<'text> {
    Int,
    Char,
    Float,
    Double,
    Void,
    String,
    Pointer(Box<Type<'text>>),
    Array(Box<Type<'text>>, usize),
    Function(Box<Type<'text>>, Vec<Type<'text>>), // return type; parameter types
    Struct(&'text str, Vec<(&'text str, Type<'text>)>),
}

#[derive(Debug)]
enum BinOp<'ast, 'text> {
    LogicalOr(&'ast LogicalAndExpr<'text>),
    LogicalAnd(&'ast LogicalAndExpr<'text>),
    BitOr(&'ast BitOrExpr<'text>),
    XOR(&'ast XORExpr<'text>),
    BitAnd(&'ast BitAndExpr<'text>),
    Equality(&'ast EqualityExpr<'text>),
    Comparision(&'ast ComparisionExpr<'text>),
    Shift(&'ast ShiftExpr<'text>),
    Additive(&'ast AdditiveExpr<'text>),
    Multiplicative(&'ast MultiplicativeExpr<'text>),
}

trait DisplayDebug: Display + Debug {}

#[derive(Debug)]
enum SemanticError<'ast, 'text> {
    UndefinedVariable(&'text str),
    VariableRedeclaration(&'text str),
    InvalidBinaryOperands(BinOp<'ast, 'text>),
    TypeMismatch(Type<'text>, Type<'text>),
    UnexpectedType {
        expected: Type<'text>,
        actual: Type<'text>,
    },
    InvalidInitializer,
    InvalidPostfixOperand(&'ast PostfixExpr<'text>),
    NotAFunction(&'ast PostfixExpr<'text>),
    InvalidFnCall(&'ast PostfixExpr<'text>),
    UndefinedMember {
        struct_name: &'text str,
        field: &'text str,
    },
    NotAStruct(&'ast PostfixExpr<'text>),
    NotAPointerToStruct(&'ast PostfixExpr<'text>),
    InvalidUnaryOperand(&'ast UnaryExpr<'text>),
    InvalidDereferenceOperand(&'ast UnaryExpr<'text>),
    InvalidTypeCast {
        from: Type<'text>,
        to: Type<'text>,
    },
}

enum Symbol<'text> {
    Var(Var<'text>),
    Label(Label<'text>),
    Enum(Enum<'text>),
}

struct Var<'text> {
    name: &'text str,
    ty: Type<'text>,
}

struct Label<'text>(&'text str);

struct Enum<'text> {
    name: &'text str,
    ty: Type<'text>,
}

type Scope<'text> = Vec<Symbol<'text>>;

struct SemanticContext<'text> {
    symbol_table: Vec<Scope<'text>>,
}

impl<'text> SemanticContext<'text> {
    fn new() -> SemanticContext<'text> {
        SemanticContext {
            symbol_table: vec![vec![]],
        }
    }

    fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.symbol_table.push(vec![]);
        let out = f(self);
        self.symbol_table.pop();
        out
    }

    fn curr_scope(&mut self) -> &mut Vec<Symbol<'text>> {
        self.symbol_table
            .last_mut()
            .expect("must have atleast one scope")
    }

    fn declare_var(&mut self, var: Var<'text>) -> bool {
        let scope = self.curr_scope();

        // cannot redeclare variable
        if scope
            .iter()
            .filter_map(|s| match s {
                Symbol::Var(v) => Some(v),
                _ => None,
            })
            .any(|var_| var_.name == var.name)
        {
            return false;
        }

        scope.push(Symbol::Var(var));
        true
    }

    fn find_var<'ctx>(&'ctx self, name: &'text str) -> Option<&'ctx Var<'text>> {
        self.symbol_table
            .iter()
            .rev()
            .flat_map(
                /* C doesn't allow redeclaring variables (shadowing).
                So, scope.iter() and scope.iter().rev() work the same.
                scope.iter.rev() is used here because of a future possiblility to allow shadowing
                NOTE: if shadowing is allowed, .rev() must be used everywhere where scope is accessed
                */
                |scope| scope.iter().rev(),
            )
            .filter_map(|s| match s {
                Symbol::Var(v) => Some(v),
                _ => None,
            })
            .find(|var| var.name == name)
    }

    fn declare_label(&mut self, label: &'text str) {
        self.curr_scope().push(Symbol::Label(Label(label)));
    }

    fn find_label<'ctx>(&'ctx self, label: &'text str) -> Option<&'ctx Label<'text>> {
        self.symbol_table
            .iter()
            .flat_map(|scope| scope.iter())
            .filter_map(|s| match s {
                Symbol::Label(label) => Some(label),
                _ => None,
            })
            .find(|Label(label_)| *label_ == label)
    }

    fn declare_enum_invariant(&mut self, e: Enum<'text>) -> bool {
        let scope = self.curr_scope();

        // cannot redeclare enum invariants
        if scope
            .iter()
            .filter_map(|s| match s {
                Symbol::Enum(e) => Some(e),
                _ => None,
            })
            .any(|e_| e_.name == e.name)
        {
            return false;
        }

        scope.push(Symbol::Enum(e));
        true
    }

    fn find_enum_invariant<'ctx>(&'ctx self, name: &'text str) -> Option<&'ctx Enum<'text>> {
        self.symbol_table
            .iter()
            .rev()
            .flat_map(
                /* C doesn't allow redeclaring enums.
                So, scope.iter() and scope.iter().rev() work the same.
                */
                |scope| scope.iter(),
            )
            .filter_map(|s| match s {
                Symbol::Enum(e) => Some(e),
                _ => None,
            })
            .find(|e| e.name == name)
    }
}

fn analyze_translation_unit<'ast, 'text>(
    translation_unit: &'ast TranslationUnit<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<(), SemanticError<'ast, 'text>> {
    for external_declaration in &translation_unit.0 {
        analyze_external_declaration(external_declaration, ctx)?;
    }
    Ok(())
}

fn analyze_external_declaration<'ast, 'text>(
    external_declaration: &'ast ExternalDeclaration<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<(), SemanticError<'ast, 'text>> {
    match external_declaration {
        ExternalDeclaration::FunctionDefinition(f) => analyze_function_definition(f, ctx)?,
        ExternalDeclaration::Declaration(d) => analyze_declaration(d, ctx)?,
    }
    Ok(())
}

fn analyze_function_definition<'ast, 'text>(
    f: &'ast FunctionDefinition<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<(), SemanticError<'ast, 'text>> {
    todo!()
}

fn analyze_declaration<'ast, 'text>(
    declaration: &'ast Declaration<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<(), SemanticError<'ast, 'text>> {
    let ty = analyze_declaration_specifiers(&declaration.declaration_specifiers, ctx)?;

    for init_d in &declaration.init_declarators {
        let d_ty = analyze_init_declarator(init_d, ctx)?;
        // if ty != d_ty {
        //     return Err(SemanticError::TypeMismatch(ty, d_ty));
        // }
    }

    Ok(())
}

struct Dss;
fn analyze_declaration_specifiers<'ast, 'text>(
    declaration: &'ast [DeclarationSpecifier<'text>],
    ctx: &mut SemanticContext<'text>,
) -> Result<Dss, SemanticError<'ast, 'text>> {
    // can't just return Result<Type, ...> because declaration specifier
    // could be a typedef

    todo!()
}

fn analyze_init_declarator<'ast, 'text>(
    init_d: &'ast InitDeclarator<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match init_d {
        InitDeclarator::Declared(d) => analyze_declarator(d, ctx),
        InitDeclarator::Initialized(d, init) => {
            let d_ty = analyze_declarator(d, ctx)?;
            let init_ty = analyze_initializer(init, ctx)?;
            match d_ty == init_ty {
                true => Ok(d_ty),
                false => Err(SemanticError::TypeMismatch(d_ty, init_ty)),
            }
        }
    }
}

fn analyze_declarator<'ast, 'text>(
    declarator: &'ast Declarator<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    if let Some(pointer) = &declarator.pointer {
        analyze_pointer(pointer, ctx)?;
    }
    analyze_direct_declarator(&declarator.d_declarator, ctx)
}

fn analyze_initializer<'ast, 'text>(
    init: &'ast Initializer<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match init {
        Initializer::Assignment(expr) => analyze_assignment_expr(expr, ctx),
        Initializer::InitializerList(inits) => todo!(),
    }
}

fn analyze_pointer<'ast, 'text>(
    pointer: &'ast Pointer,
    ctx: &mut SemanticContext<'text>,
) -> Result<(), SemanticError<'ast, 'text>> {
    Ok(())
}

fn analyze_direct_declarator<'ast, 'text>(
    d_declarator: &'ast DirectDeclarator<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    todo!()
}

fn analyze_type_name<'ast, 'text>(
    type_name: &'ast TypeName<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    todo!()
}

fn analyze_assignment_expr<'ast, 'text>(
    expr: &'ast AssignmentExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        AssignmentExpr::ConditionalExpr(expr) => analyze_conditional_expr(expr, ctx),
        AssignmentExpr::Assign(lhs, rhs)
        | AssignmentExpr::MulAssign(lhs, rhs)
        | AssignmentExpr::DivAssign(lhs, rhs)
        | AssignmentExpr::ModAssign(lhs, rhs)
        | AssignmentExpr::AddAssign(lhs, rhs)
        | AssignmentExpr::SubAssign(lhs, rhs)
        | AssignmentExpr::ShiftLeftAssign(lhs, rhs)
        | AssignmentExpr::ShiftRightAssign(lhs, rhs)
        | AssignmentExpr::BitAndAssign(lhs, rhs)
        | AssignmentExpr::XORAssign(lhs, rhs)
        | AssignmentExpr::BitOrAssign(lhs, rhs) => {
            let lhs_ty = analyze_unary_expr(lhs, ctx)?;
            let rhs_ty = analyze_assignment_expr(rhs, ctx)?;
            match lhs_ty == rhs_ty {
                true => Ok(lhs_ty),
                false => Err(SemanticError::TypeMismatch(lhs_ty, rhs_ty)),
            }
        }
    }
}

fn analyze_conditional_expr<'ast, 'text>(
    expr: &'ast ConditionalExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        ConditionalExpr::LogicalOrExpr(expr) => analyze_logicalor_expr(expr, ctx),
        ConditionalExpr::Ternary { test, pass, fail } => {
            let ty = analyze_logicalor_expr(test, ctx)?;
            if ty != Type::Int {
                return Err(SemanticError::UnexpectedType {
                    expected: Type::Int,
                    actual: ty,
                });
            }

            let pass_ty = analyze_assignment_expr(pass, ctx)?;
            let fail_ty = analyze_conditional_expr(fail, ctx)?;
            match pass_ty == fail_ty {
                true => Ok(pass_ty),
                false => Err(SemanticError::TypeMismatch(pass_ty, fail_ty)),
            }
        }
    }
}

fn analyze_logicalor_expr<'ast, 'text>(
    expr: &'ast LogicalOrExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        LogicalOrExpr::LogicalAndExpr(expr) => todo!(),
        LogicalOrExpr::LogicalOr(lhs, rhs) => match (
            analyze_logicalor_expr(lhs, ctx)?,
            analyze_logicaland_expr(rhs, ctx)?,
        ) {
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Int, ty) | (ty, Type::Int) | (ty, _) => Err(SemanticError::UnexpectedType {
                expected: Type::Int,
                actual: ty,
            }),
        },
    }
}

fn analyze_logicaland_expr<'ast, 'text>(
    expr: &'ast LogicalAndExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        LogicalAndExpr::BitOrExpr(expr) => analyze_bitor_expr(expr, ctx),
        LogicalAndExpr::LogicalAnd(lhs, rhs) => match (
            analyze_logicaland_expr(lhs, ctx)?,
            analyze_bitor_expr(rhs, ctx)?,
        ) {
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Int, ty) | (ty, Type::Int) | (ty, _) => Err(SemanticError::UnexpectedType {
                expected: Type::Int,
                actual: ty,
            }),
        },
    }
}

fn analyze_bitor_expr<'ast, 'text>(
    expr: &'ast BitOrExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        BitOrExpr::XORExpr(expr) => analyze_xor_expr(expr, ctx),
        BitOrExpr::BitOr(lhs, rhs) => todo!(),
    }
}

fn analyze_xor_expr<'ast, 'text>(
    expr: &'ast XORExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        XORExpr::BitAndExpr(bit_and_expr) => analyze_bitand_expr(bit_and_expr, ctx),
        XORExpr::XOR(lhs, rhs) => {
            // Assuming XOR can only be applied to both ints and both chars. i.e., no implicit casting for now.
            match (analyze_xor_expr(lhs, ctx)?, analyze_bitand_expr(rhs, ctx)?) {
                (Type::Int, Type::Int) | (Type::Char, Type::Char) => Ok(Type::Int),
                _ => Err(SemanticError::InvalidBinaryOperands(BinOp::XOR(expr))),
            }
        }
    }
}

fn analyze_bitand_expr<'ast, 'text>(
    expr: &'ast BitAndExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        BitAndExpr::EqualityExpr(_) => todo!(),
        BitAndExpr::BitAnd(_, _) => todo!(),
    }
}

fn analyze_multiplicative_expr<'ast, 'text>(
    expr: &'ast MultiplicativeExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        MultiplicativeExpr::CastExpr(_) => todo!(),
        MultiplicativeExpr::Mul(_, _) => todo!(),
        MultiplicativeExpr::Div(_, _) => todo!(),
        MultiplicativeExpr::Mod(_, _) => todo!(),
    }
}

fn analyze_cast_expr<'ast, 'text>(
    expr: &'ast CastExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        CastExpr::UnaryExpr(unary_expr) => analyze_unary_expr(unary_expr, ctx),
        CastExpr::Cast(type_name, sub_expr) => {
            let target_type = analyze_type_name(type_name, ctx)?;
            let curr_type = analyze_cast_expr(sub_expr, ctx)?;

            match (&target_type, &curr_type) {
                (Type::Int, Type::Char)
                | (Type::Int, Type::Float)
                | (Type::Int, Type::Double)
                | (Type::Char, Type::Int)
                | (Type::Float, Type::Int)
                | (Type::Double, Type::Int) => Ok(target_type),
                (Type::Pointer(_), Type::Pointer(_)) => Ok(target_type),
                _ => Err(SemanticError::InvalidTypeCast {
                    from: curr_type,
                    to: target_type,
                }),
            }
        }
    }
}

fn analyze_unary_expr<'ast, 'text>(
    expr: &'ast UnaryExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        UnaryExpr::PostfixExpr(postfix_expr) => analyze_postfix_expr(postfix_expr, ctx),
        UnaryExpr::PreIncr(inner_expr) | UnaryExpr::PreDecr(inner_expr) => {
            let ty = analyze_unary_expr(inner_expr, ctx)?;
            match ty {
                Type::Int | Type::Char | Type::Float | Type::Double | Type::Pointer(_) => Ok(ty),
                _ => Err(SemanticError::InvalidUnaryOperand(expr)),
            }
        }
        UnaryExpr::Ref(inner_expr) => {
            Ok(Type::Pointer(Box::new(analyze_cast_expr(inner_expr, ctx)?)))
        }
        UnaryExpr::Deref(inner_expr) => match analyze_cast_expr(inner_expr, ctx)? {
            Type::Pointer(ty) => Ok(*ty),
            _ => Err(SemanticError::InvalidDereferenceOperand(expr)),
        },
        UnaryExpr::UnaryAdd(inner_expr) | UnaryExpr::UnarySub(inner_expr) => {
            let ty = analyze_cast_expr(inner_expr, ctx)?;
            match ty {
                Type::Int | Type::Char | Type::Float | Type::Double => Ok(ty),
                _ => Err(SemanticError::InvalidUnaryOperand(expr)),
            }
        }
        UnaryExpr::OnesComplement(inner_expr) => {
            let ty = analyze_cast_expr(inner_expr, ctx)?;
            match ty {
                Type::Int | Type::Char => Ok(ty),
                _ => Err(SemanticError::InvalidUnaryOperand(expr)),
            }
        }
        UnaryExpr::Not(inner_expr) => {
            let ty = analyze_cast_expr(inner_expr, ctx)?;
            match ty {
                Type::Int => Ok(ty),
                _ => Err(SemanticError::InvalidUnaryOperand(expr)),
            }
        }
        UnaryExpr::SizeofExpr(inner_expr) => {
            analyze_unary_expr(inner_expr, ctx)?;
            Ok(Type::Int) // Sizeof always results in an integer value
        }
        UnaryExpr::SizeofTypeName(inner_expr) => {
            analyze_type_name(inner_expr, ctx)?;
            Ok(Type::Int) // Sizeof always results in an integer value
        }
    }
}

fn analyze_postfix_expr<'ast, 'text>(
    expr: &'ast PostfixExpr<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        PostfixExpr::Primary(expr) => analyze_primary_expr(expr, ctx),
        PostfixExpr::ArrayAccess(inner_expr, access) => match (
            analyze_postfix_expr(inner_expr, ctx)?,
            analyze_assignment_expr(access, ctx)?,
        ) {
            (Type::Pointer(ty) | Type::Array(ty, _), Type::Int) => Ok(*ty),
            _ => Err(SemanticError::InvalidPostfixOperand(expr)),
        },
        PostfixExpr::FunctionCall(inner_expr, args) => match analyze_postfix_expr(inner_expr, ctx)?
        {
            Type::Function(return_ty, param_tys) => {
                if args.len() != param_tys.len() {
                    return Err(SemanticError::InvalidFnCall(expr));
                }

                for (arg, param_ty) in args.iter().zip(param_tys.iter()) {
                    let arg_ty = analyze_assignment_expr(arg, ctx)?;
                    if &arg_ty != param_ty {
                        return Err(SemanticError::InvalidFnCall(expr));
                    }
                }
                Ok(*return_ty)
            }
            Type::Pointer(ty) => match *ty {
                Type::Function(return_ty, param_tys) => {
                    if args.len() != param_tys.len() {
                        return Err(SemanticError::InvalidFnCall(expr));
                    }

                    for (arg, param_ty) in args.iter().zip(param_tys.iter()) {
                        let arg_ty = analyze_assignment_expr(arg, ctx)?;
                        if &arg_ty != param_ty {
                            return Err(SemanticError::InvalidFnCall(expr));
                        }
                    }
                    Ok(*return_ty)
                }
                _ => Err(SemanticError::NotAFunction(inner_expr)),
            },
            _ => Err(SemanticError::NotAFunction(inner_expr)),
        },
        PostfixExpr::MemberAccess(inner_expr, field) => {
            match analyze_postfix_expr(inner_expr, ctx)? {
                Type::Struct(struct_name, members) => members
                    .into_iter()
                    .find(|(name, _)| name == field)
                    .map(|(_, ty)| ty)
                    .ok_or(SemanticError::UndefinedMember { struct_name, field }),
                _ => Err(SemanticError::NotAStruct(inner_expr)),
            }
        }
        PostfixExpr::PointerMemberAccess(inner_expr, field) => {
            match analyze_postfix_expr(inner_expr, ctx)? {
                Type::Pointer(inner_ty) => match *inner_ty {
                    Type::Struct(struct_name, members) => members
                        .into_iter()
                        .find(|(name, _)| name == field)
                        .map(|(_, ty)| ty)
                        .ok_or(SemanticError::UndefinedMember { struct_name, field }),
                    _ => Err(SemanticError::NotAStruct(inner_expr)),
                },
                _ => Err(SemanticError::NotAPointerToStruct(inner_expr)),
            }
        }

        PostfixExpr::PostIncr(inner_expr) | PostfixExpr::PostDecr(inner_expr) => {
            let ty = analyze_postfix_expr(inner_expr, ctx)?;
            match ty {
                Type::Int | Type::Char | Type::Float | Type::Double | Type::Pointer(_) => Ok(ty),
                _ => Err(SemanticError::InvalidPostfixOperand(expr)),
            }
        }
    }
}

fn analyze_primary_expr<'ast, 'text>(
    expr: &'ast Primary<'text>,
    ctx: &mut SemanticContext<'text>,
) -> Result<Type<'text>, SemanticError<'ast, 'text>> {
    match expr {
        Primary::Ident(ident) => match ctx.find_var(ident) {
            Some(var) => Ok(var.ty.clone()),
            None => Err(SemanticError::UndefinedVariable(ident)),
        },
        Primary::Int(_) => Ok(Type::Int),
        Primary::Char(_) => Ok(Type::Char),
        Primary::Float(_) => Ok(Type::Float),
        Primary::EnumConstant(ident) => match ctx.find_enum_invariant(ident) {
            Some(e) => Ok(e.ty.clone()),
            None => Err(SemanticError::UndefinedVariable(ident)), // there is no such thing as undefind enum
        },
        Primary::String(_) => Ok(Type::String),
        Primary::Parens(expr) => analyze_assignment_expr(expr, ctx),
    }
}

//     let return_type = analyze_declaration_specifiers(&ast.return_type, ctx)?;

//     // Add function parameters to context
//     let params = analyze_parameter_list(&ast.declarator, ctx)?;

//     // Create a new context for the function body with parameters

//     ctx.scoped(|ctx| {
//         for param in &params {
//             let a = ctx.declare_var(&param.name, param.ty.clone());

//             ctx.declare_var(&param.name, param.ty.clone())?;
//         }

//         // Analyze function body
//         analyze_compound_statement(&ast.body, &mut ctx)?;
//     });

//     Ok(())
// }

// fn analyze_parameter_list<'ast, 'text>(
//     params: &ParameterTypeList<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Vec<ParameterTypeList<'text>>, SemanticError<'ast, 'text>> {
//     match params {
//         ParameterTypeList::ParameterList(param_decls) => {
//             let mut analyzed_params = Vec::new();
//             for param_decl in param_decls {
//                 let param = analyze_parameter_declaration(param_decl, ctx)?;
//                 analyzed_params.push(param);
//             }
//             Ok(analyzed_params)
//         }
//         ParameterTypeList::VariadicParameterList(_) => {
//             // Handle variadic parameters if needed
//             unimplemented!("Variadic functions are not supported in this example");
//         }
//     }
// }

// fn analyze_parameter_declaration<'ast, 'text>(
//     param_decl: &ParameterDeclaration<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Parameter<'text>, SemanticError<'ast, 'text>> {
//     match param_decl {
//         ParameterDeclaration::WithDeclarator(decl_specifiers, declarator) => {
//             let param_ty = analyze_declaration_specifiers(decl_specifiers, ctx)?;
//             let param_name = get_param_name_from_declarator(declarator);
//             Ok(Parameter {
//                 name: param_name,
//                 ty: param_ty,
//             })
//         }
//         ParameterDeclaration::WithAbstractDeclarator(_, _) => {
//             // Handle abstract declarators if needed
//             unimplemented!("Abstract declarators are not supported in this example");
//         }
//         ParameterDeclaration::OnlySpecifiers(_) => {
//             // Handle cases where the parameter has no name
//             unimplemented!("Parameter with no name is not supported in this example");
//         }
//     }
// }

// fn get_param_name_from_declarator<'text>(declarator: &Declarator<'ast, 'text>) -> &'text str {
//     // Extract the parameter name from the declarator
//     // Implement the logic to extract the name based on your language's rules
//     // For example, if the declarator is an identifier, return its name
//     // This is a placeholder and needs to be adapted based on your specific language
//     match &declarator.d_declarator {
//         DirectDeclarator::Ident(name, _) => name,
//         _ => unimplemented!("Unsupported declarator for function parameter"),
//     }
// }

// fn analyze_compound_statement<'ast, 'text>(
//     compound_stmt: &CompoundStmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     ctx.scoped(|ctx| {
//         for block_item in &compound_stmt.0 {
//             match block_item {
//                 BlockItem::Declaration(d) => analyze_declaration(d, ctx)?,
//                 BlockItem::Statement(stmt) => analyze_stmt(stmt, ctx)?,
//             }
//         }
//         Ok(())
//     })
// }

// fn analyze_stmt<'ast, 'text>(
//     stmt: &Stmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match stmt {
//         Stmt::EmptyStmt => { /* nothing to analyze */ }
//         Stmt::Expr(expr) => analyze_expr(expr, ctx)?,
//         Stmt::Labeled(stmt) => analyze_labeled_stmt(stmt, ctx)?,
//         Stmt::Compound(stmt) => analyze_compound_statement(stmt, ctx)?,
//         Stmt::Selection(stmt) => analyze_selection_stmt(stmt, ctx)?,
//         Stmt::Iteration(stmt) => analyze_iteration_stmt(stmt, ctx)?,
//         Stmt::Jump(stmt) => analyze_jump_stmt(stmt, ctx)?,
//     }
//     Ok(())
// }

// // Implement other semantic analysis functions as needed...

// fn analyze_labeled_stmt<'ast, 'text>(
//     stmt: &LabeledStmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match stmt {
//         LabeledStmt::Ident(label, stmt) => {
//             ctx.declare_label(label);
//             analyze_stmt(stmt, ctx)?;
//         }
//         LabeledStmt::Case(expr, stmt) => {
//             // TODO: in ctx specify that we are currently inside a switch stmt
//             // because case is only allowed in switch

//             // Analyze the statement following a case label
//             analyze_stmt(stmt, ctx)?;
//         }
//         LabeledStmt::Default(stmt) => {
//             // TODO: in ctx specify that we are currently inside a switch stmt
//             // because default is only allowed in switch

//             // Analyze the statement following the default label
//             analyze_stmt(stmt, ctx)?;
//         }
//     }

//     Ok(())
// }

// fn analyze_selection_stmt<'ast, 'text>(
//     selection_stmt: &SelectionStmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match selection_stmt {
//         SelectionStmt::If { test, pass } => {
//             analyze_expr(test, &Type::Int, ctx)?;
//             analyze_stmt(pass, ctx)?;
//         }
//         SelectionStmt::IfElse { test, pass, fail } => {
//             analyze_expr(test, &Type::Int, ctx)?;
//             analyze_stmt(pass, ctx)?;
//             analyze_stmt(fail, ctx)?;
//         }
//         SelectionStmt::Switch { test, pass } => {
//             // TODO: in ctx specify that we are currently inside a switch stmt
//             // because case and default is only allowed in switch
//             // and we need a way to figure out if we are inside a switch stmt
//             // when analyzing case and default

//             analyze_expr(test, &Type::Int, ctx)?;
//             analyze_stmt(pass, ctx)?;
//         }
//     }
//     Ok(())
// }

// fn analyze_iteration_stmt<'ast, 'text>(
//     stmt: &IterationStmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match stmt {
//         IterationStmt::While { test, body } => {
//             analyze_expr(test, ctx)?;

//             // Ensure the test expression has type 'int'
//             if test_type != Type::Int {
//                 return Err(SemanticError::InvalidIterationTestType);
//             }

//             // Analyze the body of the while loop
//             analyze_stmt(body, ctx)?;
//         }
//         IterationStmt::DoWhile { test, body } => {
//             // Analyze the body of the do-while loop
//             analyze_stmt(body, ctx)?;

//             let test_type = analyze_expr(test, ctx)?;

//             // Ensure the test expression has type 'int'
//             if test_type != Type::Int {
//                 return Err(SemanticError::InvalidIterationTestType);
//             }
//         }
//         IterationStmt::For {
//             init,
//             test,
//             update,
//             body,
//         } => {
//             // Analyze the initialization expression
//             if let Some(init_expr) = init {
//                 analyze_expr(init_expr, ctx)?;
//             }

//             // Analyze the test expression
//             if let Some(test_expr) = test {
//                 let test_type = analyze_expr(test_expr, ctx)?;

//                 // Ensure the test expression has type 'int'
//                 if test_type != Type::Int {
//                     return Err(SemanticError::InvalidIterationTestType);
//                 }
//             }

//             // Analyze the update expression
//             if let Some(update_expr) = update {
//                 analyze_expr(update_expr, ctx)?;
//             }

//             // Analyze the body of the for loop
//             analyze_stmt(body, ctx)?;
//         }
//     }

//     Ok(())
// }

// fn analyze_jump_stmt<'ast, 'text>(
//     stmt: &JumpStmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match stmt {
//         JumpStmt::Goto(label) => {
//             // Check if the label is declared
//             if !ctx.is_label_declared(label) {
//                 return Err(SemanticError::UndeclaredLabel(label.to_string()));
//             }
//         }
//         JumpStmt::Return(expr) => {
//             // Analyze the return expression if present
//             if let Some(expr) = expr {
//                 analyze_expr(expr, ctx)?;
//             }
//         }
//         // 'continue' and 'break' do not require additional analysis
//         JumpStmt::Continue | JumpStmt::Break => {}
//     }

//     Ok(())
// }

// fn analyze_declaration_specifiers<'ast, 'text>(
//     declaration_specifiers: &[DeclarationSpecifier<'text>],
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Type, SemanticError<'ast, 'text>> {
//     // Implement logic to interpret declaration specifiers and return the corresponding type
//     // ...

//     Ok(Type::Int) // Placeholder, replace with actual logic
// }

// fn analyze_init_declarator<'ast, 'text>(
//     init_declarator: &InitDeclarator<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Type, SemanticError<'ast, 'text>> {
//     match init_declarator {
//         InitDeclarator::Declared(declarator) => {
//             // Analyze the declared variable and update the context
//             analyze_declarator(declarator, ty, ctx)?;
//         }
//         InitDeclarator::Initialized(declarator, initializer) => {
//             // Analyze the declared variable and update the context
//             analyze_declarator(declarator, ty, ctx)?;

//             // Analyze the initializer expression
//             analyze_initializer(initializer, ty, ctx)?;
//         }
//     }

//     Ok(())
// }

// fn analyze_declarator<'ast, 'text>(
//     declarator: &Declarator<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     // Implement logic to analyze the declarator and update the context
//     // ...

//     Ok(())
// }

// fn analyze_initializer<'ast, 'text>(
//     initializer: &Initializer<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match initializer {
//         Initializer::Assignment(assignment_expr) => {
//             // Analyze the assignment expression
//             analyze_assignment_expr(assignment_expr, ty, ctx)?;
//         }
//         Initializer::InitializerList(initializer_list) => {
//             // Analyze each initializer in the list
//             for init in initializer_list {
//                 analyze_initializer(init, ty, ctx)?;
//             }
//         }
//     }

//     Ok(())
// }

// fn analyze_assignment_expr<'ast, 'text>(
//     assignment_expr: &AssignmentExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match assignment_expr {
//         AssignmentExpr::ConditionalExpr(cond_expr) => {
//             // Analyze the conditional expression
//             analyze_conditional_expr(cond_expr, ty, ctx)?;
//         }
//         AssignmentExpr::Assign(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the assignment
//             analyze_unary_expr(lhs, ty, ctx)?;
//             analyze_assignment_expr(rhs, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         // Implement other assignment operators as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this assignment expression"),
//     }

//     Ok(())
// }

// // Continue with similar functions for other AST nodes...

// fn analyze_conditional_expr<'ast, 'text>(
//     cond_expr: &ConditionalExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     // Implement logic to analyze the conditional expression
//     // ...

//     Ok(())
// }

// fn analyze_expr<'ast, 'text>(
//     expr: &Expr<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match expr {
//         // Expr::AssignmentExpr(assignment_expr) => {
//         //     // Analyze the assignment expression
//         //     analyze_assignment_expr(assignment_expr, expected_type, ctx)?;
//         // }
//         // Expr::LogicalOrExpr(logical_or_expr) => {
//         //     // Analyze the logical OR expression
//         //     analyze_logical_or_expr(logical_or_expr, expected_type, ctx)?;
//         // }
//         // // Implement other expression types as needed...
//         // _ => unimplemented!("Semantic analysis not implemented for this expression"),
//         AssignmentExpr::ConditionalExpr(_) => analyze_conditional_expr(),
//         AssignmentExpr::Assign(_, _) => todo!(),
//         AssignmentExpr::MulAssign(_, _) => todo!(),
//         AssignmentExpr::DivAssign(_, _) => todo!(),
//         AssignmentExpr::ModAssign(_, _) => todo!(),
//         AssignmentExpr::AddAssign(_, _) => todo!(),
//         AssignmentExpr::SubAssign(_, _) => todo!(),
//         AssignmentExpr::ShiftLeftAssign(_, _) => todo!(),
//         AssignmentExpr::ShiftRightAssign(_, _) => todo!(),
//         AssignmentExpr::BitAndAssign(_, _) => todo!(),
//         AssignmentExpr::XORAssign(_, _) => todo!(),
//         AssignmentExpr::BitOrAssign(_, _) => todo!(),
//     };

//     Ok(())
// }

// fn analyze_logical_or_expr<'ast, 'text>(
//     logical_or_expr: &LogicalOrExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match logical_or_expr {
//         LogicalOrExpr::LogicalAndExpr(logical_and_expr) => {
//             // Analyze the logical AND expression
//             analyze_logical_and_expr(logical_and_expr, ty, ctx)?;
//         }
//         LogicalOrExpr::LogicalOr(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the logical OR
//             analyze_logical_or_expr(lhs, ty, ctx)?;
//             analyze_logical_and_expr(rhs, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         // Implement other logical OR operations as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this logical OR expression"),
//     }

//     Ok(())
// }

// fn analyze_logical_and_expr<'ast, 'text>(
//     logical_and_expr: &LogicalAndExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match logical_and_expr {
//         LogicalAndExpr::BitOrExpr(bit_or_expr) => {
//             // Analyze the bitwise OR expression
//             analyze_bit_or_expr(bit_or_expr, ty, ctx)?;
//         }
//         LogicalAndExpr::LogicalAnd(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the logical AND
//             analyze_logical_and_expr(lhs, ty, ctx)?;
//             analyze_bit_or_expr(rhs, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         // Implement other logical AND operations as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this logical AND expression"),
//     }

//     Ok(())
// }

// fn analyze_bit_or_expr<'ast, 'text>(
//     bit_or_expr: &BitOrExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match bit_or_expr {
//         BitOrExpr::XORExpr(xor_expr) => {
//             // Analyze the XOR expression
//             analyze_xor_expr(xor_expr, ty, ctx)?;
//         }
//         BitOrExpr::BitOr(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the bitwise OR
//             analyze_bit_or_expr(lhs, ty, ctx)?;
//             analyze_xor_expr(rhs, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         // Implement other bitwise OR operations as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this bitwise OR expression"),
//     }

//     Ok(())
// }

// fn analyze_xor_expr<'ast, 'text>(
//     xor_expr: &XORExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match xor_expr {
//         XORExpr::BitAndExpr(bit_and_expr) => {
//             // Analyze the bitwise AND expression
//             analyze_bit_and_expr(bit_and_expr, ty, ctx)?;
//         }
//         XORExpr::XOR(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the XOR
//             analyze_xor_expr(lhs, ty, ctx)?;
//             analyze_bit_and_expr(rhs, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         // Implement other XOR operations as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this XOR expression"),
//     }

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn analyze_bit_and_expr<'ast, 'text>(
//     bit_and_expr: &BitAndExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     // Implement logic to analyze the bitwise AND expression
//     // ...

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn analyze_shift_expr<'ast, 'text>(
//     shift_expr: &ShiftExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     // Implement logic to analyze the shift expression
//     // ...

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn analyze_additive_expr<'ast, 'text>(
//     additive_expr: &AdditiveExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     // Implement logic to analyze the additive expression
//     // ...

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn analyze_multiplicative_expr<'ast, 'text>(
//     multiplicative_expr: &MultiplicativeExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     // Implement logic to analyze the multiplicative expression
//     // ...

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn analyze_cast_expr<'ast, 'text>(
//     cast_expr: &CastExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match cast_expr {
//         CastExpr::UnaryExpr(unary_expr) => {
//             // Analyze the unary expression
//             analyze_unary_expr(unary_expr, ty, ctx)?;
//         }
//         CastExpr::Cast(type_name, expr) => {
//             // Analyze the type name and the expression
//             let cast_type = analyze_type_name(type_name, ctx)?;
//             analyze_expr(expr, &cast_type, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         // Implement other cast operations as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this cast expression"),
//     }

//     Ok(())
// }

// fn analyze_type_name<'ast, 'text>(
//     type_name: &TypeName<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Type, SemanticError<'ast, 'text>> {
//     // Implement logic to analyze the type name and return the corresponding type
//     // ...

//     Ok(Type::Int) // Placeholder, replace with actual logic
// }

// // Continue with similar functions for other expression types...

// fn analyze_unary_expr<'ast, 'text>(
//     unary_expr: &UnaryExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match unary_expr {
//         UnaryExpr::PostfixExpr(postfix_expr) => {
//             // Analyze the postfix expression
//             analyze_postfix_expr(postfix_expr, ty, ctx)?;
//         }
//         UnaryExpr::PreIncr(expr)
//         | UnaryExpr::PreDecr(expr)
//         | UnaryExpr::Ref(expr)
//         | UnaryExpr::Deref(expr)
//         | UnaryExpr::UnaryAdd(expr)
//         | UnaryExpr::UnarySub(expr)
//         | UnaryExpr::OnesComplement(expr)
//         | UnaryExpr::Not(expr) => {
//             // Analyze the sub-expression
//             analyze_cast_expr(expr, ty, ctx)?;
//         }
//         UnaryExpr::SizeofExpr(expr) => {
//             // Analyze the expression inside sizeof
//             analyze_unary_expr(expr, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         UnaryExpr::SizeofTypeName(type_name) => {
//             // Analyze the type name inside sizeof
//             analyze_type_name(type_name, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         // Implement other unary operations as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this unary expression"),
//     }

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn analyze_postfix_expr<'ast, 'text>(
//     postfix_expr: &PostfixExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match postfix_expr {
//         PostfixExpr::Primary(primary) => {
//             // Analyze the primary expression
//             analyze_primary(primary, ty, ctx)?;
//         }
//         PostfixExpr::ArrayAccess(expr, index_expr) => {
//             // Analyze the base expression and the index expression
//             analyze_postfix_expr(expr, ty, ctx)?;
//             analyze_expr(index_expr, &Type::Int, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         PostfixExpr::FunctionCall(expr, args) => {
//             // Analyze the base expression and the function arguments
//             analyze_postfix_expr(expr, ty, ctx)?;
//             for arg in args {
//                 analyze_assignment_expr(arg, ty, ctx)?;
//             }

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         PostfixExpr::MemberAccess(expr, member)
//         | PostfixExpr::PointerMemberAccess(expr, member) => {
//             // Analyze the base expression
//             analyze_postfix_expr(expr, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         PostfixExpr::PostIncr(expr) | PostfixExpr::PostDecr(expr) => {
//             // Analyze the sub-expression
//             analyze_postfix_expr(expr, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         // Implement other postfix operations as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this postfix expression"),
//     }

//     Ok(())
// }

// fn analyze_primary<'ast, 'text>(
//     primary: &Primary<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     match primary {
//         Primary::Ident(ident) => {
//             // Check if the identifier is declared in the current context
//             ctx.find_var(ident)
//                 .ok_or(SemanticError::UndefinedVariable(ident))?;
//         }
//         Primary::Int(_) | Primary::Char(_) | Primary::Float(_) => {
//             // Primary literals, nothing to analyze
//         }
//         Primary::EnumConstant(enum_const) => {
//             // Check if the enum constant is declared in the current context
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         Primary::String(_) => {
//             // String literal, nothing to analyze
//         }
//         Primary::Parens(expr) => {
//             // Analyze the expression inside parentheses
//             analyze_expr(expr, ty, ctx)?;
//         }
//     }

//     Ok(())
// }

// // Implement similar functions for other AST nodes...

// // Finally, you can create a top-level function to start semantic analysis
// pub fn perform_semantic_analysis<'ast, 'text>(
//     translation_unit: &TranslationUnit<'text>,
// ) -> Result<(), SemanticError<'ast, 'text>> {
//     let mut ctx = SemanticContext::new();

//     // Analyze the translation unit
//     analyze_translation_unit(translation_unit, &mut ctx)?;

//     Ok(())
// }
