// use crate::*;

// #[derive(Debug, PartialEq, Clone)]
// enum Type {
//     Int,
//     Char,
//     Float,
//     Double,
//     Void,
//     // Struct(&'text str),  // Struct type with a name
//     // Enum(&'text str),    // Enum type with a name
//     // Pointer(Box<Type<'text>>),
//     // Array(Box<Type<'text>>, usize),  // Array type with element type and size
//     // Function(Box<Type<'text>>, Vec<Type<'text>>),  // Function type with return type and parameter types
//     // Add other types as needed...
// }

// #[derive(Debug, PartialEq, Clone)]
// struct Symbol<'text> {
//     name: &'text str,
//     ty: Type,
// }

// pub type Scope<'text> = Vec<Symbol<'text>>;

// struct SemanticContext<'text> {
//     symbol_table: Vec<Scope<'text>>,
// }

// impl<'text> SemanticContext<'text> {
//     fn new() -> SemanticContext<'text> {
//         SemanticContext {
//             symbol_table: vec![vec![]],
//         }
//     }

//     fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
//         self.symbol_table.push(vec![]);
//         let r = f(self);
//         self.symbol_table.pop();
//         r
//     }

//     fn declare_variable(&mut self, name: &'text str, ty: Type) -> bool {
//         let scope = self
//             .symbol_table
//             .last_mut()
//             .expect("must have atleast one scope");

//         // cannot redeclare variable
//         if scope.iter().find(|s| s.name == name).is_some() {
//             return false;
//         }

//         scope.push(Symbol { name, ty });
//         true
//     }

//     fn find_variable<'s>(&'s self, name: &'text str) -> Option<&'s Symbol<'text>> {
//         self.symbol_table
//             .iter()
//             .rev()
//             .flat_map(|scope| scope.iter())
//             .find(|symbol| symbol.name == name)
//     }
// }

// #[derive(Debug, PartialEq, Clone)]
// pub enum SemanticError<'text> {
//     UndefinedVariable(&'text str),
//     VariableRedeclaration(&'text str),
//     InvalidBinaryOperands(&'text str),
//     // Add other semantic errors as needed...
// }

// fn semantic_analyze_translation_unit<'text>(
//     translation_unit: &TranslationUnit<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     for external_declaration in &translation_unit.0 {
//         semantic_analyze_external_declaration(external_declaration, ctx)?;
//     }
//     Ok(())
// }

// fn semantic_analyze_external_declaration<'text>(
//     external_declaration: &ExternalDeclaration<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match external_declaration {
//         ExternalDeclaration::FunctionDefinition(f) => semantic_analyze_function_definition(f, ctx)?,
//         ExternalDeclaration::Declaration(d) => semantic_analyze_declaration(d, ctx)?,
//     }
//     Ok(())
// }

// fn semantic_analyze_function_definition<'text>(
//     ast: &FunctionDefinition<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     let return_type = semantic_analyze_declaration_specifiers(&ast.return_type, ctx)?;

//     // Add function parameters to context
//     let params = semantic_analyze_parameter_list(&ast.declarator, ctx)?;

//     // Create a new context for the function body with parameters

//     ctx.scoped(|scoped_ctx| {
//         for param in &params {
//             let a = scoped_ctx.declare_variable(&param.name, param.ty.clone());

//             scoped_ctx.declare_variable(&param.name, param.ty.clone())?;
//         }

//         // Analyze function body
//         semantic_analyze_compound_statement(&ast.body, &mut scoped_ctx)?;
//     });

//     Ok(())
// }

// fn semantic_analyze_parameter_list<'text>(
//     params: &ParameterTypeList<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Vec<ParameterTypeList<'text>>, SemanticError<'text>> {
//     match params {
//         ParameterTypeList::ParameterList(param_decls) => {
//             let mut analyzed_params = Vec::new();
//             for param_decl in param_decls {
//                 let param = semantic_analyze_parameter_declaration(param_decl, ctx)?;
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

// fn semantic_analyze_parameter_declaration<'text>(
//     param_decl: &ParameterDeclaration<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Parameter<'text>, SemanticError<'text>> {
//     match param_decl {
//         ParameterDeclaration::WithDeclarator(decl_specifiers, declarator) => {
//             let param_ty = semantic_analyze_declaration_specifiers(decl_specifiers, ctx)?;
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

// fn get_param_name_from_declarator<'text>(declarator: &Declarator<'text>) -> &'text str {
//     // Extract the parameter name from the declarator
//     // Implement the logic to extract the name based on your language's rules
//     // For example, if the declarator is an identifier, return its name
//     // This is a placeholder and needs to be adapted based on your specific language
//     match &declarator.d_declarator {
//         DirectDeclarator::Ident(name, _) => name,
//         _ => unimplemented!("Unsupported declarator for function parameter"),
//     }
// }

// fn semantic_analyze_compound_statement<'text>(
//     compound_stmt: &CompoundStmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     // Create a new scope for the compound statement
//     ctx.enter_scope();

//     for block_item in &compound_stmt.0 {
//         match block_item {
//             BlockItem::Declaration(declaration) => {
//                 // Analyze declarations in the compound statement
//                 semantic_analyze_declaration(declaration, ctx)?;
//             }
//             BlockItem::Statement(stmt) => {
//                 // Analyze statements in the compound statement
//                 semantic_analyze_stmt(stmt, ctx)?;
//             }
//         }
//     }

//     // Exit the scope for the compound statement
//     ctx.exit_scope();

//     Ok(())
// }

// fn semantic_analyze_stmt<'text>(
//     stmt: &Stmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match stmt {
//         Stmt::Labeled(labeled_stmt) => {
//             // Analyze labeled statements
//             semantic_analyze_labeled_stmt(labeled_stmt, ctx)?;
//         }
//         Stmt::EmptyStmt => {
//             // Nothing to analyze for an empty statement
//         }
//         Stmt::Expr(expr) => {
//             // Analyze expressions in statements
//             semantic_analyze_expr(expr, &Type::Int, ctx)?;
//         }
//         Stmt::Compound(compound_stmt) => {
//             // Analyze compound statements
//             semantic_analyze_compound_statement(compound_stmt, ctx)?;
//         }
//         Stmt::Selection(selection_stmt) => {
//             // Analyze selection statements (if, switch)
//             semantic_analyze_selection_stmt(selection_stmt, ctx)?;
//         }
//         Stmt::Iteration(iteration_stmt) => {
//             // Analyze iteration statements (while, do-while, for)
//             semantic_analyze_iteration_stmt(iteration_stmt, ctx)?;
//         }
//         Stmt::Jump(jump_stmt) => {
//             // Analyze jump statements (goto, continue, break, return)
//             semantic_analyze_jump_stmt(jump_stmt, ctx)?;
//         }
//     }

//     Ok(())
// }

// // Implement other semantic analysis functions as needed...

// fn semantic_analyze_labeled_stmt<'text>(
//     labeled_stmt: &LabeledStmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match labeled_stmt {
//         LabeledStmt::Ident(_, stmt) => {
//             // Analyze statements following an identifier (usually used for goto)
//             semantic_analyze_stmt(stmt, ctx)?;
//         }
//         LabeledStmt::Case(_, stmt) => {
//             // Analyze the statement following a case label
//             semantic_analyze_stmt(stmt, ctx)?;
//         }
//         LabeledStmt::Default(stmt) => {
//             // Analyze the statement following the default label
//             semantic_analyze_stmt(stmt, ctx)?;
//         }
//     }

//     Ok(())
// }

// fn semantic_analyze_selection_stmt<'text>(
//     selection_stmt: &SelectionStmt<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match selection_stmt {
//         SelectionStmt::If { test, pass } => {
//             // Analyze the test expression and the statement for if
//             semantic_analyze_expr(test, &Type::Int, ctx)?;
//             semantic_analyze_stmt(pass, ctx)?;
//         }
//         SelectionStmt::IfElse { test, pass, fail } => {
//             // Analyze the test expression and both statements for if-else
//             semantic_analyze_expr(test, &Type::Int, ctx)?;
//             semantic_analyze_stmt(pass, ctx)?;
//             semantic_analyze_stmt(fail, ctx)?;
//         }
//         SelectionStmt::Switch { test, pass } => {
//             // Analyze the test expression and the statement for switch
//             semantic_analyze_expr(test, &Type::Int, ctx)?;
//             semantic_analyze_stmt(pass, ctx)?;
//         }
//     }

//     Ok(())
// }

// fn semantic_analyze_declaration<'text>(
//     declaration: &Declaration<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     let ty = semantic_analyze_declaration_specifiers(&declaration.declaration_specifiers, ctx)?;

//     for init_declarator in &declaration.init_declarators {
//         semantic_analyze_init_declarator(init_declarator, &ty, ctx)?;
//     }

//     Ok(())
// }

// fn semantic_analyze_declaration_specifiers<'text>(
//     declaration_specifiers: &[DeclarationSpecifier<'text>],
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Type, SemanticError<'text>> {
//     // Implement logic to interpret declaration specifiers and return the corresponding type
//     // ...

//     Ok(Type::Int) // Placeholder, replace with actual logic
// }

// fn semantic_analyze_init_declarator<'text>(
//     init_declarator: &InitDeclarator<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match init_declarator {
//         InitDeclarator::Declared(declarator) => {
//             // Analyze the declared variable and update the context
//             semantic_analyze_declarator(declarator, ty, ctx)?;
//         }
//         InitDeclarator::Initialized(declarator, initializer) => {
//             // Analyze the declared variable and update the context
//             semantic_analyze_declarator(declarator, ty, ctx)?;

//             // Analyze the initializer expression
//             semantic_analyze_initializer(initializer, ty, ctx)?;
//         }
//     }

//     Ok(())
// }

// fn semantic_analyze_declarator<'text>(
//     declarator: &Declarator<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     // Implement logic to analyze the declarator and update the context
//     // ...

//     Ok(())
// }

// fn semantic_analyze_initializer<'text>(
//     initializer: &Initializer<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match initializer {
//         Initializer::Assignment(assignment_expr) => {
//             // Analyze the assignment expression
//             semantic_analyze_assignment_expr(assignment_expr, ty, ctx)?;
//         }
//         Initializer::InitializerList(initializer_list) => {
//             // Analyze each initializer in the list
//             for init in initializer_list {
//                 semantic_analyze_initializer(init, ty, ctx)?;
//             }
//         }
//     }

//     Ok(())
// }

// fn semantic_analyze_assignment_expr<'text>(
//     assignment_expr: &AssignmentExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match assignment_expr {
//         AssignmentExpr::ConditionalExpr(cond_expr) => {
//             // Analyze the conditional expression
//             semantic_analyze_conditional_expr(cond_expr, ty, ctx)?;
//         }
//         AssignmentExpr::Assign(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the assignment
//             semantic_analyze_unary_expr(lhs, ty, ctx)?;
//             semantic_analyze_assignment_expr(rhs, ty, ctx)?;

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

// fn semantic_analyze_conditional_expr<'text>(
//     cond_expr: &ConditionalExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     // Implement logic to analyze the conditional expression
//     // ...

//     Ok(())
// }

// fn semantic_analyze_expr<'text>(
//     expr: &Expr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match expr {
//         Expr::AssignmentExpr(assignment_expr) => {
//             // Analyze the assignment expression
//             semantic_analyze_assignment_expr(assignment_expr, ty, ctx)?;
//         }
//         Expr::LogicalOrExpr(logical_or_expr) => {
//             // Analyze the logical OR expression
//             semantic_analyze_logical_or_expr(logical_or_expr, ty, ctx)?;
//         }
//         // Implement other expression types as needed...
//         _ => unimplemented!("Semantic analysis not implemented for this expression"),
//     }

//     Ok(())
// }

// fn semantic_analyze_logical_or_expr<'text>(
//     logical_or_expr: &LogicalOrExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match logical_or_expr {
//         LogicalOrExpr::LogicalAndExpr(logical_and_expr) => {
//             // Analyze the logical AND expression
//             semantic_analyze_logical_and_expr(logical_and_expr, ty, ctx)?;
//         }
//         LogicalOrExpr::LogicalOr(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the logical OR
//             semantic_analyze_logical_or_expr(lhs, ty, ctx)?;
//             semantic_analyze_logical_and_expr(rhs, ty, ctx)?;

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

// fn semantic_analyze_logical_and_expr<'text>(
//     logical_and_expr: &LogicalAndExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match logical_and_expr {
//         LogicalAndExpr::BitOrExpr(bit_or_expr) => {
//             // Analyze the bitwise OR expression
//             semantic_analyze_bit_or_expr(bit_or_expr, ty, ctx)?;
//         }
//         LogicalAndExpr::LogicalAnd(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the logical AND
//             semantic_analyze_logical_and_expr(lhs, ty, ctx)?;
//             semantic_analyze_bit_or_expr(rhs, ty, ctx)?;

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

// fn semantic_analyze_bit_or_expr<'text>(
//     bit_or_expr: &BitOrExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match bit_or_expr {
//         BitOrExpr::XORExpr(xor_expr) => {
//             // Analyze the XOR expression
//             semantic_analyze_xor_expr(xor_expr, ty, ctx)?;
//         }
//         BitOrExpr::BitOr(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the bitwise OR
//             semantic_analyze_bit_or_expr(lhs, ty, ctx)?;
//             semantic_analyze_xor_expr(rhs, ty, ctx)?;

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

// fn semantic_analyze_xor_expr<'text>(
//     xor_expr: &XORExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match xor_expr {
//         XORExpr::BitAndExpr(bit_and_expr) => {
//             // Analyze the bitwise AND expression
//             semantic_analyze_bit_and_expr(bit_and_expr, ty, ctx)?;
//         }
//         XORExpr::XOR(lhs, rhs) => {
//             // Analyze the left-hand side (lhs) and right-hand side (rhs) of the XOR
//             semantic_analyze_xor_expr(lhs, ty, ctx)?;
//             semantic_analyze_bit_and_expr(rhs, ty, ctx)?;

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

// fn semantic_analyze_bit_and_expr<'text>(
//     bit_and_expr: &BitAndExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     // Implement logic to analyze the bitwise AND expression
//     // ...

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn semantic_analyze_shift_expr<'text>(
//     shift_expr: &ShiftExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     // Implement logic to analyze the shift expression
//     // ...

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn semantic_analyze_additive_expr<'text>(
//     additive_expr: &AdditiveExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     // Implement logic to analyze the additive expression
//     // ...

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn semantic_analyze_multiplicative_expr<'text>(
//     multiplicative_expr: &MultiplicativeExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     // Implement logic to analyze the multiplicative expression
//     // ...

//     Ok(())
// }

// // Continue with similar functions for other expression types...

// fn semantic_analyze_cast_expr<'text>(
//     cast_expr: &CastExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match cast_expr {
//         CastExpr::UnaryExpr(unary_expr) => {
//             // Analyze the unary expression
//             semantic_analyze_unary_expr(unary_expr, ty, ctx)?;
//         }
//         CastExpr::Cast(type_name, expr) => {
//             // Analyze the type name and the expression
//             let cast_type = semantic_analyze_type_name(type_name, ctx)?;
//             semantic_analyze_expr(expr, &cast_type, ctx)?;

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

// fn semantic_analyze_type_name<'text>(
//     type_name: &TypeName<'text>,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<Type, SemanticError<'text>> {
//     // Implement logic to analyze the type name and return the corresponding type
//     // ...

//     Ok(Type::Int) // Placeholder, replace with actual logic
// }

// // Continue with similar functions for other expression types...

// fn semantic_analyze_unary_expr<'text>(
//     unary_expr: &UnaryExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match unary_expr {
//         UnaryExpr::PostfixExpr(postfix_expr) => {
//             // Analyze the postfix expression
//             semantic_analyze_postfix_expr(postfix_expr, ty, ctx)?;
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
//             semantic_analyze_cast_expr(expr, ty, ctx)?;
//         }
//         UnaryExpr::SizeofExpr(expr) => {
//             // Analyze the expression inside sizeof
//             semantic_analyze_unary_expr(expr, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         UnaryExpr::SizeofTypeName(type_name) => {
//             // Analyze the type name inside sizeof
//             semantic_analyze_type_name(type_name, ctx)?;

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

// fn semantic_analyze_postfix_expr<'text>(
//     postfix_expr: &PostfixExpr<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match postfix_expr {
//         PostfixExpr::Primary(primary) => {
//             // Analyze the primary expression
//             semantic_analyze_primary(primary, ty, ctx)?;
//         }
//         PostfixExpr::ArrayAccess(expr, index_expr) => {
//             // Analyze the base expression and the index expression
//             semantic_analyze_postfix_expr(expr, ty, ctx)?;
//             semantic_analyze_expr(index_expr, &Type::Int, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         PostfixExpr::FunctionCall(expr, args) => {
//             // Analyze the base expression and the function arguments
//             semantic_analyze_postfix_expr(expr, ty, ctx)?;
//             for arg in args {
//                 semantic_analyze_assignment_expr(arg, ty, ctx)?;
//             }

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         PostfixExpr::MemberAccess(expr, member)
//         | PostfixExpr::PointerMemberAccess(expr, member) => {
//             // Analyze the base expression
//             semantic_analyze_postfix_expr(expr, ty, ctx)?;

//             // Perform additional checks if needed
//             // ...

//             // Update the context if necessary
//             // ...
//         }
//         PostfixExpr::PostIncr(expr) | PostfixExpr::PostDecr(expr) => {
//             // Analyze the sub-expression
//             semantic_analyze_postfix_expr(expr, ty, ctx)?;

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

// fn semantic_analyze_primary<'text>(
//     primary: &Primary<'text>,
//     ty: &Type,
//     ctx: &mut SemanticContext<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     match primary {
//         Primary::Ident(ident) => {
//             // Check if the identifier is declared in the current context
//             ctx.find_variable(ident)
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
//             semantic_analyze_expr(expr, ty, ctx)?;
//         }
//     }

//     Ok(())
// }

// // Implement similar functions for other AST nodes...

// // Finally, you can create a top-level function to start semantic analysis
// pub fn perform_semantic_analysis<'text>(
//     translation_unit: &TranslationUnit<'text>,
// ) -> Result<(), SemanticError<'text>> {
//     let mut ctx = SemanticContext::new();

//     // Analyze the translation unit
//     semantic_analyze_translation_unit(translation_unit, &mut ctx)?;

//     Ok(())
// }
