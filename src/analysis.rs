use crate::error::Log;
use crate::lex::BinOp;
use crate::parse::{
    Array, ArraySize, AssignVal, AstKind, AstNode, ElseBlock, LexVisitor, Number, VarType,
    VarTypeKind,
};

fn check_if_to_be(ast: &mut AstNode, visitor: &LexVisitor, kind: &mut Option<VarType>) {
    match &mut ast.kind {
        AstKind::Block(block) => {
            for stmt in &mut block.stmts {
                match &mut stmt.kind {
                    AstKind::ExprRet(ret) => {
                        let new = var_type_of(
                            ret,
                            visitor,
                            kind.clone()
                                .unwrap_or(VarType::new(VarTypeKind::Void, true)),
                        );
                        if let Some(old) = kind {
                            if !old.clone().loosy_eq(new.clone()) {
                                visitor.error(
                                    format!(
                                        "Expected if statement to have return 
                                         type of {old} but this has a type of {new}"
                                    ),
                                    "".to_string(),
                                    &ret.span,
                                )
                            }
                        }
                        *kind = Some(new);
                    }
                    _ => {}
                }
                if kind.is_none() {
                    *kind = Some(VarType::new(VarTypeKind::Void, true));
                }
            }
        }
        _ => unreachable!(),
    }
}

pub fn var_type_of(ast: &mut AstNode, visitor: &LexVisitor, expected_type: VarType) -> VarType {
    match &mut ast.kind {
        AstKind::Ident(id) => {
            let path_id = visitor.get_id(&id);
            match path_id {
                Some(info) => info.kind.clone(),
                None => todo!(),
            }
        }
        AstKind::BinOp(lhs, op, rhs) => {
            let mut var_type = var_type_of(lhs, visitor, expected_type.clone());
            match &mut var_type.kind {
                VarTypeKind::Int | VarTypeKind::Float | VarTypeKind::String => {
                    if var_type_of(rhs, visitor, expected_type.clone()) != var_type {
                        visitor.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                var_type_of(lhs, visitor, expected_type.clone()),
                                var_type_of(rhs, visitor, expected_type)
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        );
                    } else if *op == BinOp::Eq {
                        VarType::new(VarTypeKind::Bool, true)
                    } else {
                        var_type
                    }
                }
                VarTypeKind::Ptr(inner) => {
                    let inner_lhs = inner.as_mut().clone();
                    let inner_rhs = match var_type_of(rhs, visitor, expected_type.clone()).kind {
                        VarTypeKind::Ptr(inner) => inner.as_ref().clone(),
                        _ => {
                            visitor.error(
                                format!("Expected pointer but found {rhs:?}"),
                                "".to_string(),
                                &rhs.span,
                            );
                        }
                    };

                    if !inner_lhs.loosy_eq(inner_rhs) {
                        visitor.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                var_type_of(lhs, visitor, expected_type.clone()),
                                var_type_of(rhs, visitor, expected_type)
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        );
                    } else {
                        if *op == BinOp::Eq {
                            VarType::new(VarTypeKind::Bool, true)
                        } else {
                            var_type
                        }
                    }
                }
                _ => {
                    visitor.error(
                        format!("Operator overloading not implemented yet"),
                        "".to_string(),
                        &ast.span,
                    );
                }
            }
        }
        AstKind::Block(block) => {
            if block.expected_return == VarType::new(VarTypeKind::Unknown, true) {
                block.expected_return = expected_type;
            }
            block.expected_return.clone()
        }
        AstKind::Function(_, _) => VarType::new(VarTypeKind::Void, true),
        AstKind::FunctionDecl(_, _) => VarType::new(VarTypeKind::Void, true),
        AstKind::StructDecl(_, _) => VarType::new(VarTypeKind::Void, true),
        AstKind::VarDecl(_, _, _) => VarType::new(VarTypeKind::Void, true),
        AstKind::Return(val) => var_type_of(val, visitor, expected_type),
        AstKind::Number(Number::Int(_)) => VarType::new(VarTypeKind::Int, true),
        AstKind::Number(Number::Float(_)) => VarType::new(VarTypeKind::Int, true),
        AstKind::String(_) => VarType::new(VarTypeKind::String, true),
        AstKind::Parameter(var_type, _) => var_type.clone(),
        AstKind::FunCall(path, _, _) => {
            let info = visitor
                .get_id(&visitor.get_access_id(path))
                .unwrap_or_else(|| {
                    visitor.error(
                        format!("{} not defined", crate::cpp_gen::to_c(path, visitor, None)),
                        "".to_string(),
                        &ast.span,
                    );
                });
            match &info.kind.kind {
                VarTypeKind::Function(f) => f.borrow().ret_type.clone(),
                _ => unreachable!(),
            }
        }
        AstKind::Items(_) => todo!(),
        AstKind::Reference(inner) => VarType::new(
            VarTypeKind::Ref(Box::new(var_type_of(inner, visitor, expected_type))),
            true,
        ),
        AstKind::Dereference(inner) => match var_type_of(inner, visitor, expected_type).kind {
            VarTypeKind::Ref(inner) => inner.as_ref().clone(),
            VarTypeKind::Ptr(inner) => inner.as_ref().clone(),
            t => {
                visitor.error(
                    format!("Trying to dereference non-pointer type {t}"),
                    "".to_string(),
                    &ast.span,
                );
            }
        },
        AstKind::Pointer(inner) => VarType::new(
            VarTypeKind::Ptr(Box::new(var_type_of(inner, visitor, expected_type))),
            true,
        ),
        AstKind::If(if_stmt) => {
            let mut kind: Option<VarType> = None;
            let mut current_if = if_stmt;
            loop {
                check_if_to_be(&mut current_if.block, visitor, &mut kind);
                match &mut current_if.else_if {
                    ElseBlock::Nothing => break,
                    ElseBlock::ElseIf(stmt) => {
                        if let AstKind::If(new_if) = &mut stmt.kind {
                            current_if = new_if;
                        } else {
                            unreachable!()
                        }
                    }
                    ElseBlock::Else(block) => {
                        check_if_to_be(block, visitor, &mut kind);
                        break;
                    }
                }
            }
            kind.unwrap_or(VarType::new(VarTypeKind::Void, true))
        }
        AstKind::Null => VarType::new(
            VarTypeKind::Ptr(Box::new(VarType::new(VarTypeKind::Any, true))),
            true,
        ),
        AstKind::ArrayLit(elements) => {
            if elements.is_empty() {
                return VarType::new(
                    VarTypeKind::Array(Array {
                        var_type: Box::new(VarType::new(VarTypeKind::Any, true)),
                        size: ArraySize::Fixed(0),
                    }),
                    true,
                );
            }

            let var_type = var_type_of(
                elements.first_mut().unwrap(),
                visitor,
                expected_type.clone(),
            );
            for element in elements.iter_mut().skip(1) {
                let element_type = var_type_of(element, visitor, expected_type.clone());
                if !element_type.clone().loosy_eq(var_type.clone()) {
                    visitor.error(
                        format!(
                            "Expected array of {var_type} but found element of type {element_type}"
                        ),
                        "".to_string(),
                        &element.span,
                    );
                }
            }
            VarType::new(
                VarTypeKind::Array(Array {
                    var_type: Box::new(var_type),
                    size: ArraySize::Fixed(elements.len() as i64),
                }),
                true,
            )
        }
        AstKind::BinOpEq(_, _, _) => VarType::new(VarTypeKind::Void, true),
        AstKind::Assign(_, _) => VarType::new(VarTypeKind::Void, true),
        AstKind::ExprRet(inner) => var_type_of(inner, visitor, expected_type),
        AstKind::Field(field) => field.kind.clone(),
        AstKind::Bool(_) => VarType::new(VarTypeKind::Bool, true),
        AstKind::StructLit(lit) => visitor.get_id(&lit.id).unwrap().kind.clone(),
        AstKind::Access(_, _, _) => {
            let id = visitor.get_access_id(ast);
            visitor.get_id(&id).unwrap().kind.clone()
        }
    }
}

fn type_check(ast: &mut AstNode, visitor: &LexVisitor, expected_type: VarType) {
    match &mut ast.kind {
        AstKind::Items(items) => {
            for item in items {
                type_check(item, visitor, VarType::new(VarTypeKind::Void, true));
            }
        }
        AstKind::Function(_id, func) => match func.borrow().block.clone().as_mut() {
            Some(b) => match &mut b.kind {
                AstKind::Block(_) => type_check(b, visitor, func.borrow().ret_type.clone()),
                _ => unreachable!(),
            },
            None => {}
        },
        AstKind::Block(block) => {
            for stmt in &mut block.stmts {
                type_check(stmt, visitor, block.expected_return.clone());
            }
        }
        AstKind::If(if_stmt) => {
            let conditional_type =
                var_type_of(&mut if_stmt.conditional, visitor, expected_type.clone());
            if !conditional_type
                .clone()
                .loosy_eq(VarType::new(VarTypeKind::Bool, true))
            {
                visitor.error(
                format!("Expected expression of type bool in if statement but found one of type {conditional_type}"),
                "".to_string(),
                &ast.span,
            )
            }
            type_check(&mut if_stmt.block, visitor, expected_type.clone());
            match &mut if_stmt.else_if {
                ElseBlock::Nothing => {}
                ElseBlock::ElseIf(stmt) => type_check(stmt, visitor, expected_type),
                ElseBlock::Else(stmt) => type_check(stmt, visitor, expected_type),
            }
        }
        AstKind::VarDecl(var_type, _id, val) => {
            match val {
                AssignVal::Ast(node) => type_check(node, visitor, var_type.clone()),
                AssignVal::Value(_, _) => {}
            }

            match &mut var_type.kind {
                VarTypeKind::Unknown => {
                    todo!()
                }
                t => {
                    let expr_t = match val {
                        AssignVal::Ast(node) => var_type_of(node, visitor, expected_type),
                        AssignVal::Value(value, _) => value.kind(visitor),
                    };
                    if !t.clone().loosy_eq(expr_t.kind.clone()) {
                        visitor.error(
                            format!(
                                "Expected expression of type {t} but found one of type {expr_t}"
                            ),
                            "".to_string(),
                            &ast.span,
                        )
                    }
                }
            }
        }
        AstKind::Return(val) => {
            let expr_t = var_type_of(val, visitor, expected_type.clone());
            if expected_type != expr_t {
                visitor.error(
                format!("Expected expression of type {expected_type} but found one of type {expr_t}"),
                "".to_string(),
                &ast.span,
            )
            }
        }
        AstKind::FunCall(path, params, is_macro) => {
            let path_id = visitor.get_id(&visitor.get_access_id(path));
            match path_id {
                Some(info) => match &info.kind.kind {
                    VarTypeKind::Function(func) => {
                        if func.borrow().is_macro != *is_macro {
                            visitor.error(
                                format!(
                                    "'{}' is a macro but but called without a '#' before it",
                                    &info.name
                                ),
                                "Add a '#' before the call".to_string(),
                                &ast.span,
                            )
                        }
                        if params.len() != func.borrow().params.len() {
                            visitor.error(
                                format!(
                                    "Expected {} but found {} parameters on function call {}",
                                    func.borrow().params.len(),
                                    params.len(),
                                    &info.name
                                ),
                                "".to_string(),
                                &ast.span,
                            )
                        }
                        for (i, param) in params.iter_mut().enumerate() {
                            let found = var_type_of(param, visitor, expected_type.clone());
                            let expected = var_type_of(
                                &mut func.borrow().params.clone()[i],
                                visitor,
                                expected_type.clone(),
                            );
                            if !found.clone().loosy_eq(expected.clone()) {
                                visitor.error(
                                format!("Parameter {i} has expected type of {expected} but found {found}"),
                                "".to_string(),
                                &ast.span,
                            )
                            }
                        }
                    }
                    _ => todo!(),
                },
                None => todo!(),
            }
        }
        AstKind::BinOpEq(path, op, val) => {
            let path_id = visitor.get_id(&visitor.get_access_id(path));
            let var_type = match path_id {
                Some(info) => info.kind.clone(),
                None => todo!(),
            };

            match &var_type.kind {
                VarTypeKind::Int | VarTypeKind::Float | VarTypeKind::String => {
                    if var_type_of(val, visitor, expected_type.clone()) != var_type {
                        visitor.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                var_type,
                                var_type_of(val, visitor, expected_type)
                            ),
                            "Add a 'xx' before the val to cast it".to_string(),
                            &ast.span,
                        );
                    }
                }
                VarTypeKind::Ptr(inner) => {
                    let inner_lhs = inner.as_ref().clone();
                    let inner_val = match var_type_of(val, visitor, expected_type.clone()).kind {
                        VarTypeKind::Ptr(inner) => inner.as_ref().clone(),
                        _ => {
                            visitor.error(
                                format!("Expected pointer but found {val:?}"),
                                "".to_string(),
                                &val.span,
                            );
                        }
                    };

                    if !inner_lhs.loosy_eq(inner_val) {
                        visitor.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                var_type,
                                var_type_of(val, visitor, expected_type)
                            ),
                            "Add a 'xx' before the val to cast it".to_string(),
                            &ast.span,
                        );
                    }
                }
                _ => {
                    visitor.error(
                        format!("Operator overloading not implemented yet"),
                        "".to_string(),
                        &ast.span,
                    );
                }
            }
        }
        AstKind::Assign(path, val) => {
            let path_id = visitor.get_id(&visitor.get_access_id(path));
            let var_type = match path_id {
                Some(info) => info.kind.clone(),
                None => todo!(),
            };
            if !var_type
                .clone()
                .loosy_eq(var_type_of(val, visitor, expected_type.clone()))
            {
                visitor.error(
                    format!(
                        "Could not assign value of type {} to variable '{}' of type {}",
                        var_type_of(val, visitor, expected_type),
                        visitor.get_id(&visitor.get_access_id(path)).unwrap().name,
                        var_type,
                    ),
                    "".to_string(),
                    &ast.span,
                )
            }
        }
        AstKind::StructLit(lit) => {
            let st = visitor.get_id(&lit.id).unwrap();
            match &st.kind.kind {
                VarTypeKind::Struct(s) => {
                    for (i, field_ast) in s.fields.iter().enumerate() {
                        if let AstKind::Field(struct_field) = &field_ast.kind {
                            let lit_field = lit.fields.get_mut(i).unwrap_or_else(|| {
                                visitor.error(
                                    format!("Struct literal missing field '{}'", struct_field.name),
                                    "".to_string(),
                                    &ast.span,
                                )
                            });
                            let lit_kind =
                                var_type_of(&mut lit_field.1, visitor, struct_field.kind.clone());
                            if lit_field.0 != struct_field.name {
                                visitor.error(
                                    format!(
                                        "Expected field '{}' but found '{}'",
                                        struct_field.name, lit_field.0
                                    ),
                                    "".to_string(),
                                    &lit_field.2,
                                );
                            }
                            if !struct_field.kind.clone().loosy_eq(lit_kind.clone()) {
                                visitor.error(
                                    format!(
                                        "Field '{}' has expected type '{}' but found type '{}'",
                                        lit_field.0, struct_field.kind, lit_kind,
                                    ),
                                    "".to_string(),
                                    &field_ast.span,
                                );
                            }
                        } else {
                            unreachable!()
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        AstKind::Ident(_) => {}
        AstKind::BinOp(lhs, _, rhs) => {
            type_check(lhs, visitor, expected_type.clone());
            type_check(rhs, visitor, expected_type);
        }
        AstKind::Field(_) => {}
        AstKind::FunctionDecl(_, _) => {}
        AstKind::StructDecl(_, _) => {}
        AstKind::ExprRet(node) => type_check(node, visitor, expected_type),
        AstKind::Number(_) => {}
        AstKind::String(_) => {}
        AstKind::Parameter(_, _) => {}
        AstKind::Reference(node) => type_check(node, visitor, expected_type),
        AstKind::Dereference(node) => type_check(node, visitor, expected_type),
        AstKind::Pointer(node) => type_check(node, visitor, expected_type),
        AstKind::ArrayLit(elements) => {
            for i in elements {
                type_check(i, visitor, expected_type.clone());
            }
        }
        AstKind::Access(node, _, _) => type_check(node, visitor, expected_type),
        AstKind::Null => {}
        AstKind::Bool(_) => {}
    }
}

fn const_check(ast: &mut AstNode, visitor: &mut LexVisitor) {
    match &mut ast.kind {
        AstKind::Items(items) => {
            for item in items {
                const_check(item, visitor);
            }
        }
        AstKind::Function(_id, func) => {
            if let Some(b) = &mut func.borrow().block.clone() {
                const_check(b.as_mut(), visitor)
            }
        }
        AstKind::Block(block) => {
            for stmt in &mut block.stmts {
                const_check(stmt, visitor);
            }
        }
        AstKind::If(if_stmt) => {
            const_check(&mut if_stmt.block, visitor);
            match &mut if_stmt.else_if {
                ElseBlock::Nothing => {}
                ElseBlock::ElseIf(stmt) => const_check(stmt, visitor),
                ElseBlock::Else(stmt) => const_check(stmt, visitor),
            }
        }
        AstKind::VarDecl(_, _, val) => match val {
            AssignVal::Ast(node) => const_check(node, visitor),
            AssignVal::Value(_, _) => {}
        },
        AstKind::Return(val) => const_check(val, visitor),
        AstKind::FunCall(_, params, _) => {
            for param in params {
                const_check(param, visitor);
            }
        }
        AstKind::BinOpEq(path, _, _) => {
            let var = visitor.get_id(&visitor.get_access_id(path)).unwrap();
            if var.immutable {
                visitor.error(
                    format!("Could not assign value to constant variable '{}'", var.name),
                    "Consider changing this to not be constant".to_string(),
                    &ast.span,
                )
            }
        }
        AstKind::Assign(path, _) => {
            let var = visitor.get_id(&visitor.get_access_id(path)).unwrap();
            if var.immutable {
                visitor.error(
                    format!("Could not assign value to constant variable '{}'", var.name),
                    "Consider removing constant specifier from this".to_string(),
                    &ast.span,
                )
            }
        }
        _ => (),
    }
}

pub fn semantic_analyse(ast: &mut AstNode, visitor: &mut LexVisitor) {
    type_check(ast, visitor, VarType::new(VarTypeKind::Void, true));
    const_check(ast, visitor);
}
