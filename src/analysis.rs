use crate::error::Log;
use crate::lex::BinOp;
use crate::parse::{Array, ArraySize, AstKind, AstNode, ElseBlock, LexVisiter, Number, VarType};

fn check_if_to_be(ast: &mut AstNode, visiter: &LexVisiter, kind: &mut Option<VarType>) {
    match &mut ast.kind {
        AstKind::Block(block) => {
            for stmt in &mut block.stmts {
                match &mut stmt.kind {
                    AstKind::ExprRet(ret) => {
                        let new = var_type_of(ret, visiter, kind.clone().unwrap_or(VarType::Void));
                        if let Some(old) = kind {
                            if !old.clone().loosy_eq(new.clone()) {
                                visiter.error(
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
                    *kind = Some(VarType::Void);
                }
            }
        }
        _ => unreachable!(),
    }
}

pub fn var_type_of(ast: &mut AstNode, visiter: &LexVisiter, expected_type: VarType) -> VarType {
    match &mut ast.kind {
        AstKind::Ident(id) => {
            let path_id = visiter.get_id(&id);
            match path_id {
                Some(info) => info.kind.clone(),
                None => todo!(),
            }
        }
        AstKind::BinOp(lhs, op, rhs) => {
            let mut var_type = var_type_of(lhs, visiter, expected_type.clone());
            match &mut var_type {
                VarType::Int | VarType::Float | VarType::String => {
                    if var_type_of(rhs, visiter, expected_type.clone()) != var_type {
                        visiter.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                var_type_of(lhs, visiter, expected_type.clone()),
                                var_type_of(rhs, visiter, expected_type)
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        );
                    } else if *op == BinOp::Eq {
                        VarType::Bool
                    } else {
                        var_type
                    }
                }
                VarType::Ptr(inner) => {
                    let inner_lhs = inner.as_mut().clone();
                    let inner_rhs = match var_type_of(rhs, visiter, expected_type.clone()) {
                        VarType::Ptr(inner) => inner.as_ref().clone(),
                        _ => {
                            visiter.error(
                                format!("Expected pointer but found {rhs:?}"),
                                "".to_string(),
                                &rhs.span,
                            );
                        }
                    };

                    if !inner_lhs.loosy_eq(inner_rhs) {
                        visiter.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                var_type_of(lhs, visiter, expected_type.clone()),
                                var_type_of(rhs, visiter, expected_type)
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        );
                    } else {
                        if *op == BinOp::Eq {
                            VarType::Bool
                        } else {
                            var_type
                        }
                    }
                }
                _ => {
                    visiter.error(
                        format!("Operator overloading not implemented yet"),
                        "".to_string(),
                        &ast.span,
                    );
                }
            }
        }
        AstKind::Block(block) => {
            if block.expected_return == VarType::Unknown {
                block.expected_return = expected_type;
            }
            block.expected_return.clone()
        }
        AstKind::Function(_, _) => VarType::Void,
        AstKind::FunctionDecl(_, _) => VarType::Void,
        AstKind::StructDecl(_, _) => VarType::Void,
        AstKind::VarDecl(_, _, _) => VarType::Void,
        AstKind::Return(val) => var_type_of(val, visiter, expected_type),
        AstKind::Number(Number::Int(_)) => VarType::Int,
        AstKind::Number(Number::Float(_)) => VarType::Float,
        AstKind::String(_) => VarType::String,
        AstKind::Parameter(var_type, _) => var_type.clone(),
        AstKind::FunCall(path, _, _) => visiter
            .get_id(&visiter.get_access_id(path))
            .unwrap_or_else(|| {
                visiter.error(
                    format!("{} not defined", crate::cpp_gen::to_c(path, visiter, None)),
                    "".to_string(),
                    &ast.span,
                );
            })
            .kind
            .clone(),
        AstKind::Items(_) => todo!(),
        AstKind::Reference(inner) => {
            VarType::Ref(Box::new(var_type_of(inner, visiter, expected_type)))
        }
        AstKind::Dereference(inner) => match var_type_of(inner, visiter, expected_type) {
            VarType::Ref(inner) => inner.as_ref().clone(),
            VarType::Ptr(inner) => inner.as_ref().clone(),
            t => {
                visiter.error(
                    format!("Trying to dereference non-pointer type {t}"),
                    "".to_string(),
                    &ast.span,
                );
            }
        },
        AstKind::Pointer(inner) => {
            VarType::Ptr(Box::new(var_type_of(inner, visiter, expected_type)))
        }
        AstKind::If(if_stmt) => {
            let mut kind: Option<VarType> = None;
            let mut current_if = if_stmt;
            loop {
                check_if_to_be(&mut current_if.block, visiter, &mut kind);
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
                        check_if_to_be(block, visiter, &mut kind);
                        break;
                    }
                }
            }
            kind.unwrap_or(VarType::Void)
        }
        AstKind::Null => VarType::Ptr(Box::new(VarType::Any)),
        AstKind::ArrayLit(elements) => {
            if elements.is_empty() {
                return VarType::Array(Array {
                    var_type: Box::new(VarType::Any),
                    size: ArraySize::FixedKnown(0),
                });
            }

            let var_type = var_type_of(
                elements.first_mut().unwrap(),
                visiter,
                expected_type.clone(),
            );
            for element in elements.iter_mut().skip(1) {
                let element_type = var_type_of(element, visiter, expected_type.clone());
                if !element_type.clone().loosy_eq(var_type.clone()) {
                    visiter.error(
                        format!(
                            "Expected array of {var_type} but found element of type {element_type}"
                        ),
                        "".to_string(),
                        &element.span,
                    );
                }
            }
            VarType::Array(Array {
                var_type: Box::new(var_type),
                size: ArraySize::FixedKnown(elements.len() as i64),
            })
        }
        AstKind::BinOpEq(_, _, _) => VarType::Void,
        AstKind::Assign(_, _) => VarType::Void,
        AstKind::ExprRet(inner) => var_type_of(inner, visiter, expected_type),
        AstKind::Field(field) => field.kind.clone(),
        AstKind::Bool(_) => VarType::Bool,
        AstKind::StructLit(lit) => visiter.get_id(&lit.id).unwrap().kind.clone(),
        AstKind::Access(_, _, _) => {
            let id = visiter.get_access_id(ast);
            visiter.get_id(&id).unwrap().kind.clone()
        }
    }
}

fn type_check(ast: &mut AstNode, visiter: &LexVisiter, expected_type: VarType) {
    match &mut ast.kind {
        AstKind::Items(items) => {
            for item in items {
                type_check(item, visiter, VarType::Void);
            }
        }
        AstKind::Function(_id, func) => match func.block.clone().as_mut() {
            Some(b) => match &mut b.kind {
                AstKind::Block(_) => type_check(b, visiter, func.ret_type.clone()),
                _ => unreachable!(),
            },
            None => {}
        },
        AstKind::Block(block) => {
            for stmt in &mut block.stmts {
                type_check(stmt, visiter, block.expected_return.clone());
            }
        }
        AstKind::If(if_stmt) => {
            let conditional_type =
                var_type_of(&mut if_stmt.conditional, visiter, expected_type.clone());
            if conditional_type != VarType::Bool {
                visiter.error(
                format!("Expected expression of type bool in if statement but found one of type {conditional_type}"),
                "".to_string(),
                &ast.span,
            )
            }
            type_check(&mut if_stmt.block, visiter, expected_type.clone());
            match &mut if_stmt.else_if {
                ElseBlock::Nothing => {}
                ElseBlock::ElseIf(stmt) => type_check(stmt, visiter, expected_type),
                ElseBlock::Else(stmt) => type_check(stmt, visiter, expected_type),
            }
        }
        AstKind::VarDecl(var_type, _id, val) => {
            type_check(val, visiter, var_type.clone());
            match var_type {
                VarType::Unknown => {
                    todo!()
                }
                t => {
                    let expr_t = var_type_of(val, visiter, expected_type);
                    if !t.clone().loosy_eq(expr_t.clone()) {
                        visiter.error(
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
            let expr_t = var_type_of(val, visiter, expected_type.clone());
            if expected_type != expr_t {
                visiter.error(
                format!("Expected expression of type {expected_type} but found one of type {expr_t}"),
                "".to_string(),
                &ast.span,
            )
            }
        }
        AstKind::FunCall(path, params, _) => {
            let path_id = visiter.get_id(&visiter.get_access_id(path));
            match path_id {
                Some(info) => match &info.kind {
                    VarType::Function(func) => {
                        if params.len() != func.params.len() {
                            visiter.error(
                                format!(
                                    "Expected {} but found {} parameters on function call {}",
                                    func.params.len(),
                                    params.len(),
                                    &info.name
                                ),
                                "".to_string(),
                                &ast.span,
                            )
                        }
                        for (i, param) in params.iter_mut().enumerate() {
                            let found = var_type_of(param, visiter, expected_type.clone());
                            let expected = var_type_of(
                                &mut func.params.clone()[i],
                                visiter,
                                expected_type.clone(),
                            );
                            if found != expected {
                                visiter.error(
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
            let path_id = visiter.get_id(&visiter.get_access_id(path));
            let var_type = match path_id {
                Some(info) => info.kind.clone(),
                None => todo!(),
            };

            match &var_type {
                VarType::Int | VarType::Float | VarType::String => {
                    if var_type_of(val, visiter, expected_type.clone()) != var_type {
                        visiter.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                var_type,
                                var_type_of(val, visiter, expected_type)
                            ),
                            "Add a 'xx' before the val to cast it".to_string(),
                            &ast.span,
                        );
                    }
                }
                VarType::Ptr(inner) => {
                    let inner_lhs = inner.as_ref().clone();
                    let inner_val = match var_type_of(val, visiter, expected_type.clone()) {
                        VarType::Ptr(inner) => inner.as_ref().clone(),
                        _ => {
                            visiter.error(
                                format!("Expected pointer but found {val:?}"),
                                "".to_string(),
                                &val.span,
                            );
                        }
                    };

                    if !inner_lhs.loosy_eq(inner_val) {
                        visiter.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                var_type,
                                var_type_of(val, visiter, expected_type)
                            ),
                            "Add a 'xx' before the val to cast it".to_string(),
                            &ast.span,
                        );
                    }
                }
                _ => {
                    visiter.error(
                        format!("Operator overloading not implemented yet"),
                        "".to_string(),
                        &ast.span,
                    );
                }
            }
        }
        AstKind::Assign(path, val) => {
            let path_id = visiter.get_id(&visiter.get_access_id(path));
            let var_type = match path_id {
                Some(info) => info.kind.clone(),
                None => todo!(),
            };
            if !var_type
                .clone()
                .loosy_eq(var_type_of(val, visiter, expected_type.clone()))
            {
                visiter.error(
                    format!(
                        "Could not assign value of type {} to variable '{}' of type {}",
                        var_type_of(val, visiter, expected_type),
                        visiter.get_id(&visiter.get_access_id(path)).unwrap().name,
                        var_type,
                    ),
                    "".to_string(),
                    &ast.span,
                )
            }
        }
        AstKind::StructLit(lit) => {
            let st = visiter.get_id(&lit.id).unwrap();
            match &st.kind {
                VarType::Struct(s) => {
                    for (i, field_ast) in s.fields.iter().enumerate() {
                        if let AstKind::Field(struct_field) = &field_ast.kind {
                            let lit_field = lit.fields.get_mut(i).unwrap_or_else(|| {
                                visiter.error(
                                    format!("Struct literal missing field '{}'", struct_field.name),
                                    "".to_string(),
                                    &ast.span,
                                )
                            });
                            let lit_kind =
                                var_type_of(&mut lit_field.1, visiter, struct_field.kind.clone());
                            if lit_field.0 != struct_field.name {
                                visiter.error(
                                    format!(
                                        "Expected field '{}' but found '{}'",
                                        struct_field.name, lit_field.0
                                    ),
                                    "".to_string(),
                                    &lit_field.2,
                                );
                            }
                            if !struct_field.kind.clone().loosy_eq(lit_kind.clone()) {
                                visiter.error(
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
            type_check(lhs, visiter, expected_type.clone());
            type_check(rhs, visiter, expected_type);
        }
        AstKind::Field(_) => {}
        AstKind::FunctionDecl(_, _) => {}
        AstKind::StructDecl(_, _) => {}
        AstKind::ExprRet(node) => type_check(node, visiter, expected_type),
        AstKind::Number(_) => {}
        AstKind::String(_) => {}
        AstKind::Parameter(_, _) => {}
        AstKind::Reference(node) => type_check(node, visiter, expected_type),
        AstKind::Dereference(node) => type_check(node, visiter, expected_type),
        AstKind::Pointer(node) => type_check(node, visiter, expected_type),
        AstKind::ArrayLit(elements) => {
            for i in elements {
                type_check(i, visiter, expected_type.clone());
            }
        }
        AstKind::Access(node, _, _) => type_check(node, visiter, expected_type),
        AstKind::Null => {}
        AstKind::Bool(_) => {}
    }
}

fn const_check(ast: &mut AstNode, visiter: &mut LexVisiter) {
    match &mut ast.kind {
        AstKind::Items(items) => {
            for item in items {
                const_check(item, visiter);
            }
        }
        AstKind::Function(_id, func) => {
            if let Some(b) = &mut func.block.clone() {
                const_check(b.as_mut(), visiter)
            }
        }
        AstKind::Block(block) => {
            for stmt in &mut block.stmts {
                const_check(stmt, visiter);
            }
        }
        AstKind::If(if_stmt) => {
            const_check(&mut if_stmt.block, visiter);
            match &mut if_stmt.else_if {
                ElseBlock::Nothing => {}
                ElseBlock::ElseIf(stmt) => const_check(stmt, visiter),
                ElseBlock::Else(stmt) => const_check(stmt, visiter),
            }
        }
        AstKind::VarDecl(_, _, val) => const_check(val, visiter),
        AstKind::Return(val) => const_check(val, visiter),
        AstKind::FunCall(_, params, _) => {
            for param in params {
                const_check(param, visiter);
            }
        }
        AstKind::BinOpEq(path, _, _) => {
            let var = visiter.get_id(&visiter.get_access_id(path)).unwrap();
            if var.constant {
                visiter.error(
                    format!("Could not assign value to constant variable '{}'", var.name),
                    "Consider changing this to not be constant".to_string(),
                    &ast.span,
                )
            }
        }
        AstKind::Assign(path, _) => {
            let var = visiter.get_id(&visiter.get_access_id(path)).unwrap();
            if var.constant {
                visiter.error(
                    format!("Could not assign value to constant variable '{}'", var.name),
                    "Consider removing constant specifier from this".to_string(),
                    &ast.span,
                )
            }
        }
        _ => (),
    }
}

pub fn semantic_analyse(ast: &mut AstNode, visiter: &mut LexVisiter) {
    type_check(ast, visiter, VarType::Void);
    const_check(ast, visiter);
}
