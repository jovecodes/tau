mod error;
mod lex;
mod parse;
mod pos;

use error::Log;
use lex::BinOp;
use parse::{Array, AstKind, AstNode, ElseBlock, LexVisiter, Number, Path, VarType};

fn var_type_to_c(var_type: &VarType, visiter: &LexVisiter) -> String {
    match var_type {
        VarType::Unknown => "unknown".to_owned(),
        VarType::Any => "void".to_string(),
        VarType::Void => "void".to_owned(),
        VarType::Int => "int".to_owned(),
        VarType::Float => "float".to_owned(),
        VarType::String => "String".to_owned(),
        VarType::Bool => "bool".to_owned(),
        VarType::Function(_) => todo!(),
        VarType::Struct(st) => st.name.clone(),
        VarType::Ref(inner) => var_type_to_c(inner, visiter) + "*",
        VarType::Deref(inner) => var_type_to_c(inner, visiter) + "*",
        VarType::Ptr(inner) => var_type_to_c(inner, visiter) + "*",
        VarType::Array(_) => todo!(),
    }
}

fn path_to_c(path: &Path, visiter: &LexVisiter) -> String {
    let mut output = String::new();
    if !path.segments.is_empty() {
        for seg in path.segments[..path.segments.len() - 1].iter() {
            output += &visiter.get_id(&seg.id).unwrap().name;
            output += "_";
        }
        if let Some(last) = path.segments.last() {
            output += &visiter.get_id(&last.id).unwrap().name;
        }
    }
    output
}

fn to_c(node: &AstNode, visiter: &LexVisiter) -> String {
    match &node.kind {
        parse::AstKind::Ident(id) => visiter.get_id(&id).unwrap().name.clone(),
        parse::AstKind::BinOp(lhs, op, rhs) => {
            let mut output = String::new();
            output += &to_c(&lhs, visiter);
            output += &format!(" {} ", op.to_string());
            output += &to_c(&rhs, visiter);
            output
        }
        parse::AstKind::Block(block) => {
            let mut output = String::new();
            output += "{\n";
            for stmt in &block.stmts {
                output += to_c(stmt, visiter).as_str();
                output += ";\n";
            }
            output += "}\n";
            output
        }
        parse::AstKind::Function(id, func) => {
            let mut output = String::new();
            output += &var_type_to_c(&func.ret_type, visiter);
            output += " ";
            output += &visiter.get_id(id).unwrap().name;

            output.push('(');
            if !func.params.is_empty() {
                for param in func.params[..func.params.len() - 1].iter() {
                    output += to_c(param, visiter).as_str();
                    output += ", ";
                }
                if let Some(last) = func.params.last() {
                    output += to_c(last, visiter).as_str();
                }
            }
            output.push(')');

            output.push(' ');
            output += to_c(func.block.as_ref().unwrap(), visiter).as_str();

            output
        }
        parse::AstKind::FunctionDecl(id, func) => {
            let mut output = String::new();
            output += &var_type_to_c(&func.ret_type, visiter);
            output += " ";
            output += &visiter.get_id(id).unwrap().name;

            output.push('(');
            if !func.params.is_empty() {
                for param in func.params[..func.params.len() - 1].iter() {
                    output += to_c(param, visiter).as_str();
                    output += ", ";
                }
                if let Some(last) = func.params.last() {
                    output += to_c(last, visiter).as_str();
                }
            }
            output += ");\n";

            output
        }
        parse::AstKind::VarDecl(t, id, expr) => {
            let mut output = String::new();
            let var = visiter.get_id(id).unwrap();
            if var.constant {
                output += "const ";
            }
            output += &var_type_to_c(t, visiter);
            output += " ";
            output += &var.name;
            output += " = ";
            output += &to_c(expr, visiter);
            output
        }
        parse::AstKind::Return(val) => "return ".to_string() + &to_c(val, visiter),
        parse::AstKind::Number(parse::Number::Int(n)) => n.to_string(),
        parse::AstKind::Number(parse::Number::Float(n)) => n.to_string(),
        parse::AstKind::String(s) => "\"".to_string() + &s + "\"",
        parse::AstKind::Parameter(kind, id) => {
            var_type_to_c(kind, visiter) + " " + &visiter.get_id(id).unwrap().name
        }
        parse::AstKind::FunCall(path, params) => {
            let mut output = String::new();
            output += &to_c(path, visiter);
            output += "(";
            if !params.is_empty() {
                for param in params[..params.len() - 1].iter() {
                    output += to_c(param, visiter).as_str();
                    output += ", ";
                }
                if let Some(last) = params.last() {
                    output += to_c(last, visiter).as_str();
                }
            }
            output += ")";

            output
        }
        parse::AstKind::Items(items) => {
            let mut output = String::new();
            for item in items {
                output += to_c(item, visiter).as_str();
            }
            output
        }
        parse::AstKind::Reference(expr) => "&".to_string() + &to_c(expr, visiter),
        parse::AstKind::Dereference(expr) => "*".to_string() + &to_c(expr, visiter),
        parse::AstKind::Pointer(expr) => "&".to_string() + &to_c(expr, visiter),
        AstKind::If(if_stmt) => {
            let mut output = String::new();
            output += "if (";
            output += &to_c(&if_stmt.conditional, visiter);
            output += ") ";
            output += &to_c(&if_stmt.block, visiter);
            match &if_stmt.else_if {
                ElseBlock::Nothing => (),
                ElseBlock::ElseIf(stmt) => {
                    output += "else ";
                    output += &to_c(&stmt, visiter);
                }
                ElseBlock::Else(block) => {
                    output += "else ";
                    output += &to_c(&block, visiter);
                }
            }
            output
        }
        AstKind::Null => "NULL".to_string(),
        AstKind::ArrayLit(elements) => {
            let mut output = String::new();
            output += "{";
            for element in elements {
                output += &to_c(&element, visiter);
                output += ",";
            }
            output += "}";
            output
        }
        AstKind::BinOpEq(path, op, val) => {
            let mut output = String::new();
            output += &to_c(path, visiter);
            output += &format!(" {}= ", op.to_string());
            output += &to_c(val, visiter);
            output
        }
        AstKind::Assign(path, val) => {
            let mut output = String::new();
            output += &to_c(path, visiter);
            output += " = ";
            output += &to_c(val, visiter);
            output
        }
        // AstKind::InlineBlock(block) => {
        //     let mut output = String::new();
        //     output += "{ return (";
        //     output += &to_c(&block.expr, visiter);
        //     output += "); }\n";
        //     output
        // }
        AstKind::ExprRet(inner) => {
            let mut output = String::new();
            output += "=> ";
            output += &to_c(inner, visiter);
            output
        }
        AstKind::StructDecl(id, s) => {
            let mut output = String::new();
            let name = &visiter.get_id(&id).unwrap().name;
            output += "typedef struct ";
            output += name;
            output += " {\n";
            for field in &s.fields {
                output += &to_c(&field, visiter);
            }
            output += "} ";
            output += name;
            output += ";\n";

            output
        }
        AstKind::Field(field) => {
            let mut output = String::new();
            output += &var_type_to_c(&field.kind, visiter);
            output += " ";
            output += &field.name;
            output += ";\n";

            output
        }
        AstKind::Bool(b) => b.to_string(),
        AstKind::StructLit(lit) => {
            let mut output = String::new();

            output += "(";
            output += &var_type_to_c(&visiter.get_id(&lit.id).unwrap().kind, visiter);
            output += ") ";

            output += "{";
            for (name, val) in &lit.fields {
                output += ".";
                output += &name;
                output += " = ";
                output += &to_c(val, visiter);
                output += ",\n";
            }
            output += "};\n";

            output
        }
        AstKind::Access(lhs, ident, deref) => {
            if *deref {
                to_c(lhs, visiter) + "->" + &ident
            } else {
                to_c(lhs, visiter) + "." + &ident
            }
        }
    }
}

fn check_if_to_be(ast: &mut AstNode, visiter: &LexVisiter, kind: &mut Option<VarType>) {
    match &mut ast.kind {
        AstKind::Block(block) => {
            for stmt in &mut block.stmts {
                match &mut stmt.kind {
                    AstKind::ExprRet(ret) => {
                        let new = var_type_of(ret, visiter, kind.clone().unwrap_or(VarType::Void));
                        if let Some(old) = kind {
                            if !old.clone().loosy_eq(new.clone()) {
                                visiter.error(format!("Expected if statement to have return type of {old:?} but this has a type of {new:?}"), "".to_string(), &ret.span)
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

fn var_type_of(ast: &mut AstNode, visiter: &LexVisiter, expected_type: VarType) -> VarType {
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
                                "Can not {} differing types of {:?} and {:?} without a cast",
                                op.to_string(),
                                var_type_of(lhs, visiter, expected_type.clone()),
                                var_type_of(rhs, visiter, expected_type)
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        );
                        unreachable!()
                    } else {
                        if *op == BinOp::Eq {
                            VarType::Bool
                        } else {
                            var_type
                        }
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
                            unreachable!()
                        }
                    };

                    if !inner_lhs.loosy_eq(inner_rhs) {
                        visiter.error(
                            format!(
                                "Can not {} differing types of {:?} and {:?} without a cast",
                                op.to_string(),
                                var_type_of(lhs, visiter, expected_type.clone()),
                                var_type_of(rhs, visiter, expected_type)
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        );
                        unreachable!()
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
                    unreachable!()
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
        AstKind::FunCall(path, _) => visiter
            .get_id(&visiter.get_access_id(path))
            .unwrap_or_else(|| {
                visiter.error(
                    format!("{} not defined", to_c(path, visiter)),
                    "".to_string(),
                    &ast.span,
                );
                unreachable!()
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
                    format!("Trying to dereference non-pointer type {t:?}"),
                    "".to_string(),
                    &ast.span,
                );
                unreachable!()
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
                    size: parse::ArraySize::FixedKnown(0),
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
                        format!("Expected array of {var_type:?} but found element of type {element_type:?}"),
                        "".to_string(),
                        &element.span,
                    );
                    unreachable!()
                }
            }
            VarType::Array(Array {
                var_type: Box::new(var_type),
                size: parse::ArraySize::FixedKnown(elements.len() as i64),
            })
        }
        AstKind::BinOpEq(_, _, _) => VarType::Void,
        AstKind::Assign(_, _) => VarType::Void,
        AstKind::ExprRet(inner) => var_type_of(inner, visiter, expected_type),
        AstKind::Field(field) => field.kind.clone(),
        AstKind::Bool(_) => VarType::Bool,
        AstKind::StructLit(lit) => visiter.get_id(&lit.id).unwrap().kind.clone(),
        AstKind::Access(_, _, _) => todo!(),
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
                    format!("Expected expression of type bool in if statement but found one of type {conditional_type:?}"),
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
            match var_type {
                VarType::Unknown => {
                    todo!()
                }
                t => {
                    let expr_t = var_type_of(val, visiter, expected_type);
                    if !t.clone().loosy_eq(expr_t.clone()) {
                        visiter.error(
                            format!("Expected expression of type {t:?} but found one of type {expr_t:?}"),
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
                    format!("Expected expression of type {expected_type:?} but found one of type {expr_t:?}"),
                    "".to_string(),
                    &ast.span,
                )
            }
        }
        AstKind::FunCall(path, params) => {
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
                                    format!("Parameter {i} has expected type of {expected:?} but found {found:?}"),
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
                                "Can not {} differing types of {:?} and {:?} without a cast",
                                op.to_string(),
                                var_type,
                                var_type_of(val, visiter, expected_type)
                            ),
                            "Add a 'xx' before the val to cast it".to_string(),
                            &ast.span,
                        );
                        unreachable!()
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
                            unreachable!()
                        }
                    };

                    if !inner_lhs.loosy_eq(inner_val) {
                        visiter.error(
                            format!(
                                "Can not {} differing types of {:?} and {:?} without a cast",
                                op.to_string(),
                                var_type,
                                var_type_of(val, visiter, expected_type)
                            ),
                            "Add a 'xx' before the val to cast it".to_string(),
                            &ast.span,
                        );
                        unreachable!()
                    }
                }
                _ => {
                    visiter.error(
                        format!("Operator overloading not implemented yet"),
                        "".to_string(),
                        &ast.span,
                    );
                    unreachable!()
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
                        "Could not assign value of type {:?} to variable '{}' of type {:?}",
                        var_type_of(val, visiter, expected_type),
                        visiter.get_id(&visiter.get_access_id(path)).unwrap().name,
                        var_type,
                    ),
                    "".to_string(),
                    &ast.span,
                )
            }
        }
        _ => (),
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
        AstKind::FunCall(_, params) => {
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

fn semantic_analyse(ast: &mut AstNode, visiter: &mut LexVisiter) {
    type_check(ast, visiter, VarType::Void);
    const_check(ast, visiter);
}

fn main() {
    // std::env::set_var("RUST_BACKTRACE", "1");

    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("fatal error: no input files");
    }

    let lex = lex::lex_file(args[1].clone());
    let mut visiter = parse::LexVisiter::new(&lex);

    if let Ok(mut ast) = dbg!(parse::parse_items(&mut visiter)) {
        semantic_analyse(&mut ast, &mut visiter);

        let mut output = String::new();
        output += "#include <stdio.h>\n";
        output += "#include <stdlib.h>\n";
        output += "#include <stdbool.h>\n";
        output += to_c(&ast, &visiter).as_str();
        println!("{}", output);
    } else {
        todo!()
    }
}
