mod error;
mod lex;
mod parse;
mod pos;

use error::Log;
use lex::BinOp;
use parse::{AstKind, AstNode, LexVisiter, Number, Path, VarType};

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
        VarType::User(_) => todo!(),
        VarType::Ref(inner) => var_type_to_c(inner, visiter) + "*",
        VarType::Deref(inner) => var_type_to_c(inner, visiter) + "*",
        VarType::Ptr(inner) => var_type_to_c(inner, visiter) + "*",
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
        parse::AstKind::Path(path) => path_to_c(path, visiter),
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
            var_type_to_c(t, visiter)
                + " "
                + &visiter.get_id(id).unwrap().name
                + " = "
                + &to_c(expr, visiter)
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
            output += &path_to_c(path, visiter);
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
                parse::ElseBlock::Nothing => (),
                parse::ElseBlock::ElseIf(stmt) => {
                    output += "else ";
                    output += &to_c(&stmt, visiter);
                }
                parse::ElseBlock::Else(block) => {
                    output += "else ";
                    output += &to_c(&block, visiter);
                }
            }
            output
        }
        AstKind::Null => "NULL".to_string(),
    }
}

fn var_type_of(ast: &AstNode, visiter: &LexVisiter) -> VarType {
    match &ast.kind {
        AstKind::Path(path) => {
            let path_id = visiter.get_id(&path.segments.last().unwrap().id);
            match path_id {
                Some(info) => info.kind.clone(),
                None => todo!(),
            }
        }
        AstKind::BinOp(lhs, op, rhs) => {
            let var_type = var_type_of(lhs, visiter);
            match &var_type {
                VarType::Int | VarType::Float | VarType::String => {
                    if var_type_of(rhs, visiter) != var_type {
                        visiter.error(
                            format!(
                                "Can not {} differing types of {:?} and {:?} without a cast",
                                op.to_string(),
                                var_type_of(lhs, visiter),
                                var_type_of(rhs, visiter)
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
                    let inner_lhs = inner.as_ref().clone();
                    dbg!(&inner_lhs);
                    let inner_rhs = match var_type_of(&rhs, visiter) {
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
                                var_type_of(lhs, visiter),
                                var_type_of(rhs, visiter)
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
        AstKind::Block(block) => block.expected_return.clone(),
        AstKind::Function(_, _) => VarType::Void,
        AstKind::FunctionDecl(_, _) => VarType::Void,
        AstKind::VarDecl(_, _, _) => VarType::Void,
        AstKind::Return(val) => var_type_of(val, visiter),
        AstKind::Number(Number::Int(_)) => VarType::Int,
        AstKind::Number(Number::Float(_)) => VarType::Float,
        AstKind::String(_) => VarType::String,
        AstKind::Parameter(var_type, _) => var_type.clone(),
        AstKind::FunCall(path, _) => visiter
            .get_id(&path.segments.last().unwrap().id)
            .unwrap_or_else(|| {
                visiter.error(
                    format!("{} not defined", path_to_c(path, visiter)),
                    "".to_string(),
                    &ast.span,
                );
                unreachable!()
            })
            .kind
            .clone(),
        AstKind::Items(_) => todo!(),
        AstKind::Reference(inner) => VarType::Ref(Box::new(var_type_of(inner, visiter))),
        AstKind::Dereference(inner) => match var_type_of(inner, visiter) {
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
        AstKind::Pointer(inner) => VarType::Ptr(Box::new(var_type_of(inner, visiter))),
        AstKind::If(_) => VarType::Void,
        AstKind::Null => VarType::Ptr(Box::new(VarType::Any)),
    }
}

fn type_check(ast: &AstNode, visiter: &mut LexVisiter, expected_type: VarType) {
    match &ast.kind {
        AstKind::Items(items) => {
            for item in items {
                type_check(item, visiter, VarType::Void);
            }
        }
        AstKind::Function(_id, func) => {
            if let Some(b) = &func.block {
                type_check(b.as_ref(), visiter, func.ret_type.clone())
            }
        }
        AstKind::Block(block) => {
            for stmt in &block.stmts {
                type_check(stmt, visiter, block.expected_return.clone());
            }
        }
        AstKind::If(if_stmt) => {
            let conditional_type = var_type_of(&if_stmt.conditional, visiter);
            if conditional_type != VarType::Bool {
                visiter.error(
                    format!("Expected expression of type bool in if statement but found one of type {conditional_type:?}"),
                    "".to_string(),
                    &ast.span,
                )
            }
        }
        AstKind::VarDecl(var_type, _id, val) => {
            match var_type {
                VarType::Unknown => {
                    todo!()
                }
                t => {
                    let expr_t = var_type_of(val, visiter);
                    if *t != expr_t {
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
            let expr_t = var_type_of(val, visiter);
            if expected_type != expr_t {
                visiter.error(
                    format!("Expected expression of type {expected_type:?} but found one of type {expr_t:?}"),
                    "".to_string(),
                    &ast.span,
                )
            }
        }
        AstKind::FunCall(path, params) => {
            let path_id = visiter.get_id(&path.segments.last().unwrap().id);
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
                        for (i, param) in params.iter().enumerate() {
                            let found = var_type_of(param, visiter);
                            let expected = var_type_of(&func.params[i], visiter);
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
        _ => (),
    }
}

fn main() {
    // std::env::set_var("RUST_BACKTRACE", "1");

    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("fatal error: no input files");
    }

    let lex = dbg!(lex::lex_file(args[1].clone()));
    let mut visiter = parse::LexVisiter::new(&lex);

    if let Ok(ast) = parse::parse_items(&mut visiter) {
        dbg!(&ast);
        type_check(&ast, &mut visiter, VarType::Void);

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
