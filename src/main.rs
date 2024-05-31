mod error;
mod lex;
mod parse;
mod pos;

use parse::{AstNode, LexVisiter, Path, VarType};

fn var_type_to_c(var_type: &VarType, visiter: &LexVisiter) -> String {
    match var_type {
        VarType::Unknown => "unknown".to_owned(),
        VarType::Void => "void".to_owned(),
        VarType::Int => "int".to_owned(),
        VarType::Float => "float".to_owned(),
        VarType::String => "String".to_owned(),
        VarType::Bool => "bool".to_owned(),
        VarType::Function(_) => todo!(),
        VarType::User(_) => todo!(),
        VarType::Ref(inner) => "&".to_string() + var_type_to_c(inner, visiter).as_str(),
        VarType::Deref(inner) => "*".to_string() + var_type_to_c(inner, visiter).as_str(),
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
        parse::AstKind::Path(_) => todo!(),
        parse::AstKind::BinOp(_, _, _) => todo!(),
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
            output += to_c(&func.block, visiter).as_str();

            output
        }
        parse::AstKind::VarDecl(_, _, _) => todo!(),
        parse::AstKind::Return(_) => todo!(),
        parse::AstKind::Number(_) => todo!(),
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
    }
}

fn main() {
    // std::env::set_var("RUST_BACKTRACE", "1");

    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("fatal error: no input files");
    }

    let lex = lex::lex_file(args[1].clone());
    let mut visiter = parse::LexVisiter::new(&lex);

    if let Ok(ast) = parse::parse_items(&mut visiter) {
        dbg!(&ast);

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
