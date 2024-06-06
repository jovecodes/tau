use crate::parse::{AstKind, AstNode, ElseBlock, LexVisiter, Number, VarType};

pub fn var_type_to_c(var_type: &VarType, visiter: &LexVisiter) -> String {
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

pub fn ast_to_c(ast: &mut AstNode, visiter: &LexVisiter) -> String {
    let mut output = String::new();
    output += "#include <cstdio>\n";
    output += "#include <Jovial/Std/String.h>\n";
    output += "#include <Jovial/Std/Print.h>\n";
    output += "using namespace jovial;\n\n";

    output += "void print(const String &s) { printf(\"%s\", s.utf8().get_data()); }\n";
    output += "void println(const String &s) { printf(\"%s\\n\", s.utf8().get_data()); }\n\n";
    output += to_c(&ast, visiter, None).as_str();
    output
}

pub fn to_c(node: &AstNode, visiter: &LexVisiter, exp_ret: Option<&String>) -> String {
    match &node.kind {
        AstKind::Ident(id) => visiter.get_id(&id).unwrap().name.clone(),
        AstKind::BinOp(lhs, op, rhs) => {
            let mut output = String::new();
            output += &to_c(&lhs, visiter, None);
            output += &format!(" {} ", op.to_string());
            output += &to_c(&rhs, visiter, None);
            output
        }
        AstKind::Block(block) => {
            let mut output = String::new();
            output += "{\n";
            for stmt in &block.stmts {
                output += to_c(stmt, visiter, exp_ret).as_str();
                output += ";\n";
            }
            output += "}\n";
            output
        }
        AstKind::Function(id, func) => {
            let mut output = String::new();
            output += &var_type_to_c(&func.ret_type, visiter);
            output += " ";
            output += &visiter.get_id(id).unwrap().name;

            output.push('(');
            if !func.params.is_empty() {
                for param in func.params[..func.params.len() - 1].iter() {
                    output += to_c(param, visiter, None).as_str();
                    output += ", ";
                }
                if let Some(last) = func.params.last() {
                    output += to_c(last, visiter, None).as_str();
                }
            }
            output.push(')');

            output.push(' ');
            output += to_c(func.block.as_ref().unwrap(), visiter, None).as_str();

            output
        }
        AstKind::FunctionDecl(id, func) => {
            let mut output = String::new();
            output += &var_type_to_c(&func.ret_type, visiter);
            output += " ";
            output += &visiter.get_id(id).unwrap().name;

            output.push('(');
            if !func.params.is_empty() {
                for param in func.params[..func.params.len() - 1].iter() {
                    output += to_c(param, visiter, None).as_str();
                    output += ", ";
                }
                if let Some(last) = func.params.last() {
                    output += to_c(last, visiter, None).as_str();
                }
            }
            output += ");\n";

            output
        }
        AstKind::VarDecl(t, id, expr) => {
            let mut output = String::new();
            let var = visiter.get_id(id).unwrap();
            if var.constant {
                output += "const ";
            }
            output += &var_type_to_c(t, visiter);
            output += " ";
            output += &var.name;
            match expr.kind {
                AstKind::Block(_) | AstKind::If(_) => {
                    output += ";\n";
                    output += &to_c(expr, visiter, Some(&var.name));
                }
                _ => {
                    output += " = ";
                    output += &to_c(expr, visiter, None)
                }
            }
            output
        }
        AstKind::Return(val) => "return ".to_string() + &to_c(val, visiter, None),
        AstKind::Number(Number::Int(n)) => n.to_string(),
        AstKind::Number(Number::Float(n)) => n.to_string(),
        AstKind::String(s) => "\"".to_string() + &s + "\"",
        AstKind::Parameter(kind, id) => {
            var_type_to_c(kind, visiter) + " " + &visiter.get_id(id).unwrap().name
        }
        AstKind::FunCall(path, params, is_macro) => {
            if *is_macro {
                String::new()
            } else {
                let mut output = String::new();
                output += &to_c(path, visiter, None);
                output += "(";
                if !params.is_empty() {
                    for param in params[..params.len() - 1].iter() {
                        output += to_c(param, visiter, None).as_str();
                        output += ", ";
                    }
                    if let Some(last) = params.last() {
                        output += to_c(last, visiter, None).as_str();
                    }
                }
                output += ")";

                output
            }
        }
        AstKind::Items(items) => {
            let mut output = String::new();
            for item in items {
                output += to_c(item, visiter, None).as_str();
                match &item.kind {
                    AstKind::Function(_, _) => {}
                    AstKind::FunctionDecl(_, _) => {}
                    AstKind::StructDecl(_, _) => {}
                    _ => output += ";\n",
                }
                output += "\n";
            }
            output
        }
        AstKind::Reference(expr) => "&".to_string() + &to_c(expr, visiter, exp_ret),
        AstKind::Dereference(expr) => "*".to_string() + &to_c(expr, visiter, exp_ret),
        AstKind::Pointer(expr) => "&".to_string() + &to_c(expr, visiter, exp_ret),
        AstKind::If(if_stmt) => {
            let mut output = String::new();
            output += "if (";
            output += &to_c(&if_stmt.conditional, visiter, exp_ret);
            output += ") ";
            output += &to_c(&if_stmt.block, visiter, exp_ret);
            match &if_stmt.else_if {
                ElseBlock::Nothing => (),
                ElseBlock::ElseIf(stmt) => {
                    output += "else ";
                    output += &to_c(&stmt, visiter, exp_ret);
                }
                ElseBlock::Else(block) => {
                    output += "else ";
                    output += &to_c(&block, visiter, exp_ret);
                }
            }
            output
        }
        AstKind::Null => "nullptr".to_string(),
        AstKind::ArrayLit(elements) => {
            let mut output = String::new();
            output += "{";
            for element in elements {
                output += &to_c(&element, visiter, None);
                output += ",";
            }
            output += "}";
            output
        }
        AstKind::BinOpEq(path, op, val) => {
            let mut output = String::new();
            output += &to_c(path, visiter, None);
            output += &format!(" {}= ", op.to_string());
            output += &to_c(val, visiter, None);
            output
        }
        AstKind::Assign(path, val) => {
            let mut output = String::new();
            output += &to_c(path, visiter, None);
            output += " = ";
            output += &to_c(val, visiter, None);
            output
        }
        AstKind::ExprRet(inner) => {
            let mut output = String::new();
            if let Some(ret) = exp_ret {
                output += ret;
                output += " = ";
            }
            output += &to_c(inner, visiter, None);
            output
        }
        AstKind::StructDecl(id, s) => {
            let mut output = String::new();
            let name = &visiter.get_id(&id).unwrap().name;
            output += "typedef struct ";
            output += name;
            output += " {\n";
            for field in &s.fields {
                output += &to_c(&field, visiter, None);
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
            for (name, val, _) in &lit.fields {
                output += ".";
                output += &name;
                output += " = ";
                output += &to_c(val, visiter, None);
                output += ", ";
            }
            output += "}";

            output
        }
        AstKind::Access(lhs, ident, deref) => {
            if *deref {
                to_c(lhs, visiter, None) + "->" + &ident
            } else {
                to_c(lhs, visiter, None) + "." + &ident
            }
        }
    }
}
