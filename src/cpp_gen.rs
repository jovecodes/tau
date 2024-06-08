use std::fmt::Write;

use crate::{
    parse::{
        Array, ArraySize, AssignVal, AstKind, AstNode, ElseBlock, LexVisitor, Number, VarType,
        VarTypeKind,
    },
    Value,
};

pub fn var_type_to_c(var_type: &VarType, visitor: &LexVisitor) -> String {
    match &var_type.kind {
        VarTypeKind::Unknown => "unknown".to_owned(),
        VarTypeKind::Any => "void".to_string(),
        VarTypeKind::Void => "void".to_owned(),
        VarTypeKind::Int => "int".to_owned(),
        VarTypeKind::Float => "float".to_owned(),
        VarTypeKind::String => "String".to_owned(),
        VarTypeKind::Bool => "bool".to_owned(),
        VarTypeKind::Function(_) => todo!(),
        VarTypeKind::Struct(st) => st.name.clone(),
        VarTypeKind::Ref(inner) => var_type_to_c(inner, visitor) + "*",
        VarTypeKind::Deref(inner) => var_type_to_c(inner, visitor) + "*",
        VarTypeKind::Ptr(inner) => var_type_to_c(inner, visitor) + "*",
        VarTypeKind::Array(arr) => {
            let mut output = String::new();
            match arr.size {
                ArraySize::Fixed(_) | ArraySize::Unknown => {
                    output += var_type_to_c(&arr.var_type, visitor).as_str();
                }
                ArraySize::Dynamic => {
                    output += "Vec<";
                    output += var_type_to_c(&arr.var_type, visitor).as_str();
                    output += ">"
                }
            }
            output
        }
    }
}

fn arr_decl_to_c(arr: &Array, var: &str, visitor: &LexVisitor) -> String {
    let mut output = String::new();
    match arr.size {
        ArraySize::Fixed(s) => {
            output += var_type_to_c(&arr.var_type, visitor).as_str();
            output += " ";
            output += var;
            output += " ";
            output += "[";
            output += s.to_string().as_str();
            output += "]"
        }
        ArraySize::Dynamic => {
            output += "Vec<";
            output += var_type_to_c(&arr.var_type, visitor).as_str();
            output += ">";
            output += " ";
            output += var;
        }
        ArraySize::Unknown => {
            output += var_type_to_c(&arr.var_type, visitor).as_str();
            output += " ";
            output += var;
            output += " ";
            output += "[]";
        }
    }
    output
}

fn value_to_c(value: &Value) -> String {
    match value {
        Value::Void => "(void)".to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => {
            let mut b = String::new();
            b.write_fmt(format_args!("{s:?}")).unwrap();
            b
        }
        Value::Bool(b) => b.to_string(),
        Value::Struct(lit, _) => {
            let mut v = String::new();
            v += "{";
            for (name, val) in lit {
                v += ".";
                v += name;
                v += " = ";
                v += &value_to_c(val);
                v += ", ";
            }
            v += "}";
            v
        }
    }
}

pub fn ast_to_c(ast: &mut AstNode, visitor: &LexVisitor) -> String {
    let mut output = String::new();
    output += "#include <cstdio>\n";
    output += "#include <Jovial/Std/String.h>\n";
    output += "#include <Jovial/Std/Print.h>\n";
    output += "using namespace jovial;\n\n";

    output += to_c(&ast, visitor, None).as_str();
    output
}

pub fn to_c(node: &AstNode, visitor: &LexVisitor, exp_ret: Option<&String>) -> String {
    match &node.kind {
        AstKind::Ident(id) => visitor.get_id(&id).unwrap().name.clone(),
        AstKind::BinOp(lhs, op, rhs) => {
            let mut output = String::new();
            output += &to_c(&lhs, visitor, None);
            output += &format!(" {} ", op.to_string());
            output += &to_c(&rhs, visitor, None);
            output
        }
        AstKind::Block(block) => {
            let mut output = String::new();
            output += "{\n";
            for stmt in &block.stmts {
                output += to_c(stmt, visitor, exp_ret).as_str();
                output += ";\n";
            }
            output += "}\n";
            output
        }
        AstKind::Function(id, func) => {
            let mut output = String::new();
            output += &var_type_to_c(&func.borrow().ret_type, visitor);
            output += " ";
            output += &visitor.get_id(id).unwrap().name;

            output.push('(');
            if !func.borrow().params.is_empty() {
                for param in func.borrow().params[..func.borrow().params.len() - 1].iter() {
                    output += to_c(param, visitor, None).as_str();
                    output += ", ";
                }
                if let Some(last) = func.borrow().params.last() {
                    output += to_c(last, visitor, None).as_str();
                }
            }
            output.push(')');

            output.push(' ');
            output += to_c(func.borrow().block.as_ref().unwrap(), visitor, None).as_str();

            output
        }
        AstKind::FunctionDecl(id, func) => {
            let mut output = String::new();
            output += &var_type_to_c(&func.borrow().ret_type, visitor);
            output += " ";
            output += &visitor.get_id(id).unwrap().name;

            output.push('(');
            if !func.borrow().params.is_empty() {
                for param in func.borrow().params[..func.borrow().params.len() - 1].iter() {
                    output += to_c(param, visitor, None).as_str();
                    output += ", ";
                }
                if let Some(last) = func.borrow().params.last() {
                    output += to_c(last, visitor, None).as_str();
                }
            }
            output += ");\n";

            output
        }
        AstKind::VarDecl(t, id, expr) => {
            let mut output = String::new();
            let var = visitor.get_id(id).unwrap();
            if var.immutable {
                output += "const ";
            }
            if let VarTypeKind::Array(arr) = &t.kind {
                output += &arr_decl_to_c(arr, &var.name, visitor);
            } else {
                output += &var_type_to_c(t, visitor);
                output += " ";
                output += &var.name;
            }
            match expr {
                AssignVal::Ast(node) => match node.kind {
                    AstKind::Block(_) | AstKind::If(_) => {
                        output += ";\n";
                        output += &to_c(node, visitor, Some(&var.name));
                    }
                    _ => {
                        output += " = ";
                        output += &to_c(node, visitor, None)
                    }
                },
                AssignVal::Value(value, _) => {
                    output += " = ";
                    output += value_to_c(value).as_str();
                }
            }
            output
        }
        AstKind::Return(val) => "return ".to_string() + &to_c(val, visitor, None),
        AstKind::Number(Number::Int(n)) => n.to_string(),
        AstKind::Number(Number::Float(n)) => n.to_string(),
        AstKind::String(s) => format!("{:?}", &s),
        AstKind::Parameter(kind, id) => {
            var_type_to_c(kind, visitor) + " " + &visitor.get_id(id).unwrap().name
        }
        AstKind::FunCall(path, params, is_macro) => {
            if *is_macro {
                if let AstKind::Ident(id) = &path.kind {
                    let info = visitor.get_id(id).unwrap();
                    match info.name.as_str() {
                        "asm" => match &params.first().unwrap().kind {
                            AstKind::String(s) => s.clone(),
                            _ => unreachable!(),
                        },
                        _ => todo!(),
                    }
                } else {
                    todo!()
                }
            } else {
                let mut output = String::new();
                output += &to_c(path, visitor, None);
                output += "(";
                if !params.is_empty() {
                    for param in params[..params.len() - 1].iter() {
                        output += to_c(param, visitor, None).as_str();
                        output += ", ";
                    }
                    if let Some(last) = params.last() {
                        output += to_c(last, visitor, None).as_str();
                    }
                }
                output += ")";

                output
            }
        }
        AstKind::Items(items) => {
            let mut output = String::new();
            for item in items {
                if let AstKind::Block(b) = &item.kind {
                    for i in &b.stmts {
                        output += to_c(i, visitor, None).as_str();
                    }
                } else {
                    output += to_c(item, visitor, None).as_str();
                }
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
        AstKind::Reference(expr) => "&".to_string() + &to_c(expr, visitor, exp_ret),
        AstKind::Dereference(expr) => "*".to_string() + &to_c(expr, visitor, exp_ret),
        AstKind::Pointer(expr) => "&".to_string() + &to_c(expr, visitor, exp_ret),
        AstKind::If(if_stmt) => {
            let mut output = String::new();
            output += "if (";
            output += &to_c(&if_stmt.conditional, visitor, exp_ret);
            output += ") ";
            output += &to_c(&if_stmt.block, visitor, exp_ret);
            match &if_stmt.else_if {
                ElseBlock::Nothing => (),
                ElseBlock::ElseIf(stmt) => {
                    output += "else ";
                    output += &to_c(&stmt, visitor, exp_ret);
                }
                ElseBlock::Else(block) => {
                    output += "else ";
                    output += &to_c(&block, visitor, exp_ret);
                }
            }
            output
        }
        AstKind::Null => "nullptr".to_string(),
        AstKind::ArrayLit(elements) => {
            let mut output = String::new();
            output += "{";
            for element in elements {
                output += &to_c(&element, visitor, None);
                output += ",";
            }
            output += "}";
            output
        }
        AstKind::BinOpEq(path, op, val) => {
            let mut output = String::new();
            output += &to_c(path, visitor, None);
            output += &format!(" {}= ", op.to_string());
            output += &to_c(val, visitor, None);
            output
        }
        AstKind::Assign(path, val) => {
            let mut output = String::new();
            output += &to_c(path, visitor, None);
            output += " = ";
            output += &to_c(val, visitor, None);
            output
        }
        AstKind::ExprRet(inner) => {
            let mut output = String::new();
            if let Some(ret) = exp_ret {
                output += ret;
                output += " = ";
            }
            output += &to_c(inner, visitor, None);
            output
        }
        AstKind::StructDecl(id, s) => {
            let mut output = String::new();
            let name = &visitor.get_id(&id).unwrap().name;
            output += "typedef struct ";
            output += name;
            output += " {\n";
            for field in &s.fields {
                output += &to_c(&field, visitor, None);
            }
            output += "} ";
            output += name;
            output += ";\n";

            output
        }
        AstKind::Field(field) => {
            let mut output = String::new();
            if field.kind.constant {
                output += "const ";
            }
            output += &var_type_to_c(&field.kind, visitor);
            output += " ";
            output += &field.name;
            output += ";\n";

            output
        }
        AstKind::Bool(b) => b.to_string(),
        AstKind::StructLit(lit) => {
            let mut output = String::new();

            output += "(";
            output += &var_type_to_c(&visitor.get_id(&lit.id).unwrap().kind, visitor);
            output += ") ";

            output += "{";
            for (name, val, _) in &lit.fields {
                output += ".";
                output += &name;
                output += " = ";
                output += &to_c(val, visitor, None);
                output += ", ";
            }
            output += "}";

            output
        }
        AstKind::Access(lhs, ident, deref) => {
            if *deref {
                to_c(lhs, visitor, None) + "->" + &ident
            } else {
                to_c(lhs, visitor, None) + "." + &ident
            }
        }
    }
}
