mod analysis;
mod cpp_gen;
mod error;
mod lex;
mod parse;
mod pos;

use error::Log;
use lex::BinOp;
use parse::{AstKind, AstNode, ElseBlock, LexVisitor, VarTypeKind, ID};
use pos::Span;
use std::io::Write;
use std::io::{self, BufRead, BufReader};
use std::path::Path;
use std::process::{Command, Stdio};
use std::thread;

use crate::parse::VarType;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Void,
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Struct(Vec<(String, Value)>, ID),
}

impl Eq for Value {}

impl Value {
    fn kind(&self, visitor: &LexVisitor) -> VarType {
        match self {
            Value::Void => VarType::new(VarTypeKind::Void, true),
            Value::Int(_) => VarType::new(VarTypeKind::Int, true),
            Value::Float(_) => VarType::new(VarTypeKind::Float, true),
            Value::String(_) => VarType::new(VarTypeKind::String, true),
            Value::Bool(_) => VarType::new(VarTypeKind::Bool, true),
            Value::Struct(_, id) => visitor.get_id(id).unwrap().kind.clone(),
        }
    }

    fn as_truthy(&self, visitor: &LexVisitor, span: &Span) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Struct(_, _) => todo!(),
            _ => visitor.error(
                format!(
                    "Expected boolean type but found type {}",
                    self.kind(visitor)
                ),
                "".to_string(),
                span,
            ),
        }
    }
}

fn compile_time_interpret(ast: &mut AstNode, visitor: &mut LexVisitor) -> Value {
    match &mut ast.kind {
        AstKind::Ident(id) => {
            let it = visitor.get_id(id).unwrap();
            if !it.immutable {
                visitor.error(
                    "Can not use non-constant value in compile time expression".to_string(),
                    format!("Make '{}' constant", it.name),
                    &ast.span,
                );
            }
            if it.constant_value.is_none() {
                visitor.error(
                    "Compiler Error: constant variable does not have constant value".to_string(),
                    "".to_string(),
                    &ast.span,
                );
            }
            it.constant_value.as_ref().unwrap().clone()
        }
        AstKind::BinOp(lhs, op, rhs) => {
            let lhs_val = compile_time_interpret(lhs, visitor);
            let rhs_val = compile_time_interpret(rhs, visitor);
            if *op == BinOp::Eq {
                return Value::Bool(lhs_val == rhs_val);
            }
            match &lhs_val {
                Value::Int(l) => {
                    if let Value::Int(r) = rhs_val {
                        Value::Int(l + r)
                    } else {
                        visitor.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                lhs_val.kind(visitor),
                                rhs_val.kind(visitor),
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        )
                    }
                }
                Value::Float(l) => {
                    if let Value::Float(r) = rhs_val {
                        Value::Float(l + r)
                    } else {
                        visitor.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                lhs_val.kind(visitor),
                                rhs_val.kind(visitor),
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        )
                    }
                }
                Value::String(l) => {
                    if let Value::String(r) = &rhs_val {
                        Value::String(l.to_string() + &r)
                    } else {
                        visitor.error(
                            format!(
                                "Can not {} differing types of {} and {} without a cast",
                                op.to_string(),
                                lhs_val.kind(visitor),
                                rhs_val.kind(visitor),
                            ),
                            "Add a 'xx' before the rhs to cast it".to_string(),
                            &ast.span,
                        )
                    }
                }
                Value::Bool(_) => visitor.error(
                    format!("Can not {} on type Bool", op.to_string()),
                    "Try casting it to a int".to_string(),
                    &ast.span,
                ),
                Value::Struct(_, _) => visitor.error(
                    format!("Can not {} on custom structures yet", op.to_string()),
                    "".to_string(),
                    &ast.span,
                ),
                Value::Void => visitor.error(
                    "Can not perform binary operations on void".to_string(),
                    "".to_string(),
                    &ast.span,
                ),
            }
        }
        AstKind::Field(_) => todo!(),
        AstKind::Block(block) => {
            for i in &mut block.stmts {
                match &mut i.kind {
                    AstKind::ExprRet(ret) => return compile_time_interpret(ret, visitor),
                    _ => {}
                }
            }
            Value::Void
        }
        AstKind::Function(_, _) => todo!(),
        AstKind::FunctionDecl(_, _) => todo!(),
        AstKind::StructDecl(_, _) => todo!(),
        AstKind::StructLit(lit) => {
            let mut map = Vec::new();
            for (name, value, _) in &mut lit.fields {
                map.push((name.clone(), compile_time_interpret(value, visitor)));
            }
            Value::Struct(map, lit.id.clone())
        }
        AstKind::VarDecl(_, _, _) => todo!(),
        AstKind::Return(_) => todo!(),
        AstKind::ExprRet(_) => todo!(),
        AstKind::If(if_stmt) => {
            let val = compile_time_interpret(&mut if_stmt.conditional, visitor);
            let condition = val.as_truthy(&visitor, &ast.span);
            if condition {
                compile_time_interpret(&mut if_stmt.block, visitor)
            } else {
                match &mut if_stmt.else_if {
                    ElseBlock::Nothing => visitor.error(
                        "All branches evaluate to false".to_string(),
                        "consider having an else branch here".to_string(),
                        &ast.span,
                    ),
                    ElseBlock::ElseIf(stmt) => compile_time_interpret(stmt, visitor),
                    ElseBlock::Else(stmt) => compile_time_interpret(stmt, visitor),
                }
            }
        }
        AstKind::Number(parse::Number::Float(f)) => Value::Float(*f),
        AstKind::Number(parse::Number::Int(i)) => Value::Int(*i),
        AstKind::String(s) => Value::String(s.clone()),
        AstKind::Parameter(_, _) => todo!(),
        AstKind::FunCall(_, _, _) => todo!(),
        AstKind::Items(_) => todo!(),
        AstKind::Reference(_) => todo!(),
        AstKind::Dereference(_) => todo!(),
        AstKind::Pointer(_) => todo!(),
        AstKind::ArrayLit(_) => todo!(),
        AstKind::BinOpEq(_, _, _) => todo!(),
        AstKind::Assign(_, _) => todo!(),
        AstKind::Access(_, _, _) => todo!(),
        AstKind::Null => todo!(),
        AstKind::Bool(b) => Value::Bool(*b),
        AstKind::Range(_) => todo!(),
        AstKind::For(_) => todo!(),
    }
}

fn apply_macros(ast: &mut AstNode, visitor: &mut LexVisitor) -> (Option<AstNode>, bool) {
    match &mut ast.kind {
        AstKind::Items(items) => {
            let mut new_nodes = vec![];
            let mut i = 0;
            for item in items.iter_mut() {
                let (node, should_remove) = apply_macros(item, visitor);
                if let Some(node) = node {
                    new_nodes.push((i, Some(node)));
                }
                if should_remove {
                    new_nodes.push((i, None));
                }
                i += 1;
            }

            // Apply removals and insertions
            for (i, item) in new_nodes.iter().rev() {
                if let Some(node) = item {
                    items.insert(*i, node.clone());
                } else {
                    println!("remove this");
                    items.remove(*i);
                }
            }
            (None, false)
        }
        AstKind::Function(_id, func) => {
            if let Some(b) = &mut func.borrow_mut().block {
                let (node, should_remove) = apply_macros(b.as_mut(), visitor);
                if should_remove {
                    if let Some(node) = node {
                        *b = Box::new(node);
                    } else {
                        *b = Box::new(AstNode::new(
                            AstKind::Block(parse::Block {
                                stmts: Vec::new(),
                                scope: 0,
                                expected_return: VarType::new(VarTypeKind::Void, true),
                            }),
                            b.span,
                        ));
                    }
                }
            }
            (None, false)
        }
        AstKind::Block(block) => {
            let mut new_nodes = vec![];
            let mut i = 0;
            for stmt in block.stmts.iter_mut() {
                let (node, should_remove) = apply_macros(stmt, visitor);
                if let Some(node) = node {
                    new_nodes.push((i, Some(node)));
                }
                if should_remove {
                    new_nodes.push((i, None));
                }
                i += 1;
            }

            // Apply removals and insertions
            for (i, stmt) in new_nodes.iter().rev() {
                if let Some(node) = stmt {
                    block.stmts.insert(*i, node.clone());
                } else {
                    block.stmts.remove(*i);
                }
            }

            (None, false)
        }
        AstKind::If(if_stmt) => {
            if if_stmt.is_macro {
                let val = compile_time_interpret(&mut if_stmt.conditional, visitor);
                if val == Value::Bool(true) {
                    apply_macros(&mut if_stmt.block, visitor);
                    return (Some(*if_stmt.block.clone()), true);
                } else if val != Value::Bool(true) {
                    visitor.error(
                        format!("Expected boolean type but found type {}", val.kind(visitor)),
                        "".to_string(),
                        &if_stmt.conditional.span,
                    )
                } else {
                    todo!()
                }
            } else {
                apply_macros(&mut if_stmt.block, visitor);
                match &mut if_stmt.else_if {
                    ElseBlock::Nothing => (None, false),
                    ElseBlock::ElseIf(stmt) => apply_macros(stmt, visitor),
                    ElseBlock::Else(stmt) => apply_macros(stmt, visitor),
                }
            }
        }
        AstKind::VarDecl(_, _, val) => match val {
            parse::AssignVal::Ast(node) => apply_macros(node, visitor),
            parse::AssignVal::Value(_, _) => (None, false),
        },
        AstKind::Return(val) => apply_macros(val, visitor),
        AstKind::FunCall(func, params, is_macro) => {
            if *is_macro {
                if let AstKind::Ident(id) = &func.kind {
                    let info = visitor.get_id(id).unwrap();
                    match info.name.as_str() {
                        "asm" => {}
                        _ => todo!(),
                    }
                } else {
                    todo!()
                }
            }
            for param in params {
                apply_macros(param, visitor);
            }
            (None, false)
        }
        AstKind::BinOpEq(lhs, _, rhs) => {
            apply_macros(lhs, visitor);
            apply_macros(rhs, visitor);
            (None, false)
        }
        AstKind::Assign(lhs, rhs) => {
            apply_macros(lhs, visitor);
            apply_macros(rhs, visitor);
            (None, false)
        }
        _ => (None, false),
    }
}

#[derive(Debug, PartialEq, Eq)]
enum CompileMode {
    CompileToBinary,
    CompileToC,
    Run,
}

fn term_error(msg: &str) -> ! {
    println!(
        "{}{}Error{}: {}{msg}{}",
        error::TColor::Bold,
        error::TColor::Red,
        error::TColor::Reset,
        error::TColor::Bold,
        error::TColor::Reset,
    );
    std::process::exit(1);
}

fn expect_compile_mode() -> ! {
    term_error("Expected a compile mode. eg. \"tau c input.tau\" or \"tau r input.tau\"")
}

fn print(msg: &str) {
    print!("{msg}");
    match io::stdout().flush() {
        Ok(_) => print!(""),
        Err(error) => println!("{}", error),
    }
}

fn print_done() {
    println!(
        " {}{}Done!{}",
        error::TColor::Green,
        error::TColor::Bold,
        error::TColor::Reset
    );
}

fn compile_to_binary_from_c(c_code: String, output_file_path: &Path, output_file: Option<String>) {
    let c_path = output_file_path.with_extension("cpp");
    if let Err(e) = std::fs::write(c_path.clone(), c_code) {
        term_error(&format!("{e}"))
    }

    let output = Command::new("bear")
        .args([
            "--",
            "g++",
            c_path.to_str().unwrap().to_string().as_str(),
            "-o",
            output_file.unwrap().as_str(),
            "-w",
            "-I/home/jove/Code/JovialEngine/include",
            "-I/home/jove/Code/JovialEngine/include/extern",
            "-L/home/jove/Code/JovialEngine/build",
            "-ljovial_engine",
            "-lGL",
            "-lglfw",
        ])
        .output()
        .expect("failed to execute process");

    let stderr = String::from_utf8(output.stderr).unwrap();
    if !stderr.is_empty() {
        println!("{}", stderr);
        term_error("Could not compile code to c++!")
    }
}

fn main() {
    // std::env::set_var("RUST_BACKTRACE", "1");

    let args: Vec<String> = std::env::args().collect();
    let mut i = 1;
    let mut mode = {
        match args
            .get(i)
            .unwrap_or_else(|| expect_compile_mode())
            .as_str()
        {
            "c" => CompileMode::CompileToBinary,
            "r" => CompileMode::Run,
            _ => expect_compile_mode(),
        }
    };
    i += 1;

    let mut input_file = None;
    let mut output_file = None;
    while i < args.len() {
        match args[i].as_str() {
            "-c" => {
                if mode == CompileMode::CompileToBinary {
                    mode = CompileMode::CompileToC;
                }
            }
            "-o" => {
                i += 1;
                if let Some(path) = args.get(i) {
                    if output_file.is_some() {
                        term_error("Expected only one '-o'");
                    }
                    output_file = Some(path.clone());
                } else {
                    term_error("Expected output filepath after '-o'");
                }
            }
            _ => {
                if input_file.is_some() {
                    term_error("Expected only one input file but found two. Tau does not work like c, 
                               it uses modules so you do not need to provide a list of source files");
                }
                if !std::path::Path::new(&args[i]).exists() {
                    term_error(&format!("Input file '{}' does not exist ", args[i]));
                }
                input_file = Some(args[i].clone());
            }
        }
        i += 1;
    }
    if input_file.is_none() {
        term_error("No input file provided");
    }
    let mut output_file_path = Path::new("");
    if !matches!(mode, CompileMode::Run) {
        if output_file.is_none() {
            term_error("No output file provided");
        } else {
            output_file_path = Path::new(output_file.as_ref().unwrap());
        }
    }

    if args.len() < 2 {
        panic!("fatal error: no input files");
    }

    print("Lexing...");
    let lex = lex::lex_file(input_file.as_ref().unwrap().clone());
    let mut visitor = parse::LexVisitor::new(lex, "cpp".to_string());
    print_done();

    print("Parsing...");
    let mut ast = parse::parse_items(&mut visitor, input_file.unwrap()).unwrap();
    print_done();

    print("Applying macros...");
    apply_macros(&mut ast, &mut visitor);
    print_done();

    print("Checking for errors...");
    analysis::semantic_analyse(&mut ast, &mut visitor);
    print_done();

    match mode {
        CompileMode::CompileToC => {
            print("Generating C code...");
            let c_code = cpp_gen::ast_to_c(&mut ast, &visitor);
            print_done();
            if let Err(e) = std::fs::write(output_file_path, c_code) {
                term_error(&format!("{e}"))
            }
            // println!("{}", c_code);
        }
        CompileMode::Run => {
            print("Generating C code...");
            let c_code = cpp_gen::ast_to_c(&mut ast, &visitor);
            print_done();
            let output_file_path = Path::new("tau_temp");
            let output_file = Some("tau_temp".to_string());
            compile_to_binary_from_c(c_code, output_file_path, output_file.clone());

            println!("---------------------");

            let mut process = Command::new(format!("./{}", output_file.unwrap()))
                .stderr(Stdio::piped())
                .stdout(Stdio::piped())
                .stdin(Stdio::null())
                .spawn()
                .expect("failed to execute process");

            // Capture the stdout
            let stdout = process.stdout.take().expect("failed to capture stdout");
            let stdout_reader = BufReader::new(stdout);
            let stdout_handle = thread::spawn(move || {
                for line in stdout_reader.lines() {
                    match line {
                        Ok(line) => println!("{}", line),
                        Err(err) => eprintln!("Error reading stdout: {}", err),
                    }
                }
            });

            // Capture the stderr
            let stderr = process.stderr.take().expect("failed to capture stderr");
            let stderr_reader = BufReader::new(stderr);
            let stderr_handle = thread::spawn(move || {
                for line in stderr_reader.lines() {
                    match line {
                        Ok(line) => eprintln!("{}", line),
                        Err(err) => eprintln!("Error reading stderr: {}", err),
                    }
                }
            });

            // Wait for the process to complete
            // let status = process.wait().expect("process failed to run");
            // println!("Process exited with status: {}", status);

            // Wait for the threads to complete
            stdout_handle.join().expect("failed to join stdout thread");
            stderr_handle.join().expect("failed to join stderr thread");
        }
        CompileMode::CompileToBinary => {
            print("Generating C code...");
            let c_code = cpp_gen::ast_to_c(&mut ast, &visitor);
            print_done();
            compile_to_binary_from_c(c_code, output_file_path, output_file.clone())
        }
    }
}
