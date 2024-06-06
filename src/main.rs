mod analysis;
mod cpp_gen;
mod error;
mod lex;
mod parse;
mod pos;

use parse::{AstKind, AstNode, ElseBlock, LexVisiter};
use std::io::Write;
use std::io::{self, BufRead, BufReader};
use std::path::Path;
use std::process::{Command, Stdio};
use std::thread;

fn apply_macros(ast: &mut AstNode, visiter: &mut LexVisiter) -> (Option<AstNode>, bool) {
    match &mut ast.kind {
        AstKind::Items(items) => {
            let mut new_nodes = vec![];
            for (i, item) in items.iter_mut().enumerate() {
                if let (Some(node), should_remove) = apply_macros(item, visiter) {
                    if !should_remove {
                        new_nodes.push((i, node));
                    }
                }
            }
            for (i, item) in new_nodes {
                items.insert(i, item);
            }
            (None, false)
        }
        AstKind::Function(_id, func) => {
            if let Some(b) = &mut func.block.clone() {
                apply_macros(b.as_mut(), visiter);
            }
            (None, false)
        }
        AstKind::Block(block) => {
            let mut new_nodes = vec![];
            for (i, stmt) in block.stmts.iter_mut().enumerate() {
                if let (Some(node), should_remove) = apply_macros(stmt, visiter) {
                    if !should_remove {
                        new_nodes.push((i, node));
                    }
                }
            }
            for (i, stmt) in new_nodes {
                block.stmts.insert(i, stmt);
            }
            (None, false)
        }
        AstKind::If(if_stmt) => {
            apply_macros(&mut if_stmt.block, visiter);
            match &mut if_stmt.else_if {
                ElseBlock::Nothing => (None, false),
                ElseBlock::ElseIf(stmt) => apply_macros(stmt, visiter),
                ElseBlock::Else(stmt) => apply_macros(stmt, visiter),
            }
        }
        AstKind::VarDecl(_, _, val) => apply_macros(val, visiter),
        AstKind::Return(val) => apply_macros(val, visiter),
        AstKind::FunCall(func, params, is_macro) => {
            if *is_macro {
                if let AstKind::Ident(id) = &func.kind {
                    let _info = visiter.get_id(id).unwrap();
                    todo!()
                } else {
                    todo!()
                }
            }
            for param in params {
                apply_macros(param, visiter);
            }
            (None, false)
        }
        AstKind::BinOpEq(lhs, _, rhs) => {
            apply_macros(lhs, visiter);
            apply_macros(rhs, visiter);
            (None, false)
        }
        AstKind::Assign(lhs, rhs) => {
            apply_macros(lhs, visiter);
            apply_macros(rhs, visiter);
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
    let mut visiter = parse::LexVisiter::new(lex);
    print_done();

    print("Parsing...");
    let mut ast = parse::parse_items(&mut visiter, input_file.unwrap()).unwrap();
    print_done();

    print("Applying macros...");
    apply_macros(&mut ast, &mut visiter);
    print_done();

    print("Checking for errors...");
    analysis::semantic_analyse(&mut ast, &mut visiter);
    print_done();

    match mode {
        CompileMode::CompileToC => {
            print("Generating C code...");
            let c_code = cpp_gen::ast_to_c(&mut ast, &visiter);
            print_done();
            if let Err(e) = std::fs::write(output_file_path, c_code) {
                term_error(&format!("{e}"))
            }
            // println!("{}", c_code);
        }
        CompileMode::Run => {
            print("Generating C code...");
            let c_code = cpp_gen::ast_to_c(&mut ast, &visiter);
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
            let c_code = cpp_gen::ast_to_c(&mut ast, &visiter);
            print_done();
            compile_to_binary_from_c(c_code, output_file_path, output_file.clone())
        }
    }
}
