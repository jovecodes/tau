mod error;
mod lex;
mod pos;

use lex::{BinOp, Token, TokenKind};
use pos::Span;

use crate::{error::Log, lex::TokenKindName};
use core::panic;
use std::{collections::HashMap, error::Error, iter::Peekable, rc::Rc, slice::Iter};

type ScopeID = u64;

#[derive(Debug)]
struct Scope {
    id: ScopeID,
    symbols: HashMap<String, ID>,
    children: Vec<ID>,
}

#[derive(Debug)]
struct ScopeTable {
    items: HashMap<ScopeID, Scope>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct ID {
    scope: ScopeID,
    val: u64,
}

#[derive(Debug, Clone)]
enum VarType {
    Unknown,
    Void,
    Int,
    Float,
    String,
    Bool,
    Function(Rc<Function>),
    User(ID),
    Ptr(Box<VarType>),
    Ref(Box<VarType>),
    Deref(Box<VarType>),
}

#[derive(Debug, Clone)]
struct SymbolInfo {
    kind: VarType,
}

#[derive(Debug)]
struct SymbolTable {
    items: HashMap<ID, SymbolInfo>,
}

fn as_result<T>(o: Option<T>) -> Result<T, ()> {
    match o {
        Some(value) => Ok(value),
        None => Err(()),
    }
}

#[derive(Debug)]
struct LexVisiter<'a> {
    path: String,
    file: String,
    tokens: Peekable<Iter<'a, Token>>,

    symbols: SymbolTable,
    scopes: ScopeTable,
    current_scope: ScopeID,
}

impl LexVisiter<'_> {
    fn expect(&mut self, valid: impl Fn(&TokenKind) -> bool) -> Result<&Token, Option<&Token>> {
        match self.tokens.next() {
            Some(t) => {
                if valid(&t.kind) {
                    Ok(t)
                } else {
                    Err(Some(t))
                }
            }
            None => Err(None),
        }
    }

    fn find_id(&self, name: &str) -> Option<ID> {
        for (symbol, id) in &self.scopes.items[&self.current_scope].symbols {
            if symbol == name {
                return Some(id.clone());
            }
        }
        None
    }

    fn push_id(&mut self, name: String, info: SymbolInfo) -> Result<ID, SymbolInfo> {
        let symbols = &mut self
            .scopes
            .items
            .get_mut(&self.current_scope)
            .unwrap()
            .symbols;

        let id = ID {
            scope: self.current_scope,
            val: symbols.len() as u64 + 1,
        };
        symbols.insert(name, id.clone());
        if self.symbols.items.contains_key(&id) {
            Err(self.symbols.items[&id].clone())
        } else {
            self.symbols.items.insert(id.clone(), info);
            Ok(id)
        }
    }

    fn expect_auto_err(&mut self, kind: u32) -> &Token {
        {
            let as_int = {
                self.tokens
                    .peek()
                    .map(|t| t.kind.as_int())
                    .unwrap_or(u32::MAX)
            };
            if as_int == kind {
                return self.tokens.next().unwrap();
            }
        }
        let res = self.tokens.next();
        if let Some(v) = res {
            let s = v.span.clone();
            let name = v.kind.as_int().token_kind_name();
            self.error(
                format!("Expected token {} but got {}", kind.token_kind_name(), name),
                format!(""),
                &s,
            );
            panic!()
        } else {
            let s = Span::default();
            self.error(
                format!("Expected token {} but got <eof>", kind.token_kind_name()),
                format!(""),
                &s,
            );
            panic!()
        }
    }
}

#[derive(Debug, Clone)]
struct Path {
    segments: Vec<PathSegment>,
}

impl Path {
    fn new(segments: Vec<PathSegment>) -> Self {
        Self { segments }
    }
}

#[derive(Debug, Clone)]
struct PathSegment {
    // ident: String,
    id: ID,
    // args: Option<P<GenericArgs>>,
}

impl PathSegment {
    fn new(id: ID) -> Self {
        Self { id }
    }
}

#[derive(Debug, Clone)]
enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
struct Function {
    params: Vec<AstNode>,
    ret_type: VarType,
    block: Box<AstNode>,
}

#[derive(Debug, Clone)]
enum AstKind {
    Path(Path),
    BinOp(Box<AstNode>, BinOp, Box<AstNode>),
    Block(Vec<AstNode>),
    Function(ID, Rc<Function>),
    VarDecl(VarType, ID, Box<AstNode>),
    Return(Box<AstNode>),
    Number(Number),
}

#[derive(Debug, Clone)]
struct AstNode {
    kind: AstKind,
    span: Span,
}

impl AstNode {
    fn new(kind: AstKind, span: Span) -> Self {
        Self { kind, span }
    }
}

fn combine_expr(lhs: AstNode, op: &Token, rhs: AstNode) -> AstNode {
    let op_val = match op.kind.as_bin_op() {
        Some(v) => v,
        None => panic!("Invalid binary operator"),
    };
    let span = lhs.span.expand_to(&rhs.span);
    AstNode::new(AstKind::BinOp(Box::new(lhs), op_val, Box::new(rhs)), span)
}

fn parse_expression_1<'a>(
    lex: &'a mut LexVisiter,
    mut lhs: AstNode,
    min_prec: i32,
) -> Result<AstNode, ()> {
    let mut lookahead = match lex.tokens.peek() {
        Some(token) => *token,
        None => return Ok(lhs),
    };

    while lookahead.kind.precedence() >= min_prec {
        let op = lookahead;
        lex.tokens.next();
        let mut rhs = parse_primary(lex)?;
        lookahead = match lex.tokens.peek() {
            Some(token) => token,
            None => {
                lhs = combine_expr(lhs, op, rhs);
                dbg!(&lhs);
                break;
            }
        };
        while lookahead.kind.precedence() > op.kind.precedence() {
            rhs = parse_expression_1(lex, rhs, op.kind.precedence() + 1)?;
            dbg!(&rhs);
            lookahead = match lex.tokens.peek() {
                Some(token) => token,
                None => return Ok(combine_expr(lhs, op, rhs)),
            };
        }
        lhs = combine_expr(lhs, op, rhs);
    }
    dbg!(&lookahead);
    Ok(lhs)
}

fn parse_primary<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    match lex.tokens.peek().map(|x| &x.kind) {
        Some(TokenKind::Ident(_)) => {
            let from = lex.tokens.peek().unwrap().span.from;
            let path = parse_path(lex)?;
            return Ok(AstNode::new(
                AstKind::Path(path),
                Span::new(from, lex.tokens.peek().unwrap().span.to),
            ));
        }
        _ => {}
    };
    match lex.tokens.next() {
        Some(tok) => match &tok.kind {
            TokenKind::Int(i) => Ok(AstNode::new(AstKind::Number(Number::Int(*i)), tok.span)),
            TokenKind::Float(f) => Ok(AstNode::new(AstKind::Number(Number::Float(*f)), tok.span)),
            TokenKind::OpenDelim(lex::Delimiter::Paren) => {
                let res = parse_expression(lex);
                if let Err(e) = lex.expect(|t| t.is_close_delim(lex::Delimiter::Paren)) {
                    if let Some(t) = e {
                        let span = t.span.clone();
                        lex.error(
                            "Expected ')'".to_string(),
                            "Put ')' here".to_string(),
                            &span,
                        )
                    }
                }

                res
            }
            t => {
                lex.error(
                    format!("Could not parse token: {t:?} as primary node"),
                    "This is a bug in the compiler not your code".to_string(),
                    &tok.span,
                );
                Err(())
            }
        },
        None => {
            lex.error(
                format!("Expected primary expression but found <eof>"),
                "This is an invalid end of a program".to_string(),
                &Span::default(),
            );
            Err(())
        }
    }
}

fn parse_expression<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let primary = parse_primary(lex)?;
    parse_expression_1(lex, primary, 0)
}

fn parse_path<'a>(lex: &'a mut LexVisiter) -> Result<Path, ()> {
    let token = as_result(lex.tokens.next())?;

    let mut path = Path {
        segments: Vec::new(),
    };

    match &token.kind {
        TokenKind::Ident(ident) => path
            .segments
            .push(PathSegment::new(lex.find_id(ident).ok_or(())?)),
        _ => return Ok(path),
    }

    Ok(path)
}

fn parse_var_decl<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let from_pos = lex.tokens.peek().unwrap().span.from;
    let kind = parse_type(lex)?;
    lex.tokens.next();

    let ident_token = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
    let ident = match &ident_token.kind {
        TokenKind::Ident(name) => name.clone(),
        _ => panic!("this should be a ident"),
    };

    lex.expect_auto_err(TokenKind::Assign.as_int());

    let value = parse_expression(lex)?;
    lex.expect_auto_err(TokenKind::Semi.as_int());

    let span = Span::new(from_pos, value.span.to);

    let id = lex.push_id(ident.clone(), SymbolInfo { kind: kind.clone() });
    if let Err(_) = id {
        lex.error(
            format!("Redefining variable {}", &ident),
            format!("Change this variables name"),
            &span,
        );
        Err(())
    } else {
        Ok(AstNode::new(
            AstKind::VarDecl(kind, id.unwrap(), Box::new(value)),
            span,
        ))
    }
}

fn parse_fn_parameter<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    todo!();
}

fn parse_type<'a>(lex: &'a mut LexVisiter) -> Result<VarType, ()> {
    match lex.tokens.peek() {
        Some(t) => match &t.kind {
            TokenKind::Ident(name) => Ok(VarType::User(lex.find_id(name).ok_or(())?)),
            TokenKind::Keyword(lex::Keyword::Var) => Ok(VarType::Unknown),
            TokenKind::Keyword(lex::Keyword::Int) => Ok(VarType::Int),
            TokenKind::Keyword(lex::Keyword::Float) => Ok(VarType::Float),
            TokenKind::Keyword(lex::Keyword::String) => Ok(VarType::String),
            TokenKind::Keyword(lex::Keyword::Bool) => Ok(VarType::Bool),
            _ => Err(()),
        },
        None => {
            lex.error(
                "Expected type but found <eof>".to_owned(),
                "".to_owned(),
                &Span::null(),
            );
            Err(())
        }
    }
}

fn parse_block<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let mut stmts = Vec::new();

    let start_pos = lex
        .expect(|x| matches!(x, TokenKind::OpenDelim(lex::Delimiter::Brace)))
        .unwrap()
        .span
        .from;

    while !matches!(
        lex.tokens.peek().map(|x| &x.kind),
        Some(TokenKind::CloseDelim(lex::Delimiter::Brace))
    ) {
        stmts.push(parse_stmt(lex)?)
    }

    let end_pos = lex
        .expect(|x| matches!(x, TokenKind::CloseDelim(lex::Delimiter::Brace)))
        .unwrap()
        .span
        .to;

    Ok(AstNode::new(
        AstKind::Block(stmts),
        Span::new(start_pos, end_pos),
    ))
}

fn parse_fn<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let fn_start_pos = lex
        .expect(|x| matches!(x, TokenKind::Keyword(lex::Keyword::Function)))
        .unwrap()
        .span
        .from;

    let ident = {
        let ident_token = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
        match &ident_token.kind {
            TokenKind::Ident(name) => name.clone(),
            _ => panic!("this should be a ident"),
        }
    };
    let mut parameters = Vec::new();

    lex.expect(|x| matches!(x, TokenKind::OpenDelim(lex::Delimiter::Paren)))
        .unwrap();

    while !matches!(
        lex.tokens.peek().map(|x| &x.kind),
        Some(TokenKind::CloseDelim(lex::Delimiter::Paren))
    ) {
        parameters.push(parse_fn_parameter(lex)?);
        if !matches!(lex.tokens.peek().map(|x| &x.kind), Some(TokenKind::Comma)) {
            lex.tokens.next();
        }
    }

    lex.expect(|x| matches!(x, TokenKind::CloseDelim(lex::Delimiter::Paren)))
        .unwrap();

    let kind = parse_type(lex).unwrap_or(VarType::Void);
    if !matches!(
        lex.tokens.peek().map(|x| &x.kind),
        Some(TokenKind::OpenDelim(lex::Delimiter::Brace))
    ) {
        lex.tokens.next();
    }

    let block = parse_block(lex)?;
    let span = Span::new(fn_start_pos, block.span.to);

    let func = Rc::new(Function {
        params: parameters,
        ret_type: kind,
        block: Box::new(block),
    });

    let id = lex.push_id(
        ident.clone(),
        SymbolInfo {
            kind: VarType::Function(func.clone()),
        },
    );

    if let Err(_) = id {
        lex.error(
            format!("Function {} has already been defined!", &ident),
            format!("consider using a different name"),
            &span,
        );
        Err(())
    } else {
        Ok(AstNode::new(AstKind::Function(id.unwrap(), func), span))
    }
}

fn parse_return_stmt<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let start_pos = lex
        .expect(|x| matches!(x, TokenKind::Keyword(lex::Keyword::Return)))
        .unwrap()
        .span
        .from;

    let value = parse_expression(lex)?;

    let end_pos = lex
        .expect(|x| matches!(x, TokenKind::Semi))
        .unwrap()
        .span
        .from;

    let span = Span::new(start_pos, end_pos);
    Ok(AstNode::new(AstKind::Return(Box::new(value)), span))
}

fn parse_stmt<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    match &as_result(lex.tokens.peek())?.kind {
        t if t.is_type() => parse_var_decl(lex),
        TokenKind::Keyword(lex::Keyword::Function) => parse_fn(lex),
        TokenKind::Keyword(lex::Keyword::Return) => parse_return_stmt(lex),
        TokenKind::Semi => {
            lex.tokens.next();
            parse_stmt(lex)
        }
        TokenKind::Ident(_ident) => {
            todo!()
        }
        _ => parse_expression(lex),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("fatal error: no input files");
    }

    let lex = dbg!(lex::lex_file(args[1].clone()));

    let mut vister = LexVisiter {
        path: lex.path,
        file: lex.file,
        tokens: lex.tokens.iter().peekable(),
        symbols: SymbolTable {
            items: HashMap::new(),
        },
        scopes: ScopeTable {
            items: HashMap::new(),
        },
        current_scope: 0,
    };

    vister.scopes.items.insert(
        vister.current_scope,
        Scope {
            id: vister.current_scope,
            symbols: HashMap::new(),
            children: Vec::new(),
        },
    );

    if let Ok(val) = dbg!(parse_fn(&mut vister)) {
        vister.info(
            format!("Bin op span"),
            format!("This is the span"),
            &val.span,
        );
        if let Some((lhs, rhs)) = match &val.kind {
            AstKind::BinOp(lhs, _, rhs) => Some((lhs, rhs)),
            _ => None,
        } {
            vister.info(
                format!("Lhs"),
                format!("This is the left hand side"),
                &lhs.span,
            );
            vister.info(
                format!("rhs"),
                format!("This is the right hand side"),
                &rhs.span,
            );
        }
    }

    Ok(())
}
