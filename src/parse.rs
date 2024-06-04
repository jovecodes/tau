use crate::lex::{BinOp, Delimiter, Keyword, Token, TokenKind};
use crate::pos::{Pos, Span};

use crate::var_type_of;
use crate::{error::Log, lex::TokenKindName};
use core::panic;
use std::fmt::Display;
use std::{collections::HashMap, iter::Peekable, rc::Rc, slice::Iter};

type ScopeID = u64;

#[derive(Debug)]
pub struct Scope {
    pub id: ScopeID,
    parent: Option<ScopeID>,
    symbols: HashMap<String, ID>,
    children: Vec<ScopeID>,
}

#[derive(Debug)]
pub struct ScopeTable {
    items: HashMap<ScopeID, Scope>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ID {
    scope: ScopeID,
    val: u64,
}

impl ID {
    pub fn null() -> Self {
        Self { scope: 0, val: 0 }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArraySize {
    Fixed(Box<AstNode>),
    FixedKnown(i64),
    Dynamic,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub var_type: Box<VarType>,
    pub size: ArraySize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarType {
    Unknown,
    Any,
    Void,
    Int,
    Float,
    String,
    Bool,
    Function(Rc<Function>),
    Struct(Rc<Struct>),
    Ref(Box<VarType>),
    Ptr(Box<VarType>),
    Deref(Box<VarType>),
    Array(Array),
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Function(_) => todo!(),
            VarType::Struct(s) => write!(f, "{}", s.name),
            VarType::Ref(inner) => write!(f, "&{inner}"),
            VarType::Ptr(inner) => write!(f, "@{inner}"),
            VarType::Deref(inner) => write!(f, "*{inner}"),
            VarType::Array(_) => todo!(),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl VarType {
    pub fn loosy_eq(self, other: VarType) -> bool {
        match &self {
            VarType::Any => return true,
            VarType::Array(arr) => match &other {
                VarType::Array(other_arr) => {
                    if !arr.var_type.clone().loosy_eq(*other_arr.var_type.clone()) {
                        return false;
                    }
                    match arr.size {
                        ArraySize::Fixed(_) => todo!(),
                        ArraySize::FixedKnown(_) => todo!(),
                        ArraySize::Dynamic => return true,
                        ArraySize::Unknown => return true,
                    }
                }
                _ => return false,
            },
            _ => {}
        }
        if matches!(other, VarType::Any) {
            return true;
        }
        self == other
    }
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub kind: VarType,
    pub name: String,
    pub constant: bool,
}

#[derive(Debug)]
pub struct SymbolTable {
    items: HashMap<ID, SymbolInfo>,
}

fn as_result<T>(o: Option<T>) -> Result<T, ()> {
    match o {
        Some(value) => Ok(value),
        None => Err(()),
    }
}

#[derive(Debug)]
pub struct LexVisiter<'a> {
    pub path: &'a str,
    pub file: &'a str,
    pub tokens: Peekable<Iter<'a, Token>>,

    pub symbols: SymbolTable,
    pub scopes: ScopeTable,
    pub current_scope: ScopeID,
    pub global_scope: ScopeID,
}

impl<'a> LexVisiter<'a> {
    pub fn new(lex: &'a crate::lex::Lex) -> LexVisiter<'a> {
        let mut vister = LexVisiter {
            path: &lex.path,
            file: &lex.file,
            tokens: lex.tokens.iter().peekable(),
            symbols: SymbolTable {
                items: HashMap::new(),
            },
            scopes: ScopeTable {
                items: HashMap::new(),
            },
            current_scope: 0,
            global_scope: 0,
        };

        vister.scopes.items.insert(
            vister.current_scope,
            Scope {
                id: vister.current_scope,
                parent: None,
                symbols: HashMap::new(),
                children: Vec::new(),
            },
        );

        vister
    }

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
        let mut current = self.current_scope;
        loop {
            for (symbol, id) in &self.scopes.items[&current].symbols {
                if symbol == name {
                    return Some(id.clone());
                }
            }
            if let Some(parent) = &self.scopes.items[&current].parent {
                current = *parent;
            } else {
                return None;
            }
        }
    }

    fn push_scope(&mut self) -> ScopeID {
        let id = self.scopes.items.len() as u64;
        self.scopes.items.insert(
            id,
            Scope {
                id,
                parent: Some(self.current_scope),
                symbols: HashMap::new(),
                children: Vec::new(),
            },
        );
        self.scopes
            .items
            .get_mut(&self.current_scope)
            .unwrap()
            .children
            .push(id);
        self.current_scope = id;
        id
    }

    fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes.items[&self.current_scope].parent {
            self.current_scope = parent;
        } else {
            let span = self.tokens.peek().map(|x| x.span).unwrap_or(Span::null());
            self.error(
                "Compiler Error: trying to call pop_scope on global_scope scope".to_string(),
                "Here".to_string(),
                &span,
            );
        }
    }

    pub fn get_mut_id(&mut self, id: &ID) -> Option<&mut SymbolInfo> {
        return self.symbols.items.get_mut(id);
    }

    pub fn get_id(&self, id: &ID) -> Option<&SymbolInfo> {
        return self.symbols.items.get(id);
    }

    pub fn get_access_id(&self, path: &mut AstNode) -> ID {
        match &mut path.kind {
            AstKind::Ident(id) => id.clone(),
            AstKind::Access(lhs, access, _) => match var_type_of(lhs, self, VarType::Any) {
                VarType::Struct(s) => {
                    for field in &s.fields {
                        if let AstKind::Field(f) = &field.kind {
                            if &f.name == access {
                                return f.id.clone();
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    unreachable!()
                }
                VarType::Ref(_) => todo!(),
                VarType::Ptr(_) => todo!(),
                VarType::Deref(_) => todo!(),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn push_id(&mut self, info: SymbolInfo) -> Result<ID, SymbolInfo> {
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
        symbols.insert(info.name.clone(), id.clone());
        if self.symbols.items.contains_key(&id) {
            Err(self.symbols.items[&id].clone())
        } else {
            self.symbols.items.insert(id.clone(), info);
            Ok(id)
        }
    }

    fn expect_auto_err(&mut self, kind: (u32, i64)) -> &Token {
        {
            let as_int = {
                self.tokens
                    .peek()
                    .map(|t| t.kind.as_int())
                    .unwrap_or((u32::MAX, 0))
            };
            if as_int == kind {
                return self.tokens.next().unwrap();
            }
        }
        let res = self.tokens.next();
        if let Some(v) = res {
            let s = v.span.clone();
            let name = v.kind.as_int().0.token_kind_name();
            self.error(
                format!(
                    "Expected token {} but got {}",
                    kind.0.token_kind_name(),
                    name
                ),
                format!(""),
                &s,
            );
            unreachable!()
        } else {
            let s = Span::default();
            self.error(
                format!("Expected token {} but got <eof>", kind.0.token_kind_name()),
                format!(""),
                &s,
            );
            unreachable!()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathSegment {
    // ident: String,
    pub id: ID,
    // args: Option<P<GenericArgs>>,
}

impl PathSegment {
    fn new(id: ID) -> Self {
        Self { id }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Eq for Number {}

#[derive(Debug, Clone, Eq)]
pub struct Function {
    pub params: Vec<AstNode>,
    pub ret_type: VarType,
    pub block: Option<Box<AstNode>>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        if self.params.len() != other.params.len() {
            false
        } else {
            for (i, param) in self.params.iter().enumerate() {
                match &param.kind {
                    AstKind::Parameter(t, _) => match &other.params[i].kind {
                        AstKind::Parameter(ot, _) => {
                            if t != ot {
                                return false;
                            }
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            true
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<AstNode>,
    pub scope: ScopeID,
    pub expected_return: VarType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElseBlock {
    Nothing,
    ElseIf(Box<AstNode>),
    Else(Box<AstNode>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement {
    pub conditional: Box<AstNode>,
    pub block: Box<AstNode>,
    pub else_if: ElseBlock,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub id: ID,
    pub kind: VarType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    pub fields: Vec<AstNode>,
    pub scope: ScopeID,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructLit {
    pub id: ID,
    pub fields: HashMap<String, AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstKind {
    Ident(ID),
    BinOp(Box<AstNode>, BinOp, Box<AstNode>),
    Field(Field),
    Block(Block),
    Function(ID, Rc<Function>),
    FunctionDecl(ID, Rc<Function>),
    StructDecl(ID, Rc<Struct>),
    StructLit(StructLit),
    VarDecl(VarType, ID, Box<AstNode>),
    Return(Box<AstNode>),
    ExprRet(Box<AstNode>),
    If(IfStatement),
    Number(Number),
    String(String),
    Parameter(VarType, ID),
    FunCall(Box<AstNode>, Vec<AstNode>),
    Items(Vec<AstNode>),
    Reference(Box<AstNode>),
    Dereference(Box<AstNode>),
    Pointer(Box<AstNode>),
    ArrayLit(Vec<AstNode>),
    BinOpEq(Box<AstNode>, BinOp, Box<AstNode>),
    Assign(Box<AstNode>, Box<AstNode>),
    Access(Box<AstNode>, String, bool),
    Null,
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNode {
    pub kind: AstKind,
    pub span: Span,
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
        let mut rhs = parse_primary(lex).unwrap();
        lookahead = match lex.tokens.peek() {
            Some(token) => token,
            None => {
                lhs = combine_expr(lhs, op, rhs);
                break;
            }
        };
        while lookahead.kind.precedence() > op.kind.precedence() {
            rhs = parse_expression_1(lex, rhs, op.kind.precedence() + 1).unwrap();
            lookahead = match lex.tokens.peek() {
                Some(token) => token,
                None => return Ok(combine_expr(lhs, op, rhs)),
            };
        }
        lhs = combine_expr(lhs, op, rhs);
    }
    Ok(lhs)
}

fn parse_fn_call<'a>(lex: &'a mut LexVisiter, path: AstNode) -> Result<AstNode, ()> {
    let start_pos = lex
        .expect(|x| matches!(x, TokenKind::OpenDelim(Delimiter::Paren)))
        .unwrap()
        .span
        .from;

    let mut params = Vec::new();

    loop {
        match lex.tokens.peek() {
            Some(t) => match t.kind {
                TokenKind::CloseDelim(Delimiter::Paren) => break,
                _ => params.push(parse_expression(lex).unwrap()),
            },
            None => lex.error(
                "expected parameter but found <eof>".to_string(),
                "".to_string(),
                &Span::null(),
            ),
        }
    }

    let end_pos = lex
        .expect(|x| matches!(x, TokenKind::CloseDelim(Delimiter::Paren)))
        .unwrap()
        .span
        .from;

    Ok(AstNode::new(
        AstKind::FunCall(Box::new(path), params),
        Span::new(start_pos, end_pos),
    ))
}

fn parse_struct_lit<'a>(
    lex: &'a mut LexVisiter,
    mut struct_type: AstNode,
    start_pos: Pos,
) -> Result<AstNode, ()> {
    lex.expect_auto_err(TokenKind::OpenDelim(Delimiter::Paren).as_int());
    let mut fields = HashMap::new();
    loop {
        if matches!(
            lex.tokens.peek().unwrap().kind,
            TokenKind::CloseDelim(Delimiter::Paren)
        ) {
            break;
        }
        let (name, span) = {
            let ident = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
            match &ident.kind {
                TokenKind::Ident(i) => (i.clone(), ident.span.clone()),
                _ => unreachable!(),
            }
        };

        lex.expect_auto_err(TokenKind::Colon.as_int());
        let value = parse_expression(lex).unwrap();
        if matches!(lex.tokens.peek().unwrap().kind, TokenKind::Comma) {
            lex.tokens.next();
        }

        if fields.contains_key(&name) {
            lex.error(
                format!("Struct literal already has field '{name}' filled"),
                "".to_string(),
                &span,
            )
        }
        fields.insert(name, value);
    }
    let to_pos = lex
        .expect_auto_err(TokenKind::CloseDelim(Delimiter::Paren).as_int())
        .span
        .to;
    dbg!(lex.tokens.peek());
    Ok(AstNode::new(
        AstKind::StructLit(StructLit {
            id: lex.get_access_id(&mut struct_type),
            fields,
        }),
        Span::new(start_pos, to_pos),
    ))
}

fn parse_primary<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    match lex.tokens.peek().map(|x| &x.kind) {
        Some(TokenKind::Ident(ident)) => {
            let span = lex.tokens.next().unwrap().span.clone();
            let mut path = parse_access(
                lex,
                AstNode::new(
                    AstKind::Ident(lex.find_id(ident).unwrap_or_else(|| {
                        lex.error(
                            format!("Use of undefined symbol '{ident}'"),
                            "".to_string(),
                            &span,
                        );
                        unreachable!()
                    })),
                    span,
                ),
            )
            .unwrap();

            match lex.tokens.peek().map(|x| &x.kind) {
                Some(TokenKind::OpenDelim(Delimiter::Paren)) => {
                    match lex.get_id(&lex.get_access_id(&mut path)) {
                        Some(info) => {
                            if matches!(info.kind, VarType::Struct(_)) {
                                return parse_struct_lit(lex, path, span.from);
                            } else if matches!(info.kind, VarType::Function(_)) {
                                return parse_fn_call(lex, path);
                            }
                        }
                        None => return parse_fn_call(lex, path),
                    }
                }
                _ => {
                    return Ok(path);
                }
            }
        }
        Some(TokenKind::OpenDelim(Delimiter::Brace)) => {
            return parse_block(lex, true, VarType::Unknown)
        }
        _ => {}
    };
    match lex.tokens.next() {
        Some(tok) => match &tok.kind {
            TokenKind::Keyword(Keyword::Null) => Ok(AstNode::new(AstKind::Null, tok.span)),
            TokenKind::Keyword(Keyword::True) => Ok(AstNode::new(AstKind::Bool(true), tok.span)),
            TokenKind::Keyword(Keyword::False) => Ok(AstNode::new(AstKind::Bool(false), tok.span)),
            TokenKind::Keyword(Keyword::If) => parse_if_expr(lex, tok.span.from),
            TokenKind::Int(i) => Ok(AstNode::new(AstKind::Number(Number::Int(*i)), tok.span)),
            TokenKind::Float(f) => Ok(AstNode::new(AstKind::Number(Number::Float(*f)), tok.span)),
            TokenKind::String(s) => Ok(AstNode::new(AstKind::String(s.clone()), tok.span)),
            TokenKind::FatArrow => Ok(AstNode::new(
                AstKind::ExprRet(Box::new(parse_expression(lex).unwrap())),
                tok.span,
            )),
            TokenKind::BinOpEq(op) => {
                lex.error(
                    format!("Unexpected '{}' found in expression", op.to_string()),
                    "Move this to a seperate statement".to_string(),
                    &tok.span,
                );
                unreachable!()
            }
            TokenKind::OpenDelim(Delimiter::Paren) => {
                let res = parse_expression(lex);
                if let Err(e) = lex.expect(|t| t.is_close_delim(Delimiter::Paren)) {
                    if let Some(t) = e {
                        let span = t.span.clone();
                        lex.error(
                            "Expected ')'".to_string(),
                            "Put ')' here".to_string(),
                            &span,
                        )
                    }
                }

                parse_access(lex, res.unwrap())
            }
            TokenKind::OpenDelim(Delimiter::Bracket) => {
                let start_pos = tok.span.from;
                let mut elements = Vec::new();
                let end_pos;
                loop {
                    elements.push(parse_expression(lex).unwrap());
                    if matches!(lex.tokens.peek().unwrap().kind, TokenKind::Comma) {
                        lex.tokens.next();
                    }
                    if matches!(
                        lex.tokens.peek().unwrap().kind,
                        TokenKind::CloseDelim(Delimiter::Bracket)
                    ) {
                        end_pos = lex.tokens.next().unwrap().span.to;
                        break;
                    }
                }

                Ok(AstNode::new(
                    AstKind::ArrayLit(elements),
                    Span::new(start_pos, end_pos),
                ))
            }
            TokenKind::BinOp(BinOp::BitAnd) => Ok(AstNode::new(
                AstKind::Reference(Box::new(parse_primary(lex).unwrap())),
                tok.span,
            )),
            TokenKind::BinOp(BinOp::Mul) => Ok(AstNode::new(
                AstKind::Dereference(Box::new(parse_primary(lex).unwrap())),
                tok.span,
            )),
            TokenKind::BinOp(BinOp::Ptr) => Ok(AstNode::new(
                AstKind::Pointer(Box::new(parse_primary(lex).unwrap())),
                tok.span,
            )),
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
    let primary = parse_primary(lex).unwrap();
    parse_expression_1(lex, primary, 0)
}

fn parse_var_decl<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let from_pos = lex.tokens.peek().unwrap().span.from;
    let kind = parse_type(lex).unwrap();
    parse_var_decl_of_type(lex, kind, from_pos)
}

fn parse_var_decl_of_type<'a>(
    lex: &'a mut LexVisiter,
    kind: VarType,
    from_pos: Pos,
) -> Result<AstNode, ()> {
    let ident_token = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
    let ident = match &ident_token.kind {
        TokenKind::Ident(name) => name.clone(),
        _ => panic!("this should be a ident"),
    };

    let constant = match lex.tokens.next() {
        Some(t) => match &t.kind {
            TokenKind::Colon => true,
            TokenKind::Assign => false,
            e => {
                lex.error(
                    format!("Expected '=' or ':' but found {e:?}"),
                    "".to_string(),
                    &t.span,
                );
                unreachable!()
            }
        },
        None => {
            lex.error(
                "Expected '=' but found <eof>".to_string(),
                "".to_string(),
                &Span::null(),
            );
            unreachable!()
        }
    };

    let no_semi_needed = match lex.tokens.peek().map(|x| &x.kind) {
        Some(TokenKind::Keyword(Keyword::If)) => true,
        Some(TokenKind::OpenDelim(Delimiter::Brace)) => true,
        _ => false,
    };

    let value = parse_expression(lex).unwrap();

    if !no_semi_needed {
        lex.expect_auto_err(TokenKind::Semi.as_int());
    }

    let span = Span::new(from_pos, value.span.to);

    let id = lex.push_id(SymbolInfo {
        kind: kind.clone(),
        name: ident.clone(),
        constant,
    });
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
    let start_pos = lex.tokens.peek().unwrap().span.from;
    let paren_type = match parse_type(lex) {
        Ok(t) => t,
        Err(_) => {
            let span = Span::new(start_pos, lex.tokens.peek().unwrap().span.to);
            lex.error(
                "Unknown type of parameter".to_string(),
                "".to_string(),
                &span,
            );
            unreachable!()
        }
    };
    let span;
    let ident = {
        let ident_token = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
        match &ident_token.kind {
            TokenKind::Ident(name) => {
                span = Span::new(start_pos, ident_token.span.to);
                name.clone()
            }
            _ => unreachable!(),
        }
    };
    let id = lex
        .push_id(SymbolInfo {
            kind: paren_type.clone(),
            name: ident,
            constant: false,
        })
        .map_err(|_| ())
        .unwrap();
    Ok(AstNode::new(AstKind::Parameter(paren_type, id), span))
}

fn parse_type_inner<'a>(lex: &'a mut LexVisiter) -> Result<VarType, ()> {
    match lex.tokens.peek() {
        Some(t) => match &t.kind {
            TokenKind::Ident(name) => Ok(VarType::Struct(
                match &lex
                    .get_id(&lex.find_id(name).ok_or(()).unwrap())
                    .unwrap()
                    .kind
                {
                    VarType::Struct(s) => s.clone(),
                    _ => todo!(),
                },
            )),
            TokenKind::Keyword(Keyword::Var) => Ok(VarType::Unknown),
            TokenKind::Keyword(Keyword::Int) => Ok(VarType::Int),
            TokenKind::Keyword(Keyword::Float) => Ok(VarType::Float),
            TokenKind::Keyword(Keyword::String) => Ok(VarType::String),
            TokenKind::Keyword(Keyword::Bool) => Ok(VarType::Bool),
            _ => Err(()),
        },
        None => {
            lex.error(
                "Expected type but found <eof>".to_owned(),
                "".to_owned(),
                &Span::null(),
            );
            unreachable!()
        }
    }
}

fn parse_type<'a>(lex: &'a mut LexVisiter) -> Result<VarType, ()> {
    let mut var_type = parse_type_inner(lex).unwrap();
    lex.tokens.next();
    loop {
        match lex.tokens.peek().map(|x| &x.kind) {
            Some(k) => match k {
                TokenKind::BinOp(BinOp::BitAnd) => {
                    lex.tokens.next();
                    var_type = VarType::Ref(Box::new(var_type));
                }
                TokenKind::BinOp(BinOp::Ptr) => {
                    lex.tokens.next();
                    var_type = VarType::Ptr(Box::new(var_type));
                }
                TokenKind::BinOp(BinOp::Mul) => {
                    lex.tokens.next();
                    var_type = VarType::Deref(Box::new(var_type));
                }
                TokenKind::OpenDelim(Delimiter::Bracket) => {
                    lex.tokens.next();
                    let size = match &lex.tokens.peek() {
                        Some(t) => match &t.kind {
                            TokenKind::CloseDelim(_) => ArraySize::Unknown,
                            TokenKind::Keyword(Keyword::Dynamic) => {
                                lex.tokens.next();
                                ArraySize::Dynamic
                            }
                            _ => ArraySize::Fixed(Box::new(parse_expression(lex).unwrap())),
                        },
                        None => {
                            lex.error(
                                "Unexpected end of token stream".to_owned(),
                                "".to_string(),
                                &Span::null(),
                            );
                            unreachable!()
                        }
                    };
                    var_type = VarType::Array(Array {
                        var_type: Box::new(var_type),
                        size,
                    });
                    lex.expect_auto_err(TokenKind::CloseDelim(Delimiter::Bracket).as_int());
                }
                _ => {
                    break;
                }
            },
            _ => break,
        }
    }
    Ok(var_type)
}

fn parse_block<'a>(
    lex: &'a mut LexVisiter,
    new_scope: bool,
    ret_type: VarType,
) -> Result<AstNode, ()> {
    match lex.tokens.peek().unwrap().kind {
        TokenKind::FatArrow => {
            return parse_inline_block(lex, ret_type);
        }
        _ => {}
    }

    let mut stmts = Vec::new();

    let scope;
    if new_scope {
        scope = lex.push_scope();
    } else {
        scope = lex.current_scope;
    }

    let start_pos = lex
        .expect_auto_err(TokenKind::OpenDelim(Delimiter::Brace).as_int())
        .span
        .from;

    while !matches!(
        lex.tokens.peek().map(|x| &x.kind),
        Some(TokenKind::CloseDelim(Delimiter::Brace))
    ) {
        stmts.push(parse_stmt(lex).unwrap())
    }

    lex.pop_scope();

    let end_pos = lex
        .expect_auto_err(TokenKind::CloseDelim(Delimiter::Brace).as_int())
        .span
        .to;

    let block = Block {
        stmts,
        scope,
        expected_return: ret_type,
    };

    Ok(AstNode::new(
        AstKind::Block(block),
        Span::new(start_pos, end_pos),
    ))
}

fn parse_inline_block<'a>(lex: &'a mut LexVisiter, ret_type: VarType) -> Result<AstNode, ()> {
    let scope = lex.push_scope();
    let expr = parse_expression(lex).unwrap();
    let semi = lex.expect_auto_err(TokenKind::Semi.as_int());

    let span = Span::new(expr.span.from, semi.span.to);

    let block = AstNode::new(
        AstKind::Block(Block {
            stmts: vec![expr],
            scope,
            expected_return: ret_type,
        }),
        span,
    );
    lex.pop_scope();
    Ok(block)
}

fn parse_fn<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let fn_start_pos = lex
        .expect_auto_err(TokenKind::Keyword(Keyword::Function).as_int())
        .span
        .from;

    let ident = {
        let ident_token = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
        match &ident_token.kind {
            TokenKind::Ident(name) => name.clone(),
            _ => panic!("this should be a ident"),
        }
    };

    let id = lex.push_id(SymbolInfo {
        kind: VarType::Unknown,
        name: ident.clone(),
        constant: true,
    });

    let mut parameters = Vec::new();

    lex.push_scope();

    lex.expect(|x| matches!(x, TokenKind::OpenDelim(Delimiter::Paren)))
        .unwrap();

    while !matches!(
        lex.tokens.peek().map(|x| &x.kind),
        Some(TokenKind::CloseDelim(Delimiter::Paren))
    ) {
        parameters.push(parse_fn_parameter(lex).unwrap());
        let (kind, span) = lex.tokens.peek().map(|x| (&x.kind, x.span)).unwrap();
        match kind {
            TokenKind::Comma => {
                lex.tokens.next();
            }
            TokenKind::CloseDelim(Delimiter::Paren) => break,
            t => lex.error(
                format!("Expected comma or end of parameter list but found {t:?}"),
                "put ',' here".to_string(),
                &span,
            ),
        }
    }

    lex.expect(|x| matches!(x, TokenKind::CloseDelim(Delimiter::Paren)))
        .unwrap();

    let kind = {
        let t = parse_type(lex);
        if t.is_err() {
            VarType::Void
        } else {
            t.unwrap()
        }
    };

    match lex.tokens.peek().map(|x| &x.kind) {
        Some(TokenKind::OpenDelim(Delimiter::Brace)) | Some(TokenKind::FatArrow) => {
            let block = parse_block(lex, false, kind.clone()).unwrap();
            let span = Span::new(fn_start_pos, block.span.to);

            let func = Rc::new(Function {
                params: parameters,
                ret_type: kind,
                block: Some(Box::new(block)),
            });

            *lex.get_mut_id(id.as_ref().unwrap()).unwrap() = SymbolInfo {
                kind: VarType::Function(func.clone()),
                name: ident.clone(),
                constant: true,
            };

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
        Some(TokenKind::Semi) => {
            let span = lex.tokens.next().unwrap().span;
            let span = Span::new(fn_start_pos, span.to);
            let func = Rc::new(Function {
                params: parameters,
                ret_type: kind,
                block: None,
            });
            *lex.get_mut_id(id.as_ref().unwrap()).unwrap() = SymbolInfo {
                kind: VarType::Function(func.clone()),
                name: ident.clone(),
                constant: true,
            };
            Ok(AstNode::new(AstKind::FunctionDecl(id.unwrap(), func), span))
        }

        None => {
            lex.error(
                "expected function declaration but found <eof>".to_string(),
                String::new(),
                &Span::null(),
            );
            unreachable!()
        }
        t => {
            let span = lex.tokens.next().unwrap().span;
            lex.error(
                format!("expected function declaration but found {t:?}"),
                String::new(),
                &span,
            );
            unreachable!()
        }
    }
}
fn parse_if_stmt<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let start_pos = lex
        .expect_auto_err(TokenKind::Keyword(Keyword::If).as_int())
        .span
        .from;
    parse_if_expr(lex, start_pos)
}

fn parse_if_expr<'a>(lex: &'a mut LexVisiter, start_pos: Pos) -> Result<AstNode, ()> {
    let conditional = Box::new(dbg!(parse_expression(lex)).unwrap());
    dbg!(lex.tokens.peek());
    let block = Box::new(parse_block(lex, true, VarType::Void).unwrap());
    let else_if = match lex.tokens.peek().map(|x| &x.kind) {
        Some(TokenKind::Keyword(Keyword::Else)) => {
            lex.tokens.next();
            if matches!(
                lex.tokens.peek().unwrap().kind,
                TokenKind::Keyword(Keyword::If)
            ) {
                ElseBlock::ElseIf(Box::new(parse_if_stmt(lex).unwrap()))
            } else {
                ElseBlock::Else(Box::new(parse_block(lex, true, VarType::Void).unwrap()))
            }
        }
        _ => ElseBlock::Nothing,
    };

    let end_pos = lex.tokens.peek().unwrap().span.to; // todo: fix this hack
    let span = Span::new(start_pos, end_pos);

    Ok(AstNode::new(
        AstKind::If(IfStatement {
            conditional,
            block,
            else_if,
        }),
        span,
    ))
}

fn parse_return_stmt<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let start_pos = lex
        .expect(|x| matches!(x, TokenKind::Keyword(Keyword::Return)))
        .unwrap()
        .span
        .from;

    let value = parse_expression(lex).unwrap();

    let end_pos = lex.expect_auto_err(TokenKind::Semi.as_int()).span.from;

    let span = Span::new(start_pos, end_pos);
    Ok(AstNode::new(AstKind::Return(Box::new(value)), span))
}

fn parse_access<'a>(lex: &'a mut LexVisiter, lhs: AstNode) -> Result<AstNode, ()> {
    let mut res = lhs;
    while matches!(lex.tokens.peek().map(|x| &x.kind), Some(TokenKind::Dot)) {
        lex.expect_auto_err(TokenKind::Dot.as_int());
        let ident = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
        let name = match &ident.kind {
            TokenKind::Ident(i) => i.clone(),
            _ => unreachable!(),
        };
        let span = Span::new(res.span.from, ident.span.to);
        let deref = match var_type_of(&mut res, lex, VarType::Any) {
            VarType::Ref(_) => true,
            VarType::Ptr(_) => true,
            _ => false,
        };
        res = AstNode::new(AstKind::Access(Box::new(res), name, deref), span);
    }
    Ok(res)
}

fn parse_stmt<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    match &as_result(lex.tokens.peek()).unwrap().kind {
        t if t.is_type() => parse_var_decl(lex),
        TokenKind::Keyword(Keyword::Function) => parse_fn(lex),
        TokenKind::Keyword(Keyword::Return) => parse_return_stmt(lex),
        TokenKind::Keyword(Keyword::If) => parse_if_stmt(lex),
        TokenKind::Semi => {
            lex.tokens.next();
            parse_stmt(lex)
        }
        TokenKind::Ident(outer_ident) => {
            let start_pos = lex.tokens.peek().unwrap().span.from;
            let path = parse_expression(lex).unwrap();
            let next = as_result(lex.tokens.peek()).unwrap();
            let res;
            match &next.kind {
                TokenKind::BinOpEq(op) => {
                    lex.tokens.next();
                    let expr = parse_expression(lex).unwrap();
                    let span = Span::new(start_pos, expr.span.to);
                    res = Ok(AstNode::new(
                        AstKind::BinOpEq(Box::new(path), *op, Box::new(expr)),
                        span,
                    ));
                }
                TokenKind::Assign => {
                    lex.tokens.next();
                    let expr = parse_expression(lex).unwrap();
                    let span = Span::new(start_pos, expr.span.to);
                    res = Ok(AstNode::new(
                        AstKind::Assign(Box::new(path), Box::new(expr)),
                        span,
                    ));
                }
                TokenKind::OpenDelim(Delimiter::Paren) => res = parse_fn_call(lex, path),
                TokenKind::Ident(_) => {
                    let span = Span::new(start_pos, next.span.to);
                    let id = lex.find_id(&outer_ident);
                    if id.is_none() {
                        lex.error(
                            format!("Use of undefined type '{outer_ident}'"),
                            "".to_string(),
                            &span,
                        );
                        unreachable!()
                    }
                    let s = match &lex.get_id(&id.unwrap()).unwrap().kind {
                        VarType::Struct(s) => s.clone(),
                        _ => todo!(),
                    };
                    let kind = VarType::Struct(s);
                    return parse_var_decl_of_type(lex, kind, start_pos);
                }
                t => {
                    let span = next.span.clone();
                    lex.error(
                        format!("Expected statement but found {t:?}"),
                        "".to_string(),
                        &span,
                    );
                    res = Err(())
                }
            }
            lex.expect_auto_err(TokenKind::Semi.as_int());
            res
        }
        TokenKind::OpenDelim(Delimiter::Brace) => parse_block(lex, true, VarType::Void),
        _ => {
            let res = parse_expression(lex);
            lex.expect_auto_err(TokenKind::Semi.as_int());
            res
        }
    }
}

fn parse_struct<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let from_pos = lex
        .expect_auto_err(TokenKind::Keyword(Keyword::Struct).as_int())
        .span
        .from;

    let ident = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
    let name = match &ident.kind {
        TokenKind::Ident(i) => i.clone(),
        _ => unreachable!(),
    };
    lex.expect_auto_err(TokenKind::OpenDelim(Delimiter::Brace).as_int());

    let mut fields = Vec::new();

    let scope_id = lex.push_scope();
    loop {
        if matches!(
            lex.tokens.peek().map(|x| &x.kind),
            Some(TokenKind::CloseDelim(Delimiter::Brace))
        ) {
            break;
        }

        let from_pos = lex.tokens.peek().unwrap().span.from;
        let kind = parse_type(lex).unwrap();
        let ident = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
        let name = match &ident.kind {
            TokenKind::Ident(i) => i.clone(),
            _ => unreachable!(),
        };

        let span = Span::new(from_pos, ident.span.to);
        let id = lex
            .push_id(SymbolInfo {
                kind: kind.clone(),
                name: name.clone(),
                constant: false,
            })
            .unwrap_or_else(|_| {
                lex.error(
                    format!("struct already has a field of name '{name}'"),
                    "".to_string(),
                    &span,
                );
                unreachable!()
            });
        fields.push(AstNode::new(AstKind::Field(Field { kind, id, name }), span));

        lex.expect_auto_err(TokenKind::Semi.as_int());
    }
    lex.pop_scope();

    let to_pos = lex
        .expect_auto_err(TokenKind::CloseDelim(Delimiter::Brace).as_int())
        .span
        .to;

    let span = Span::new(from_pos, to_pos);
    let struct_type = Rc::new(Struct {
        fields,
        scope: scope_id,
        name: name.clone(),
    });

    let id = lex
        .push_id(SymbolInfo {
            kind: VarType::Struct(struct_type.clone()),
            name,
            constant: true,
        })
        .unwrap();

    Ok(AstNode::new(AstKind::StructDecl(id, struct_type), span))
}

pub fn parse_items<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let mut items = Vec::new();
    while lex.tokens.peek().is_some() {
        match &as_result(lex.tokens.peek()).unwrap().kind {
            TokenKind::Keyword(Keyword::Function) => items.push(parse_fn(lex).unwrap()),
            TokenKind::Keyword(Keyword::Struct) => items.push(parse_struct(lex).unwrap()),
            t if t.is_type() => items.push(parse_var_decl(lex).unwrap()),
            t => {
                let span = lex.tokens.peek().unwrap().span;
                lex.error(
                    format!("Expected item but found {t:?}"),
                    "".to_owned(),
                    &span,
                );
            }
        }
    }
    Ok(AstNode::new(AstKind::Items(items), Span::null()))
}
