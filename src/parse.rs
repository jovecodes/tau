use crate::lex::{BinOp, Delimiter, Keyword, Lex, Token, TokenKind};
use crate::pos::{Pos, Span};

use crate::analysis::var_type_of;
use crate::{compile_time_interpret, Value};
use crate::{error::Log, lex::TokenKindName};
use core::panic;
use std::cell::RefCell;
use std::fmt::Display;
use std::{collections::HashMap, rc::Rc};

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
    pub items: HashMap<ScopeID, Scope>,
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
    Fixed(i64),
    Dynamic,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub var_type: Box<VarType>,
    pub size: ArraySize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarTypeKind {
    Unknown,
    Any,
    Void,
    Int,
    Float,
    String,
    Bool,
    Function(Rc<RefCell<Function>>),
    Struct(Rc<Struct>),
    Ref(Box<VarType>),
    Ptr(Box<VarType>),
    Deref(Box<VarType>),
    Array(Array),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarType {
    pub kind: VarTypeKind,
    pub constant: bool,
    // static: bool,
}

impl VarTypeKind {
    fn array_dimension(&self) -> i64 {
        match self {
            VarTypeKind::Array(inner) => 1 + inner.var_type.as_ref().kind.array_dimension(),
            _ => 0,
        }
    }
}

impl Display for ArraySize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArraySize::Fixed(s) => write!(f, "[{s}]"),
            ArraySize::Dynamic => write!(f, "[dyn]"),
            ArraySize::Unknown => write!(f, "[]"),
        }
    }
}

impl Display for VarTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarTypeKind::Function(_) => write!(f, "SomeFunction"),
            VarTypeKind::Struct(s) => write!(f, "{}", s.name),
            VarTypeKind::Ref(inner) => write!(f, "&{inner}"),
            VarTypeKind::Ptr(inner) => write!(f, "@{inner}"),
            VarTypeKind::Deref(inner) => write!(f, "*{inner}"),
            VarTypeKind::Array(arr) => {
                write!(f, "{}{}", arr.var_type, arr.size)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            VarTypeKind::Ref(_) => write!(f, "{}", self.kind),
            VarTypeKind::Ptr(_) => write!(f, "{}", self.kind),
            _ => {
                if self.constant {
                    write!(f, "const {}", self.kind)
                } else {
                    write!(f, "{}", self.kind)
                }
            }
        }
    }
}

impl VarTypeKind {
    pub fn loosy_eq(self, other: VarTypeKind) -> bool {
        match &self {
            VarTypeKind::Any => return true,
            VarTypeKind::Array(arr) => match &other {
                VarTypeKind::Array(other_arr) => {
                    if !arr.var_type.clone().loosy_eq(*other_arr.var_type.clone()) {
                        return false;
                    }
                    match arr.size {
                        ArraySize::Fixed(s) => match other_arr.size {
                            ArraySize::Fixed(other_s) => return s == other_s,
                            ArraySize::Dynamic => return false,
                            ArraySize::Unknown => return true,
                        },
                        ArraySize::Dynamic => return true,
                        ArraySize::Unknown => return true,
                    }
                }
                _ => return false,
            },
            _ => {}
        }
        if matches!(other, VarTypeKind::Any) {
            return true;
        }
        self == other
    }
}

impl VarType {
    pub fn new(kind: VarTypeKind, constant: bool) -> Self {
        Self { kind, constant }
    }

    pub fn loosy_eq(self, other: VarType) -> bool {
        self.kind.loosy_eq(other.kind)
    }
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub kind: VarType,
    pub name: String,
    pub immutable: bool,
    pub constant_value: Option<Value>,
}

#[derive(Debug)]
pub struct SymbolTable {
    pub items: HashMap<ID, SymbolInfo>,
}

fn as_result<T>(o: Option<T>) -> Result<T, ()> {
    match o {
        Some(value) => Ok(value),
        None => Err(()),
    }
}

#[derive(Debug)]
pub struct LexVisitor {
    pub lexs: Vec<Lex>,
    pub current_lex: usize,
    pub symbols: SymbolTable,
    pub scopes: ScopeTable,
    pub current_scope: ScopeID,
    pub global_scope: ScopeID,
    pub modules: HashMap<String, ScopeID>,
}

impl LexVisitor {
    pub fn new(lex: crate::lex::Lex, target_language: String) -> LexVisitor {
        let mut vister = LexVisitor {
            lexs: vec![lex],
            current_lex: 0,
            symbols: SymbolTable {
                items: HashMap::new(),
            },
            scopes: ScopeTable {
                items: HashMap::new(),
            },
            current_scope: 0,
            global_scope: 0,
            modules: HashMap::new(),
        };

        vister.scopes.items.insert(
            vister.global_scope,
            Scope {
                id: vister.current_scope,
                parent: None,
                symbols: HashMap::new(),
                children: Vec::new(),
            },
        );

        vister
            .push_id(SymbolInfo {
                kind: VarType::new(
                    VarTypeKind::Function(Rc::new(RefCell::new(Function {
                        params: vec![AstNode::new(
                            AstKind::Parameter(VarType::new(VarTypeKind::String, true), ID::null()),
                            Span::null(),
                        )],
                        ret_type: VarType::new(VarTypeKind::Void, true),
                        block: None,
                        is_macro: true,
                    }))),
                    true,
                ),
                name: "asm".to_string(),
                immutable: true,
                constant_value: None,
            })
            .unwrap();

        // vister
        //     .push_id(SymbolInfo {
        //         kind: VarType::Function(Rc::new(RefCell::new(Function {
        //             params: vec![AstNode::new(
        //                 AstKind::Parameter(VarType::String, ID::null()),
        //                 Span::null(),
        //             )],
        //             ret_type: VarType::new(VarTypeKind::Void, true),
        //             block: None,
        //             is_macro: false,
        //         }))),
        //         name: "print".to_string(),
        //         constant: true,
        //         constant_value: None,
        //     })
        //     .unwrap();
        //
        // vister
        //     .push_id(SymbolInfo {
        //         kind: VarType::Function(Rc::new(RefCell::new(Function {
        //             params: vec![AstNode::new(
        //                 AstKind::Parameter(VarType::String, ID::null()),
        //                 Span::null(),
        //             )],
        //             ret_type: VarType::new(VarTypeKind::Void, true),
        //             block: None,
        //             is_macro: false,
        //         }))),
        //         name: "println".to_string(),
        //         constant: true,
        //         constant_value: None,
        //     })
        //     .unwrap();

        vister
            .push_id(SymbolInfo {
                kind: VarType::new(VarTypeKind::String, true),
                name: "__TARGET_LANGUAGE__".to_string(),
                immutable: true,
                constant_value: Some(Value::String(target_language)),
            })
            .unwrap();

        vister
    }

    fn next(&mut self) -> Option<Token> {
        self.lexs[self.current_lex].tokens.pop_front()
    }

    fn peek(&self) -> Option<&Token> {
        self.lexs[self.current_lex].tokens.get(0)
    }

    fn expect(&mut self, valid: impl Fn(&TokenKind) -> bool) -> Result<Token, Option<Token>> {
        match self.next() {
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
            let span = self.peek().map(|x| x.span).unwrap_or(Span::null());
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
            AstKind::Access(lhs, access, _) => {
                match var_type_of(lhs, self, VarType::new(VarTypeKind::Any, true)).kind {
                    VarTypeKind::Struct(s) => {
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
                    VarTypeKind::Ref(_) => todo!(),
                    VarTypeKind::Ptr(_) => todo!(),
                    VarTypeKind::Deref(_) => todo!(),
                    _ => todo!(),
                }
            }
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

    fn expect_auto_err(&mut self, kind: (u32, i64)) -> Token {
        {
            let as_int = {
                self.peek()
                    .map(|t| t.kind.as_int())
                    .unwrap_or((u32::MAX, 0))
            };
            if as_int == kind {
                return self.next().unwrap();
            }
        }
        let res = self.next();
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
            )
        } else {
            let s = Span::default();
            self.error(
                format!("Expected token {} but got <eof>", kind.0.token_kind_name()),
                format!(""),
                &s,
            )
        }
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
    pub is_macro: bool,
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
    pub is_macro: bool,
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
    pub fields: Vec<(String, AstNode, Span)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignVal {
    Ast(Box<AstNode>),
    Value(Value, Span),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstKind {
    Ident(ID),
    BinOp(Box<AstNode>, BinOp, Box<AstNode>),
    Field(Field),
    Block(Block),
    Function(ID, Rc<RefCell<Function>>),
    FunctionDecl(ID, Rc<RefCell<Function>>),
    StructDecl(ID, Rc<Struct>),
    StructLit(StructLit),
    VarDecl(VarType, ID, AssignVal),
    Return(Box<AstNode>),
    ExprRet(Box<AstNode>),
    If(IfStatement),
    Number(Number),
    String(String),
    Parameter(VarType, ID),
    FunCall(Box<AstNode>, Vec<AstNode>, bool),
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
    pub fn new(kind: AstKind, span: Span) -> Self {
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

fn parse_expression_1(
    lex: &mut LexVisitor,
    mut lhs: AstNode,
    min_prec: i32,
) -> Result<AstNode, ()> {
    let mut lookahead = match lex.peek() {
        Some(token) => token.clone(),
        None => return Ok(lhs),
    };

    while lookahead.kind.precedence() >= min_prec {
        let op = lookahead;
        lex.next();
        let mut rhs = parse_primary(lex).unwrap();
        lookahead = match lex.peek() {
            Some(token) => token.clone(),
            None => {
                lhs = combine_expr(lhs, &op, rhs);
                break;
            }
        };
        while lookahead.kind.precedence() > op.kind.precedence() {
            rhs = parse_expression_1(lex, rhs, op.kind.precedence() + 1).unwrap();
            lookahead = match lex.peek() {
                Some(token) => token.clone(),
                None => return Ok(combine_expr(lhs, &op, rhs)),
            };
        }
        lhs = combine_expr(lhs, &op, rhs);
    }
    Ok(lhs)
}

fn parse_fn_call(lex: &mut LexVisitor, path: AstNode, is_macro: bool) -> Result<AstNode, ()> {
    let start_pos = lex
        .expect_auto_err(TokenKind::OpenDelim(Delimiter::Paren).as_int())
        .span
        .from;

    let mut params = Vec::new();

    loop {
        match lex.peek() {
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
        AstKind::FunCall(Box::new(path), params, is_macro),
        Span::new(start_pos, end_pos),
    ))
}

fn parse_struct_lit(
    lex: &mut LexVisitor,
    mut struct_type: AstNode,
    start_pos: Pos,
) -> Result<AstNode, ()> {
    lex.expect_auto_err(TokenKind::OpenDelim(Delimiter::Paren).as_int());
    let mut fields = Vec::new();
    loop {
        if matches!(
            lex.peek().unwrap().kind,
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
        if matches!(lex.peek().unwrap().kind, TokenKind::Comma) {
            lex.next();
        }

        fields.push((name, value, span));
    }
    let to_pos = lex
        .expect_auto_err(TokenKind::CloseDelim(Delimiter::Paren).as_int())
        .span
        .to;
    Ok(AstNode::new(
        AstKind::StructLit(StructLit {
            id: lex.get_access_id(&mut struct_type),
            fields,
        }),
        Span::new(start_pos, to_pos),
    ))
}

fn parse_primary(lex: &mut LexVisitor) -> Result<AstNode, ()> {
    let is_macro = matches!(lex.peek().map(|x| &x.kind), Some(TokenKind::Hash));
    if is_macro {
        lex.next();
    }
    parse_primary_with_is_macro(lex, is_macro)
}

fn parse_primary_with_is_macro(lex: &mut LexVisitor, is_macro: bool) -> Result<AstNode, ()> {
    let kind = lex.peek().map(|x| x.kind.clone());
    match kind {
        Some(TokenKind::Ident(ident)) => {
            let span = lex.next().unwrap().span.clone();
            let mut path = parse_access(
                lex,
                AstNode::new(
                    AstKind::Ident(lex.find_id(&ident).unwrap_or_else(|| {
                        lex.error(
                            format!("Use of undefined symbol '{ident}'"),
                            "".to_string(),
                            &span,
                        )
                    })),
                    span,
                ),
            )
            .unwrap();

            match lex.peek().map(|x| &x.kind) {
                Some(TokenKind::OpenDelim(Delimiter::Paren)) => {
                    match lex.get_id(&lex.get_access_id(&mut path)) {
                        Some(info) => {
                            if matches!(info.kind.kind, VarTypeKind::Struct(_)) {
                                if is_macro {
                                    let span = lex.peek().unwrap().span;
                                    lex.error(
                                        format!("Undefined use of macro '{ident}'"),
                                        "".to_string(),
                                        &span,
                                    )
                                }
                                return parse_struct_lit(lex, path, span.from);
                            } else if matches!(info.kind.kind, VarTypeKind::Function(_)) {
                                return parse_fn_call(lex, path, is_macro);
                            } else {
                                lex.error(
                                    format!(
                                        "Unexpected function call like invocation of '{ident}'"
                                    ),
                                    "".to_string(),
                                    &span,
                                );
                            }
                        }
                        None => return parse_fn_call(lex, path, is_macro),
                    }
                }
                _ => {
                    return Ok(path);
                }
            }
        }
        Some(TokenKind::OpenDelim(Delimiter::Brace)) => {
            return parse_block(lex, true, VarType::new(VarTypeKind::Unknown, true))
        }
        _ => {}
    };
    match lex.next() {
        Some(tok) => match &tok.kind {
            TokenKind::Keyword(Keyword::Null) => Ok(AstNode::new(AstKind::Null, tok.span)),
            TokenKind::Keyword(Keyword::True) => Ok(AstNode::new(AstKind::Bool(true), tok.span)),
            TokenKind::Keyword(Keyword::False) => Ok(AstNode::new(AstKind::Bool(false), tok.span)),
            TokenKind::Keyword(Keyword::If) => parse_if_expr(lex, is_macro, tok.span.from),
            TokenKind::Int(i) => Ok(AstNode::new(AstKind::Number(Number::Int(*i)), tok.span)),
            TokenKind::Float(f) => Ok(AstNode::new(AstKind::Number(Number::Float(*f)), tok.span)),
            TokenKind::String(s) => Ok(AstNode::new(AstKind::String(s.clone()), tok.span)),
            TokenKind::FatArrow => Ok(AstNode::new(
                AstKind::ExprRet(Box::new(parse_expression(lex).unwrap())),
                tok.span,
            )),
            TokenKind::BinOpEq(op) => lex.error(
                format!("Unexpected '{}' found in expression", op.to_string()),
                "Move this to a seperate statement".to_string(),
                &tok.span,
            ),
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
                    if !matches!(
                        lex.peek().unwrap().kind,
                        TokenKind::CloseDelim(Delimiter::Bracket)
                    ) {
                        lex.expect_auto_err(TokenKind::Comma.as_int());
                    }
                    if matches!(
                        lex.peek().unwrap().kind,
                        TokenKind::CloseDelim(Delimiter::Bracket)
                    ) {
                        end_pos = lex.next().unwrap().span.to;
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
            t => lex.error(
                format!("Could not parse token: {t:?} as primary node"),
                "This is a bug in the compiler not your code".to_string(),
                &tok.span,
            ),
        },
        None => lex.error(
            format!("Expected primary expression but found <eof>"),
            "This is an invalid end of a program".to_string(),
            &Span::default(),
        ),
    }
}

fn parse_expression(lex: &mut LexVisitor) -> Result<AstNode, ()> {
    let primary = parse_primary(lex).unwrap();
    parse_expression_1(lex, primary, 0)
}

fn parse_var_decl(lex: &mut LexVisitor) -> Result<AstNode, ()> {
    let from_pos = lex.peek().unwrap().span.from;
    let kind = parse_type(lex).unwrap();
    parse_var_decl_of_type(lex, kind, from_pos)
}

fn parse_var_decl_of_type(
    lex: &mut LexVisitor,
    kind: VarType,
    from_pos: Pos,
) -> Result<AstNode, ()> {
    let ident_token = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
    let ident = match &ident_token.kind {
        TokenKind::Ident(name) => name.clone(),
        _ => panic!("this should be a ident"),
    };

    let immutable = match lex.next() {
        Some(t) => match &t.kind {
            TokenKind::Colon => true,
            TokenKind::Assign => false,
            e => lex.error(
                format!("Expected '=' or ':' but found {e:?}"),
                "".to_string(),
                &t.span,
            ),
        },
        None => lex.error(
            "Expected '=' but found <eof>".to_string(),
            "".to_string(),
            &Span::null(),
        ),
    } || kind.constant;

    let no_semi_needed = match lex.peek().map(|x| &x.kind) {
        Some(TokenKind::Keyword(Keyword::If)) => true,
        Some(TokenKind::OpenDelim(Delimiter::Brace)) => true,
        _ => false,
    };

    // let dimension = kind.kind.array_dimension();
    // if dimension > 0 {
    //     todo!("array parsing");
    // }
    let mut value = parse_expression(lex).unwrap();

    if !no_semi_needed {
        lex.expect_auto_err(TokenKind::Semi.as_int());
    }

    let span = Span::new(from_pos, value.span.to);

    let constant_value = if immutable {
        Some(compile_time_interpret(&mut value, lex))
    } else {
        None
    };
    let id = lex.push_id(SymbolInfo {
        kind: kind.clone(),
        name: ident.clone(),
        immutable,
        constant_value: constant_value.clone(),
    });
    if let Err(_) = id {
        lex.error(
            format!("Redefining variable {}", &ident),
            format!("Change this variables name"),
            &span,
        )
    } else {
        if let Some(constval) = constant_value {
            Ok(AstNode::new(
                AstKind::VarDecl(kind, id.unwrap(), AssignVal::Value(constval, span.clone())),
                span,
            ))
        } else {
            Ok(AstNode::new(
                AstKind::VarDecl(kind, id.unwrap(), AssignVal::Ast(Box::new(value))),
                span,
            ))
        }
    }
}

fn parse_fn_parameter(lex: &mut LexVisitor) -> Result<AstNode, ()> {
    let start_pos = lex.peek().unwrap().span.from;
    let paren_type = match parse_type(lex) {
        Ok(t) => t,
        Err(_) => {
            let span = Span::new(start_pos, lex.peek().unwrap().span.to);
            lex.error(
                "Unknown type of parameter".to_string(),
                "".to_string(),
                &span,
            )
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
            immutable: false,
            constant_value: None,
        })
        .map_err(|_| ())
        .unwrap();
    Ok(AstNode::new(AstKind::Parameter(paren_type, id), span))
}

fn parse_type_inner(lex: &mut LexVisitor) -> Result<VarType, ()> {
    match lex.peek() {
        Some(t) => match &t.kind {
            TokenKind::Ident(name) => Ok(VarType::new(
                VarTypeKind::Struct(
                    match &lex
                        .get_id(&lex.find_id(name).ok_or(()).unwrap_or_else(|_| {
                            let help = match name.as_str() {
                                "String" => "change this to 'string'".to_string(),
                                _ => String::new(),
                            };
                            lex.error(format!("Use of undefined type '{name}'"), help, &t.span)
                        }))
                        .unwrap()
                        .kind
                        .kind
                    {
                        VarTypeKind::Struct(s) => s.clone(),
                        _ => todo!(),
                    },
                ),
                false,
            )),
            TokenKind::Keyword(Keyword::Var) => Ok(VarType::new(VarTypeKind::Unknown, false)),
            TokenKind::Keyword(Keyword::Int) => Ok(VarType::new(VarTypeKind::Int, false)),
            TokenKind::Keyword(Keyword::Float) => Ok(VarType::new(VarTypeKind::Float, false)),
            TokenKind::Keyword(Keyword::String) => Ok(VarType::new(VarTypeKind::String, false)),
            TokenKind::Keyword(Keyword::Bool) => Ok(VarType::new(VarTypeKind::Bool, false)),
            _ => Err(()),
        },
        None => lex.error(
            "Expected type but found <eof>".to_owned(),
            "".to_owned(),
            &Span::null(),
        ),
    }
}

fn parse_type(lex: &mut LexVisitor) -> Result<VarType, ()> {
    let span = lex.peek().unwrap().span;
    let constant = match lex.peek().unwrap().kind {
        TokenKind::Keyword(Keyword::Const) => {
            lex.next();
            true
        }
        _ => false,
    };
    let mut var_type = parse_type_inner(lex)
        .unwrap_or_else(|_| lex.error("Could not parse type".to_string(), "".to_string(), &span));
    var_type.constant = constant;
    lex.next();
    loop {
        match lex.peek().map(|x| &x.kind) {
            Some(k) => match k {
                TokenKind::BinOp(BinOp::BitAnd) => {
                    lex.next();
                    var_type = VarType::new(VarTypeKind::Ref(Box::new(var_type)), constant);
                }
                TokenKind::BinOp(BinOp::Ptr) => {
                    lex.next();
                    var_type = VarType::new(VarTypeKind::Ptr(Box::new(var_type)), constant);
                }
                TokenKind::BinOp(BinOp::Mul) => {
                    lex.next();
                    var_type = VarType::new(VarTypeKind::Deref(Box::new(var_type)), constant);
                }
                TokenKind::OpenDelim(Delimiter::Bracket) => {
                    lex.next();
                    let size = match &lex.peek() {
                        Some(t) => match &t.kind {
                            TokenKind::CloseDelim(_) => ArraySize::Unknown,
                            TokenKind::Keyword(Keyword::Dynamic) => {
                                lex.next();
                                ArraySize::Dynamic
                            }
                            _ => {
                                let mut node = parse_expression(lex).unwrap();
                                let size = match compile_time_interpret(&mut node, lex) {
                                    Value::Int(i) => i,
                                    _ => lex.error(
                                        "Expected int for array size".to_string(),
                                        "".to_string(),
                                        &node.span,
                                    ),
                                };
                                ArraySize::Fixed(size)
                            }
                        },
                        None => lex.error(
                            "Unexpected end of token stream".to_owned(),
                            "".to_string(),
                            &Span::null(),
                        ),
                    };
                    var_type = VarType::new(
                        VarTypeKind::Array(Array {
                            var_type: Box::new(var_type),
                            size,
                        }),
                        constant,
                    );
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

fn parse_block(lex: &mut LexVisitor, new_scope: bool, ret_type: VarType) -> Result<AstNode, ()> {
    match lex.peek().unwrap().kind {
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
        lex.peek().map(|x| &x.kind),
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

fn parse_inline_block(lex: &mut LexVisitor, ret_type: VarType) -> Result<AstNode, ()> {
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

fn parse_fn(lex: &mut LexVisitor) -> Result<AstNode, ()> {
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
        kind: VarType::new(VarTypeKind::Unknown, true),
        name: ident.clone(),
        immutable: true,
        constant_value: None,
    });

    let mut parameters = Vec::new();

    lex.push_scope();

    lex.expect(|x| matches!(x, TokenKind::OpenDelim(Delimiter::Paren)))
        .unwrap();

    while !matches!(
        lex.peek().map(|x| &x.kind),
        Some(TokenKind::CloseDelim(Delimiter::Paren))
    ) {
        parameters.push(parse_fn_parameter(lex).unwrap());
        let (kind, span) = lex.peek().map(|x| (&x.kind, x.span)).unwrap();
        match kind {
            TokenKind::Comma => {
                lex.next();
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

    let kind = match lex.peek().unwrap().kind {
        TokenKind::Semi => VarType::new(VarTypeKind::Void, true),
        TokenKind::OpenDelim(Delimiter::Brace) => VarType::new(VarTypeKind::Void, true),
        TokenKind::FatArrow => VarType::new(VarTypeKind::Void, true),
        _ => parse_type(lex).unwrap(),
    };

    match lex.peek().map(|x| x.kind.clone()) {
        Some(TokenKind::OpenDelim(Delimiter::Brace)) | Some(TokenKind::FatArrow) => {
            let block = parse_block(lex, false, kind.clone()).unwrap();
            let span = Span::new(fn_start_pos, block.span.to);

            let func = Rc::new(RefCell::new(Function {
                params: parameters,
                ret_type: kind,
                block: Some(Box::new(block)),
                is_macro: false,
            }));

            *lex.get_mut_id(id.as_ref().unwrap()).unwrap() = SymbolInfo {
                kind: VarType::new(VarTypeKind::Function(func.clone()), true),
                name: ident.clone(),
                immutable: true,
                constant_value: None,
            };

            if let Err(_) = id {
                lex.error(
                    format!("Function {} has already been defined!", &ident),
                    format!("consider using a different name"),
                    &span,
                )
            } else {
                Ok(AstNode::new(AstKind::Function(id.unwrap(), func), span))
            }
        }
        Some(TokenKind::Semi) => {
            let span = lex.next().unwrap().span;
            let span = Span::new(fn_start_pos, span.to);
            let func = Rc::new(RefCell::new(Function {
                params: parameters,
                ret_type: kind,
                block: None,
                is_macro: false,
            }));
            *lex.get_mut_id(id.as_ref().unwrap()).unwrap() = SymbolInfo {
                kind: VarType::new(VarTypeKind::Function(func.clone()), true),
                name: ident.clone(),
                immutable: true,
                constant_value: None,
            };
            Ok(AstNode::new(AstKind::FunctionDecl(id.unwrap(), func), span))
        }

        None => lex.error(
            "expected function declaration but found <eof>".to_string(),
            String::new(),
            &Span::null(),
        ),
        t => {
            let span = lex.next().unwrap().span;
            lex.error(
                format!("expected function declaration but found {t:?}"),
                String::new(),
                &span,
            )
        }
    }
}

fn parse_if_stmt(lex: &mut LexVisitor) -> Result<AstNode, ()> {
    let is_macro = matches!(lex.peek().map(|x| &x.kind), Some(TokenKind::Hash));
    if is_macro {
        lex.next();
    }
    parse_if_stmt_with_is_macro(lex, is_macro)
}

fn parse_if_stmt_with_is_macro(lex: &mut LexVisitor, is_macro: bool) -> Result<AstNode, ()> {
    let start_pos = lex
        .expect_auto_err(TokenKind::Keyword(Keyword::If).as_int())
        .span
        .from;
    parse_if_expr(lex, is_macro, start_pos)
}

fn parse_if_expr(lex: &mut LexVisitor, is_macro: bool, start_pos: Pos) -> Result<AstNode, ()> {
    let conditional = Box::new(parse_expression(lex).unwrap());
    let block = Box::new(parse_block(lex, true, VarType::new(VarTypeKind::Void, true)).unwrap());

    let else_if = match lex.peek().map(|x| &x.kind) {
        Some(TokenKind::Keyword(Keyword::Else)) => {
            lex.next();
            if matches!(lex.peek().unwrap().kind, TokenKind::Keyword(Keyword::If)) {
                ElseBlock::ElseIf(Box::new(parse_if_stmt_with_is_macro(lex, true).unwrap()))
            } else {
                ElseBlock::Else(Box::new(
                    parse_block(lex, true, VarType::new(VarTypeKind::Void, true)).unwrap(),
                ))
            }
        }
        _ => ElseBlock::Nothing,
    };

    let end_pos = lex.peek().unwrap().span.to; // todo: fix this hack
    let span = Span::new(start_pos, end_pos);

    Ok(AstNode::new(
        AstKind::If(IfStatement {
            conditional,
            block,
            else_if,
            is_macro,
        }),
        span,
    ))
}

fn parse_return_stmt(lex: &mut LexVisitor) -> Result<AstNode, ()> {
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

fn parse_access(lex: &mut LexVisitor, lhs: AstNode) -> Result<AstNode, ()> {
    let mut res = lhs;
    while matches!(lex.peek().map(|x| &x.kind), Some(TokenKind::Dot)) {
        lex.expect_auto_err(TokenKind::Dot.as_int());
        let ident = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
        let name = match &ident.kind {
            TokenKind::Ident(i) => i.clone(),
            _ => unreachable!(),
        };
        let span = Span::new(res.span.from, ident.span.to);
        let deref = match var_type_of(&mut res, lex, VarType::new(VarTypeKind::Any, true)).kind {
            VarTypeKind::Ref(_) => true,
            VarTypeKind::Ptr(_) => true,
            _ => false,
        };
        res = AstNode::new(AstKind::Access(Box::new(res), name, deref), span);
    }
    Ok(res)
}

fn parse_stmt(lex: &mut LexVisitor) -> Result<AstNode, ()> {
    match &as_result(lex.peek()).unwrap().kind.clone() {
        t if t.is_type() => parse_var_decl(lex),
        TokenKind::Keyword(Keyword::Function) => parse_fn(lex),
        TokenKind::Keyword(Keyword::Return) => parse_return_stmt(lex),
        TokenKind::Keyword(Keyword::If) => parse_if_stmt(lex),
        TokenKind::Semi => {
            lex.next();
            parse_stmt(lex)
        }
        TokenKind::Hash => {
            lex.next();
            let lookahead = lex.peek().unwrap();
            match &lookahead.kind {
                TokenKind::Keyword(Keyword::If) => parse_if_stmt_with_is_macro(lex, true),
                TokenKind::Ident(ident) => {
                    let path = parse_access(
                        lex,
                        AstNode::new(
                            AstKind::Ident(lex.find_id(&ident).unwrap_or_else(|| {
                                lex.error(
                                    format!("Use of undefined symbol '{ident}'"),
                                    "".to_string(),
                                    &lookahead.span,
                                )
                            })),
                            lookahead.span,
                        ),
                    )
                    .unwrap();
                    lex.next();
                    let res = parse_fn_call(lex, path, true);
                    // println!("{:?}", res);
                    lex.expect_auto_err(TokenKind::Semi.as_int());
                    res
                }
                t => lex.error(
                    format!("Expected either a macro call or a compile time if but found {t:?}"),
                    "".to_string(),
                    &lookahead.span,
                ),
            }
        }
        TokenKind::Keyword(Keyword::Const) => {
            let res = parse_var_decl(lex);
            res
        }
        TokenKind::Ident(outer_ident) => {
            let start_pos = lex.peek().unwrap().span.from;
            let path = parse_expression(lex).unwrap();
            let next = lex.peek().unwrap().clone();
            let res;
            match &next.kind {
                TokenKind::BinOpEq(op) => {
                    lex.next();
                    let expr = parse_expression(lex).unwrap();
                    let span = Span::new(start_pos, expr.span.to);
                    res = Ok(AstNode::new(
                        AstKind::BinOpEq(Box::new(path), *op, Box::new(expr)),
                        span,
                    ));
                }
                TokenKind::Assign => {
                    lex.next();
                    let expr = parse_expression(lex).unwrap();
                    let span = Span::new(start_pos, expr.span.to);
                    res = Ok(AstNode::new(
                        AstKind::Assign(Box::new(path), Box::new(expr)),
                        span,
                    ));
                }
                TokenKind::OpenDelim(Delimiter::Paren) => res = parse_fn_call(lex, path, false),
                TokenKind::Ident(_) => {
                    let span = Span::new(start_pos, next.span.to);
                    let id = lex.find_id(&outer_ident);
                    if id.is_none() {
                        lex.error(
                            format!("Use of undefined type '{outer_ident}'"),
                            "".to_string(),
                            &span,
                        );
                    }
                    let s = match &lex.get_id(&id.unwrap()).unwrap().kind.kind {
                        VarTypeKind::Struct(s) => s.clone(),
                        _ => todo!(),
                    };
                    let kind = VarType::new(VarTypeKind::Struct(s), false);
                    return parse_var_decl_of_type(lex, kind, start_pos);
                }
                TokenKind::Semi => {
                    lex.next();
                    return Ok(path);
                }
                t => {
                    let span = next.span.clone();
                    lex.error(
                        format!("Expected statement but found {t:?}"),
                        "".to_string(),
                        &span,
                    );
                }
            }
            lex.expect_auto_err(TokenKind::Semi.as_int());
            res
        }
        TokenKind::OpenDelim(Delimiter::Brace) => {
            parse_block(lex, true, VarType::new(VarTypeKind::Void, true))
        }
        _ => {
            let res = parse_expression(lex);
            lex.expect_auto_err(TokenKind::Semi.as_int());
            res
        }
    }
}

fn parse_struct(lex: &mut LexVisitor) -> Result<AstNode, ()> {
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
            lex.peek().map(|x| &x.kind),
            Some(TokenKind::CloseDelim(Delimiter::Brace))
        ) {
            break;
        }

        let from_pos = lex.peek().unwrap().span.from;
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
                immutable: false,
                constant_value: None,
            })
            .unwrap_or_else(|_| {
                lex.error(
                    format!("struct already has a field of name '{name}'"),
                    "".to_string(),
                    &span,
                )
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
            kind: VarType::new(VarTypeKind::Struct(struct_type.clone()), true),
            name,
            immutable: true,
            constant_value: None,
        })
        .unwrap();

    Ok(AstNode::new(AstKind::StructDecl(id, struct_type), span))
}

pub fn parse_items(lex: &mut LexVisitor, _mod_name: String) -> Result<AstNode, ()> {
    // let scope = lex.push_scope();
    // lex.modules.insert(mod_name, scope);

    let mut items = Vec::new();
    while lex.peek().is_some() {
        match &as_result(lex.peek()).unwrap().kind {
            TokenKind::Keyword(Keyword::Use) => {
                let start_pos = lex.peek().unwrap().span.from;
                let mut end_pos = start_pos.clone();
                lex.next();
                let mut path = Vec::new();
                while !matches!(lex.peek().unwrap().kind, TokenKind::Semi) {
                    let token = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
                    let segment = match token.kind {
                        TokenKind::Ident(s) => s.clone(),
                        _ => unreachable!(),
                    };
                    path.push(segment);

                    let next = lex.next().unwrap();
                    match next.kind {
                        TokenKind::Dot => {}
                        TokenKind::Semi => {
                            end_pos = next.span.to;
                            break;
                        }
                        k => lex.error(
                            format!("Expected '.' or ';' but found {k:?}"),
                            "".to_owned(),
                            &next.span,
                        ),
                    }
                }
                let input_file = "./".to_string() + &path.join("/") + ".tau";

                if !std::path::Path::new(&input_file).exists() {
                    lex.error(
                        format!("Could not find file '{input_file}'"),
                        "".to_string(),
                        &Span::new(start_pos, end_pos),
                    );
                }

                let old = lex.current_lex;

                lex.lexs.push(crate::lex::lex_file(input_file));
                lex.current_lex = lex.lexs.len() - 1;

                items.push(parse_items(lex, path.last().unwrap().clone()).unwrap());

                lex.current_lex = old;
            }
            TokenKind::Keyword(Keyword::Function) => items.push(parse_fn(lex).unwrap()),
            TokenKind::Keyword(Keyword::Struct) => items.push(parse_struct(lex).unwrap()),
            TokenKind::Hash => items.push(parse_stmt(lex).unwrap()),
            t if t.is_type() => items.push(parse_var_decl(lex).unwrap()),
            t => {
                let span = lex.peek().unwrap().span;
                lex.error(
                    format!("Expected item but found {t:?}"),
                    "".to_owned(),
                    &span,
                );
            }
        }
    }

    // lex.pop_scope();
    Ok(AstNode::new(AstKind::Items(items), Span::null()))
}
