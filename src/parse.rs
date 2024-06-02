use crate::lex::{BinOp, Delimiter, Keyword, Token, TokenKind};
use crate::pos::Span;

use crate::{error::Log, lex::TokenKindName};
use core::panic;
use std::{collections::HashMap, iter::Peekable, rc::Rc, slice::Iter};

type ScopeID = u64;

#[derive(Debug)]
pub struct Scope {
    id: ScopeID,
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

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Unknown,
    Any,
    Void,
    Int,
    Float,
    String,
    Bool,
    Function(Rc<Function>),
    User(ID),
    Ref(Box<VarType>),
    Ptr(Box<VarType>),
    Deref(Box<VarType>),
}

impl VarType {
    pub fn loosy_eq(self, other: VarType) -> bool {
        if matches!(self, VarType::Any) {
            return true;
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
            unreachable!()
        } else {
            let s = Span::default();
            self.error(
                format!("Expected token {} but got <eof>", kind.token_kind_name()),
                format!(""),
                &s,
            );
            unreachable!()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<AstNode>,
    scope: ScopeID,
    pub expected_return: VarType,
}

#[derive(Debug, Clone)]
pub enum ElseBlock {
    Nothing,
    ElseIf(Box<AstNode>),
    Else(Box<AstNode>),
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub conditional: Box<AstNode>,
    pub block: Box<AstNode>,
    pub else_if: ElseBlock,
}

#[derive(Debug, Clone)]
pub enum AstKind {
    Path(Path),
    BinOp(Box<AstNode>, BinOp, Box<AstNode>),
    Block(Block),
    Function(ID, Rc<Function>),
    FunctionDecl(ID, Rc<Function>),
    VarDecl(VarType, ID, Box<AstNode>),
    Return(Box<AstNode>),
    If(IfStatement),
    Number(Number),
    String(String),
    Parameter(VarType, ID),
    FunCall(Path, Vec<AstNode>),
    Items(Vec<AstNode>),
    Reference(Box<AstNode>),
    Dereference(Box<AstNode>),
    Pointer(Box<AstNode>),
    Null,
}

#[derive(Debug, Clone)]
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
    Ok(lhs)
}

fn parse_fn_call<'a>(lex: &'a mut LexVisiter, path: Path) -> Result<AstNode, ()> {
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
                _ => params.push(parse_expression(lex)?),
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
        AstKind::FunCall(path, params),
        Span::new(start_pos, end_pos),
    ))
}

fn parse_primary<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    match lex.tokens.peek().map(|x| &x.kind) {
        Some(TokenKind::Ident(_)) => {
            let from = lex.tokens.peek().unwrap().span.from;
            let path = parse_path(lex)?;
            if matches!(
                lex.tokens.peek().map(|x| &x.kind),
                Some(TokenKind::OpenDelim(Delimiter::Paren))
            ) {
                return parse_fn_call(lex, path);
            } else {
                return Ok(AstNode::new(
                    AstKind::Path(path),
                    Span::new(from, lex.tokens.peek().unwrap().span.to),
                ));
            }
        }
        _ => {}
    };
    match lex.tokens.next() {
        Some(tok) => match &tok.kind {
            TokenKind::Keyword(Keyword::Null) => Ok(AstNode::new(AstKind::Null, tok.span)),
            TokenKind::Int(i) => Ok(AstNode::new(AstKind::Number(Number::Int(*i)), tok.span)),
            TokenKind::Float(f) => Ok(AstNode::new(AstKind::Number(Number::Float(*f)), tok.span)),
            TokenKind::String(s) => Ok(AstNode::new(AstKind::String(s.clone()), tok.span)),
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

                res
            }
            TokenKind::BinOp(BinOp::BitAnd) => Ok(AstNode::new(
                AstKind::Reference(Box::new(parse_primary(lex)?)),
                tok.span,
            )),
            TokenKind::BinOp(BinOp::Mul) => Ok(AstNode::new(
                AstKind::Dereference(Box::new(parse_primary(lex)?)),
                tok.span,
            )),
            TokenKind::BinOp(BinOp::Ptr) => Ok(AstNode::new(
                AstKind::Pointer(Box::new(parse_primary(lex)?)),
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
    let primary = parse_primary(lex)?;
    parse_expression_1(lex, primary, 0)
}

fn parse_path<'a>(lex: &'a mut LexVisiter) -> Result<Path, ()> {
    let token = as_result(lex.tokens.next())?;

    let mut path = Path {
        segments: Vec::new(),
    };

    match &token.kind {
        TokenKind::Ident(ident) => {
            let id = lex.find_id(ident).ok_or(());
            if let Ok(id) = id {
                path.segments.push(PathSegment::new(id));
            } else {
                lex.error(
                    format!("Use of undefined symbol '{ident}'"),
                    "".to_string(),
                    &token.span,
                )
            }
        }
        _ => return Ok(path),
    }

    Ok(path)
}

fn parse_var_decl<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let from_pos = lex.tokens.peek().unwrap().span.from;
    let kind = parse_type(lex)?;

    let ident_token = lex.expect_auto_err(TokenKind::Ident(String::new()).as_int());
    let ident = match &ident_token.kind {
        TokenKind::Ident(name) => name.clone(),
        _ => panic!("this should be a ident"),
    };

    lex.expect_auto_err(TokenKind::Assign.as_int());

    let value = parse_expression(lex)?;
    lex.expect_auto_err(TokenKind::Semi.as_int());

    let span = Span::new(from_pos, value.span.to);

    let id = lex.push_id(SymbolInfo {
        kind: kind.clone(),
        name: ident.clone(),
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
        })
        .map_err(|_| ())?;
    Ok(AstNode::new(AstKind::Parameter(paren_type, id), span))
}

fn parse_type_inner<'a>(lex: &'a mut LexVisiter) -> Result<VarType, ()> {
    match lex.tokens.peek() {
        Some(t) => match &t.kind {
            TokenKind::Ident(name) => Ok(VarType::User(lex.find_id(name).ok_or(())?)),
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
            Err(())
        }
    }
}

fn parse_type<'a>(lex: &'a mut LexVisiter) -> Result<VarType, ()> {
    let mut var_type = parse_type_inner(lex)?;
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
    let mut stmts = Vec::new();

    let scope;
    if new_scope {
        scope = lex.push_scope();
    } else {
        scope = lex.current_scope;
    }

    let start_pos = match lex.expect(|x| matches!(x, TokenKind::OpenDelim(Delimiter::Brace))) {
        Err(t) => {
            let found = t.unwrap().clone();
            lex.error(
                format!("Expected '{{' but found {found:?}"),
                "".to_string(),
                &found.span,
            );
            unreachable!()
        }
        Ok(v) => v.span.from,
    };

    while !matches!(
        lex.tokens.peek().map(|x| &x.kind),
        Some(TokenKind::CloseDelim(Delimiter::Brace))
    ) {
        stmts.push(parse_stmt(lex)?)
    }

    lex.pop_scope();

    let end_pos = lex
        .expect(|x| matches!(x, TokenKind::CloseDelim(Delimiter::Brace)))
        .unwrap()
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

fn parse_fn<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let fn_start_pos = lex
        .expect(|x| matches!(x, TokenKind::Keyword(Keyword::Function)))
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

    let id = lex.push_id(SymbolInfo {
        kind: VarType::Unknown,
        name: ident.clone(),
    });

    let mut parameters = Vec::new();

    lex.push_scope();

    lex.expect(|x| matches!(x, TokenKind::OpenDelim(Delimiter::Paren)))
        .unwrap();

    while !matches!(
        lex.tokens.peek().map(|x| &x.kind),
        Some(TokenKind::CloseDelim(Delimiter::Paren))
    ) {
        parameters.push(parse_fn_parameter(lex)?);
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
    dbg!(lex.tokens.peek());

    match lex.tokens.peek().map(|x| &x.kind) {
        Some(TokenKind::OpenDelim(Delimiter::Brace)) => {
            let block = parse_block(lex, false, kind.clone())?;
            let span = Span::new(fn_start_pos, block.span.to);

            let func = Rc::new(Function {
                params: parameters,
                ret_type: kind,
                block: Some(Box::new(block)),
            });

            *lex.get_mut_id(id.as_ref().unwrap()).unwrap() = SymbolInfo {
                kind: VarType::Function(func.clone()),
                name: ident.clone(),
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
        .expect(|x| matches!(x, TokenKind::Keyword(Keyword::If)))
        .unwrap()
        .span
        .from;

    let conditional = Box::new(parse_expression(lex)?);
    let block = Box::new(parse_block(lex, true, VarType::Void)?);
    let else_if = match lex.tokens.peek().map(|x| &x.kind) {
        Some(TokenKind::Keyword(Keyword::Else)) => {
            lex.tokens.next();
            if matches!(
                lex.tokens.peek().unwrap().kind,
                TokenKind::Keyword(Keyword::If)
            ) {
                ElseBlock::ElseIf(Box::new(parse_if_stmt(lex)?))
            } else {
                ElseBlock::Else(Box::new(parse_block(lex, true, VarType::Void)?))
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
        TokenKind::Keyword(Keyword::Function) => parse_fn(lex),
        TokenKind::Keyword(Keyword::Return) => parse_return_stmt(lex),
        TokenKind::Keyword(Keyword::If) => parse_if_stmt(lex),
        TokenKind::Semi => {
            lex.tokens.next();
            parse_stmt(lex)
        }
        TokenKind::Ident(_) => {
            let path = parse_path(lex)?;
            let next = dbg!(as_result(lex.tokens.peek())?);
            let res;
            match &next.kind {
                TokenKind::BinOpEq(_) => todo!(),
                TokenKind::Assign => todo!(),
                TokenKind::OpenDelim(Delimiter::Paren) => res = parse_fn_call(lex, path),
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

pub fn parse_items<'a>(lex: &'a mut LexVisiter) -> Result<AstNode, ()> {
    let mut items = Vec::new();
    while lex.tokens.peek().is_some() {
        match &as_result(lex.tokens.peek())?.kind {
            TokenKind::Keyword(Keyword::Function) => items.push(parse_fn(lex)?),
            t if t.is_type() => items.push(parse_var_decl(lex)?),
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
