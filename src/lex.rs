use crate::{
    error::Log,
    pos::{Pos, Span},
};
use std::{collections::VecDeque, fs};

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Return,
    Function,
    Struct,
    Dynamic,
    If,
    Else,
    Var,
    Int,
    Float,
    String,
    Bool,
    Null,
    True,
    False,
    Use,
    Const,
    For,
    In,
    To,
}

impl TryFrom<&str> for Keyword {
    type Error = bool;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "return" => Ok(Keyword::Return),
            "fn" => Ok(Keyword::Function),
            "struct" => Ok(Keyword::Struct),
            "dyn" => Ok(Keyword::Dynamic),
            "else" => Ok(Keyword::Else),
            "if" => Ok(Keyword::If),
            "var" => Ok(Keyword::Var),
            "int" => Ok(Keyword::Int),
            "float" => Ok(Keyword::Float),
            "string" => Ok(Keyword::String),
            "bool" => Ok(Keyword::Bool),
            "null" => Ok(Keyword::Null),
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "use" => Ok(Keyword::Use),
            "const" => Ok(Keyword::Const),
            "for" => Ok(Keyword::For),
            "in" => Ok(Keyword::In),
            "to" => Ok(Keyword::To),
            _ => Err(false),
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u32)]
pub enum TokenKind {
    Ident(String),
    Int(i64),
    String(String),
    Float(f64),
    BinOp(BinOp),
    BinOpEq(BinOp),
    OpenDelim(Delimiter),
    CloseDelim(Delimiter),
    Keyword(Keyword),
    FatArrow,
    Semi,
    Colon,
    Assign,
    LogAnd,
    LogOr,
    Comma,
    Dot,
    Hash,
}

impl TokenKind {
    pub fn precedence(&self) -> i32 {
        self.as_bin_op().map(|b| b.prec()).unwrap_or(i32::MIN)
    }

    pub fn is_type(&self) -> bool {
        match self {
            TokenKind::Keyword(Keyword::Var) => true,
            TokenKind::Keyword(Keyword::Int) => true,
            TokenKind::Keyword(Keyword::Float) => true,
            TokenKind::Keyword(Keyword::String) => true,
            TokenKind::Keyword(Keyword::Bool) => true,
            _ => false,
        }
    }

    pub fn as_bin_op(&self) -> Option<BinOp> {
        match self {
            TokenKind::BinOp(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_int(&self) -> (u32, i64) {
        match self {
            TokenKind::Ident(_) => (1, 0),
            TokenKind::Int(i) => (2, *i),
            TokenKind::Float(f) => {
                let bytes = f.to_le_bytes();
                let mut int_value = 0i64;
                for (i, &byte) in bytes.iter().enumerate() {
                    int_value |= (byte as i64) << (i * 8);
                }
                (3, int_value)
            }
            TokenKind::BinOp(op) => (
                4,
                match op {
                    BinOp::Add => 1,
                    BinOp::Sub => 2,
                    BinOp::Mul => 3,
                    BinOp::Div => 4,
                    BinOp::Mod => 5,
                    BinOp::Pow => 6,
                    BinOp::BitAnd => 7,
                    BinOp::BitOr => 8,
                    BinOp::Shl => 9,
                    BinOp::Shr => 10,
                    BinOp::Eq => 11,
                    BinOp::Gt => 12,
                    BinOp::Lt => 13,
                    BinOp::Ptr => 14,
                },
            ),
            TokenKind::BinOpEq(op) => (
                5,
                match op {
                    BinOp::Add => 1,
                    BinOp::Sub => 2,
                    BinOp::Mul => 3,
                    BinOp::Div => 4,
                    BinOp::Mod => 5,
                    BinOp::Pow => 6,
                    BinOp::BitAnd => 7,
                    BinOp::BitOr => 8,
                    BinOp::Shl => 9,
                    BinOp::Shr => 10,
                    BinOp::Eq => 11,
                    BinOp::Gt => 12,
                    BinOp::Lt => 13,
                    BinOp::Ptr => 14,
                },
            ),
            TokenKind::OpenDelim(d) => (
                6,
                match d {
                    Delimiter::Paren => 1,
                    Delimiter::Brace => 2,
                    Delimiter::Bracket => 3,
                },
            ),
            TokenKind::CloseDelim(d) => (
                7,
                match d {
                    Delimiter::Paren => 1,
                    Delimiter::Brace => 2,
                    Delimiter::Bracket => 3,
                },
            ),
            TokenKind::Keyword(k) => (
                8,
                match k {
                    Keyword::Return => 1,
                    Keyword::Function => 2,
                    Keyword::If => 3,
                    Keyword::Else => 4,
                    Keyword::Var => 5,
                    Keyword::Int => 6,
                    Keyword::Float => 7,
                    Keyword::String => 8,
                    Keyword::Bool => 9,
                    Keyword::Null => 10,
                    Keyword::Dynamic => 11,
                    Keyword::Struct => 12,
                    Keyword::True => 13,
                    Keyword::False => 14,
                    Keyword::Use => 15,
                    Keyword::Const => 16,
                    Keyword::For => 17,
                    Keyword::In => 18,
                    Keyword::To => 19,
                },
            ),
            TokenKind::Semi => (9, 0),
            TokenKind::Assign => (10, 0),
            TokenKind::LogAnd => (11, 0),
            TokenKind::LogOr => (12, 0),
            TokenKind::Comma => (13, 0),
            TokenKind::String(_) => (14, 0),
            TokenKind::Colon => (15, 0),
            TokenKind::FatArrow => (16, 0),
            TokenKind::Dot => (17, 0),
            TokenKind::Hash => (18, 0),
        }
    }

    pub fn is_close_delim(&self, delim: Delimiter) -> bool {
        match self {
            TokenKind::CloseDelim(d) => delim == *d,
            _ => false,
        }
    }
}

pub trait TokenKindName {
    fn token_kind_name(self) -> String;
}

impl TokenKindName for u32 {
    fn token_kind_name(self) -> String {
        match self {
            1 => "Ident".to_string(),
            2 => "Int".to_string(),
            3 => "Float".to_string(),
            4 => "BinOp".to_string(),
            5 => "BinOpEq".to_string(),
            6 => "OpenDelim".to_string(),
            7 => "CloseDelim".to_string(),
            8 => "Keyword".to_string(),
            9 => "Semi".to_string(),
            10 => "Assign".to_string(),
            11 => "LogAnd".to_string(),
            12 => "LogOr".to_string(),
            13 => "Comma".to_string(),
            14 => "String".to_string(),
            15 => "Colon".to_string(),
            17 => "Dot".to_string(),
            v => format!("Unknown({v})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Gt,
    Lt,
    Ptr,
}

impl BinOp {
    fn prec(&self) -> i32 {
        match self {
            BinOp::Gt | BinOp::Lt | BinOp::Eq => 1,
            BinOp::Add | BinOp::Sub => 2,
            BinOp::Mul | BinOp::Div | BinOp::Mod => 3,
            BinOp::Pow => 5,
            BinOp::BitAnd => 6,
            BinOp::BitOr => 7,
            BinOp::Shl | BinOp::Shr => 8,
            BinOp::Ptr => 9,
        }
    }
}

impl TryFrom<&str> for BinOp {
    type Error = bool;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use BinOp::*;
        match value {
            "+" => Ok(Add),
            "-" => Ok(Sub),
            "*" => Ok(Mul),
            "/" => Ok(Div),
            "^" => Ok(Pow),
            "&" => Ok(BitAnd),
            "%" => Ok(Mod),
            "|" => Ok(BitOr),
            ">" => Ok(Gt),
            "<" => Ok(Lt),
            "==" => Ok(Eq),
            ">>" => Ok(Shr),
            "<<" => Ok(Shl),
            "@" => Ok(Ptr),
            _ => Err(false),
        }
    }
}

impl ToString for BinOp {
    fn to_string(&self) -> String {
        use BinOp::*;
        match self {
            Add => "+".to_string(),
            Sub => "-".to_string(),
            Mul => "*".to_string(),
            Div => "/".to_string(),
            Mod => "%".to_string(),
            Pow => "^".to_string(),
            BitAnd => "&".to_string(),
            BitOr => "|".to_string(),
            Shl => "<<".to_string(),
            Shr => ">>".to_string(),
            Gt => ">".to_string(),
            Lt => "<".to_string(),
            Ptr => "@".to_string(),
            Eq => "==".to_string(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delimiter {
    /// `( ... )`
    Paren,
    /// `{ ... }`
    Brace,
    /// `[ ... ]`
    Bracket,
    // `∅ ... ∅`
    // An invisible delimiter, that may, for example, appear around tokens coming from a
    // "macro variable" `$var`. It is important to preserve operator priorities in cases like
    // `$var * 3` where `$var` is `1 + 2`.
    // Invisible delimiters might not survive roundtrip of a token stream through a string.
    // Invisible,
}

trait IsDelim {
    fn is_delim(&self) -> bool;
    fn as_delim(&self) -> Option<(Delimiter, bool)>;
}

impl IsDelim for char {
    fn is_delim(&self) -> bool {
        match self {
            '(' => true,
            ')' => true,
            '[' => true,
            ']' => true,
            '{' => true,
            '}' => true,
            _ => false,
        }
    }

    fn as_delim(&self) -> Option<(Delimiter, bool)> {
        use Delimiter::*;
        match self {
            '(' => Some((Paren, true)),
            ')' => Some((Paren, false)),
            '[' => Some((Bracket, true)),
            ']' => Some((Bracket, false)),
            '{' => Some((Brace, true)),
            '}' => Some((Brace, false)),
            _ => None,
        }
    }
}

fn lex_number(lex: &Lex, pos: &mut Pos) -> Option<Token> {
    let from = *pos;
    let mut num_str = pos.advance(&lex.file).unwrap().to_string();
    let mut is_float = false;

    while let Some(next_char) = pos.peek(&lex.file) {
        match next_char {
            c if c.is_numeric() => {
                pos.advance(&lex.file);
                num_str.push(c);
            }
            '.' => {
                pos.advance(&lex.file);
                num_str.push('.');
                if is_float {
                    lex.error(
                        format!("Expected only one '.' in float literal"),
                        format!("Remove second '.'"),
                        &Span::single(pos.clone()),
                    );
                } else {
                    is_float = true
                }
            }
            _ => {
                break;
            }
        }
    }
    if is_float {
        let numeric_value: f64 = num_str.parse().unwrap();
        let token = Token::new(TokenKind::Float(numeric_value), Span::new(from, *pos));
        Some(token)
    } else {
        let numeric_value: i64 = num_str.parse().unwrap();
        let token = Token::new(TokenKind::Int(numeric_value), Span::new(from, *pos));
        Some(token)
    }
}

fn lex_delim(lex: &Lex, pos: &mut Pos) -> Option<Token> {
    let start_pos = *pos;

    if let Some((delim, open)) = pos.advance(&lex.file).unwrap().as_delim() {
        if open {
            Some(Token::new(
                TokenKind::OpenDelim(delim),
                Span::single(start_pos),
            ))
        } else {
            Some(Token::new(
                TokenKind::CloseDelim(delim),
                Span::single(start_pos),
            ))
        }
    } else {
        None
    }
}

fn skip_comments(lex: &Lex, pos: &mut Pos) -> Option<Token> {
    if pos.peek_is(&lex.file, '/') {
        while let Some(c) = pos.advance(&lex.file) {
            if c == '\n' {
                return next_token(lex, pos);
            }
        }
    } else if pos.peek_is(&lex.file, '*') {
        while let Some(c) = pos.advance(&lex.file) {
            if c == '*' && pos.peek_is(&lex.file, '/') {
                pos.advance(&lex.file);
                return next_token(lex, pos);
            }
        }
    }
    None
}

fn lex_op(lex: &Lex, pos: &mut Pos) -> Option<Token> {
    let start_pos = *pos;
    let mut s = pos.advance(&lex.file).unwrap().to_string();
    if let Ok(tok) = s.as_str().try_into() {
        if pos.peek_is(&lex.file, '=') {
            pos.advance(&lex.file);
            Some(Token::new(TokenKind::BinOpEq(tok), Span::single(start_pos)))
        } else {
            Some(Token::new(TokenKind::BinOp(tok), Span::single(start_pos)))
        }
    } else if let Some(n) = pos.advance(&lex.file) {
        s.push(n);
        if let Ok(tok) = s.as_str().try_into() {
            if pos.peek_is(&lex.file, '=') {
                pos.advance(&lex.file);
                Some(Token::new(TokenKind::BinOpEq(tok), Span::single(start_pos)))
            } else {
                Some(Token::new(TokenKind::BinOp(tok), Span::single(start_pos)))
            }
        } else {
            match s.as_str() {
                "&&" => Some(Token::new(TokenKind::LogAnd, Span::single(start_pos))),
                "||" => Some(Token::new(TokenKind::LogOr, Span::single(start_pos))),
                _ => {
                    lex.error(
                        format!("Unexpected character found when lexing double punc"),
                        format!("Unknown character"),
                        &Span::single(start_pos),
                    );
                }
            }
        }
    } else {
        lex.error(
            format!("Unexpected character found when lexing"),
            format!("Unknown character"),
            &Span::single(start_pos),
        );
    }
}

fn lex_ident(lex: &Lex, pos: &mut Pos) -> Option<Token> {
    let from = *pos;

    let mut ident_str = String::new();
    ident_str.push(pos.advance(&lex.file).unwrap());
    while let Some(next_char) = pos.peek(&lex.file) {
        if next_char.is_alphanumeric() || next_char == '_' {
            pos.advance(&lex.file);
            ident_str.push(next_char);
        } else {
            break;
        }
    }
    if let Ok(keyword) = ident_str.as_str().try_into() {
        Some(Token::new(
            TokenKind::Keyword(keyword),
            Span::new(from, *pos),
        ))
    } else {
        Some(Token::new(
            TokenKind::Ident(ident_str),
            Span::new(from, *pos),
        ))
    }
}

fn next_token(lex: &Lex, pos: &mut Pos) -> Option<Token> {
    match pos.peek(&lex.file) {
        Some(c) => {
            match c {
                n if n.is_whitespace() => {
                    pos.advance(&lex.file);
                    next_token(lex, pos)
                }
                n if n.is_numeric() => lex_number(lex, pos),
                n if n.is_delim() => lex_delim(lex, pos),
                ';' => {
                    let span = Span::single(*pos);
                    pos.advance(&lex.file);
                    Some(Token::new(TokenKind::Semi, span))
                }
                '.' => {
                    let span = Span::single(*pos);
                    pos.advance(&lex.file);
                    Some(Token::new(TokenKind::Dot, span))
                }
                ':' => {
                    let span = Span::single(*pos);
                    pos.advance(&lex.file);
                    Some(Token::new(TokenKind::Colon, span))
                }
                ',' => {
                    let span = Span::single(*pos);
                    pos.advance(&lex.file);
                    Some(Token::new(TokenKind::Comma, span))
                }
                '#' => {
                    let span = Span::single(*pos);
                    pos.advance(&lex.file);
                    Some(Token::new(TokenKind::Hash, span))
                }
                '=' => {
                    let span = Span::single(*pos);
                    pos.advance(&lex.file);
                    if pos.peek_is(&lex.file, '=') {
                        pos.advance(&lex.file);
                        Some(Token::new(TokenKind::BinOp(BinOp::Eq), span))
                    } else if pos.peek_is(&lex.file, '>') {
                        pos.advance(&lex.file);
                        Some(Token::new(TokenKind::FatArrow, span))
                    } else {
                        Some(Token::new(TokenKind::Assign, span))
                    }
                }
                '"' => {
                    let start_pos = *pos;
                    pos.advance(&lex.file);
                    let mut s = String::new();
                    loop {
                        match pos.advance(&lex.file) {
                            Some('"') => break,
                            Some('\\') => match pos.advance(&lex.file) {
                                Some('n') => s.push('\n'),
                                Some('t') => s.push('\t'),
                                Some('r') => s.push('\r'),
                                Some('0') => s.push('\0'),
                                Some('"') => s.push('"'),
                                Some('\\') => s.push('\\'),
                                c => lex.error(
                                    format!("Unexpected escape character but found found {:?} when lexing", c),
                                    format!("fix this, dingus"),
                                    &Span::single(*pos),
                                ),
                            },
                            Some(c) => s.push(c),
                            None => break,
                        }
                    }
                    Some(Token::new(TokenKind::String(s), Span::new(start_pos, *pos)))
                }
                n if n.is_ascii_punctuation() && n != '_' => {
                    if n == '/' {
                        let op = lex_op(lex, pos);
                        if let Some(tok) = skip_comments(lex, pos) {
                            Some(tok)
                        } else {
                            op
                        }
                    } else {
                        lex_op(lex, pos)
                    }
                }
                n if n.is_ascii() => lex_ident(lex, pos),
                _ => {
                    lex.error(
                        format!("Unexpected character found when lexing"),
                        format!("remove this"),
                        &Span::single(*pos),
                    );
                }
            }
        }
        None => None,
    }
}

#[derive(Debug)]
pub struct Lex {
    pub tokens: VecDeque<Token>,
    pub file: String,
    pub path: String,
}

pub fn lex_file(path: String) -> Lex {
    let mut lex = Lex {
        file: fs::read_to_string(&path).expect(&format!("Could not read file '{path}'")),
        path,
        tokens: VecDeque::new(),
    };
    let mut current_pos = Pos::start();

    loop {
        if let Some(tok) = next_token(&lex, &mut current_pos) {
            lex.tokens.push_back(tok);
        } else {
            break;
        }
    }
    lex
}
