use crate::{
    error::Log,
    pos::{Pos, Span},
};
use std::fs;

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Return,
    Function,
    Var,
    Int,
    Float,
    String,
    Bool,
}

impl TryFrom<&str> for Keyword {
    type Error = bool;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "return" => Ok(Keyword::Return),
            "fn" => Ok(Keyword::Function),
            "var" => Ok(Keyword::Var),
            "int" => Ok(Keyword::Int),
            "float" => Ok(Keyword::Float),
            "string" => Ok(Keyword::String),
            "bool" => Ok(Keyword::Bool),
            _ => Err(false),
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u32)]
pub enum TokenKind {
    Ident(String) = 1,
    Int(i64) = 2,
    Float(f64) = 3,
    BinOp(BinOp) = 4,
    BinOpEq(BinOp) = 5,
    OpenDelim(Delimiter) = 6,
    CloseDelim(Delimiter) = 7,
    Keyword(Keyword) = 8,
    Semi = 9,
    Assign = 10,
    LogAnd = 11,
    LogOr = 12,
    Comma = 13,
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
            TokenKind::BinOpEq(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_int(&self) -> u32 {
        match self {
            TokenKind::Ident(_) => 1,
            TokenKind::Int(_) => 2,
            TokenKind::Float(_) => 3,
            TokenKind::BinOp(_) => 4,
            TokenKind::BinOpEq(_) => 5,
            TokenKind::OpenDelim(_) => 6,
            TokenKind::CloseDelim(_) => 7,
            TokenKind::Keyword(_) => 8,
            TokenKind::Semi => 9,
            TokenKind::Assign => 10,
            TokenKind::LogAnd => 11,
            TokenKind::LogOr => 12,
            TokenKind::Comma => 13,
        }
    }

    pub fn is_assign(&self) -> bool {
        matches!(self, TokenKind::Assign)
    }

    pub fn is_open_delim(&self, delim: Delimiter) -> bool {
        match self {
            TokenKind::OpenDelim(d) => delim == *d,
            _ => false,
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
            _ => "Unknown".to_string(),
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

#[derive(Clone, PartialEq, Hash, Debug, Copy)]
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
    Gt,
    Lt,
}

impl BinOp {
    fn prec(&self) -> i32 {
        match self {
            BinOp::Gt | BinOp::Lt => 1,
            BinOp::Add | BinOp::Sub => 2,
            BinOp::Mul | BinOp::Div | BinOp::Mod => 3,
            BinOp::Pow => 5,
            BinOp::BitAnd => 6,
            BinOp::BitOr => 7,
            BinOp::Shl | BinOp::Shr => 8,
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
            ">>" => Ok(Shr),
            "<<" => Ok(Shl),
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
                    return None;
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
    let mut start_pos = *pos;

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
    let mut start_pos = *pos;
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
                    None
                }
            }
        }
    } else {
        lex.error(
            format!("Unexpected character found when lexing"),
            format!("Unknown character"),
            &Span::single(start_pos),
        );
        None
    }
}

fn lex_ident(lex: &Lex, pos: &mut Pos) -> Option<Token> {
    let mut from = *pos;

    let mut ident_str = String::new();
    ident_str.push(pos.advance(&lex.file).unwrap());
    while let Some(next_char) = pos.peek(&lex.file) {
        if next_char.is_alphanumeric() {
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
        Some(c) => match c {
            n if n.is_whitespace() => {
                pos.advance(&lex.file);
                next_token(lex, pos)
            }
            n if n.is_numeric() => lex_number(lex, pos),
            n if n.is_delim() => lex_delim(lex, pos),
            ';' => {
                pos.advance(&lex.file);
                Some(Token::new(TokenKind::Semi, Span::single(*pos)))
            }
            ',' => {
                pos.advance(&lex.file);
                Some(Token::new(TokenKind::Comma, Span::single(*pos)))
            }
            '=' => {
                pos.advance(&lex.file);
                Some(Token::new(TokenKind::Assign, Span::single(*pos)))
            }
            n if n.is_ascii_punctuation() => {
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
                None
            }
        },
        None => None,
    }
}

#[derive(Debug)]
pub struct Lex {
    pub tokens: Vec<Token>,
    pub file: String,
    pub path: String,
}

pub fn lex_file(path: String) -> Lex {
    let mut lex = Lex {
        file: fs::read_to_string(&path).unwrap(),
        path,
        tokens: Vec::new(),
    };
    let mut current_pos = Pos::start();

    loop {
        if let Some(tok) = next_token(&lex, &mut current_pos) {
            lex.tokens.push(tok);
        } else {
            break;
        }
    }
    lex
}
