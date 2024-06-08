use crate::{lex::Lex, parse::LexVisitor, pos::Span};
use core::fmt;
use std::{process::exit, usize};

#[derive(Debug, Clone, Copy)]
pub enum TColor {
    // Black,
    Red,
    Green,
    // Yellow,
    // Blue,
    Magenta,
    // Cyan,
    // White,
    Bold,  // Bold text
    Reset, // Reset to default color
}

impl fmt::Display for TColor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl TColor {
    pub fn to_string(&self) -> String {
        match self {
            // TColor::Black => "\x1b[30m".to_string(),
            TColor::Red => "\x1b[31m".to_string(),
            TColor::Green => "\x1b[32m".to_string(),
            // TColor::Yellow => "\x1b[33m".to_string(),
            // TColor::Blue => "\x1b[34m".to_string(),
            TColor::Magenta => "\x1b[35m".to_string(),
            // TColor::Cyan => "\x1b[36m".to_string(),
            // TColor::White => "\x1b[37m".to_string(),
            TColor::Bold => "\x1b[1m".to_string(),
            TColor::Reset => "\x1b[0m".to_string(),
        }
    }
}

pub trait Log {
    fn error(&self, msg: String, help: String, span: &Span) -> !;
    fn warn(&self, msg: String, help: String, span: &Span);
    fn info(&self, msg: String, help: String, span: &Span);
}

struct Logger<'a> {
    path: &'a str,
    file: &'a str,
    level: &'a str,
    msg: String,
    help: String,
    span: &'a Span,
    color: TColor,
}

impl Logger<'_> {
    fn log(self) {
        println!(
            "{}{}{}{}: {}{}{}",
            TColor::Bold,
            self.color,
            self.level,
            TColor::Reset,
            TColor::Bold,
            self.msg,
            TColor::Reset
        );
        println!(
            "At {}:{}:{}",
            self.path, self.span.from.line, self.span.from.column
        );

        let line_num = self.span.from.line.to_string();
        let padding = " ".repeat(line_num.len());

        let line = self
            .file
            .lines()
            .nth((self.span.from.line - 1) as usize)
            .unwrap();

        let length = {
            if self.span.from.line != self.span.to.line {
                1
            } else {
                self.span.to.column - self.span.from.column
            }
        };
        let at_pad = " ".repeat((self.span.from.column - 1) as usize);
        let carrots = "^".repeat(length as usize);

        println!("{padding} |");
        println!("{line_num} | {line}");
        println!(
            "{padding} | {}{at_pad}{carrots} {}{}",
            self.color,
            self.help,
            TColor::Reset
        );
    }
}

impl Log for Lex {
    fn error(&self, msg: String, help: String, span: &Span) -> ! {
        Logger {
            path: &self.path,
            file: &self.file,
            level: "Error",
            msg,
            help,
            span,
            color: TColor::Red,
        }
        .log();
        exit(1);
    }

    fn warn(&self, msg: String, help: String, span: &Span) {
        Logger {
            path: &self.path,
            file: &self.file,
            level: "Warning",
            msg,
            help,
            span,
            color: TColor::Magenta,
        }
        .log();
    }

    fn info(&self, msg: String, help: String, span: &Span) {
        Logger {
            path: &self.path,
            file: &self.file,
            level: "Info",
            msg,
            help,
            span,
            color: TColor::Green,
        }
        .log();
    }
}

impl Log for LexVisitor {
    fn error(&self, msg: String, help: String, span: &Span) -> ! {
        Logger {
            path: &self.lexs[self.current_lex].path,
            file: &self.lexs[self.current_lex].file,
            level: "Error",
            msg,
            help,
            span,
            color: TColor::Red,
        }
        .log();
        exit(1);
    }

    fn warn(&self, msg: String, help: String, span: &Span) {
        Logger {
            path: &self.lexs[self.current_lex].path,
            file: &self.lexs[self.current_lex].file,
            level: "Warning",
            msg,
            help,
            span,
            color: TColor::Magenta,
        }
        .log();
    }

    fn info(&self, msg: String, help: String, span: &Span) {
        Logger {
            path: &self.lexs[self.current_lex].path,
            file: &self.lexs[self.current_lex].file,
            level: "Info",
            msg,
            help,
            span,
            color: TColor::Green,
        }
        .log();
    }
}
