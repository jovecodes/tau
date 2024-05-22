#[derive(Copy, Clone)]
pub struct Pos {
    pub line: u64,
    pub column: u64,
    pub offset: usize,
}

impl core::fmt::Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Default for Pos {
    fn default() -> Self {
        Self::start()
    }
}

impl Pos {
    pub fn new(line: u64, column: u64, offset: usize) -> Pos {
        Self {
            line,
            column,
            offset,
        }
    }

    pub fn start() -> Pos {
        Self::new(1, 1, 0)
    }

    pub fn peek_is(&self, file: &str, c: char) -> bool {
        if let Some(peek) = self.peek(file) {
            peek == c
        } else {
            false
        }
    }

    pub fn peek(&self, file: &str) -> Option<char> {
        match file.chars().nth(self.offset) {
            Some(c) => match c {
                '\n' => Some('\n'),
                _ => Some(c),
            },
            None => None,
        }
    }

    pub fn advance(&mut self, file: &str) -> Option<char> {
        self.offset += 1;
        match file.chars().nth(self.offset - 1) {
            Some(c) => {
                match c {
                    '\n' => {
                        self.column = 1;
                        self.line += 1;
                        Some('\n')
                    }
                    _ => {
                        self.column += 1; // Increment column for non-newline characters
                        Some(c)
                    }
                }
            }
            None => None,
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct Span {
    pub from: Pos,
    pub to: Pos,
}

impl core::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}-{:?}", self.from, self.to)
    }
}

impl Span {
    pub fn new(from: Pos, to: Pos) -> Self {
        Self { from, to }
    }

    pub fn single(pos: Pos) -> Self {
        Self {
            from: pos,
            to: Pos::new(pos.line, pos.column + 1, pos.offset + 1),
        }
    }

    pub fn expand_to(&self, other: &Span) -> Self {
        Span::new(self.from, other.to)
    }

    pub fn null() -> Self {
        Span::new(Pos::default(), Pos::default())
    }
}
