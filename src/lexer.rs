use std::{
    fmt::Display,
    fs::File,
    io::{self, Read},
};

use strum::{EnumIter, IntoEnumIterator};

#[derive(Clone, Debug)]
pub enum LexerError {
    UnexpectedChar(char),
    IllegalIdentifier(String),
    IllegalTypeIdentifier(String),
    IllegalValueIdentifier(String),
    NoSourceProvided,
    MultipleDecimalPoints,
    IllegalNumber(String),
}

#[derive(Clone, Debug)]
pub struct Lexer {
    source: Option<String>,
    tokens: Vec<LocatedToken>,
    cur_tok: String,
    tok_kind: TokKind,
    prev: [char; 2],
    stall: usize,
    is_decimal: bool,
    line: usize,
    col: usize,
}

impl Lexer {
    /// Constructs a new `Lexer` by reading the contents of a file.
    pub fn new_from_file(file: &mut File) -> Result<Self, io::Error> {
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        Ok(Self {
            source: Some(source),
            tokens: Vec::new(),
            cur_tok: String::new(),
            tok_kind: TokKind::Unknown,
            prev: ['\0'; 2],
            stall: 0,
            is_decimal: false,
            line: 1,
            col: 1,
        })
    }

    /// Constructs a new `Lexer` from the contents of a string.
    pub fn new_from_string(source: String) -> Self {
        Self {
            source: Some(source),
            tokens: Vec::new(),
            cur_tok: String::new(),
            tok_kind: TokKind::Unknown,
            prev: ['\0'; 2],
            stall: 0,
            is_decimal: false,
            line: 1,
            col: 1,
        }
    }

    /// Attempts to tokenize an identifier. Returns `true` if the character `c` was part of an
    /// identifier.
    fn tokenize_ident(&mut self, c: char) -> Result<bool, LexerError> {
        if self.tok_kind == TokKind::Number {
            return Ok(false);
        }
        if c.is_ascii_alphabetic() || c == '_' {
            // determine kind based on first character
            if self.tok_kind == TokKind::Unknown {
                if c.is_ascii_uppercase() {
                    self.tok_kind = TokKind::TIdent;
                }
                if c.is_ascii_lowercase() || c == '_' {
                    self.tok_kind = TokKind::VIdent;
                }
            }

            // if the kind is a type identifier, prohibit all instances of `_`
            if self.tok_kind == TokKind::TIdent && c == '_' {
                return Err(LexerError::IllegalTypeIdentifier(self.cur_tok.clone()));
            }

            // if the kind is a value identifier, prohibit uppercase characters
            if self.tok_kind == TokKind::VIdent && c.is_ascii_uppercase() {
                return Err(LexerError::IllegalValueIdentifier(self.cur_tok.clone()));
            }

            self.cur_tok.push(c);
            Ok(true)
        } else if c.is_numeric() && self.tok_kind != TokKind::Unknown {
            self.cur_tok.push(c);
            Ok(true)
        } else if !self.cur_tok.is_empty() {
            let len = self.cur_tok.len();
            let mut tok_ident = String::new();
            std::mem::swap(&mut self.cur_tok, &mut tok_ident);
            self.tokens.push(LocatedToken {
                token: match self.tok_kind {
                    TokKind::TIdent => Token::TIdent(tok_ident),
                    TokKind::VIdent => Token::VIdent(tok_ident),
                    _ => return Err(LexerError::IllegalIdentifier(self.cur_tok.clone())),
                },
                line: self.line,
                col_start: self.col - len,
                col_end: self.col - 1,
            });
            self.tok_kind = TokKind::Unknown;
            Ok(false)
        } else {
            Ok(false)
        }
    }

    /// Attempts to tokenize a number. Returns `true` if the character `c` was part of a number.
    fn tokenize_number(&mut self, c: char) -> Result<bool, LexerError> {
        if self.tok_kind == TokKind::VIdent || self.tok_kind == TokKind::TIdent {
            return Ok(false);
        }
        if c.is_numeric() || c == '_' || (c == '.' && self.tok_kind == TokKind::Number) {
            self.tok_kind = TokKind::Number;
            self.cur_tok.push(c);
            if c == '.' {
                if self.is_decimal {
                    return Err(LexerError::MultipleDecimalPoints);
                } else {
                    self.is_decimal = true;
                }
            }
            Ok(true)
        } else if !self.cur_tok.is_empty() {
            let len = self.cur_tok.len();
            let mut tok_ident = String::new();
            std::mem::swap(&mut self.cur_tok, &mut tok_ident);
            match self.tok_kind {
                TokKind::Number => self.tokens.push(LocatedToken {
                    token: Token::Number(tok_ident),
                    line: self.line,
                    col_start: self.col - len,
                    col_end: self.col - 1,
                }),
                _ => return Err(LexerError::IllegalNumber(self.cur_tok.clone())),
            }
            self.tok_kind = TokKind::Unknown;
            Ok(false)
        } else {
            Ok(false)
        }
    }

    /// Attempts to tokenize the given character and two-character lookahead.
    pub fn tokenize_char(&mut self, c: [char; 3]) -> Result<(), LexerError> {
        if self.tokenize_ident(c[0])? {
            return Ok(());
        };
        if self.tokenize_number(c[0])? {
            return Ok(());
        };
        let mut push_token = |token, len| {
            self.tokens.push(LocatedToken {
                token,
                line: self.line,
                col_start: self.col,
                col_end: self.col + (len - 1),
            })
        };
        match c {
            ['\0', _, _] => (),
            [' ' | '\t', _, _] => (),
            [':', '=', _] => push_token(Token::Assign, 2),
            ['=', _, _] => push_token(Token::Equals, 1),
            [':', _, _] => push_token(Token::Colon, 1),
            [',', _, _] => push_token(Token::Separator(Separator::Comma), 1),
            ['\n', '\n', _] => {
                push_token(Token::Separator(Separator::DoubleNewline), 2);
                self.stall = 2;
            }
            ['\n', _, _] => push_token(Token::Separator(Separator::Newline), 1),
            ['(', _, _] => push_token(Token::Open(Grouping::Paren), 1),
            ['[', _, _] => push_token(Token::Open(Grouping::Bracket), 1),
            ['{', _, _] => push_token(Token::Open(Grouping::Curly), 1),
            ['<', _, _] => push_token(Token::Open(Grouping::Angle), 1),
            [')', _, _] => push_token(Token::Close(Grouping::Paren), 1),
            [']', _, _] => push_token(Token::Close(Grouping::Bracket), 1),
            ['}', _, _] => push_token(Token::Close(Grouping::Curly), 1),
            ['>', _, _] => push_token(Token::Close(Grouping::Angle), 1),
            _ => {
                // determine the operator
                let mut single_op = None;
                let mut double_op = None;
                for op in Operator::iter() {
                    match op.chars() {
                        OperatorChars::Single(c1) => {
                            if c[0] == c1 && single_op.is_none() {
                                single_op = Some(op);
                            }
                        }
                        OperatorChars::Double(c1, c2) => {
                            if c[0] == c1 && c[1] == c2 {
                                double_op = Some(op);
                                break;
                            }
                        }
                    }
                }

                // find the `=` sign (if it exists) for "equals" operators
                let (op, len) = match (single_op, double_op) {
                    (None, None) => return Err(LexerError::UnexpectedChar(c[0])),
                    (Some(single_op), None) => (single_op, 1),
                    (_, Some(double_op)) => (double_op, 2),
                };
                if c[len] == '=' {
                    push_token(Token::OperatorEquals(op), len);
                    self.stall += len;
                } else {
                    push_token(Token::Operator(op), len);
                    self.stall += len - 1;
                }
            }
        }
        Ok(())
    }

    /// Attempts to tokenize the source file.
    pub fn tokenize(mut self) -> Result<Vec<LocatedToken>, LexerError> {
        let Some(source) = self.source.take() else {
            return Err(LexerError::NoSourceProvided);
        };
        // queue of current line and column
        let mut line = [1; 4];
        let mut col = [1; 4];

        // process a single character
        let mut process_char = |c| {
            if self.prev[0] != '\0' && self.stall == 0 {
                match self.tokenize_char([self.prev[0], self.prev[1], c]) {
                    Ok(()) => (),
                    Err(e) => return Err(e),
                };
            }
            self.prev[0] = self.prev[1];
            self.prev[1] = c;

            if self.stall > 0 {
                self.stall -= 1;
            }

            if c == '\n' {
                col[3] = 1;
                line[3] += 1;
            } else if c == '\t' {
                col[3] += 4;
            } else {
                col[3] += 1;
            }
            for i in 0..3 {
                col[i] = col[i + 1];
                line[i] = line[i + 1];
            }
            self.col = col[0];
            self.line = line[0];
            Ok(())
        };

        // process each character in source, plus a bit of "overflow" to clear buffers
        for c in source.chars() {
            process_char(c)?;
        }
        for _ in 0..3 {
            process_char('\0')?;
        }

        // clean up trailing separators
        while matches!(
            self.tokens.last(),
            Some(LocatedToken {
                token: Token::Separator(_),
                ..
            })
        ) {
            self.tokens.pop();
        }

        Ok(self.tokens)
    }
}

#[derive(Clone, Debug)]
pub enum Token {
    VIdent(String),
    TIdent(String),
    Number(String),
    Character(char),
    String(String),
    Equals,
    Assign,
    OperatorEquals(Operator),
    Colon,
    Separator(Separator),
    Comment(String),
    Open(Grouping),
    Close(Grouping),
    Operator(Operator),
}

#[derive(Clone, Debug)]
pub enum Grouping {
    Paren,
    Bracket,
    Angle,
    Curly,
}

impl Grouping {
    pub fn open_char(&self) -> char {
        match self {
            Grouping::Paren => '(',
            Grouping::Bracket => '[',
            Grouping::Angle => '<',
            Grouping::Curly => '{',
        }
    }
    pub fn close_char(&self) -> char {
        match self {
            Grouping::Paren => ')',
            Grouping::Bracket => ']',
            Grouping::Angle => '>',
            Grouping::Curly => '}',
        }
    }
}

#[derive(Clone, Debug, EnumIter)]
pub enum Separator {
    Comma,
    Newline,
    DoubleNewline,
}

impl Display for Separator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Separator::Comma => write!(f, ","),
            Separator::Newline => writeln!(f),
            Separator::DoubleNewline => writeln!(f, "\n"),
        }
    }
}

#[derive(Clone, Copy, Debug, EnumIter, PartialEq, Eq)]
pub enum Operator {
    Dot,
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Concat,
    Repeat,
    And,
    Or,
    Not,
    Backslash,
    ThinArrow,
    FatArrow,
    Question,
    Length,
    Range,
    DoubleEquals,
    NotEquals,
    Greater,
    Less,
    GreaterEquals,
    LessEquals,
}

impl Operator {
    pub fn chars(&self) -> OperatorChars {
        match self {
            Operator::Dot => OperatorChars::Single('.'),
            Operator::Plus => OperatorChars::Single('+'),
            Operator::Minus => OperatorChars::Single('-'),
            Operator::Times => OperatorChars::Single('*'),
            Operator::Divide => OperatorChars::Single('/'),
            Operator::Modulo => OperatorChars::Single('%'),
            Operator::Concat => OperatorChars::Double('+', '+'),
            Operator::Repeat => OperatorChars::Double('*', '*'),
            Operator::And => OperatorChars::Single('&'),
            Operator::Or => OperatorChars::Single('|'),
            Operator::Not => OperatorChars::Single('!'),
            Operator::Backslash => OperatorChars::Single('\\'),
            Operator::ThinArrow => OperatorChars::Double('-', '>'),
            Operator::FatArrow => OperatorChars::Double('=', '>'),
            Operator::Question => OperatorChars::Single('?'),
            Operator::Length => OperatorChars::Single('#'),
            Operator::Range => OperatorChars::Double('.', '.'),
            Operator::DoubleEquals => OperatorChars::Double('=', '='),
            Operator::NotEquals => OperatorChars::Double('!', '='),
            Operator::Greater => OperatorChars::Single('>'),
            Operator::Less => OperatorChars::Single('<'),
            Operator::GreaterEquals => OperatorChars::Double('>', '='),
            Operator::LessEquals => OperatorChars::Double('<', '='),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OperatorChars {
    Single(char),
    Double(char, char),
}

impl Display for OperatorChars {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorChars::Single(c1) => write!(f, "{}", c1),
            OperatorChars::Double(c1, c2) => write!(f, "{}{}", c1, c2),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TokKind {
    Unknown,
    TIdent,
    VIdent,
    Number,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::VIdent(i) => write!(f, "{}", i),
            Token::TIdent(i) => write!(f, "{}", i),
            Token::Number(n) => write!(f, "{}", n),
            Token::Character(c) => write!(f, "'{}'", c),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Equals => write!(f, "="),
            Token::Assign => write!(f, ":="),
            Token::OperatorEquals(operator) => write!(f, "{}=", operator.chars()),
            Token::Colon => write!(f, ":"),
            Token::Separator(s) => write!(f, "{}", s),
            Token::Comment(c) => write!(f, "//{}", c),
            Token::Open(grouping) => write!(f, "{}", grouping.open_char()),
            Token::Close(grouping) => write!(f, "{}", grouping.close_char()),
            Token::Operator(operator) => write!(f, "{}", operator.chars()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LocatedToken {
    pub token: Token,
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
}
