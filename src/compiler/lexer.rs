use std::fmt::{Debug, Display, Formatter};
use std::str::Chars;
use crate::compiler::error::{ErrorReporter, Diagnostic, DiagnosticContext, Severity, NoteKind, FileId};
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::util;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenType {
    None,

    ParenthesisLeft, ParenthesisRight,
    BracketLeft, BracketRight,
    SquareBracketLeft, SquareBracketRight,
    Dot, Comma, Semicolon, Colon, ColonColon,

    Assign, Equal,
    Not, NotEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    Plus, PlusAssign,
    Minus, MinusAssign,
    Multiply, MultiplyAssign,
    Divide, DivideAssign,

    And, ShortcircuitAnd,
    Or, ShortcircuitOr,

    Identifier, Underscore,
    LiteralInt, LiteralFloat,
    LiteralString,

    // Keywords
    Builtin,
    Interface, Class,
    Template, This, Inline, Optimize,
    Export,
    If, Else,
    Module, Include, Import,
    Type,
    True, False,
    Int, Float, Boolean, String, Object, Array,
    As,

    // EOF
    Eof,

    // Error token is emitted when a lexer error has occurred
    Error,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token<'source> {
    pub token_type: TokenType,
    pub source: &'source str,
    pub span: Span, file_id: FileId,
}

impl<'source> Token<'source> {
    pub fn new(token_type: TokenType, source: &'source str, span: Span, file_id: FileId) -> Self {
        Token {
            token_type, source,
            span, file_id,
        }
    }

    pub fn empty() -> Self {
        Self::new(TokenType::Error, "", Span::new(0, 0), 0)
    }

    pub fn token_type(&self) -> TokenType { self.token_type }
    pub fn source(&self) -> &'source str { &self.source }
    pub fn span(&self) -> Span { self.span }
    pub fn file_id(&self) -> FileId { self.file_id }
}

impl<'source> Display for Token<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.token_type {
            TokenType::None => f.write_str("None"),
            TokenType::Eof => f.write_str("Eof"),
            TokenType::LiteralString => write!(f, "`\"{}\"`", self.source),
            _ => write!(f, "`{}`", self.source),
        }
    }
}

impl<'source> Debug for Token<'source> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub type MessageMarker = ();

#[derive(Debug, Clone)]
pub enum LexerError {
    UnexpectedEof,

    UnexpectedCharacter(char),
    ExpectedCharacter {
        expected: char,
        got: char
    },
    UnterminatedString,
}

impl LexerError {
    pub fn char_byte_offset(&self) -> u32 {
        match self {
            Self::UnexpectedEof => 0,
            Self::UnexpectedCharacter(c) => c.len_utf8() as u32,
            Self::ExpectedCharacter { got, .. } => got.len_utf8() as u32,
            _ => 0,
        }
    }
}

impl Diagnostic<MessageMarker> for LexerError {
    fn name(&self) -> &'static str {
        match self {
            Self::UnexpectedEof => "lexer/unexpected_eof",
            Self::UnexpectedCharacter(_) => "lexer/unexpected_character",
            Self::ExpectedCharacter { .. } => "lexer/expected_character",
            Self::UnterminatedString => "lexer/unterminated_string",
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self, _context: &DiagnosticContext<'_, ()>) -> String {
        match self {
            Self::UnexpectedEof => String::from("unexpected EOF"),
            Self::UnexpectedCharacter(c) => format!("unexpected character '{}'", c),
            Self::ExpectedCharacter { expected, got } => format!("expected character '{}', got '{}'", expected, got),
            Self::UnterminatedString => String::from("unterminated string"),
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        None
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(SpanWithFile, Option<String>)> {
        Vec::new()
    }

    fn primary_note(&self, _context: &DiagnosticContext<'_, ()>) -> Option<(NoteKind, String)> {
        None
    }

    fn additional_notes(&self, _context: &DiagnosticContext<'_, ()>) -> Vec<(NoteKind, String)> {
        Vec::new()
    }
}

pub type LexerErrorReporter = ErrorReporter<MessageMarker, LexerError>;

macro_rules! try_or_report {
    ($self:expr, $reporter:expr, $block:expr) => {
        match $block {
            Ok(result) => result,
            Err(LexerError::UnexpectedEof) => {
                return $self.make_token(TokenType::Eof);
            },
            Err(err) => {
                let byte_offset = err.char_byte_offset();
                $self.report_char($reporter, err, byte_offset);
                return $self.make_token(TokenType::Error);
            },
        }
    }
}

pub struct Lexer<'source> {
    input: &'source str, file_id: FileId,

    chars: Chars<'source>,
    peek_1: Option<char>,
    peek_2: Option<char>,

    start_index: u32,
    current_index: u32,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str, file_id: FileId) -> Lexer<'source> {
        Lexer {
            input: source, file_id,

            chars: source.chars(),
            peek_1: None,
            peek_2: None,

            start_index: 0,
            current_index: 0,
        }
    }

    pub fn scan_token(&mut self, reporter: &mut LexerErrorReporter) -> Token<'source> {
        loop {
            self.skip_whitespace();
            self.start_index = self.current_index.clone();

            if self.is_eof() {
                return self.make_token(TokenType::Eof);
            }

            let c = try_or_report!(self, reporter, self.consume());

            return match c {
                '(' => self.make_token(TokenType::ParenthesisLeft),
                ')' => self.make_token(TokenType::ParenthesisRight),
                '{' => self.make_token(TokenType::BracketLeft),
                '}' => self.make_token(TokenType::BracketRight),
                '[' => self.make_token(TokenType::SquareBracketLeft),
                ']' => self.make_token(TokenType::SquareBracketRight),
                '.' => self.make_token(TokenType::Dot),
                ',' => self.make_token(TokenType::Comma),
                ';' => self.make_token(TokenType::Semicolon),
                ':' => if self.expect(':') { self.make_token(TokenType::ColonColon) } else {
                    self.make_token(TokenType::Colon)
                },

                '=' => if self.expect('=') { self.make_token(TokenType::Equal) } else {
                    self.make_token(TokenType::Assign)
                },
                '!' => if self.expect('=') { self.make_token(TokenType::NotEqual) } else {
                    self.make_token(TokenType::Not)
                },
                '>' => if self.expect('=') { self.make_token(TokenType::GreaterEqual) } else {
                    self.make_token(TokenType::Greater)
                },
                '<' => if self.expect('=') { self.make_token(TokenType::LessEqual) } else {
                    self.make_token(TokenType::Less)
                },
                '&' => if self.expect('&') { self.make_token(TokenType::ShortcircuitAnd) } else {
                    self.make_token(TokenType::And)
                },
                '|' => if self.expect('|') { self.make_token(TokenType::ShortcircuitOr) } else {
                    self.make_token(TokenType::Or)
                },

                // Only `+` and `*` are used, maybe `-`
                '+' => if self.expect('=') { self.make_token(TokenType::PlusAssign) } else {
                    self.make_token(TokenType::Plus)
                },
                '-' => {
                    let next = try_or_report!(self, reporter, self.peek());

                    if ('0'..='9').contains(&next) {
                        self.scan_number(reporter)
                    } else {
                        if self.expect('=') { self.make_token(TokenType::MinusAssign) } else {
                            self.make_token(TokenType::Minus)
                        }
                    }
                },
                '*' => if self.expect('=') { self.make_token(TokenType::MultiplyAssign) } else {
                    self.make_token(TokenType::Multiply)
                },
                '/' => if self.expect('=') { self.make_token(TokenType::DivideAssign) } else if self.expect('/') {
                    self.skip_line();
                    continue;
                } // Skip line comments
                else if self.expect('*') {
                    /* Skip block comments */
                    self.skip_block_comment();
                    continue;
                } else {
                    self.make_token(TokenType::Divide)
                },

                '"' => self.scan_string(reporter),
                '0'..='9' => self.scan_number(reporter),
                c if util::is_alphabetic(c) => self.scan_identifier(reporter),

                _ => {
                    self.report_char(reporter, LexerError::UnexpectedCharacter(c), c.len_utf8() as u32);
                    self.make_token(TokenType::Error)
                },
            };
        }
    }

    fn scan_string(&mut self, reporter: &mut LexerErrorReporter) -> Token<'source> {
        while let Ok(c) = self.peek() {
            if c == '"' {
                break;
            }

            let _ = self.consume();
        }

        if self.is_eof() {
            reporter.report(Span::new(self.start_index, self.current_index), LexerError::UnterminatedString, false);
            self.make_token(TokenType::Error)
        } else {
            let _ = self.consume(); // the trailing '"'

            // Don't add leading and trailing '"' characters to token
            Token {
                token_type: TokenType::LiteralString,
                source: &self.input[(self.start_index as usize + 1)..(self.current_index as usize - 1)],
                span: Span::new(self.start_index, self.current_index), file_id: self.file_id,
            }
        }
    }

    fn scan_number(&mut self, _reporter: &mut LexerErrorReporter) -> Token<'source> {
        while let Ok('0'..='9') = self.peek() {
            let _ = self.consume();
        }

        let mut floating_point = false;

        if let Ok('.') = self.peek() {
            if let Ok('0'..='9') = self.peek_next() {
                let _ = self.consume();
                floating_point = true;

                while let Ok('0'..='9') = self.peek() {
                    let _ = self.consume();
                }

                if let Ok('e' | 'E') = self.peek() {
                    let _ = self.consume();

                    self.expect_any(&['+', '-']);

                    while let Ok('0'..='9') = self.peek() {
                        let _ = self.consume();
                    }
                }
            }
        }

        self.make_token(if floating_point { TokenType::LiteralFloat } else { TokenType::LiteralInt })
    }

    fn scan_identifier(&mut self, _reporter: &mut LexerErrorReporter) -> Token<'source> {
        while let Ok(c) = self.peek() {
            if !util::is_alphanumeric(c) {
                break;
            }

            let _ = self.consume();
        }

        let name: &str = &self.input[self.start_index as usize..self.current_index as usize];
        let mut chars = name.chars();

        let token_type = match chars.next().expect("Internal compiler error: Empty identifier") {
            'a' => if let Some(c) = chars.next() {
                match c {
                    'r' => Lexer::check_keyword(name, 2, "array", TokenType::Array),
                    's' => Lexer::check_keyword(name, 2, "as", TokenType::As),
                    _ => TokenType::Identifier,
                }
            } else { TokenType::Identifier },
            'b' => if let Some(c) = chars.next() {
                match c {
                    'o' => Lexer::check_keyword(name, 2, "boolean", TokenType::Boolean),
                    'u' => Lexer::check_keyword(name, 2, "builtin", TokenType::Builtin),
                    _ => TokenType::Identifier,
                }
            } else { TokenType::Identifier },
            'c' => Lexer::check_keyword(name, 1, "class", TokenType::Class),
            'e' => if let Some(c) = chars.next() {
                match c {
                    'l' => Lexer::check_keyword(name, 2, "else", TokenType::Else),
                    'x' => Lexer::check_keyword(name, 2, "export", TokenType::Export),
                    _ => TokenType::Identifier,
                }
            } else { TokenType::Identifier },
            'f' => if let Some(c) = chars.next() {
                match c {
                    'a' => Lexer::check_keyword(name, 2, "false", TokenType::False),
                    'l' => Lexer::check_keyword(name, 2, "float", TokenType::Float),
                    _ => TokenType::Identifier,
                }
            } else {TokenType::Identifier },
            'i' => if let Some(c) = chars.next() {
                match c {
                    'f' => if chars.next().is_some() { TokenType::Identifier } else { TokenType::If },
                    'n' => if let Some(c) = chars.next() {
                        match c {
                            'c' => Lexer::check_keyword(name, 3, "include", TokenType::Include),
                            'l' => Lexer::check_keyword(name, 3, "inline", TokenType::Inline),
                            't' => if let Some(c) = chars.next() {
                                match c {
                                    'e' => Lexer::check_keyword(name, 4, "interface", TokenType::Interface),
                                    _ => TokenType::Identifier,
                                }
                            } else { TokenType::Int },
                            _ => TokenType::Identifier,
                        }
                    } else { TokenType::Identifier }
                    'm' => Lexer::check_keyword(name, 2, "import", TokenType::Import),
                    _ => TokenType::Identifier,
                }
            } else { TokenType::Identifier },
            'm' => Lexer::check_keyword(name, 1, "module", TokenType::Module),
            'o' => if let Some(c) = chars.next() {
                match c {
                    'b' => Lexer::check_keyword(name, 2, "object", TokenType::Object),
                    'p' => Lexer::check_keyword(name, 2, "optimize", TokenType::Optimize),
                    _ => TokenType::Identifier,
                }
            } else { TokenType::Identifier },
            's' => Lexer::check_keyword(name, 1, "string", TokenType::String),
            't' => if let Some(c) = chars.next() {
                match c {
                    'e' => Lexer::check_keyword(name, 2, "template", TokenType::Template),
                    'h' => Lexer::check_keyword(name, 2, "this", TokenType::This),
                    'r' => Lexer::check_keyword(name, 2, "true", TokenType::True),
                    'y' => Lexer::check_keyword(name, 2, "type", TokenType::Type),
                    _ => TokenType::Identifier,
                }
            } else { TokenType::Identifier },
            '_' => if chars.next().is_some() { TokenType::Identifier } else { TokenType::Underscore },
            _ => TokenType::Identifier,
        };

        Token { source: name, token_type, span: Span::new(self.start_index, self.current_index), file_id: self.file_id, }
    }

    fn check_keyword(name: &str, start: usize, keyword: &'static str, token_type: TokenType) -> TokenType {
        if name[start..] == keyword[start..] {
            token_type
        } else {
            TokenType::Identifier
        }
    }

    fn make_token(&self, token_type: TokenType) -> Token<'source> {
        Token {
            token_type,
            source: &self.input[self.start_index as usize..self.current_index as usize],

            span: Span::new(self.start_index, self.current_index),
            file_id: self.file_id,
        }
    }

    fn consume(&mut self) -> Result<char, LexerError> {
        (if let Some(c) = self.peek_1.take() {
            self.peek_1 = self.peek_2.take();
            Ok(c)
        } else {
            self.chars.next().ok_or(LexerError::UnexpectedEof)
        }).map(|c| {
            self.current_index += c.len_utf8() as u32;
            c
        })
    }

    fn peek(&mut self) -> Result<char, LexerError> {
        if let Some(c) = self.peek_1 {
            Ok(c)
        } else if let Some(c) = self.chars.next() {
            self.peek_1 = Some(c);
            Ok(c)
        } else {
            Err(LexerError::UnexpectedEof)
        }
    }

    fn peek_next(&mut self) -> Result<char, LexerError> {
        if let Some(c) = self.peek_2 {
            Ok(c)
        } else if self.peek_1.is_some() {
            if let Some(c) = self.chars.next() {
                self.peek_2 = Some(c);
                Ok(c)
            } else {
                Err(LexerError::UnexpectedEof)
            }
        } else if let Some(peek_1) = self.chars.next() {
            self.peek_1 = Some(peek_1);

            if let Some(c) = self.chars.next() {
                self.peek_2 = Some(c);
                Ok(c)
            } else {
                Err(LexerError::UnexpectedEof)
            }
        } else {
            Err(LexerError::UnexpectedEof)
        }
    }

    fn expect(&mut self, expected: char) -> bool {
        let actual = match self.peek() {
            Ok(c) => c,
            Err(_) => return false,
        };

        if expected == actual {
            let _ = self.consume();
            true
        } else {
            false
        }
    }

    fn expect_any(&mut self, expected: &[char]) -> bool {
        let actual = match self.peek() {
            Ok(c) => c,
            Err(_) => return false,
        };

        if expected.contains(&actual) {
            let _ = self.consume();
            true
        } else {
            false
        }
    }

    fn skip_whitespace(&mut self) {
        while let Ok(c) = self.peek() {
            if !c.is_whitespace() {
                return;
            }

            let _ = self.consume();
        }
    }

    fn skip_line(&mut self) {
        while let Ok(c) = self.peek() {
            let _ = self.consume();

            if c == '\n' {
                return;
            }
        }
    }

    fn skip_block_comment(&mut self) {
        let mut comment_count = 1;

        while let Ok(c) = self.consume() {
            if c == '/' {
                if let Ok('*') = self.consume() {
                    comment_count += 1;
                }
            } else if c == '*' {
                if let Ok('/') = self.consume() {
                    comment_count -= 1;
                }
            }

            if comment_count <= 0 {
                return;
            }
        }
    }

    fn is_eof(&self) -> bool {
        self.current_index as usize >= self.input.len()
    }

    fn report_char(&self, reporter: &mut LexerErrorReporter, error: LexerError, byte_offset: u32) {
        reporter.report(Span::new(self.start_index, self.current_index + byte_offset),
            error, false);
    }
}
