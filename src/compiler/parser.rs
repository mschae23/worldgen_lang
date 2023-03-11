use std::path::PathBuf;
use lazy_static::lazy_static;
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::ErrorReporter;
use crate::compiler::lexer;
use crate::compiler::lexer::{Lexer, Token, TokenType};

lazy_static! {
    static ref ALLOWED_TEMPLATE_NAME_TYPES: [TokenType; 16] = [
        TokenType::Identifier,
        TokenType::Plus, TokenType::Minus, TokenType::Multiply, TokenType::Divide,
        TokenType::Equal, TokenType::NotEqual,
        TokenType::Greater, TokenType::GreaterEqual,
        TokenType::Less, TokenType::LessEqual,
        TokenType::And, TokenType::ShortcircuitAnd,
        TokenType::Or, TokenType::ShortcircuitOr,
        TokenType::SquareBracketLeft, // [] operator
    ];

    static ref ALLOWED_VARIABLE_TYPES: [TokenType; 8] = [
        TokenType::Int, TokenType::Float,
        TokenType::Boolean,
        TokenType::String,
        TokenType::Object, TokenType::Array,
        TokenType::Module,
        TokenType::Underscore,
    ];
}

pub type MessageMarker = ();

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    previous: Token<'source>, current: Token<'source>,

    path: PathBuf,

    had_error: bool,
    panic_mode: bool,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>, path: PathBuf) -> Parser<'_> {
        Parser {
            lexer,
            path,
            previous: Token::empty(),
            current: Token::empty(),
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    // Declaration parsing

    pub fn parse(&mut self, lexer_reporter: &mut ErrorReporter<lexer::MessageMarker>) -> Vec<Decl<'source>> {
        self.consume(lexer_reporter);

        let mut declarations = Vec::new();

        while !self.is_eof() {
            declarations.push(self.parse_declaration());
        }

        declarations
    }

    fn parse_declaration(&mut self) -> Decl<'source> {
        todo!()
    }

    fn consume(&mut self, lexer_reporter: &mut ErrorReporter<lexer::MessageMarker>) {
        std::mem::swap(&mut self.previous, &mut self.current); // self.previous = self.current; self.current gets replaced below

        loop {
            let token = self.lexer.scan_token(lexer_reporter);

            match token {
                Token { token_type: TokenType::Error, .. } => {},
                token => {
                    self.current = token;
                    break;
                },
            }
        }
    }

    fn expect(&mut self, token_type: TokenType, _message: &str, lexer_reporter: &mut ErrorReporter<lexer::MessageMarker>) {
        if self.current.token_type() == token_type {
            self.consume(lexer_reporter);
            return;
        }

        // self.error_at_current(message, true);
    }

    fn expect_any(&mut self, token_types: &[TokenType], _message: &str, lexer_reporter: &mut ErrorReporter<lexer::MessageMarker>) {
        for token_type in token_types {
            if self.current.token_type() == *token_type {
                self.consume(lexer_reporter);
                return;
            }
        }

        // self.error_at_current(message, true);
    }

    #[inline]
    fn expect_statement_end(&mut self, lexer_reporter: &mut ErrorReporter<lexer::MessageMarker>) {
        self.expect(TokenType::Semicolon, "Expected ';' after statement", lexer_reporter);
    }

    fn matches(&mut self, token_type: TokenType, lexer_reporter: &mut ErrorReporter<lexer::MessageMarker>) -> bool { // Should be called "match", but that's a keyword
        if !self.check(token_type) {
            return false;
        }

        self.consume(lexer_reporter);
        true
    }

    fn matches_any(&mut self, token_types: &[TokenType], lexer_reporter: &mut ErrorReporter<lexer::MessageMarker>) -> bool {
        for token_type in token_types {
            if self.check(*token_type) {
                self.consume(lexer_reporter);
                return true;
            }
        }

        false
    }

    #[inline]
    fn check(&mut self, token_type: TokenType) -> bool {
        self.current.token_type() == token_type
    }

    fn is_eof(&self) -> bool {
        self.current.token_type() == TokenType::Eof
    }

    // Error handling

    fn synchronize(&mut self, lexer_reporter: &mut ErrorReporter<lexer::MessageMarker>) {
        self.panic_mode = false;

        while self.current.token_type() != TokenType::Eof {
            if self.previous.token_type() == TokenType::Semicolon {
                return;
            }

            match self.current.token_type() {
                TokenType::Export => return,
                TokenType::Template => return,
                TokenType::Interface => return,
                TokenType::Class => return,
                TokenType::Optimize => return,
                TokenType::Module => return,
                TokenType::Include => return,
                TokenType::Import => return,
                _ => {},
            };

            self.consume(lexer_reporter);
        }
    }
}
