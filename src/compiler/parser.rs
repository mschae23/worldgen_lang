use lazy_static::lazy_static;
use crate::compiler::ast::simple::Decl;
use crate::compiler::error::{ErrorReporter, Message, MessageContext, MessageKind, NoteKind};
use crate::compiler::error::span::Span;
use crate::compiler::lexer::{Lexer, LexerErrorReporter, Token, TokenType};

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParserError {
}

impl Message<MessageMarker> for ParserError {
    fn name(&self) -> &'static str {
        todo!()
    }

    fn kind(&self) -> MessageKind {
        todo!()
    }

    fn description(&mut self, _context: &MessageContext<'_, MessageMarker>) -> String {
        todo!()
    }

    fn notes(&mut self, _context: &MessageContext<'_, MessageMarker>) -> Vec<(NoteKind, String)> {
        todo!()
    }
}

pub type ParserErrorReporter = ErrorReporter<MessageMarker, ParserError>;

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    previous: Token<'source>, current: Token<'source>,

    had_error: bool,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>) -> Parser<'_> {
        Parser {
            lexer,
            previous: Token::empty(),
            current: Token::empty(),
            had_error: false,
        }
    }

    pub fn had_error(&self) -> bool {
        self.had_error
    }

    // Declaration parsing

    pub fn parse(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Vec<Decl<'source>> {
        self.consume(lexer_reporter);

        let mut declarations = Vec::new();

        while !self.is_eof() {
            declarations.push(self.parse_declaration(reporter));
        }

        declarations
    }

    fn parse_declaration(&mut self, _reporter: &mut ParserErrorReporter) -> Decl<'source> {
        todo!()
    }

    fn consume(&mut self, lexer_reporter: &mut LexerErrorReporter) {
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

    fn expect(&mut self, token_type: TokenType, _message: &str, _reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        if self.current.token_type() == token_type {
            self.consume(lexer_reporter);
            return;
        }

        // self.error_at_current(message, true);
    }

    fn expect_any(&mut self, token_types: &[TokenType], _message: &str, _reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        for token_type in token_types {
            if self.current.token_type() == *token_type {
                self.consume(lexer_reporter);
                return;
            }
        }

        // self.error_at_current(message, true);
    }

    #[inline]
    fn expect_statement_end(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        self.expect(TokenType::Semicolon, "Expected ';' after statement", reporter, lexer_reporter);
    }

    fn matches(&mut self, token_type: TokenType, lexer_reporter: &mut LexerErrorReporter) -> bool { // Should be called "match", but that's a keyword
        if !self.check(token_type) {
            return false;
        }

        self.consume(lexer_reporter);
        true
    }

    fn matches_any(&mut self, token_types: &[TokenType], lexer_reporter: &mut LexerErrorReporter) -> bool {
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

    fn synchronize(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        reporter.exit_panic_mode();

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

    fn error_at_current(&mut self, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        self.error_at_span(Span::from(&self.current), message, panic, reporter);
    }

    fn error(&mut self, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        self.error_at_span(Span::from(&self.previous), message, panic, reporter);
    }

    fn error_at(&mut self, token: &Token<'source>, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        self.error_at_span(Span::from(token), message, panic, reporter);
    }

    fn error_at_span(&mut self, span: Span, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        Self::error_at_impl(&mut self.had_error, span, message, panic, reporter);
    }

    fn error_at_impl(had_error: &mut bool, span: Span, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        reporter.report(span, message, panic);
        *had_error = true;
    }
}
