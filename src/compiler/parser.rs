use lazy_static::lazy_static;
use non_empty_vec::ne_vec;
use crate::compiler::ast::simple::{Decl, SingleImplementsPart, TypePart, TypeReferencePart};
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

    static ref ALLOWED_VARIABLE_TYPES: [TokenType; 9] = [
        TokenType::Int, TokenType::Float,
        TokenType::Boolean,
        TokenType::String,
        TokenType::Object, TokenType::Array,
        TokenType::Module,
        TokenType::Identifier,
        TokenType::Underscore,
    ];
}

pub type MessageMarker = ();

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParserError {
    ExpectedStatementEnd,
    ExpectedAfter(&'static str, &'static str),
    ExpectedBefore(&'static str, &'static str),
    Expected(&'static str),
    ExpectedDeclaration,
}

impl Message<MessageMarker> for ParserError {
    fn name(&self) -> &'static str {
        match self {
            Self::ExpectedStatementEnd => "parser/expected_statement_end",
            Self::ExpectedAfter(_, _) => "parser/expected_after",
            Self::ExpectedBefore(_, _) => "parser/expected_before",
            Self::Expected(_) => "parser/expected",
            Self::ExpectedDeclaration => "parser/expected_declaration",
        }
    }

    fn kind(&self) -> MessageKind {
        match self {
            Self::ExpectedStatementEnd
                | Self::ExpectedAfter(_, _)
                | Self::ExpectedBefore(_, _)
                | Self::Expected(_)
                | Self::ExpectedDeclaration => MessageKind::Error,
        }
    }

    fn description(&self, _context: &MessageContext<'_, MessageMarker>) -> String {
        match self {
            Self::ExpectedStatementEnd => String::from("Expected `;` after statement"),
            Self::ExpectedAfter(a, b) => format!("Expected {} after {}", a, b),
            Self::ExpectedBefore(a, b) => format!("Expected {} before {}", a, b),
            Self::Expected(a) => format!("Expected {}", a),
            Self::ExpectedDeclaration => String::from("Expected declaration"),
        }
    }

    fn primary_annotation(&self, _context: &MessageContext<'_, MessageMarker>) -> Option<String> {
        match self {
            Self::ExpectedStatementEnd => Some(String::from("expected `;` here")),
            Self::ExpectedAfter(a, _) => Some(format!("expected {} here", a)),
            Self::ExpectedBefore(a, _) => Some(format!("expected {} here", a)),
            Self::Expected(a) => Some(format!("expected {} here", a)),
            Self::ExpectedDeclaration => Some(String::from("expected declaration here")),
        }
    }

    fn additional_annotations(&self, _context: &MessageContext<'_, MessageMarker>) -> Vec<(Span, Option<String>)> {
        match self {
            _ => Vec::new(),
        }
    }

    fn primary_note(&self, _context: &MessageContext<'_, MessageMarker>) -> Option<(NoteKind, String)> {
        match self {
            _ => None,
        }
    }

    fn additional_notes(&self, _context: &MessageContext<'_, MessageMarker>) -> Vec<(NoteKind, String)> {
        match self {
            _ => Vec::new(),
        }
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
            declarations.push(self.parse_declaration(reporter, lexer_reporter));
        }

        declarations
    }

    fn parse_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        // eprintln!("[debug] Parsing decl");

        if self.matches(TokenType::Module, lexer_reporter) {
            return self.parse_module_declaration(reporter, lexer_reporter);
        } /* else if self.matches(TokenType::Export, lexer_reporter) {
            return self.parse_export_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Template, lexer_reporter) {
            return self.parse_template_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Include, lexer_reporter) {
            return self.parse_include_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Import, lexer_reporter) {
            return self.parse_import_declaration(reporter, lexer_reporter);
        } */

        self.error_at_current(ParserError::ExpectedDeclaration, true, reporter);
        let decl = Decl::Error; // self.parse_statement(reporter, lexer_reporter);

        if reporter.panic_mode() {
            // eprintln!("[debug] Synchronizing");
            self.synchronize(reporter, lexer_reporter);
        }

        decl
    }

    fn parse_module_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        self.expect(TokenType::Identifier, ParserError::ExpectedAfter("name", "`module`"), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect(TokenType::BracketLeft, ParserError::ExpectedAfter("`{`", "module name"), reporter, lexer_reporter);

        let mut declarations = Vec::new();

        while !self.check(TokenType::BracketRight) && !self.check(TokenType::Eof) {
            declarations.push(self.parse_declaration(reporter, lexer_reporter));
        }

        self.expect(TokenType::BracketRight, ParserError::ExpectedAfter("`}`", "statements in module"), reporter, lexer_reporter);
        Decl::Module { name, declarations }
    }

    fn parse_type_reference_part(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TypeReferencePart<'source> {
        // Assumes the first token to be consumed already

        let start_pos = self.previous.start.index;
        let mut tokens = ne_vec![self.previous.clone()];

        while self.matches(TokenType::ColonColon, lexer_reporter) {
            if tokens.last().token_type != TokenType::Identifier {
                self.error_at(tokens.last(), ParserError::ExpectedBefore("module name", "`::`"), true, reporter);
            }

            self.expect_any(&*ALLOWED_VARIABLE_TYPES, ParserError::ExpectedAfter("type name", "`::`"), reporter, lexer_reporter);
            tokens.push(self.current.clone());
        }

        let end_pos = tokens.last().end.index;
        TypeReferencePart(tokens, Span::new(start_pos, end_pos))
    }

    fn parse_type_part(&mut self, previous: &'static str, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TypePart<'source> {
        // Expects the first token, like `:`, to be consumed already (represented by "previous")

        if self.matches(TokenType::ParenthesisLeft, lexer_reporter) {
            let (args, args_span) = if !self.check(TokenType::ParenthesisRight) {
                let start_pos = self.current.start.index;
                let mut args = vec![self.parse_type_part("`(`", reporter, lexer_reporter)];

                while self.matches(TokenType::Comma, lexer_reporter) {
                    args.push(self.parse_type_part("`,`", reporter, lexer_reporter));
                }

                let end_pos = self.previous.end.index;
                self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "template parameter types"), reporter, lexer_reporter);
                (args, Span::new(start_pos, end_pos))
            } else {
                let start_pos = self.previous.start.index;
                self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "`(`"), reporter, lexer_reporter);
                let end_pos = self.previous.end.index;

                (Vec::new(), Span::new(start_pos, end_pos))
            };

            self.expect(TokenType::Colon, ParserError::ExpectedAfter(previous, "template parameter list"), reporter, lexer_reporter);
            let return_type = self.parse_type_part("`:`", reporter, lexer_reporter);

            TypePart::Template {
                args,
                return_type: Box::new(return_type),
                args_span,
            }
        } else {
            self.expect_any(&*ALLOWED_VARIABLE_TYPES, ParserError::ExpectedAfter("type name", "`:`"), reporter, lexer_reporter);
            TypePart::Name(self.parse_type_reference_part(reporter, lexer_reporter))
        }
    }

    fn parse_single_implements_part(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> SingleImplementsPart<'source> {
        // Expects the first token `:` to be consumed already

        let start_pos = self.current.start.index;
        self.expect_any(&*ALLOWED_VARIABLE_TYPES, ParserError::ExpectedAfter("type name", "`:`"), reporter, lexer_reporter);
        let name = self.parse_type_reference_part(reporter, lexer_reporter);

        self.expect(TokenType::ParenthesisLeft, ParserError::ExpectedAfter("`(`", "type name"), reporter, lexer_reporter);

        let (args, args_span) = /* if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.current.start.index;
            let mut args = vec![self.parse_expr(reporter, lexer_reporter)];

            while self.matches(TokenType::Comma, lexer_reporter) {
                args.push(self.parse_expr(reporter, lexer_reporter));
            }

            let end_pos = self.previous.end.index;
            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "arguments"), reporter, lexer_reporter);
            (args, Span::new(start_pos, end_pos))
        } else */ {
            let start_pos = self.previous.start.index;
            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "`(`"), reporter, lexer_reporter);
            let end_pos = self.previous.end.index;

            (Vec::new(), Span::new(start_pos, end_pos))
        };

        let end_pos = self.previous.end.index;
        SingleImplementsPart {
            name,
            parameters: args,
            span: Span::new(start_pos, end_pos),
            parameter_span: args_span,
        }
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

    fn expect(&mut self, token_type: TokenType, message: ParserError, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        if self.current.token_type() == token_type {
            self.consume(lexer_reporter);
            return;
        }

        self.error_at_current(message, true, reporter);
    }

    fn expect_any(&mut self, token_types: &[TokenType], message: ParserError, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        for token_type in token_types {
            if self.current.token_type() == *token_type {
                self.consume(lexer_reporter);
                return;
            }
        }

        self.error_at_current(message, true, reporter);
    }

    #[inline]
    fn expect_statement_end(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        self.expect(TokenType::Semicolon, ParserError::ExpectedStatementEnd, reporter, lexer_reporter);
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
        self.consume(lexer_reporter); // DEBUG

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
