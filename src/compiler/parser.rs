use lazy_static::lazy_static;
use non_empty_vec::ne_vec;
use crate::compiler::ast::simple::{ClassImplementsPart, Decl, Expr, ParameterPart, SingleImplementsPart, TemplateDeclKind, TemplateExpr, TemplateKind, TypePart, TypeReferencePart, VariableKind};
use crate::compiler::error::{ErrorReporter, Diagnostic, DiagnosticContext, Severity, NoteKind};
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
    ExpectedDeclarationEnd,
    ExpectedAfter(&'static str, &'static str, Option<Span>), // Span represents the token that made the parser expect this
    ExpectedBefore(&'static str, &'static str, Option<Span>), // Same here
    Expected(&'static str),
    ParameterCount(u32, u32, Span),
    ExpectedDeclaration,
    Unimplemented(&'static str),
}

impl Diagnostic<MessageMarker> for ParserError {
    fn name(&self) -> &'static str {
        match self {
            Self::ExpectedDeclarationEnd => "parser/expected_declaration_end",
            Self::ExpectedAfter(_, _, _) => "parser/expected_after",
            Self::ExpectedBefore(_, _, _) => "parser/expected_before",
            Self::Expected(_) => "parser/expected",
            Self::ParameterCount(_, _, _) => "parser/parameter_count",
            Self::ExpectedDeclaration => "parser/expected_declaration",
            Self::Unimplemented(_) => "parser/unimplemented",
        }
    }

    fn severity(&self) -> Severity {
        match self {
            Self::ExpectedDeclarationEnd
            | Self::ExpectedAfter(_, _, _)
            | Self::ExpectedBefore(_, _, _)
            | Self::Expected(_)
            | Self::ParameterCount(_, _, _)
            | Self::ExpectedDeclaration
            | Self::Unimplemented(_) => Severity::Error,
        }
    }

    fn message(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> String {
        match self {
            Self::ExpectedDeclarationEnd => String::from("Expected `;` after declaration"),
            Self::ExpectedAfter(a, b, _) => format!("Expected {} after {}", a, b),
            Self::ExpectedBefore(a, b, _) => format!("Expected {} before {}", a, b),
            Self::Expected(a) => format!("Expected {}", a),
            Self::ParameterCount(expected, got, _) => format!("Expected {} {}, found {}", *expected,
                if *expected == 1 { "parameter" } else { "parameters" }, *got),
            Self::ExpectedDeclaration => String::from("Expected declaration"),
            Self::Unimplemented(msg) => format!("Not implemented yet: {}", msg),
        }
    }

    fn primary_annotation(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<String> {
        match self {
            Self::ExpectedDeclarationEnd => Some(String::from("expected `;` here")),
            Self::ExpectedAfter(a, _, _) => Some(format!("expected {} here", a)),
            Self::ExpectedBefore(a, _, _) => Some(format!("expected {} here", a)),
            Self::Expected(a) => Some(format!("expected {} here", a)),
            Self::ParameterCount(expected, _, _) => Some(format!("expected {} {} here",
                *expected, if *expected == 1 { "parameter" } else { "parameters" })),
            Self::ExpectedDeclaration => Some(String::from("expected declaration here")),
            Self::Unimplemented(_) => Some(String::from("unimplemented feature used here")),
        }
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(Span, Option<String>)> {
        match self {
            Self::ExpectedAfter(_, _, Some(span)) => vec![(*span, Some(String::from("due to this")))],
            Self::ExpectedBefore(_, _, Some(span)) => vec![(*span, Some(String::from("due to this")))],
            Self::ParameterCount(_, _, span) => vec![(*span, Some(String::from("due to this")))],
            _ => Vec::new(),
        }
    }

    fn primary_note(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<(NoteKind, String)> {
        match self {
            _ => None,
        }
    }

    fn additional_notes(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(NoteKind, String)> {
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
        if self.matches(TokenType::Module, lexer_reporter) {
            return self.parse_module_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Interface, lexer_reporter) {
            return self.parse_interface_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Class, lexer_reporter) {
            return self.parse_class_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Type, lexer_reporter) {
            return self.parse_type_alias_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Template, lexer_reporter) {
            return self.parse_template_declaration(TemplateDeclKind::Template, reporter, lexer_reporter);
        } else if self.matches(TokenType::Optimize, lexer_reporter) {
            return self.parse_template_declaration(TemplateDeclKind::Optimize, reporter, lexer_reporter);
        } else if self.matches(TokenType::Include, lexer_reporter) {
            return self.parse_include_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Import, lexer_reporter) {
            return self.parse_import_declaration(reporter, lexer_reporter);
        } else if self.matches(TokenType::Export, lexer_reporter) {
            let variable_span = Some(self.previous.span());
            self.expect(TokenType::Identifier, ParserError::ExpectedAfter("name", "`export`", variable_span), reporter, lexer_reporter);
            let name = self.previous.clone();

            return self.parse_variable_declaration(variable_span, VariableKind::Export, name, reporter, lexer_reporter);
        } else if self.matches(TokenType::Inline, lexer_reporter) {
            let variable_span = Some(self.previous.span());
            self.expect(TokenType::Identifier, ParserError::ExpectedAfter("name", "`inline`", variable_span), reporter, lexer_reporter);
            let name = self.previous.clone();

            return self.parse_variable_declaration(variable_span, VariableKind::Inline, name, reporter, lexer_reporter);
        }

        if self.matches(TokenType::Identifier, lexer_reporter) {
            let name = self.previous.clone();

            if self.check(TokenType::Assign) {
                return self.parse_variable_declaration(None, VariableKind::Auto, name, reporter, lexer_reporter);
            } else {
                self.error_at(&name, ParserError::ExpectedDeclaration, true, reporter);
            }
        } else {
            self.error_at_current(ParserError::ExpectedDeclaration, true, reporter);
        }

        let decl = Decl::Error; // self.parse_statement(reporter, lexer_reporter);

        if reporter.panic_mode() {
            // eprintln!("[debug] Synchronizing");
            self.synchronize(reporter, lexer_reporter);
        }

        decl
    }

    fn parse_module_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let module_span = self.previous.span();

        self.expect(TokenType::Identifier, ParserError::ExpectedAfter("name", "`module`", Some(module_span)), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect(TokenType::BracketLeft, ParserError::ExpectedAfter("`{`", "module name", Some(module_span)), reporter, lexer_reporter);

        let mut declarations = Vec::new();

        while !self.check(TokenType::BracketRight) && !self.check(TokenType::Eof) {
            declarations.push(self.parse_declaration(reporter, lexer_reporter));
        }

        self.expect(TokenType::BracketRight, ParserError::ExpectedAfter("`}`", "statements in module", Some(module_span)), reporter, lexer_reporter);
        Decl::Module { name, declarations }
    }

    fn parse_interface_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let interface_span = self.previous.span();

        self.expect(TokenType::Identifier, ParserError::ExpectedAfter("name", "`interface`", Some(interface_span)), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect(TokenType::ParenthesisLeft, ParserError::ExpectedAfter("`(`", "interface name", Some(interface_span)), reporter, lexer_reporter);

        let (parameters, parameter_span) = if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.previous.start.index;

            let mut parameters = vec![self.parse_parameter_part("`(`", reporter, lexer_reporter)];

            while self.matches(TokenType::Comma, lexer_reporter) {
                parameters.push(self.parse_parameter_part("`,`", reporter, lexer_reporter));
            }

            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "interface parameter list", Some(interface_span)), reporter, lexer_reporter);
            let end_pos = self.previous.end.index;
            (parameters, Span::new(start_pos, end_pos))
        } else {
            let start_pos = self.previous.start.index;
            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "`(`", Some(interface_span)), reporter, lexer_reporter);
            let end_pos = self.previous.end.index;

            (Vec::new(), Span::new(start_pos, end_pos))
        };

        let implements = if self.matches(TokenType::Colon, lexer_reporter) {
            Some(self.parse_class_implements_part(reporter, lexer_reporter))
        } else { None };

        if self.matches(TokenType::Assign, lexer_reporter) {
            self.error(ParserError::Unimplemented("interface class representation"), false, reporter);

            while !self.check(TokenType::Semicolon) && !self.check(TokenType::Eof) {
                self.consume(lexer_reporter);
            }
        }

        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Interface {
            name,
            parameters,
            implements,
            class_repr: None,
            parameter_span,
        }
    }

    fn parse_class_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let class_span = self.previous.span();

        self.expect(TokenType::Identifier, ParserError::ExpectedAfter("name", "`class`", Some(class_span)), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect(TokenType::ParenthesisLeft, ParserError::ExpectedAfter("`(`", "class name", Some(class_span)), reporter, lexer_reporter);

        let (parameters, parameter_span) = if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.previous.start.index;

            let mut parameters = vec![self.parse_parameter_part("`(`", reporter, lexer_reporter)];

            while self.matches(TokenType::Comma, lexer_reporter) {
                parameters.push(self.parse_parameter_part("`,`", reporter, lexer_reporter));
            }

            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "class parameter list", Some(class_span)), reporter, lexer_reporter);
            let end_pos = self.previous.end.index;
            (parameters, Span::new(start_pos, end_pos))
        } else {
            let start_pos = self.previous.start.index;
            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "`(`", Some(class_span)), reporter, lexer_reporter);
            let end_pos = self.previous.end.index;

            (Vec::new(), Span::new(start_pos, end_pos))
        };

        self.expect(TokenType::Colon, ParserError::ExpectedAfter("`:`", "`)`", Some(class_span)), reporter, lexer_reporter);
        let implements = self.parse_class_implements_part(reporter, lexer_reporter);

        if self.matches(TokenType::Assign, lexer_reporter) {
            self.error(ParserError::Unimplemented("class representation"), false, reporter);

            while !self.check(TokenType::Semicolon) && !self.check(TokenType::Eof) {
                self.consume(lexer_reporter);
            }
        }

        let class_repr = None;

        while !self.check(TokenType::Semicolon) && !self.check(TokenType::Eof) {
            self.consume(lexer_reporter);
        }

        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Class {
            name,
            parameters,
            implements,
            class_repr,
            parameter_span,
        }
    }

    fn parse_type_alias_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let type_span = self.previous.span();

        self.expect(TokenType::Identifier, ParserError::ExpectedAfter("name", "`type`", Some(type_span)), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect(TokenType::Assign, ParserError::ExpectedAfter("`=`", "type alias name", Some(type_span)), reporter, lexer_reporter);
        let to = self.parse_type_part("`=`", reporter, lexer_reporter);

        if self.matches(TokenType::If, lexer_reporter) {
            self.error(ParserError::Unimplemented("type alias condition"), false, reporter);

            while !self.check(TokenType::Semicolon) && !self.check(TokenType::Eof) {
                self.consume(lexer_reporter);
            }
        }

        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::TypeAlias {
            name,
            to,
            condition: None,
        }
    }

    fn parse_template_declaration(&mut self, decl_kind: TemplateDeclKind, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let template_span = self.previous.span();

        let kind = if decl_kind == TemplateDeclKind::Template {
            if self.matches(TokenType::Type, lexer_reporter) {
                TemplateKind::Conversion {
                    span: self.previous.span(),
                }
            } else {
                self.expect_any(&*ALLOWED_TEMPLATE_NAME_TYPES, ParserError::ExpectedAfter("name", "`template`", Some(template_span)), reporter, lexer_reporter);

                TemplateKind::Template {
                    name: self.previous.clone(),
                }
            }
        } else { // TemplateDeclKind::Optimize
            let on = self.parse_type_part("`optimize`", reporter, lexer_reporter);

            TemplateKind::Optimize {
                on,
            }
        };

        self.expect(TokenType::ParenthesisLeft, ParserError::ExpectedAfter("`(`", "template name", Some(template_span)), reporter, lexer_reporter);

        let (parameters, parameter_span) = if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.previous.start.index;

            let mut parameters = vec![self.parse_parameter_part("`(`", reporter, lexer_reporter)];

            while self.matches(TokenType::Comma, lexer_reporter) {
                parameters.push(self.parse_parameter_part("`,`", reporter, lexer_reporter));
            }

            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "template parameter list", Some(template_span)), reporter, lexer_reporter);
            let end_pos = self.previous.end.index;
            (parameters, Span::new(start_pos, end_pos))
        } else {
            let start_pos = self.previous.start.index;
            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "`(`", Some(template_span)), reporter, lexer_reporter);
            let end_pos = self.previous.end.index;

            (Vec::new(), Span::new(start_pos, end_pos))
        };

        if let TemplateKind::Conversion { span } = kind {
            if parameters.is_empty() {
                self.error_at_span(parameter_span, ParserError::ParameterCount(1, 0, span), false, reporter);
            } else if parameters.len() > 1 {
                self.error_at_span(parameter_span, ParserError::ParameterCount(1, parameters.len() as u32, span), false, reporter);
            }
        };

        self.expect(TokenType::Colon, ParserError::ExpectedAfter("`:`", "`)`", Some(template_span)), reporter, lexer_reporter);
        let return_type = self.parse_type_part("`:`", reporter, lexer_reporter);

        self.expect(TokenType::BracketLeft, ParserError::ExpectedAfter("`{`", "template return type", Some(template_span)), reporter, lexer_reporter);
        self.error(ParserError::Unimplemented("template body"), false, reporter);

        while !self.check(TokenType::BracketRight) && !self.check(TokenType::Eof) {
            self.consume(lexer_reporter);
        }

        self.expect(TokenType::BracketRight, ParserError::ExpectedAfter("`}`", "template body", Some(template_span)), reporter, lexer_reporter);

        Decl::Template {
            kind,
            parameters,
            return_type,
            expr: TemplateExpr::Simple(Expr::Error),
            parameter_span,
        }
    }

    fn parse_include_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let include_span = self.previous.span();

        self.expect(TokenType::LiteralString, ParserError::ExpectedAfter("include path", "`include`", Some(include_span)), reporter, lexer_reporter);
        let path = self.previous.clone();

        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Include {
            path,
        }
    }

    fn parse_import_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let import_span = self.previous.span();
        let start_pos = self.previous.start.index;

        self.expect(TokenType::Identifier, ParserError::ExpectedAfter("module name", "`import`", Some(import_span)), reporter, lexer_reporter);
        let mut path = ne_vec![self.previous.clone()];
        let mut selector = None;

        while self.matches(TokenType::ColonColon, lexer_reporter) {
            if path.last().token_type != TokenType::Identifier {
                self.error_at(path.last(), ParserError::ExpectedBefore("module name", "`::`", None), true, reporter);
            }

            if self.matches(TokenType::BracketLeft, lexer_reporter) {
                self.expect_any(&*ALLOWED_TEMPLATE_NAME_TYPES, ParserError::ExpectedAfter("name", "`{`", Some(import_span)), reporter, lexer_reporter);
                let mut items = ne_vec![self.previous.clone()];

                while self.matches(TokenType::Comma, lexer_reporter) {
                    self.expect_any(&*ALLOWED_TEMPLATE_NAME_TYPES, ParserError::ExpectedAfter("name", "`,`", Some(import_span)), reporter, lexer_reporter);
                    items.push(self.previous.clone());
                }

                self.expect(TokenType::BracketRight, ParserError::ExpectedAfter("`}`", "name", Some(import_span)), reporter, lexer_reporter);
                selector = Some(items);
                break;
            }

            self.expect_any(&*ALLOWED_TEMPLATE_NAME_TYPES, ParserError::ExpectedAfter("name", "`::`", None), reporter, lexer_reporter);
            path.push(self.current.clone());
        }

        let end_pos = path.last().end.index;
        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Import {
            path,
            selector,
            span: Span::new(start_pos, end_pos),
        }
    }

    fn parse_variable_declaration(&mut self, variable_span: Option<Span>, kind: VariableKind, name: Token<'source>, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let start_pos = variable_span.map(|span| span.start).unwrap_or_else(|| name.start.index);

        self.expect(TokenType::Assign, ParserError::ExpectedAfter("`=`", "variable name", variable_span), reporter, lexer_reporter);
        self.error(ParserError::Unimplemented("variable expression"), false, reporter);

        while !self.check(TokenType::Semicolon) && !self.check(TokenType::Eof) {
            self.consume(lexer_reporter);
        }

        let end_pos = self.previous.end.index;
        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Variable {
            kind,
            name,
            expr: Expr::Error,
            span: Span::new(start_pos, end_pos),
        }
    }

    fn parse_parameter_part(&mut self, previous: &'static str, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> ParameterPart<'source> {
        self.expect(TokenType::Identifier, ParserError::ExpectedAfter("parameter name", previous, None), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect(TokenType::Colon, ParserError::ExpectedAfter("`:`", "parameter name", None), reporter, lexer_reporter);
        let parameter_type = self.parse_type_part("`:`", reporter, lexer_reporter);

        ParameterPart {
            name, parameter_type,
        }
    }

    fn parse_type_reference_part(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TypeReferencePart<'source> {
        // Assumes the first token to be consumed already

        let start_pos = self.previous.start.index;
        let mut tokens = ne_vec![self.previous.clone()];

        while self.matches(TokenType::ColonColon, lexer_reporter) {
            if tokens.last().token_type != TokenType::Identifier {
                self.error_at(tokens.last(), ParserError::ExpectedBefore("module name", "`::`", None), true, reporter);
            }

            self.expect_any(&*ALLOWED_VARIABLE_TYPES, ParserError::ExpectedAfter("type name", "`::`", None), reporter, lexer_reporter);
            tokens.push(self.current.clone());
        }

        let end_pos = tokens.last().end.index;
        TypeReferencePart(tokens, Span::new(start_pos, end_pos))
    }

    fn parse_type_part(&mut self, previous: &'static str, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TypePart<'source> {
        // Expects the first token, like `:`, to be consumed already (represented by "previous")

        if self.matches(TokenType::ParenthesisLeft, lexer_reporter) {
            let (args, args_span) = if !self.check(TokenType::ParenthesisRight) {
                let start_pos = self.previous.start.index;
                let mut args = vec![self.parse_type_part("`(`", reporter, lexer_reporter)];

                while self.matches(TokenType::Comma, lexer_reporter) {
                    args.push(self.parse_type_part("`,`", reporter, lexer_reporter));
                }

                self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "template parameter types", None), reporter, lexer_reporter);
                let end_pos = self.previous.end.index;
                (args, Span::new(start_pos, end_pos))
            } else {
                let start_pos = self.previous.start.index;
                self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "`(`", None), reporter, lexer_reporter);
                let end_pos = self.previous.end.index;

                (Vec::new(), Span::new(start_pos, end_pos))
            };

            self.expect(TokenType::Colon, ParserError::ExpectedAfter(previous, "template parameter list", None), reporter, lexer_reporter);
            let return_type = self.parse_type_part("`:`", reporter, lexer_reporter);

            TypePart::Template {
                args,
                return_type: Box::new(return_type),
                args_span,
            }
        } else {
            self.expect_any(&*ALLOWED_VARIABLE_TYPES, ParserError::ExpectedAfter("type name", "`:`", None), reporter, lexer_reporter);
            TypePart::Name(self.parse_type_reference_part(reporter, lexer_reporter))
        }
    }

    fn parse_single_implements_part(&mut self, previous: &'static str, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> SingleImplementsPart<'source> {
        // Expects the first token `:` or `,` to be consumed already

        let start_pos = self.current.start.index;
        self.expect_any(&*ALLOWED_VARIABLE_TYPES, ParserError::ExpectedAfter("type name", previous, None), reporter, lexer_reporter);
        let name = self.parse_type_reference_part(reporter, lexer_reporter);

        self.expect(TokenType::ParenthesisLeft, ParserError::ExpectedAfter("`(`", "type name", None), reporter, lexer_reporter);

        let (args, args_span) = if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.previous.start.index;
            let /* mut */ args = vec![/* self.parse_expr(reporter, lexer_reporter) */];

            // while self.matches(TokenType::Comma, lexer_reporter) {
            //     args.push(self.parse_expr(reporter, lexer_reporter));
            // }

            self.error_at_current(ParserError::Unimplemented("expressions"), false, reporter);

            while !self.check(TokenType::ParenthesisRight) && !self.check(TokenType::Eof) {
                self.consume(lexer_reporter);
            }

            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "arguments", None), reporter, lexer_reporter);
            let end_pos = self.previous.end.index;
            (args, Span::new(start_pos, end_pos))
        } else {
            let start_pos = self.previous.start.index;
            self.expect(TokenType::ParenthesisRight, ParserError::ExpectedAfter("`)`", "`(`", None), reporter, lexer_reporter);
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

    fn parse_class_implements_part(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> ClassImplementsPart<'source> {
        // Expects the first token `:` to be consumed already

        let start_pos = self.current.start.index;
        let mut parts = ne_vec![self.parse_single_implements_part("`:`", reporter, lexer_reporter)];

        while self.matches(TokenType::Comma, lexer_reporter) {
            parts.push(self.parse_single_implements_part("`,`", reporter, lexer_reporter));
        }

        ClassImplementsPart {
            parts,
            span: Span::new(start_pos, self.previous.end.index),
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
    fn expect_declaration_end(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        self.expect(TokenType::Semicolon, ParserError::ExpectedDeclarationEnd, reporter, lexer_reporter);
    }

    fn matches(&mut self, token_type: TokenType, lexer_reporter: &mut LexerErrorReporter) -> bool { // Should be called "match", but that's a keyword
        if !self.check(token_type) {
            return false;
        }

        self.consume(lexer_reporter);
        true
    }

    #[allow(unused)]
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

    #[allow(unused)]
    fn error(&mut self, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        self.error_at_span(Span::from(&self.previous), message, panic, reporter);
    }

    #[allow(unused)]
    fn error_at(&mut self, token: &Token<'source>, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        self.error_at_span(Span::from(token), message, panic, reporter);
    }

    #[allow(unused)]
    fn error_at_span(&mut self, span: Span, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        Self::error_at_impl(&mut self.had_error, span, message, panic, reporter);
    }

    fn error_at_impl(had_error: &mut bool, span: Span, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        reporter.report(span, message, panic);
        *had_error = true;
    }
}
