use std::fmt::{Display, Formatter};
use lazy_static::lazy_static;
use non_empty_vec::ne_vec;
use crate::compiler::ast::simple::{ClassImplementsPart, ClassReprPart, Decl, Expr, ParameterPart, PrimitiveTypeKind, TemplateDeclKind, TemplateExpr, TemplateKind, TypePart, TypeReferencePart, VariableKind};
use crate::compiler::error::{ErrorReporter, Diagnostic, DiagnosticContext, Severity, NoteKind, FileId};
use crate::compiler::error::span::{Span, SpanWithFile};
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
        TokenType::Type,
        TokenType::Identifier,
        TokenType::Underscore,
    ];
}

pub type MessageMarker = ();

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Float, Int,
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralKind::Float => write!(f, "float"),
            LiteralKind::Int => write!(f, "int"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParserError {
    ExpectedDeclarationEnd,
    ExpectedAfter(&'static str, &'static str, Option<SpanWithFile>), // Span represents the token that made the parser expect this
    ExpectedBefore(&'static str, &'static str, Option<SpanWithFile>), // Same here
    Expected(&'static str),
    ParameterCount(u32, u32, SpanWithFile),
    TooManyArguments(u32),
    FailedParseLiteral(LiteralKind), // If the lexer is implemented properly, it shouldn't actually be possible for this to happen
    ExpectedDeclaration,
    ExpectedExpression,
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
            Self::TooManyArguments(_) => "parser/argument_count",
            Self::FailedParseLiteral(_) => "parser/failed_parse_literal",
            Self::ExpectedDeclaration => "parser/expected_declaration",
            Self::ExpectedExpression => "parser/expected_expression",
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
            | Self::TooManyArguments(_)
            | Self::FailedParseLiteral(_)
            | Self::ExpectedDeclaration
            | Self::ExpectedExpression
            | Self::Unimplemented(_) => Severity::Error,
        }
    }

    fn message(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> String {
        match self {
            Self::ExpectedDeclarationEnd => String::from("expected `;` after declaration"),
            Self::ExpectedAfter(a, b, _) => format!("expected {} after {}", a, b),
            Self::ExpectedBefore(a, b, _) => format!("expected {} before {}", a, b),
            Self::Expected(a) => format!("expected {}", a),
            Self::ParameterCount(expected, got, _) => format!("expected {} {}, found {}", *expected,
                if *expected == 1 { "parameter" } else { "parameters" }, *got),
            Self::TooManyArguments(actual) => format!("more than 255 arguments to a template call are not supported (found {})", *actual),
            Self::FailedParseLiteral(kind) => format!("failed to parse {} literal", kind),
            Self::ExpectedDeclaration => String::from("expected declaration"),
            Self::ExpectedExpression => String::from("expected expression"),
            Self::Unimplemented(msg) => format!("not implemented yet: {}", msg),
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
            Self::TooManyArguments(_) => Some(String::from("found too many arguments here")),
            Self::FailedParseLiteral(_) => None,
            Self::ExpectedDeclaration => Some(String::from("expected declaration here")),
            Self::ExpectedExpression => Some(String::from("expected expression here")),
            Self::Unimplemented(_) => Some(String::from("unimplemented feature used here")),
        }
    }

    fn additional_annotations(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(SpanWithFile, Option<String>)> {
        match self {
            Self::ExpectedAfter(_, _, Some(span)) => vec![(*span, Some(String::from("due to this")))],
            Self::ExpectedBefore(_, _, Some(span)) => vec![(*span, Some(String::from("due to this")))],
            Self::ParameterCount(_, _, span) => vec![(*span, Some(String::from("due to this")))],
            _ => Vec::new(),
        }
    }

    fn primary_note(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Option<(NoteKind, String)> {
        #[allow(clippy::match_single_binding)]
        match self {
            _ => None,
        }
    }

    fn additional_notes(&self, _context: &DiagnosticContext<'_, MessageMarker>) -> Vec<(NoteKind, String)> {
        #[allow(clippy::match_single_binding)]
        match self {
            _ => Vec::new(),
        }
    }
}

pub type ParserErrorReporter = ErrorReporter<MessageMarker, ParserError>;

pub struct Parser<'source> {
    file_id: FileId,
    lexer: Lexer<'source>,
    previous: Token<'source>, current: Token<'source>,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>, file_id: FileId) -> Parser<'_> {
        Parser {
            file_id,
            lexer,
            previous: Token::empty(),
            current: Token::empty(),
        }
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
            self.expect_after(TokenType::Identifier, "name", "`export`", variable_span, reporter, lexer_reporter);
            let name = self.previous.clone();

            return self.parse_variable_declaration(variable_span, VariableKind::Export, name, reporter, lexer_reporter);
        } else if self.matches(TokenType::Inline, lexer_reporter) {
            let variable_span = Some(self.previous.span());
            self.expect_after(TokenType::Identifier, "name", "`inline`", variable_span, reporter, lexer_reporter);
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

        self.consume(lexer_reporter);
        let decl = Decl::Error;

        if reporter.panic_mode() {
            // eprintln!("[debug] Synchronizing");
            self.synchronize(reporter, lexer_reporter);
        }

        decl
    }

    fn parse_module_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let module_span = self.previous.span();

        self.expect_after(TokenType::Identifier, "name", "`module`", Some(module_span), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect_after(TokenType::BracketLeft, "`{`", "module name", Some(module_span), reporter, lexer_reporter);

        let mut declarations = Vec::new();

        while !self.check(TokenType::BracketRight) && !self.check(TokenType::Eof) {
            declarations.push(self.parse_declaration(reporter, lexer_reporter));
        }

        self.expect_after(TokenType::BracketRight, "`}`", "statements in module", Some(module_span), reporter, lexer_reporter);

        Decl::Module {
            key_span: SpanWithFile::new(self.file_id, module_span),
            name, declarations
        }
    }

    fn parse_interface_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let interface_span = self.previous.span();

        self.expect_after(TokenType::Identifier, "name", "`interface`", Some(interface_span), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect_after(TokenType::ParenthesisLeft, "`(`", "interface name", Some(interface_span), reporter, lexer_reporter);

        let (parameters, parameter_span) = if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.previous.span.start;

            let mut parameters = vec![self.parse_parameter_part("`(`", reporter, lexer_reporter)];

            while self.matches(TokenType::Comma, lexer_reporter) {
                parameters.push(self.parse_parameter_part("`,`", reporter, lexer_reporter));
            }

            self.expect_after(TokenType::ParenthesisRight, "`)`", "interface parameter list", Some(interface_span), reporter, lexer_reporter);
            let end_pos = self.previous.span.end;
            (parameters, Span::new(start_pos, end_pos))
        } else {
            let start_pos = self.previous.span.start;
            self.expect_after(TokenType::ParenthesisRight, "`)`", "`(`", Some(interface_span), reporter, lexer_reporter);
            let end_pos = self.previous.span.end;

            (Vec::new(), Span::new(start_pos, end_pos))
        };

        let implements = if self.matches(TokenType::Colon, lexer_reporter) {
            Some(self.parse_class_implements_part("`:`", reporter, lexer_reporter))
        } else { None };

        let repr = if self.matches(TokenType::Assign, lexer_reporter) {
            let repr_start = self.current.span.start;
            let repr = self.parse_expression(reporter, lexer_reporter);
            let repr_end = self.previous.span.end;

            Some(ClassReprPart { expr: repr, span: Span::new(repr_start, repr_end), })
        } else {
            None
        };

        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Class {
            key_span: SpanWithFile::new(self.file_id, interface_span),
            interface: true,
            name,
            parameters,
            implements,
            class_repr: repr,
            parameter_span,
        }
    }

    fn parse_class_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let class_span = self.previous.span();

        self.expect_after(TokenType::Identifier, "name", "`class`", Some(class_span), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect_after(TokenType::ParenthesisLeft, "`(`", "class name", Some(class_span), reporter, lexer_reporter);

        let (parameters, parameter_span) = if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.previous.span.start;

            let mut parameters = vec![self.parse_parameter_part("`(`", reporter, lexer_reporter)];

            while self.matches(TokenType::Comma, lexer_reporter) {
                parameters.push(self.parse_parameter_part("`,`", reporter, lexer_reporter));
            }

            self.expect_after(TokenType::ParenthesisRight, "`)`", "class parameter list", Some(class_span), reporter, lexer_reporter);
            let end_pos = self.previous.span.end;
            (parameters, Span::new(start_pos, end_pos))
        } else {
            let start_pos = self.previous.span.start;
            self.expect_after(TokenType::ParenthesisRight, "`)`", "`(`", Some(class_span), reporter, lexer_reporter);
            let end_pos = self.previous.span.end;

            (Vec::new(), Span::new(start_pos, end_pos))
        };

        self.expect_after(TokenType::Colon, "`:`", "`)`", Some(class_span), reporter, lexer_reporter);
        let implements = self.parse_class_implements_part("`:`", reporter, lexer_reporter);

        let repr = if self.matches(TokenType::Assign, lexer_reporter) {
            let repr_start = self.current.span.start;
            let repr = self.parse_expression(reporter, lexer_reporter);
            let repr_end = self.previous.span.end;

            Some(ClassReprPart { expr: repr, span: Span::new(repr_start, repr_end), })
        } else {
            None
        };

        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Class {
            key_span: SpanWithFile::new(self.file_id, class_span),
            interface: false,
            name,
            parameters,
            implements: Some(implements),
            class_repr: repr,
            parameter_span,
        }
    }

    fn parse_type_alias_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let type_span = self.previous.span();

        self.expect_after(TokenType::Identifier, "name", "`type`", Some(type_span), reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect_after(TokenType::Assign, "`=`", "type alias name", Some(type_span), reporter, lexer_reporter);
        let to = self.parse_type_part("`=`", reporter, lexer_reporter);

        let condition = if self.matches(TokenType::If, lexer_reporter) {
            Some(self.parse_expression(reporter, lexer_reporter))
        } else { None };

        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::TypeAlias {
            key_span: SpanWithFile::new(self.file_id, type_span),
            name,
            to,
            condition,
        }
    }

    fn parse_template_declaration(&mut self, decl_kind: TemplateDeclKind, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let mut template_span = self.previous.span();

        let kind = if decl_kind == TemplateDeclKind::Template {
            if self.matches(TokenType::Type, lexer_reporter) {
                template_span = template_span.mix(self.previous.span());

                TemplateKind::Conversion {
                    span: template_span,
                }
            } else {
                self.expect_any_after(&*ALLOWED_TEMPLATE_NAME_TYPES, "name", "`template`", Some(template_span), reporter, lexer_reporter);

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

        self.expect_after(TokenType::ParenthesisLeft, "`(`", "template name", Some(template_span), reporter, lexer_reporter);

        let (parameters, parameter_span) = if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.previous.span.start;

            let mut parameters = vec![self.parse_parameter_part("`(`", reporter, lexer_reporter)];

            while self.matches(TokenType::Comma, lexer_reporter) {
                parameters.push(self.parse_parameter_part("`,`", reporter, lexer_reporter));
            }

            self.expect_after(TokenType::ParenthesisRight, "`)`", "template parameter list", Some(template_span), reporter, lexer_reporter);
            let end_pos = self.previous.span.end;
            (parameters, Span::new(start_pos, end_pos))
        } else {
            let start_pos = self.previous.span.start;
            self.expect_after(TokenType::ParenthesisRight, "`)`", "`(`", Some(template_span), reporter, lexer_reporter);
            let end_pos = self.previous.span.end;

            (Vec::new(), Span::new(start_pos, end_pos))
        };

        if let TemplateKind::Conversion { span } = kind {
            if parameters.is_empty() {
                self.error_at_span(parameter_span, ParserError::ParameterCount(1, 0, SpanWithFile::new(self.file_id, span)), false, reporter);
            } else if parameters.len() > 1 {
                self.error_at_span(parameter_span, ParserError::ParameterCount(1, parameters.len() as u32, SpanWithFile::new(self.file_id, span)), false, reporter);
            }
        };

        self.expect_after(TokenType::Colon, "`:`", "`)`", Some(template_span), reporter, lexer_reporter);
        let return_type = self.parse_type_part("`:`", reporter, lexer_reporter);

        self.expect_after(TokenType::BracketLeft, "`{`", "template return type", Some(template_span), reporter, lexer_reporter);
        let expr = self.parse_block_template_expression(reporter, lexer_reporter);

        Decl::Template {
            key_span: SpanWithFile::new(self.file_id, template_span),
            kind,
            parameters,
            return_type,
            expr,
            parameter_span,
        }
    }

    fn parse_include_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let include_span = self.previous.span();

        self.expect_after(TokenType::LiteralString, "include path", "`include`", Some(include_span), reporter, lexer_reporter);
        let path = self.previous.clone();

        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Include {
            key_span: SpanWithFile::new(self.file_id, include_span),
            path,
        }
    }

    fn parse_import_declaration(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let import_span = self.previous.span();
        let start_pos = self.previous.span.start;

        self.expect_after(TokenType::Identifier, "module name", "`import`", Some(import_span), reporter, lexer_reporter);
        let mut path = ne_vec![self.previous.clone()];
        let mut selector = None;

        while self.matches(TokenType::ColonColon, lexer_reporter) {
            if path.last().token_type != TokenType::Identifier {
                self.error_before(path.last().span(), "module name", "`::`", None, reporter);
            }

            if self.matches(TokenType::BracketLeft, lexer_reporter) {
                self.expect_any_after(&*ALLOWED_TEMPLATE_NAME_TYPES, "name", "`{`", Some(import_span), reporter, lexer_reporter);
                let mut items = ne_vec![self.previous.clone()];

                while self.matches(TokenType::Comma, lexer_reporter) {
                    self.expect_any_after(&*ALLOWED_TEMPLATE_NAME_TYPES, "name", "`,`", Some(import_span), reporter, lexer_reporter);
                    items.push(self.previous.clone());
                }

                self.expect_after(TokenType::BracketRight, "`}`", "name", Some(import_span), reporter, lexer_reporter);
                selector = Some(items);
                break;
            }

            self.expect_any_after(&*ALLOWED_TEMPLATE_NAME_TYPES, "name", "`::`", None, reporter, lexer_reporter);
            path.push(self.current.clone());
        }

        let end_pos = path.last().span.end;
        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Import {
            key_span: SpanWithFile::new(self.file_id, import_span),
            path,
            selector,
            span: Span::new(start_pos, end_pos),
        }
    }

    fn parse_variable_declaration(&mut self, variable_span: Option<Span>, kind: VariableKind, name: Token<'source>, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Decl<'source> {
        let start_pos = variable_span.map(|span| span.start).unwrap_or_else(|| name.span.start);
        let key_span = variable_span.unwrap_or(self.previous.span());

        self.expect_after(TokenType::Assign, "`=`", "variable name", variable_span, reporter, lexer_reporter);
        let expr = self.parse_expression(reporter, lexer_reporter);

        let end_pos = self.previous.span.end;
        self.expect_declaration_end(reporter, lexer_reporter);

        Decl::Variable {
            key_span: SpanWithFile::new(self.file_id, key_span),
            kind,
            name,
            expr,
            span: Span::new(start_pos, end_pos),
        }
    }

    fn parse_parameter_part(&mut self, previous: &'static str, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> ParameterPart<'source> {
        self.expect_after(TokenType::Identifier, "parameter name", previous, None, reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect_after(TokenType::Colon, "`:`", "parameter name", None, reporter, lexer_reporter);
        let parameter_type = self.parse_type_part("`:`", reporter, lexer_reporter);

        ParameterPart {
            name, parameter_type,
        }
    }

    fn parse_type_reference_part(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TypeReferencePart<'source> {
        // Assumes the first token to be consumed already

        let start_pos = self.previous.span.start;
        let mut tokens = ne_vec![self.previous.clone()];

        while self.matches(TokenType::ColonColon, lexer_reporter) {
            if tokens.last().token_type != TokenType::Identifier {
                self.error_before(tokens.last().span(), "module name", "`::`", None, reporter);
            }

            self.expect_any_after(&*ALLOWED_VARIABLE_TYPES, "type name", "`::`", None, reporter, lexer_reporter);
            tokens.push(self.current.clone());
        }

        let end_pos = tokens.last().span.end;
        TypeReferencePart(tokens, Span::new(start_pos, end_pos))
    }

    fn parse_type_part(&mut self, previous: &'static str, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TypePart<'source> {
        // Expects the first token, like `:`, to be consumed already (represented by "previous")

        if self.matches(TokenType::ParenthesisLeft, lexer_reporter) {
            let (args, args_span) = if !self.check(TokenType::ParenthesisRight) {
                let start_pos = self.previous.span.start;
                let mut args = vec![self.parse_type_part("`(`", reporter, lexer_reporter)];

                while self.matches(TokenType::Comma, lexer_reporter) {
                    args.push(self.parse_type_part("`,`", reporter, lexer_reporter));
                }

                self.expect_after(TokenType::ParenthesisRight, "`)`", "template parameter types", None, reporter, lexer_reporter);
                let end_pos = self.previous.span.end;
                (args, Span::new(start_pos, end_pos))
            } else {
                let start_pos = self.previous.span.start;
                self.expect_after(TokenType::ParenthesisRight, "`)`", "`(`", None, reporter, lexer_reporter);
                let end_pos = self.previous.span.end;

                (Vec::new(), Span::new(start_pos, end_pos))
            };

            self.expect_after(TokenType::Colon, previous, "template parameter list", None, reporter, lexer_reporter);
            let return_type = self.parse_type_part("`:`", reporter, lexer_reporter);

            TypePart::Template {
                args,
                return_type: Box::new(return_type),
                args_span,
            }
        } else {
            self.expect_any_after(&*ALLOWED_VARIABLE_TYPES, "type name", "`:`", None, reporter, lexer_reporter);

            match self.previous.token_type() {
                TokenType::Int => return TypePart::Primitive(PrimitiveTypeKind::Int, self.previous.span()),
                TokenType::Float => return TypePart::Primitive(PrimitiveTypeKind::Float, self.previous.span()),
                TokenType::Boolean => return TypePart::Primitive(PrimitiveTypeKind::Boolean, self.previous.span()),
                TokenType::String => return TypePart::Primitive(PrimitiveTypeKind::String, self.previous.span()),
                TokenType::Object => return TypePart::Primitive(PrimitiveTypeKind::Object, self.previous.span()),
                TokenType::Array => return TypePart::Primitive(PrimitiveTypeKind::Array, self.previous.span()),
                TokenType::Type => return TypePart::Primitive(PrimitiveTypeKind::Type, self.previous.span()),
                _ => {},
            }

            TypePart::Name(self.parse_type_reference_part(reporter, lexer_reporter))
        }
    }

    fn parse_class_implements_part(&mut self, previous: &'static str, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> ClassImplementsPart<'source> {
        // Expects the first token `:` or `,` to be consumed already

        let start_pos = self.current.span.start;
        self.expect_any_after(&*ALLOWED_VARIABLE_TYPES, "type name", previous, None, reporter, lexer_reporter);
        let name = self.parse_type_reference_part(reporter, lexer_reporter);

        self.expect_after(TokenType::ParenthesisLeft, "`(`", "type name", None, reporter, lexer_reporter);

        let (args, args_span) = if !self.check(TokenType::ParenthesisRight) {
            let start_pos = self.previous.span.start;
            let mut args = vec![self.parse_expression(reporter, lexer_reporter)];

            while self.matches(TokenType::Comma, lexer_reporter) {
                args.push(self.parse_expression(reporter, lexer_reporter));
            }

            self.expect_after(TokenType::ParenthesisRight, "`)`", "arguments", None, reporter, lexer_reporter);
            let end_pos = self.previous.span.end;
            (args, Span::new(start_pos, end_pos))
        } else {
            let start_pos = self.previous.span.start;
            self.expect_after(TokenType::ParenthesisRight, "`)`", "`(`", None, reporter, lexer_reporter);
            let end_pos = self.previous.span.end;

            (Vec::new(), Span::new(start_pos, end_pos))
        };

        let end_pos = self.previous.span.end;
        ClassImplementsPart {
            name,
            parameters: args,
            span: Span::new(start_pos, end_pos),
            parameter_span: args_span,
        }
    }

    // Template expression parsing

    fn parse_block_template_expression(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TemplateExpr<'source> {
        // Expects `{` to be matched already
        let start_pos = self.previous.span.start;
        let open_span = self.previous.span();

        let mut expressions = ne_vec![self.parse_template_expression(reporter, lexer_reporter)];

        while self.matches(TokenType::Semicolon, lexer_reporter) {
            expressions.push(self.parse_template_expression(reporter, lexer_reporter));
        }

        self.expect_after(TokenType::BracketRight, "`}`", "last expression in block", Some(open_span), reporter, lexer_reporter);
        let end_pos = self.previous.span.end;

        TemplateExpr::Block {
            expressions,
            span: Span::new(start_pos, end_pos),
        }
    }

    fn parse_template_expression(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TemplateExpr<'source> {
        if self.matches(TokenType::If, lexer_reporter) {
            self.parse_if_template_expression(reporter, lexer_reporter)
        } else {
            self.parse_simple_template_expression(reporter, lexer_reporter)
        }
    }

    fn parse_if_template_expression(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TemplateExpr<'source> {
        let if_span = self.previous.span();

        self.expect_after(TokenType::ParenthesisLeft, "`(`", "`if``", None, reporter, lexer_reporter);
        let condition_start_pos = self.previous.span.start;
        let condition = self.parse_expression(reporter, lexer_reporter);
        self.expect_after(TokenType::ParenthesisRight, "`}`", "if expression condition`", None, reporter, lexer_reporter);
        let condition_end_pos = self.previous.span.end;

        let then_block = if self.matches(TokenType::BracketLeft, lexer_reporter) {
            self.parse_block_template_expression(reporter, lexer_reporter)
        } else {
            self.parse_template_expression(reporter, lexer_reporter)
        };

        self.expect_after(TokenType::Else, "`else`", "`}`", Some(if_span), reporter, lexer_reporter);

        let otherwise_block = if self.matches(TokenType::BracketLeft, lexer_reporter) {
            self.parse_block_template_expression(reporter, lexer_reporter)
        } else {
            self.parse_template_expression(reporter, lexer_reporter)
        };

        TemplateExpr::If {
            condition,
            then: Box::new(then_block),
            otherwise: Box::new(otherwise_block),
            condition_span: Span::new(condition_start_pos, condition_end_pos),
        }
    }

    fn parse_simple_template_expression(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> TemplateExpr<'source> {
        TemplateExpr::Simple(self.parse_expression(reporter, lexer_reporter))
    }

    // Expression parsing

    fn parse_expression(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        self.parse_or(reporter, lexer_reporter)
    }

    fn parse_or(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let mut expr = self.parse_and(reporter, lexer_reporter);

        while self.matches(TokenType::ShortcircuitOr, lexer_reporter) {
            let operator = self.previous.clone();
            let right = self.parse_and(reporter, lexer_reporter);

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_and(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let mut expr = self.parse_equality(reporter, lexer_reporter);

        while self.matches(TokenType::ShortcircuitAnd, lexer_reporter) {
            let operator = self.previous.clone();
            let right = self.parse_equality(reporter, lexer_reporter);

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_equality(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let mut expr = self.parse_comparison(reporter, lexer_reporter);

        while self.matches_any(&[TokenType::Equal, TokenType::NotEqual], lexer_reporter) {
            let operator = self.previous.clone();
            let right = self.parse_comparison(reporter, lexer_reporter);

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_comparison(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let mut expr = self.parse_term(reporter, lexer_reporter);

        while self.matches_any(&[TokenType::Less, TokenType::LessEqual, TokenType::Greater, TokenType::GreaterEqual], lexer_reporter) {
            let operator = self.previous.clone();
            let right = self.parse_term(reporter, lexer_reporter);

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_term(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let mut expr = self.parse_factor(reporter, lexer_reporter);

        while self.matches_any(&[TokenType::Plus, TokenType::Minus], lexer_reporter) {
            let operator = self.previous.clone();
            let right = self.parse_factor(reporter, lexer_reporter);

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_factor(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let mut expr = self.parse_unary(reporter, lexer_reporter);

        while self.matches_any(&[TokenType::Multiply, TokenType::Divide], lexer_reporter) {
            let operator = self.previous.clone();
            let right = self.parse_unary(reporter, lexer_reporter);

            expr = Expr::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right) };
        }

        expr
    }

    fn parse_unary(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        if self.matches_any(&[TokenType::Minus, TokenType::Not], lexer_reporter) {
            let operator = self.previous.clone();
            let right = self.parse_unary(reporter, lexer_reporter);

            return Expr::UnaryOperator { operator, expr: Box::new(right) };
        }

        self.parse_cast(reporter, lexer_reporter)
    }

    fn parse_cast(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let mut expr = self.parse_call(reporter, lexer_reporter);

        loop {
            if self.matches(TokenType::As, lexer_reporter) {
                let operator_span = self.previous.span();
                let to = self.parse_type_part("`as`", reporter, lexer_reporter);

                expr = Expr::TypeCast { expr: Box::new(expr), operator_span, to, }
            } else {
                break;
            }
        }

        expr
    }

    fn parse_call(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let mut expr = self.parse_primary(reporter, lexer_reporter);

        loop {
            if self.matches(TokenType::ParenthesisLeft, lexer_reporter) {
                expr = self.finish_call(expr, reporter, lexer_reporter);
            } else if self.matches(TokenType::ColonColon, lexer_reporter) {
                self.expect_after(TokenType::Identifier, "member name", "`::`", None, reporter, lexer_reporter);
                let name = self.previous.clone();

                expr = Expr::Member { receiver: Box::new(expr), name, }
            } else if self.matches(TokenType::Dot, lexer_reporter) {
                self.expect_any_after(&[TokenType::Identifier, TokenType::Type], "member name", "`.`", None, reporter, lexer_reporter);
                let name = self.previous.clone();
                // let is_type = name.token_type() == TokenType::Type;

                expr = Expr::Receiver { receiver: Box::new(expr), name, };

                // if !is_type {
                //     self.expect_after(TokenType::ParenthesisLeft, "`(`", "template name", None, reporter, lexer_reporter);
                //     expr = self.finish_call(expr, reporter, lexer_reporter);
                // }
            } else if self.matches(TokenType::SquareBracketLeft, lexer_reporter) {
                let operator_span = self.previous.span();

                let index = self.parse_expression(reporter, lexer_reporter);
                self.expect_after(TokenType::SquareBracketRight, "`]`", "index expression", None, reporter, lexer_reporter);

                expr = Expr::Index { receiver: Box::new(expr), operator_span, index: Box::new(index), }
            } else {
                break;
            }
        }

        expr
    }

    fn finish_call(&mut self, callee: Expr<'source>, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let parenthesis_left = self.previous.span.start;

        let mut arguments = vec![];

        if !self.check(TokenType::ParenthesisRight) {
            arguments.push(self.parse_expression(reporter, lexer_reporter));

            while self.matches(TokenType::Comma, lexer_reporter) {
                if arguments.len() >= 255 {
                    self.error_at_current(ParserError::TooManyArguments(arguments.len() as u32), false, reporter);
                }

                arguments.push(self.parse_expression(reporter, lexer_reporter));
            }
        }

        self.expect_after(TokenType::ParenthesisRight, "`)`", "function call arguments", None, reporter, lexer_reporter);
        let parenthesis_right = self.previous.span.end;

        Expr::FunctionCall { callee: Box::new(callee), args_span: Span::new(parenthesis_left, parenthesis_right), args: arguments, }
    }

    fn parse_primary(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        if self.matches(TokenType::LiteralFloat, lexer_reporter) {
            let number_token = self.previous.clone();
            let parsed: Result<f64, _> = number_token.source().parse();

            return match parsed {
                Ok(number) => Expr::ConstantFloat(number, number_token.span()),
                Err(_) => {
                    self.error(ParserError::FailedParseLiteral(LiteralKind::Float), true, reporter);
                    Expr::Error
                },
            }
        } else if self.matches(TokenType::LiteralInt, lexer_reporter) {
            let number_token = self.previous.clone();
            let parsed: Result<i32, _> = number_token.source().parse();

            return match parsed {
                Ok(number) => Expr::ConstantInt(number, number_token.span()),
                Err(_) => {
                    self.error(ParserError::FailedParseLiteral(LiteralKind::Int), true, reporter);
                    Expr::Error
                },
            }
        } else if self.matches(TokenType::True, lexer_reporter) {
            return Expr::ConstantBoolean(true, self.previous.span())
        } else if self.matches(TokenType::False, lexer_reporter) {
            return Expr::ConstantBoolean(false, self.previous.span())
        } else if self.matches(TokenType::This, lexer_reporter) {
            return Expr::Identifier(self.previous.clone());
        } else if self.matches(TokenType::Identifier, lexer_reporter) || self.matches(TokenType::This, lexer_reporter) {
            return Expr::Identifier(self.previous.clone())
        } else if self.matches(TokenType::LiteralString, lexer_reporter) {
            return Expr::ConstantString(self.previous.source().to_owned(), self.previous.span())
        } else if self.matches(TokenType::ParenthesisLeft, lexer_reporter) {
            let expr = self.parse_expression(reporter, lexer_reporter);
            self.expect_after(TokenType::ParenthesisRight, "`)`", "expression", None, reporter, lexer_reporter);

            return expr;
        } else if self.matches(TokenType::BracketLeft, lexer_reporter) {
            return self.parse_object_expression(reporter, lexer_reporter);
        } else if self.matches(TokenType::SquareBracketLeft, lexer_reporter) {
            return self.parse_array_expression(reporter, lexer_reporter);
        } else if self.matches(TokenType::Underscore, lexer_reporter) {
            return Expr::Replacement(self.previous.span());
        } else if self.matches(TokenType::Builtin, lexer_reporter) {
            return self.parse_builtin_call(reporter, lexer_reporter);
        }

        self.consume(lexer_reporter);
        self.error(ParserError::ExpectedExpression, true, reporter);

        Expr::Error
    }

    fn parse_object_expression(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let start_pos = self.previous.span.start;

        let mut fields = vec![];

        loop {
            if self.check(TokenType::BracketRight) {
                break;
            }

            let merge_expr_start = self.current.span.start;
            let key = self.parse_expression(reporter, lexer_reporter);

            if let Expr::ConstantString(_, _) = &key {
                let name = self.previous.clone();

                self.expect_after(TokenType::Colon, "`:`", "object field key", None, reporter, lexer_reporter);
                let expr = self.parse_expression(reporter, lexer_reporter);
                fields.push((name, expr));
            } else {
                let merge_expr_span = Span::new(merge_expr_start, self.previous.span.end);

                self.expect_after(TokenType::BracketRight, "`}`", "object merge expression", None, reporter, lexer_reporter);
                let end_pos = self.previous.span.end;

                return Expr::Object { fields, span: Span::new(start_pos, end_pos), merge_expr: Some((Box::new(key), merge_expr_span)) };
            }

            if !self.matches(TokenType::Comma, lexer_reporter) {
                break;
            }
        }

        self.expect_after(TokenType::BracketRight, "`}`", "object fields", None, reporter, lexer_reporter);
        let end_pos = self.previous.span.end;

        Expr::Object { fields, span: Span::new(start_pos, end_pos), merge_expr: None, }
    }

    fn parse_array_expression(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        let start_pos = self.previous.span.start;

        let mut elements = vec![];

        if !self.check(TokenType::SquareBracketRight) {
            let expr = self.parse_expression(reporter, lexer_reporter);
            elements.push(expr);

            while self.matches(TokenType::Comma, lexer_reporter) {
                if self.check(TokenType::SquareBracketRight) {
                    break;
                }

                let expr = self.parse_expression(reporter, lexer_reporter);
                elements.push(expr);
            }
        }

        self.expect_after(TokenType::SquareBracketRight, "`]`", "array elements", None, reporter, lexer_reporter);
        let end_pos = self.previous.span.end;

        Expr::Array { elements, span: Span::new(start_pos, end_pos), }
    }

    fn parse_builtin_call(&mut self, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) -> Expr<'source> {
        // let builtin_span = self.previous.span();

        self.expect_after(TokenType::ColonColon, "`::`", "`builtin`", None, reporter, lexer_reporter);
        self.expect_after(TokenType::Identifier, "name", "`::`", None, reporter, lexer_reporter);
        let name = self.previous.clone();

        self.expect_after(TokenType::ParenthesisLeft, "`(`", "built-in function name", None, reporter, lexer_reporter);
        let args_start = self.previous.span.start;

        let mut arguments = vec![];

        if !self.check(TokenType::ParenthesisRight) {
            arguments.push(self.parse_expression(reporter, lexer_reporter));

            while self.matches(TokenType::Comma, lexer_reporter) {
                if arguments.len() >= 255 {
                    self.error_at_current(ParserError::TooManyArguments(arguments.len() as u32), false, reporter);
                }

                arguments.push(self.parse_expression(reporter, lexer_reporter));
            }
        }

        self.expect_after(TokenType::ParenthesisRight, "`)`", "built-in function call arguments", None, reporter, lexer_reporter);
        let args_end = self.previous.span.end;

        Expr::BuiltinFunctionCall { name, args: arguments, args_span: Span::new(args_start, args_end) }
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

    #[inline]
    fn expect_after(&mut self, token_type: TokenType, expected: &'static str, after: &'static str, due_to: Option<Span>, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        self.expect(token_type, ParserError::ExpectedAfter(expected, after, due_to.map(|span| SpanWithFile::new(self.file_id, span))), reporter, lexer_reporter);
    }

    #[inline]
    fn expect_any_after(&mut self, token_types: &[TokenType], expected: &'static str, after: &'static str, due_to: Option<Span>, reporter: &mut ParserErrorReporter, lexer_reporter: &mut LexerErrorReporter) {
        self.expect_any(token_types, ParserError::ExpectedAfter(expected, after, due_to.map(|span| SpanWithFile::new(self.file_id, span))), reporter, lexer_reporter);
    }

    #[inline]
    fn error_before(&mut self, at: Span, expected: &'static str, before: &'static str, due_to: Option<Span>, reporter: &mut ParserErrorReporter) {
        self.error_at_span(at, ParserError::ExpectedBefore(expected, before, due_to.map(|span| SpanWithFile::new(self.file_id, span))), true, reporter);
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
        self.error_at_span(self.current.span(), message, panic, reporter);
    }

    fn error(&mut self, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        self.error_at_span(self.previous.span(), message, panic, reporter);
    }

    fn error_at(&mut self, token: &Token<'source>, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        self.error_at_span(token.span(), message, panic, reporter);
    }

    fn error_at_span(&mut self, span: Span, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        Self::error_at_impl(span, message, panic, reporter);
    }

    fn error_at_impl(span: Span, message: ParserError, panic: bool, reporter: &mut ParserErrorReporter) {
        reporter.report(span, message, panic);
    }
}
