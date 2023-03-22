use std::fmt::Debug;
use std::path::PathBuf;
use std::rc::Rc;
use diagnostic_render::diagnostic::AnnotationStyle;
use diagnostic_render::file::SimpleFile;
use diagnostic_render::render::DiagnosticRenderer;
#[allow(deprecated)]
use crate::compiler::error::render::{self, ColorConfig};
use crate::compiler::error::span::Span;
use crate::Config;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CompileStage {
    Lexer,
    Parser,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error, Warning,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NoteKind {
    Note, Help,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    span: Span,
    label: Option<String>,
}

impl Annotation {
    pub fn new(span: Span, label: Option<String>) -> Self {
        Annotation {
            span, label,
        }
    }
}

#[derive(Debug)]
pub struct SubmittedDiagnosticData<M> {
    pub span: Span,
    pub message: M,
    pub suppressed_messages: Vec<SubmittedDiagnosticData<M>>,
}

impl<M> SubmittedDiagnosticData<M> {
    pub fn new(span: Span, message: M) -> Self {
        SubmittedDiagnosticData {
            span, message,
            suppressed_messages: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticContext<'d, D> {
    pub span: Span,
    pub custom_data: &'d D,
}

impl<'d, D> DiagnosticContext<'d, D> {
    pub fn new(span: Span, marker: &'d D) -> Self {
        DiagnosticContext {
            span,
            custom_data: marker,
        }
    }
}

pub trait Diagnostic<D>: Debug {
    fn name(&self) -> &'static str;

    fn severity(&self) -> Severity;

    fn message(&self, context: &DiagnosticContext<'_, D>) -> String;

    fn primary_annotation(&self, context: &DiagnosticContext<'_, D>) -> Option<String>;

    fn additional_annotations(&self, context: &DiagnosticContext<'_, D>) -> Vec<(Span, Option<String>)>;

    fn primary_note(&self, context: &DiagnosticContext<'_, D>) -> Option<(NoteKind, String)>;

    fn additional_notes(&self, context: &DiagnosticContext<'_, D>) -> Vec<(NoteKind, String)>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DiagnosticData {
    pub span: Span,
    pub stage: CompileStage,
    pub name: &'static str,
    pub severity: Severity,
    pub message: String,
    pub primary_annotation: Option<String>,
    pub additional_annotations: Vec<(Span, Option<String>)>,
    pub primary_note: Option<(NoteKind, String)>,
    pub additional_notes: Vec<(NoteKind, String)>,
    pub suppressed_messages: u32,
}

impl DiagnosticData {
    #[allow(clippy::too_many_arguments)]
    pub fn new(span: Span, stage: CompileStage, name: &'static str, severity: Severity,
               message: String,
               primary_annotation: Option<String>, additional_annotations: Vec<(Span, Option<String>)>,
               primary_note: Option<(NoteKind, String)>, additional_notes: Vec<(NoteKind, String)>,
               suppressed_messages: u32) -> Self {
        DiagnosticData {
            span, stage, name,
            severity, message,
            primary_annotation,
            additional_annotations,
            primary_note, additional_notes,
            suppressed_messages,
        }
    }
}

pub struct ErrorReporting<'source> {
    config: Rc<Config>,
    source: &'source str, path: Rc<PathBuf>,
    diagnostics: Vec<DiagnosticData>,
}

impl<'source> ErrorReporting<'source> {
    pub fn new(config: Rc<Config>, source: &'source str, path: Rc<PathBuf>) -> Self {
        ErrorReporting {
            config, source, path,
            diagnostics: Vec::new(),
        }
    }

    pub fn create_for_stage<D, M>(&self, stage: CompileStage, marker: D) -> ErrorReporter<D, M> {
        ErrorReporter::new(stage, marker)
    }

    pub fn submit<D: Default, M: Diagnostic<D>>(&mut self, mut reporter: ErrorReporter<D, M>) {
        let stage = reporter.stage;
        let marker = std::mem::take(&mut reporter.custom_data);
        let diagnostics = std::mem::take(&mut reporter.diagnostics);
        drop(reporter);

        self.diagnostics.extend(diagnostics.into_iter().map(|diagnostic| {
            let context = DiagnosticContext::new(diagnostic.span, &marker);

            let name: &'static str = diagnostic.message.name();
            let severity = diagnostic.message.severity();
            let message = diagnostic.message.message(&context);
            let primary_annotation = diagnostic.message.primary_annotation(&context);
            let additional_annotations = diagnostic.message.additional_annotations(&context);
            let primary_note = diagnostic.message.primary_note(&context);
            let additional_notes = diagnostic.message.additional_notes(&context);

            DiagnosticData::new(diagnostic.span, stage, name, severity, message,
                primary_annotation, additional_annotations, primary_note, additional_notes, diagnostic.suppressed_messages.len() as u32)
        }));
    }

    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    // Prints with TerminalErrorRenderer
    #[allow(deprecated)]
    pub fn print_simple(&mut self) {
        let mut renderer = render::TerminalErrorRenderer::new(Rc::clone(&self.config), Rc::clone(&self.path), ColorConfig::Default, self.source, &self.diagnostics);
        renderer.render_to_stderr();
    }

    // Prints with diagnostic_render
    pub fn print_stderr(&mut self) {
        let file = SimpleFile::new(self.path.display().to_string(), self.source);

        let mut stream = termcolor::BufferedStandardStream::stderr(termcolor::ColorChoice::Auto);
        let mut renderer = DiagnosticRenderer::new(&mut stream,
            diagnostic_render::render::color::DefaultColorConfig, file, diagnostic_render::render::RenderConfig {
                surrounding_lines: self.config.error_surrounding_lines as usize,
            });

        let diagnostics = self.diagnostics.iter()
            .map(|diagnostic| {
                let mut d = diagnostic_render::diagnostic::Diagnostic::new(match diagnostic.severity {
                    Severity::Error => diagnostic_render::diagnostic::Severity::Error,
                    Severity::Warning => diagnostic_render::diagnostic::Severity::Warning,
                }).with_name(diagnostic.name).with_message(&diagnostic.message);

                let mut primary_annotation = diagnostic_render::diagnostic::Annotation::new(AnnotationStyle::Primary, (),
                    diagnostic.span.start as usize..diagnostic.span.end as usize);

                if let Some(label) = diagnostic.primary_annotation.as_ref() {
                    primary_annotation = primary_annotation.with_label(label);
                }

                d = d.with_annotation(primary_annotation);

                for (span, label) in diagnostic.additional_annotations.iter() {
                    let mut annotation = diagnostic_render::diagnostic::Annotation::new(AnnotationStyle::Secondary, (),
                        span.start as usize..span.end as usize);

                    if let Some(label) = label.as_ref() {
                        annotation = annotation.with_label(label);
                    }

                    d = d.with_annotation(annotation);
                }

                d
            }).collect::<Vec<_>>();

        renderer.render(diagnostics).expect("There was an error while formatting errors.");
    }
}

pub struct ErrorReporter<D, M> {
    stage: CompileStage,
    pub custom_data: D,
    diagnostics: Vec<SubmittedDiagnosticData<M>>,
    panic_mode: bool,
}

impl<D, M> ErrorReporter<D, M> {
    fn new(stage: CompileStage, marker: D) -> Self {
        ErrorReporter {
            stage,
            custom_data: marker,
            diagnostics: Vec::new(),
            panic_mode: false,
        }
    }

    pub fn custom_data(&self) -> &D {
        &self.custom_data
    }

    pub fn custom_data_mut(&mut self) -> &mut D {
        &mut self.custom_data
    }

    pub fn report(&mut self, span: Span, message: M, panic: bool) { // panic is unrelated to a Rust "panic!"
        if self.panic_mode {
            self.diagnostics.last_mut().expect("Error reporter is in panic mode, but there are no errors")
                .suppressed_messages.push(SubmittedDiagnosticData::new(span, message));
        } else {
            self.diagnostics.push(SubmittedDiagnosticData::new(span, message));

            if panic {
                self.panic_mode = true;
            }
        }
    }

    pub fn panic_mode(&self) -> bool {
        self.panic_mode
    }

    pub fn exit_panic_mode(&mut self) {
        self.panic_mode = false;
    }
}
