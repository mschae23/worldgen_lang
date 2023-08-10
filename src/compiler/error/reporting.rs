use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::rc::Rc;
use diagnostic_render::diagnostic::AnnotationStyle;
use diagnostic_render::file::SimpleFiles;
use diagnostic_render::render::DiagnosticRenderer;
use crate::compiler::error::span::{Span, SpanWithFile};
use crate::Config;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CompileStage {
    Lexer,
    Parser,
    ForwardDeclarer,
    TypeChecker,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Severity {
    Warning, Error,
}

impl Severity {
    pub fn max(self, other: Self) -> Self {
        if other > self {
            other
        } else {
            self
        }
    }
}

const SEVERITY_COUNT: usize = 2;

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

pub type FileId = u32;

#[derive(Debug)]
pub struct SubmittedDiagnosticData<M> {
    pub file_id: FileId, pub span: Span,
    pub message: M,
    pub suppressed_messages: Vec<SubmittedDiagnosticData<M>>,
}

impl<M> SubmittedDiagnosticData<M> {
    pub fn new(file_id: FileId, span: Span, message: M) -> Self {
        SubmittedDiagnosticData {
            file_id, span, message,
            suppressed_messages: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticContext<'d, D> {
    pub file_id: FileId,
    pub span: Span,
    pub custom_data: &'d D,
}

impl<'d, D> DiagnosticContext<'d, D> {
    pub fn new(file_id: FileId, span: Span, custom_data: &'d D) -> Self {
        DiagnosticContext {
            file_id, span,
            custom_data,
        }
    }
}

pub trait Diagnostic<D>: Debug {
    fn name(&self) -> &'static str;

    fn severity(&self) -> Severity;

    fn message(&self, context: &DiagnosticContext<'_, D>) -> String;

    fn primary_annotation(&self, context: &DiagnosticContext<'_, D>) -> Option<String>;

    fn additional_annotations(&self, context: &DiagnosticContext<'_, D>) -> Vec<(SpanWithFile, Option<String>)>;

    fn primary_note(&self, context: &DiagnosticContext<'_, D>) -> Option<(NoteKind, String)>;

    fn additional_notes(&self, context: &DiagnosticContext<'_, D>) -> Vec<(NoteKind, String)>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DiagnosticData {
    pub file_id: FileId,
    pub span: Span,
    pub stage: CompileStage,
    pub name: &'static str,
    pub severity: Severity,
    pub message: String,
    pub primary_annotation: Option<String>,
    pub additional_annotations: Vec<(SpanWithFile, Option<String>)>,
    pub primary_note: Option<(NoteKind, String)>,
    pub additional_notes: Vec<(NoteKind, String)>,
    pub suppressed_messages: u32,
}

impl DiagnosticData {
    #[allow(clippy::too_many_arguments)]
    pub fn new(file_id: FileId, span: Span, stage: CompileStage,
               name: &'static str, severity: Severity, message: String,
               primary_annotation: Option<String>, additional_annotations: Vec<(SpanWithFile, Option<String>)>,
               primary_note: Option<(NoteKind, String)>, additional_notes: Vec<(NoteKind, String)>,
               suppressed_messages: u32) -> Self {
        DiagnosticData {
            file_id, span, stage,
            name, severity, message,
            primary_annotation,
            additional_annotations,
            primary_note, additional_notes,
            suppressed_messages,
        }
    }
}

pub struct ErrorReporting {
    config: Rc<Config>, working_dir: Rc<PathBuf>,
    file_ids: HashMap<Rc<PathBuf>, FileId>,
    files: Vec<(String, Rc<PathBuf>)>,
    diagnostics: Vec<DiagnosticData>,
    diagnostic_counts: [usize; SEVERITY_COUNT],
    highest_severity: Option<Severity>,
}

impl ErrorReporting {
    pub fn new(config: Rc<Config>, working_dir: PathBuf) -> Self {
        ErrorReporting {
            config, working_dir: Rc::new(working_dir),
            file_ids: HashMap::new(), files: Vec::new(),
            diagnostics: Vec::new(),
            diagnostic_counts: [0; SEVERITY_COUNT], highest_severity: None,
        }
    }

    // path should already be canonicalized
    pub fn get_file_id(&mut self, path: Rc<PathBuf>, source: String) -> FileId {
        let next_id: FileId = self.files.len() as u32;

        *self.file_ids.entry(Rc::clone(&path)).or_insert_with(|| {
            self.files.push((source, path));

            next_id
        })
    }

    pub fn create_for_stage<D, M>(&self, stage: CompileStage, file_id: FileId, marker: D) -> ErrorReporter<D, M> {
        ErrorReporter::new(stage, file_id, marker, Rc::clone(&self.working_dir))
    }

    pub fn submit<D: Default, M: Diagnostic<D>>(&mut self, mut reporter: ErrorReporter<D, M>) {
        let stage = reporter.stage;
        let marker = std::mem::take(&mut reporter.custom_data);
        let diagnostics = std::mem::take(&mut reporter.diagnostics);
        drop(reporter);

        self.diagnostics.extend(diagnostics.into_iter().map(|diagnostic| {
            let context = DiagnosticContext::new(diagnostic.file_id, diagnostic.span, &marker);

            let name: &'static str = diagnostic.message.name();
            let severity = diagnostic.message.severity();
            let message = diagnostic.message.message(&context);
            let primary_annotation = diagnostic.message.primary_annotation(&context);
            let additional_annotations = diagnostic.message.additional_annotations(&context);
            let primary_note = diagnostic.message.primary_note(&context);
            let additional_notes = diagnostic.message.additional_notes(&context);

            self.diagnostic_counts[severity as u8 as usize] += 1;
            self.highest_severity = self.highest_severity.map(|s| severity.max(s)).or(Some(severity));

            DiagnosticData::new(diagnostic.file_id, diagnostic.span, stage, name, severity, message,
                primary_annotation, additional_annotations, primary_note, additional_notes, diagnostic.suppressed_messages.len() as u32)
        }));
    }

    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub fn get_diagnostic_count_for_severity(&self, severity: Severity) -> usize {
        self.diagnostic_counts[severity as u8 as usize]
    }

    pub fn get_highest_severity(&self) -> Option<Severity> {
        self.highest_severity
    }

    // Prints with diagnostic_render
    pub fn print_diagnostics_to_stderr(&mut self) {
        let mut files: SimpleFiles<_, &str> = SimpleFiles::new();

        for (source, path) in self.files.iter() {
            files.add(path.strip_prefix(&**self.working_dir).map(|path| path.to_string_lossy())
                .unwrap_or_else(|_| path.to_string_lossy()), source);
        }

        let mut stream = termcolor::BufferedStandardStream::stderr(termcolor::ColorChoice::Auto);
        let mut renderer = DiagnosticRenderer::new(&mut stream,
            diagnostic_render::render::color::DefaultColorConfig, files, diagnostic_render::render::RenderConfig {
                surrounding_lines: self.config.error_surrounding_lines as usize,
            });

        let diagnostics = self.diagnostics.iter()
            .map(|diagnostic| {
                let mut d = diagnostic_render::diagnostic::Diagnostic::new(match diagnostic.severity {
                    Severity::Error => diagnostic_render::diagnostic::Severity::Error,
                    Severity::Warning => diagnostic_render::diagnostic::Severity::Warning,
                }).with_name(diagnostic.name).with_message(&diagnostic.message).with_suppressed_count(diagnostic.suppressed_messages);

                let mut primary_annotation = diagnostic_render::diagnostic::Annotation::new(AnnotationStyle::Primary, diagnostic.file_id as usize,
                    diagnostic.span.start as usize..diagnostic.span.end as usize);

                if let Some(label) = diagnostic.primary_annotation.as_ref() {
                    primary_annotation = primary_annotation.with_label(label);
                }

                d = d.with_annotation(primary_annotation);

                for (span, label) in diagnostic.additional_annotations.iter() {
                    let mut annotation = diagnostic_render::diagnostic::Annotation::new(AnnotationStyle::Secondary, span.file_id as usize,
                        span.span.start as usize..span.span.end as usize);

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
    current_file_id: FileId, working_dir: Rc<PathBuf>,
    panic_mode: bool,
}

impl<D, M> ErrorReporter<D, M> {
    fn new(stage: CompileStage, file_id: FileId, marker: D, working_dir: Rc<PathBuf>) -> Self {
        ErrorReporter {
            stage,
            custom_data: marker,
            diagnostics: Vec::new(),
            current_file_id: file_id, working_dir,
            panic_mode: false,
        }
    }

    pub fn custom_data(&self) -> &D {
        &self.custom_data
    }

    pub fn custom_data_mut(&mut self) -> &mut D {
        &mut self.custom_data
    }

    pub fn report_with_file(&mut self, span: SpanWithFile, message: M, panic: bool) { // panic is unrelated to a Rust "panic!"
        if self.panic_mode {
            self.diagnostics.last_mut().expect("Error reporter is in panic mode, but there are no errors")
                .suppressed_messages.push(SubmittedDiagnosticData::new(span.file_id, span.span, message));
        } else {
            self.diagnostics.push(SubmittedDiagnosticData::new(span.file_id, span.span, message));

            if panic {
                self.panic_mode = true;
            }
        }
    }

    pub fn report(&mut self, span: Span, message: M, panic: bool) { // panic is unrelated to a Rust "panic!"
        self.report_with_file(SpanWithFile::new(self.current_file_id, span), message, panic)
    }

    pub fn set_current_file_id(&mut self, file_id: FileId) {
        self.current_file_id = file_id;
    }

    pub fn working_dir(&self) -> &Rc<PathBuf> { &self.working_dir }

    pub fn panic_mode(&self) -> bool {
        self.panic_mode
    }

    pub fn exit_panic_mode(&mut self) {
        self.panic_mode = false;
    }
}
