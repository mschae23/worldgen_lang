use std::fmt::Debug;
use std::path::PathBuf;
use std::rc::Rc;
use crate::compiler::error::render;
use crate::compiler::error::render::ColorConfig;
use crate::compiler::error::span::Span;
use crate::Config;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CompileStage {
    Lexer,
    Parser,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MessageKind {
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
pub struct SubmittedMessageData<M> {
    pub span: Span,
    pub message: M,
    pub suppressed_messages: Vec<SubmittedMessageData<M>>,
}

impl<M> SubmittedMessageData<M> {
    pub fn new(span: Span, message: M) -> Self {
        SubmittedMessageData {
            span, message,
            suppressed_messages: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MessageContext<'d, D> {
    pub span: Span,
    pub custom_data: &'d D,
}

impl<'d, D> MessageContext<'d, D> {
    pub fn new(span: Span, marker: &'d D) -> Self {
        MessageContext {
            span,
            custom_data: marker,
        }
    }
}

pub trait Message<D>: Debug {
    fn name(&self) -> &'static str;

    fn kind(&self) -> MessageKind;

    fn description(&self, context: &MessageContext<'_, D>) -> String;

    fn primary_annotation(&self, context: &MessageContext<'_, D>) -> Option<String>;

    fn additional_annotations(&self, context: &MessageContext<'_, D>) -> Vec<(Span, Option<String>)>;

    fn primary_note(&self, context: &MessageContext<'_, D>) -> Option<(NoteKind, String)>;

    fn additional_notes(&self, context: &MessageContext<'_, D>) -> Vec<(NoteKind, String)>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MessageData {
    pub span: Span,
    pub stage: CompileStage,
    pub name: &'static str,
    pub kind: MessageKind,
    pub description: String,
    pub primary_annotation: Option<String>,
    pub additional_annotations: Vec<(Span, Option<String>)>,
    pub primary_note: Option<(NoteKind, String)>,
    pub additional_notes: Vec<(NoteKind, String)>,
    pub suppressed_messages: u32,
}

impl MessageData {
    pub fn new(span: Span, stage: CompileStage, name: &'static str, kind: MessageKind,
               description: String,
               primary_annotation: Option<String>, additional_annotations: Vec<(Span, Option<String>)>,
               primary_note: Option<(NoteKind, String)>, additional_notes: Vec<(NoteKind, String)>,
               suppressed_messages: u32) -> Self {
        MessageData {
            span, stage, name, kind, description,
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
    messages: Vec<MessageData>,
}

impl<'source> ErrorReporting<'source> {
    pub fn new(config: Rc<Config>, source: &'source str, path: Rc<PathBuf>) -> Self {
        ErrorReporting {
            config, source, path,
            messages: Vec::new(),
        }
    }

    pub fn create_for_stage<D, M>(&self, stage: CompileStage, marker: D) -> ErrorReporter<D, M> {
        ErrorReporter::new(stage, marker)
    }

    pub fn submit<D: Default, M: Message<D>>(&mut self, mut reporter: ErrorReporter<D, M>) {
        let stage = reporter.stage;
        let marker = std::mem::take(&mut reporter.custom_data);
        let messages = std::mem::take(&mut reporter.messages);
        drop(reporter);

        self.messages.extend(messages.into_iter().map(|message| {
            let context = MessageContext::new(message.span, &marker);

            let name: &'static str = message.message.name();
            let kind = message.message.kind();
            let description = message.message.description(&context);
            let primary_annotation = message.message.primary_annotation(&context);
            let additional_annotations = message.message.additional_annotations(&context);
            let primary_note = message.message.primary_note(&context);
            let additional_notes = message.message.additional_notes(&context);

            MessageData::new(message.span, stage, name, kind, description,
                primary_annotation, additional_annotations, primary_note, additional_notes, message.suppressed_messages.len() as u32)
        }));
    }

    pub fn has_messages(&self) -> bool {
        !self.messages.is_empty()
    }

    pub fn print_simple(&mut self) {
        let mut renderer = render::TerminalErrorRenderer::new(Rc::clone(&self.config), Rc::clone(&self.path), ColorConfig::Default, &self.source, &self.messages);
        renderer.render_to_stderr();
    }
}

pub struct ErrorReporter<D, M> {
    stage: CompileStage,
    pub custom_data: D,
    messages: Vec<SubmittedMessageData<M>>,
    panic_mode: bool,
}

impl<D, M> ErrorReporter<D, M> {
    fn new(stage: CompileStage, marker: D) -> Self {
        ErrorReporter {
            stage,
            custom_data: marker,
            messages: Vec::new(),
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
            self.messages.last_mut().expect("Error reporter is in panic mode, but there are no errors")
                .suppressed_messages.push(SubmittedMessageData::new(span, message));
        } else {
            self.messages.push(SubmittedMessageData::new(span, message));

            if panic {
                self.panic_mode = true;
            }
        }
    }

    pub fn exit_panic_mode(&mut self) {
        self.panic_mode = false;
    }
}
