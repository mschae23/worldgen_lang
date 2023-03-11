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
pub struct MessageContext<'md, MD> {
    pub span: Span,
    pub marker: &'md MD,
}

impl<'md, MD> MessageContext<'md, MD> {
    pub fn new(span: Span, marker: &'md MD) -> Self {
        MessageContext {
            span, marker,
        }
    }
}

pub trait Message<MD>: Debug {
    fn name(&self) -> &'static str;

    fn kind(&self) -> MessageKind;

    fn description(&mut self, context: &MessageContext<'_, MD>) -> String;

    fn main_inline_note(&mut self, context: &MessageContext<'_, MD>) -> Option<String>;

    fn additional_inline_notes(&mut self, context: &MessageContext<'_, MD>) -> Vec<(Span, Option<String>)>;

    fn notes(&mut self, context: &MessageContext<'_, MD>) -> Vec<(NoteKind, String)>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MessageData {
    pub span: Span,
    pub stage: CompileStage,
    pub name: &'static str,
    pub kind: MessageKind,
    pub description: String,
    pub main_inline_note: Option<String>,
    pub additional_inline_notes: Vec<(Span, Option<String>)>,
    pub notes: Vec<(NoteKind, String)>,
    pub suppressed_messages: u32,
}

impl MessageData {
    pub fn new(span: Span, stage: CompileStage, name: &'static str, kind: MessageKind,
               description: String,
               main_inline_note: Option<String>, additional_inline_notes: Vec<(Span, Option<String>)>,
               notes: Vec<(NoteKind, String)>, suppressed_messages: u32) -> Self {
        MessageData {
            span, stage, name, kind, description, main_inline_note, additional_inline_notes, notes, suppressed_messages,
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

    pub fn create_for_stage<MD, M>(&self, stage: CompileStage, marker: MD) -> ErrorReporter<MD, M> {
        ErrorReporter::new(stage, marker)
    }

    pub fn submit<MD: Default, M: Message<MD>>(&mut self, mut reporter: ErrorReporter<MD, M>) {
        let stage = reporter.stage;
        let marker = std::mem::take(&mut reporter.marker);
        let messages = std::mem::take(&mut reporter.messages);
        drop(reporter);

        self.messages.extend(messages.into_iter().map(|mut message| {
            let context = MessageContext::new(message.span, &marker);

            let name: &'static str = message.message.name();
            let kind = message.message.kind();
            let description = message.message.description(&context);
            let main_inline_note = message.message.main_inline_note(&context);
            let additional_inline_notes = message.message.additional_inline_notes(&context);
            let notes = message.message.notes(&context);

            MessageData::new(message.span, stage, name, kind, description,
                main_inline_note, additional_inline_notes, notes, message.suppressed_messages.len() as u32)
        }));
    }

    pub fn has_messages(&self) -> bool {
        !self.messages.is_empty()
    }

    pub fn print_simple(&mut self) {
        let mut renderer = render::TerminalErrorRenderer::new(Rc::clone(&self.config), Rc::clone(&self.path), ColorConfig::ColoredDefault, &self.source, &self.messages);
        renderer.render();
    }
}

pub struct ErrorReporter<MD, M> {
    stage: CompileStage,
    pub marker: MD,
    messages: Vec<SubmittedMessageData<M>>,
    panic_mode: bool,
}

impl<MD, M> ErrorReporter<MD, M> {
    fn new(stage: CompileStage, marker: MD) -> Self {
        ErrorReporter {
            stage,
            marker,
            messages: Vec::new(),
            panic_mode: false,
        }
    }

    pub fn marker(&self) -> &MD {
        &self.marker
    }

    pub fn marker_mut(&mut self) -> &mut MD {
        &mut self.marker
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
