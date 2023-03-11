use std::fmt::Debug;
use std::path::PathBuf;
use std::rc::Rc;
use line_col::LineColLookup;
use crate::compiler::error::span::Span;
use crate::Config;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CompileStage {
    Lexer,
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
    pub message: Box<dyn Message<M>>,
    pub suppressed_messages: Vec<SubmittedMessageData<M>>,
}

impl<M> SubmittedMessageData<M> {
    pub fn new<M2: Message<M> + 'static>(span: Span, message: M2) -> Self {
        SubmittedMessageData {
            span, message: Box::new(message),
            suppressed_messages: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MessageContext<'m, M> {
    pub span: Span,
    pub marker: &'m M,
}

impl<'m, M> MessageContext<'m, M> {
    pub fn new(span: Span, marker: &'m M) -> Self {
        MessageContext {
            span, marker,
        }
    }
}

pub trait Message<M>: Debug {
    fn name(&self) -> &'static str;

    fn kind(&self) -> MessageKind;

    fn description(&mut self, context: &MessageContext<'_, M>) -> String;

    fn notes(&mut self, context: &MessageContext<'_, M>) -> Vec<(NoteKind, String)>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MessageData {
    pub span: Span,
    pub stage: CompileStage,
    pub name: &'static str,
    pub kind: MessageKind,
    pub description: String,
    pub notes: Vec<(NoteKind, String)>,
    pub suppressed_messages: u32,
}

impl MessageData {
    pub fn new(span: Span, stage: CompileStage, name: &'static str, kind: MessageKind, description: String, notes: Vec<(NoteKind, String)>, suppressed_messages: u32) -> Self {
        MessageData {
            span, stage, name, kind, description, notes, suppressed_messages,
        }
    }
}

pub struct ErrorReporting<'source> {
    config: Rc<Config>,
    source: &'source str, path: Rc<PathBuf>,
    lines: Option<Vec<&'source str>>, line_col_lookup: LineColLookup<'source>,
    messages: Vec<MessageData>,
}

impl<'source> ErrorReporting<'source> {
    pub fn new(config: Rc<Config>, source: &'source str, path: Rc<PathBuf>) -> Self {
        ErrorReporting {
            config, source, path,
            // LineColLookup only builds on first lookup, so just creating it here should be fine
            lines: None, line_col_lookup: LineColLookup::new(source),
            messages: Vec::new(),
        }
    }

    pub fn create_for_stage<M>(&self, stage: CompileStage, marker: M) -> ErrorReporter<M> {
        ErrorReporter::new(stage, marker)
    }

    pub fn submit<M: Default>(&mut self, mut reporter: ErrorReporter<M>) {
        let stage = reporter.stage;
        let marker = std::mem::take(&mut reporter.marker);
        let messages = std::mem::take(&mut reporter.messages);
        drop(reporter);

        self.messages.extend(messages.into_iter().map(|mut message| {
            let context = MessageContext::new(message.span, &marker);

            let name: &'static str = message.message.name();
            let kind = message.message.kind();
            let description = message.message.description(&context);
            let notes = message.message.notes(&context);

            MessageData::new(message.span, stage, name, kind, description, notes, message.suppressed_messages.len() as u32)
        }));
    }

    pub fn has_messages(&self) -> bool {
        !self.messages.is_empty()
    }

    pub fn print_simple(&mut self) {
        if self.messages.is_empty() {
            return;
        }

        let lines = self.lines.get_or_insert_with(|| self.source.lines().collect());

        for (i, message) in self.messages.iter().enumerate() {
            eprint!("{}", match message.kind {
                MessageKind::Error => "error",
                MessageKind::Warning => "warning",
            });

            eprint!("[{}]: ", message.name);
            eprintln!("{}", message.description);

            let (start_line, start_column) = self.line_col_lookup.get(message.span.start as usize);
            let (end_line, _end_column) = self.line_col_lookup.get(message.span.start as usize);

            let line_digits = (end_line + self.config.error_surrounding_lines as usize).ilog10() + 1;
            let empty_line_number = " ".repeat(line_digits as usize);

            eprintln!("{}--> {}:{}:{}", &empty_line_number, self.path.display(), start_line, start_column);

            let print_start_line = if self.config.error_surrounding_lines as usize > start_line {
                0
            } else {
                start_line - self.config.error_surrounding_lines as usize
            };
            let print_end_line = (end_line + self.config.error_surrounding_lines as usize).min(lines.len());

            for line in print_start_line..=print_end_line {
                if line >= start_line && line <= end_line {
                    eprintln!("{:>fill$} | {}", line, lines[line - 1], fill = line_digits as usize);
                } else {
                    eprintln!("{} | {}", &empty_line_number, lines[line - 1]);
                }
            }

            if message.suppressed_messages > 0 {
                eprintln!("{} |", &empty_line_number);
                eprintln!("{} | ... and {} more", &empty_line_number, message.suppressed_messages);
            }

            if i < self.messages.len() - 1 {
                eprintln!();
            }
        }
    }

    #[allow(unused)]
    fn lines(&mut self) -> &[&'source str] {
        self.lines.get_or_insert_with(|| self.source.lines().collect())
    }
}

pub struct ErrorReporter<M> {
    stage: CompileStage,
    pub marker: M,
    messages: Vec<SubmittedMessageData<M>>,
    panic_mode: bool,
}

impl<M> ErrorReporter<M> {
    fn new(stage: CompileStage, marker: M) -> Self {
        ErrorReporter {
            stage,
            marker,
            messages: Vec::new(),
            panic_mode: false,
        }
    }

    pub fn marker(&self) -> &M {
        &self.marker
    }

    pub fn marker_mut(&mut self) -> &mut M {
        &mut self.marker
    }

    pub fn report<M2: Message<M> + 'static>(&mut self, span: Span, message: M2, panic: bool) { // panic is unrelated to a Rust "panic!"
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
