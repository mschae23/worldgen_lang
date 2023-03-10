use std::fmt::Debug;
use std::path::PathBuf;
use std::rc::Rc;
use line_col::LineColLookup;
use crate::compiler::lexer::TokenPos;
use crate::Config;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CompileStage {
    Lexer,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn from(start: TokenPos, end: TokenPos) -> Self {
        Span {
            start: start.index, end: end.index,
        }
    }

    pub fn start(&self) -> u32 {
        self.start
    }

    pub fn end(&self) -> u32 {
        self.end
    }
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
}

impl<M> SubmittedMessageData<M> {
    pub fn new<M2: Message<M> + 'static>(span: Span, message: M2) -> Self {
        SubmittedMessageData {
            span, message: Box::new(message),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MessageContext<'m, M> {
    pub span: Span,
    pub markers: &'m [M],
}

impl<'m, M> MessageContext<'m, M> {
    pub fn new(span: Span, markers: &'m [M]) -> Self {
        MessageContext {
            span, markers,
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
}

impl MessageData {
    pub fn new(span: Span, stage: CompileStage, name: &'static str, kind: MessageKind, description: String, notes: Vec<(NoteKind, String)>) -> Self {
        MessageData {
            span, stage, name, kind, description, notes,
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

    pub fn create_for_stage<M>(&self, stage: CompileStage) -> ErrorReporter<M> {
        ErrorReporter::new(stage)
    }

    pub fn submit<M>(&mut self, mut reporter: ErrorReporter<M>) {
        let stage = reporter.stage;
        let markers = std::mem::take(&mut reporter.markers);
        let messages = std::mem::take(&mut reporter.messages);
        drop(reporter);

        self.messages.extend(messages.into_iter().map(|mut message| {
            let context = MessageContext::new(message.span, &markers);

            let name: &'static str = message.message.name();
            let kind = message.message.kind();
            let description = message.message.description(&context);
            let notes = message.message.notes(&context);

            MessageData::new(message.span, stage, name, kind, description, notes)
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
            print!("{}", match message.kind {
                MessageKind::Error => "error",
                MessageKind::Warning => "warning",
            });

            print!("[{}]: ", message.name);
            println!("{}", message.description);

            let (start_line, start_column) = self.line_col_lookup.get(message.span.start as usize);
            let (end_line, _end_column) = self.line_col_lookup.get(message.span.start as usize);

            let line_digits = (end_line + self.config.error_surrounding_lines as usize).ilog10() + 1;

            println!("{}--> {}:{}:{}", " ".repeat(line_digits as usize), self.path.display(), start_line, start_column);

            let print_start_line = if self.config.error_surrounding_lines as usize > start_line {
                0
            } else {
                start_line - self.config.error_surrounding_lines as usize
            };
            let print_end_line = (end_line + self.config.error_surrounding_lines as usize).min(lines.len());

            for line in print_start_line..=print_end_line {
                if line >= start_line && line <= end_line {
                    println!("{:>fill$} | {}", line, lines[line - 1], fill = line_digits as usize);
                } else {
                    println!("{} | {}", " ".repeat(line_digits as usize), lines[line - 1]);
                }
            }

            if i < self.messages.len() - 1 {
                println!();
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
    pub markers: Vec<M>,
    messages: Vec<SubmittedMessageData<M>>,
}

impl<M> ErrorReporter<M> {
    fn new(stage: CompileStage) -> Self {
        ErrorReporter {
            stage,
            markers: Vec::new(),
            messages: Vec::new(),
        }
    }

    pub fn mark(&mut self, marker: M) {
        self.markers.push(marker);
    }

    pub fn report<M2: Message<M> + 'static>(&mut self, span: Span, message: M2) {
        self.messages.push(SubmittedMessageData::new(span, message));
    }
}
