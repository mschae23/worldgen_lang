use std::fmt::Write;
use std::path::PathBuf;
use std::rc::Rc;
use colored::Colorize;
use line_col::LineColLookup;
use crate::compiler::error::{MessageData, MessageKind};
use crate::compiler::error::span::Span;
use crate::Config;

#[derive(Clone, Debug, PartialEq)]
pub enum ColorConfig {
    Uncolored,
    ColoredDefault,
}

impl ColorConfig {
    pub fn error(&self, string: &str) -> colored::ColoredString {
        match self {
            ColorConfig::Uncolored => colored::ColoredString::from(string),
            ColorConfig::ColoredDefault => string.red(),
        }
    }

    pub fn warning(&self, string: &str) -> colored::ColoredString {
        match self {
            ColorConfig::Uncolored => colored::ColoredString::from(string),
            ColorConfig::ColoredDefault => string.yellow(),
        }
    }

    pub fn message(&self, string: &str, kind: MessageKind) -> colored::ColoredString {
        match kind {
            MessageKind::Error => self.error(string),
            MessageKind::Warning => self.warning(string),
        }
    }

    pub fn description(&self, string: &str) -> colored::ColoredString {
        match self {
            ColorConfig::Uncolored => colored::ColoredString::from(string),
            ColorConfig::ColoredDefault => string.bold(),
        }
    }

    pub fn line_number(&self, string: &str) -> colored::ColoredString {
        match self {
            ColorConfig::Uncolored => colored::ColoredString::from(string),
            ColorConfig::ColoredDefault => string.bright_blue().bold(),
        }
    }

    pub fn line_number_separator(&self, string: &str) -> colored::ColoredString {
        match self {
            ColorConfig::Uncolored => colored::ColoredString::from(string),
            ColorConfig::ColoredDefault => string.bright_blue(),
        }
    }

    pub fn additional_annotation(&self, string: &str) -> colored::ColoredString {
        match self {
            ColorConfig::Uncolored => colored::ColoredString::from(string),
            ColorConfig::ColoredDefault => string.bright_blue(),
        }
    }

    pub fn annotation(&self, string: &str, annotation: &AnnotationData) -> colored::ColoredString {
        if annotation.primary {
            self.message(string, annotation.message_kind)
        } else {
            self.additional_annotation(string)
        }
    }

    pub fn source(&self, string: &str) -> colored::ColoredString {
        colored::ColoredString::from(string)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

impl LineColumn {
    pub fn new(line: u32, column: u32) -> Self {
        LineColumn {
            line, column,
        }
    }

    pub fn from_usize_lossy(line_col: (usize, usize)) -> Self {
        Self::new(line_col.0 as u32, line_col.1 as u32)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AnnotationDisplayData {
    Singleline {
        rightmost_index: u32, // Index of this annotation of all singleline ones on this line, from the right
        vertical_offset: u32, // How many annotations for this line have to be displayed before this one
    },
    Multiline {
        continuing_bar_index: u32, // What position the `|` to the left of the source code is at
        vertical_offset: u32, // How many annotations for the end line have to be displayed before this one
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnnotationData {
    span: Span,
    message_kind: MessageKind, primary: bool, // error / warning (for colors), primary (for color and underline char)
    label: Option<String>,
    start: LineColumn, end: LineColumn,
    display_data: AnnotationDisplayData,
}

pub struct TerminalErrorRenderer<'source, 'm> {
    config: Rc<Config>, path: Rc<PathBuf>, colors: ColorConfig,
    lines: Vec<&'source str>,
    line_col_lookup: LineColLookup<'source>,
    messages: &'m [MessageData],

    current_message: Option<&'m MessageData>,
    annotations: Vec<AnnotationData>,
    max_nested_blocks: usize, line_digits: u32,
}

impl<'source, 'm> TerminalErrorRenderer<'source, 'm> {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, color_config: ColorConfig, source: &'source str, messages: &'m [MessageData]) -> Self {
        TerminalErrorRenderer {
            config, path,
            colors: color_config, lines: source.lines().collect(), line_col_lookup: LineColLookup::new(source), messages,
            current_message: None, annotations: Vec::new(),
            max_nested_blocks: 0, line_digits: 0,
        }
    }

    pub fn render_to_stderr(&mut self) {
        if self.messages.is_empty() {
            return;
        }

        eprint!("{}", self.render_to_string());
    }

    pub fn render_to_string(&mut self) -> String {
        if self.messages.is_empty() {
            return String::new();
        }

        let mut buf = String::new();
        self.render_impl(&mut buf).expect("Failed to render error messages");
        buf
    }

    fn render_impl(&mut self, f: &mut impl Write) -> std::fmt::Result {
        for (i, message) in self.messages.iter().enumerate() {
            self.current_message = Some(message);
            self.print_message(f, message)?;

            if i < self.messages.len() - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }

    fn print_message(&mut self, f: &mut impl Write, message: &MessageData) -> std::fmt::Result {
        write!(f, "{}", self.colors.message(match message.kind {
            MessageKind::Error => "error",
            MessageKind::Warning => "warning",
        }, message.kind))?;

        write!(f, "{}{}{}: ", self.colors.message("[", message.kind), self.colors.message(message.name, message.kind), self.colors.message("]", message.kind))?;
        writeln!(f, "{}", self.colors.description(&message.description))?;

        self.collect_annotations(message);
        self.annotations.sort_by(|a, b| a.start.line.cmp(&b.start.line));

        self.print_source_block_with_notes(f)?;

        if message.suppressed_messages > 0 {
            writeln!(f, "... and {} more", message.suppressed_messages)?;
        }

        self.annotations.clear();
        self.max_nested_blocks = 0;
        self.line_digits = 0;

        Ok(())
    }

    fn collect_annotations(&mut self, message: &MessageData) {
        self.add_initial_annotation(message.kind, true, message.span, message.primary_annotation.clone());

        for (span, label) in message.additional_annotations.iter() {
            self.add_initial_annotation(message.kind, false, *span, label.clone());
        }
    }

    fn add_initial_annotation(&mut self, message_kind: MessageKind, primary: bool, span: Span, label: Option<String>) {
        let start = LineColumn::from_usize_lossy(self.line_col_lookup.get(span.start as usize));
        let end = LineColumn::from_usize_lossy(self.line_col_lookup.get(span.end as usize));

        self.annotations.push(AnnotationData {
            span,
            message_kind,
            primary,
            label,
            start,
            end,
            display_data: if start.line == end.line {
                AnnotationDisplayData::Singleline {
                    rightmost_index: 0,
                    vertical_offset: 0,
                }
            } else {
                AnnotationDisplayData::Multiline {
                    continuing_bar_index: 0,
                    vertical_offset: 0,
                }
            },
        });
    }

    fn print_source_block_with_notes(&mut self, f: &mut impl Write) -> std::fmt::Result {
        {
            let mut max_nested_blocks = 0;
            let mut current_nested_blocks = Vec::new();

            for annotation in self.annotations.iter() {
                if annotation.start.line == annotation.end.line {
                    continue;
                }

                current_nested_blocks.retain(|end| *end >= annotation.start.line);
                current_nested_blocks.push(annotation.end.line);
                max_nested_blocks = max_nested_blocks.max(current_nested_blocks.len());
            }

            self.max_nested_blocks = max_nested_blocks;
        }

        let last_note_line = self.annotations.last()
            .map(|annotation| annotation.end.line).expect("Error contains no annotations") as u32;

        let last_print_line = (last_note_line + self.config.error_surrounding_lines).min(self.lines.len() as u32);

        self.line_digits = last_print_line.ilog10() + 1;
        let empty_line_number = " ".repeat(self.line_digits as usize);

        let primary_annotation = self.annotations.iter().find(|annotation| annotation.primary).expect("Error contains no primary annotation");
        writeln!(f, "{}{} {}:{}:{}", &empty_line_number, "-->".bright_blue(), self.path.display(), primary_annotation.start.line, primary_annotation.start.column)?;

        self.annotations.reverse(); // First span is now the last element, and vice versa

        {
            let mut current_line = None;
            let mut already_printed_to_line = 0;
            let mut annotations_from_current_line = Vec::new();
            let mut continuing_notes = Vec::new();

            for (i, annotation) in self.annotations.iter().enumerate().rev() {
                if current_line.as_ref().map(|(current_line, _)| annotation.start.line > *current_line).unwrap_or(false) {
                    // next annotation begins on the next line
                    // Ignore annotation and print current_line

                    let line = current_line.take().expect("Current line number doesn't exist (this shouldn't have happened)").0 as u32;
                    self.print_part_lines(f, line, Some(annotation.start.line as u32),
                        &mut annotations_from_current_line, &mut continuing_notes, &mut already_printed_to_line)?;
                }

                if current_line.is_none() {
                    current_line = Some((annotation.start.line, annotation.end.line));
                }

                let pos = annotations_from_current_line.binary_search_by(|a| self.annotations[*a].start.column.cmp(&(annotation.start.column as u32)).reverse()).unwrap_or_else(|e| e);
                annotations_from_current_line.insert(pos, i);
            }

            if let Some((line, to_line)) = current_line.take() {
                let line = line as u32;
                let to_line = ((to_line as usize).min(self.lines.len())) as u32;

                self.print_part_lines(f, line, None,
                    &mut annotations_from_current_line, &mut continuing_notes,
                    &mut already_printed_to_line)?;

                self.print_part_lines(f, to_line, None,
                    &mut annotations_from_current_line, &mut continuing_notes,
                    &mut already_printed_to_line)?;
            }
        }

        Ok(())
    }

    fn print_part_lines(&mut self, f: &mut impl Write, line: u32, next_start_line: Option<u32>,
                        annotations_from_current_line: &mut Vec<usize>,
                        continuing_annotations: &mut Vec<usize>,
                        already_printed_to_line: &mut u32) -> std::fmt::Result {
        let first_print_line = if self.config.error_surrounding_lines > line - 1 { 1 } else { line - self.config.error_surrounding_lines }
            .max(*already_printed_to_line + 1);

        if first_print_line > *already_printed_to_line + 1 {
            write!(f, "{:>fill$} {} ", "", self.colors.line_number_separator("|"), fill = self.line_digits as usize)?;

            for annotation in continuing_annotations.iter().map(|i| &self.annotations[*i]) {
                write!(f, "{} ", self.colors.annotation("|", annotation))?;
            }

            writeln!(f, "{}...", " ".repeat(2 * (self.max_nested_blocks - continuing_annotations.len())))?;
        }

        for print_line in first_print_line..line {
            self.print_single_source_line(f, print_line, line, annotations_from_current_line, continuing_annotations, &self.lines[print_line as usize - 1])?;
        }

        self.print_single_source_line(f, line, line, annotations_from_current_line, continuing_annotations, &self.lines[line as usize - 1])?;
        *already_printed_to_line = line;

        if line as usize + 1 <= self.lines.len() {
            for print_line in line + 1..=line + self.config.error_surrounding_lines {
                if print_line as usize > self.lines.len()
                    || next_start_line.as_ref().map(|next_start_line| print_line > *next_start_line).unwrap_or(false) {
                    break;
                }

                self.print_single_source_line(f, print_line, line, annotations_from_current_line, continuing_annotations, &self.lines[print_line as usize - 1])?;
                *already_printed_to_line = print_line;
            }
        }

        Ok(())
    }

    fn print_single_source_line(&mut self, f: &mut impl Write, line: u32, main_line: u32,
                                annotations_from_current_line: &mut Vec<usize>,
                                continuing_annotations: &mut Vec<usize>,
                                line_str: &str) -> std::fmt::Result {
        write!(f, "{:>fill$} {} ", self.colors.line_number(&line.to_string()), self.colors.line_number_separator("|"), fill = self.line_digits as usize)?;

        for annotation in continuing_annotations.iter().map(|i| &self.annotations[*i]) {
            write!(f, "{} ", self.colors.annotation("|", annotation))?;
        }

        write!(f, "{}", " ".repeat(2 * (self.max_nested_blocks - continuing_annotations.len())))?;
        writeln!(f, "{}", self.colors.source(line_str))?;

        let mut done_note_indices = Vec::new();

        for (i, annotation) in annotations_from_current_line.iter().map(|i| &self.annotations[*i])
            .filter(|annotation| annotation.start.line == annotation.end.line).enumerate() {
            if annotation.start.line > line {
                continue;
            }

            write!(f, "{:>fill$} {} ", "", self.colors.line_number_separator("|"), fill = self.line_digits as usize)?;

            for annotation2 in continuing_annotations.iter().map(|i| &self.annotations[*i]) {
                write!(f, "{} ", self.colors.annotation("|", annotation2))?;
            }

            write!(f, "{}", " ".repeat(2 * (self.max_nested_blocks - continuing_annotations.len())))?;
            write!(f, "{}", " ".repeat(annotation.start.column as usize - 1))?;

            let annotation_underline = if annotation.primary { "^" } else { "-" }.repeat(annotation.end.column as usize - annotation.start.column as usize);
            write!(f, "{}", self.colors.annotation(&annotation_underline, annotation))?;

            if let Some(label) = annotation.label.as_ref() {
                if i == 0 { // The note with the rightmost start column
                    writeln!(f, " {}", self.colors.annotation(label, annotation))?;
                } else {
                    writeln!(f)?;

                    write!(f, "{:>fill$} {} ", "", self.colors.line_number_separator("|"), fill = self.line_digits as usize)?;

                    for annotation2 in continuing_annotations.iter().map(|i| &self.annotations[*i]) {
                        write!(f, "{} ", self.colors.annotation("|", annotation2))?;
                    }

                    write!(f, "{}", " ".repeat(2 * (self.max_nested_blocks - continuing_annotations.len())))?;
                    writeln!(f, "{}{}", " ".repeat(annotation.start.column as usize - 1), self.colors.annotation("|", annotation))?;

                    write!(f, "{:>fill$} {} ", "", self.colors.line_number_separator("|"), fill = self.line_digits as usize)?;

                    for annotation2 in continuing_annotations.iter().map(|i| &self.annotations[*i]) {
                        write!(f, "{} ", self.colors.annotation("|", annotation2))?;
                    }

                    write!(f, "{}", " ".repeat(2 * (self.max_nested_blocks - continuing_annotations.len())))?;
                    writeln!(f, "{} {}", " ".repeat(annotation.start.column as usize - 1), self.colors.annotation(label, annotation))?;
                }
            } else {
                writeln!(f)?;
            }

            done_note_indices.push(i);
        }

        for (i, index) in done_note_indices.into_iter().enumerate() {
            annotations_from_current_line.remove(index - i);
        }

        let mut done_note_indices = Vec::new();

        for (i, annotation) in continuing_annotations.iter().map(|i| &self.annotations[*i]).enumerate() {
            // writeln!(f, "[debug] continuing note end (line {} (at {}, main line: {}) of {})", end_line, line, main_line, lines.len())?;

            if annotation.end.line > line && (annotation.end.line as usize <= self.lines.len() || line as usize != self.lines.len()) {
                continue;
            }

            if !(annotation.end.line as usize > self.lines.len() && line as usize == self.lines.len()) {
                assert_eq!(line, annotation.end.line);
            }

            write!(f, "{:>fill$} {} ", "", self.colors.line_number_separator("|"), fill = self.line_digits as usize)?;

            for (j, annotation2) in continuing_annotations.iter().map(|i| &self.annotations[*i]).enumerate() {
                if j >= i {
                    break;
                }

                write!(f, "{} ", self.colors.annotation("|", annotation2))?;
            }

            write!(f, "{}", self.colors.annotation("|", annotation))?;
            let underscores = "_".repeat(2 * (self.max_nested_blocks - continuing_annotations.len() - i) + annotation.end.column as usize /* - 1 + 1 */);

            write!(f, "{}{}", self.colors.annotation(&underscores, annotation), self.colors.annotation(if annotation.primary { "^" } else { "-" }, annotation))?;

            if let Some(label) = annotation.label.as_ref() {
                writeln!(f, " {}", self.colors.annotation(label, annotation))?;
            } else {
                writeln!(f, )?;
            }

            done_note_indices.push(i);
        }

        for (i, index) in done_note_indices.into_iter().enumerate() {
            continuing_annotations.remove(index - i);
        }

        for annotation in annotations_from_current_line.iter().map(|i| &self.annotations[*i])
            .filter(|annotation| annotation.start.line < annotation.end.line) {
            if annotation.start.line > line {
                continue;
            }

            write!(f, "{:>fill$} {} ", "", self.colors.line_number_separator("|"), fill = self.line_digits as usize)?;

            for annotation2 in continuing_annotations.iter().map(|i| &self.annotations[*i]) {
                write!(f, "{} ", self.colors.annotation("|", annotation2))?;
            }

            write!(f, " ")?;
            let underscores = "_".repeat(annotation.start.column as usize /* - 1 + 1 */);

            writeln!(f, "{}{}", self.colors.annotation(&underscores, annotation), self.colors.annotation(if annotation.primary { "^" } else { "-" }, annotation))?;
        }

        if line == main_line {
            continuing_annotations.extend(annotations_from_current_line.drain(0..annotations_from_current_line.len()));
            annotations_from_current_line.clear();
        }

        Ok(())
    }
}
