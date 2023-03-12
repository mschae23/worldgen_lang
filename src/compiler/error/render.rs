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
        rightmost_index: u32, // Index of this annotation of all starting or ending on this line, from the right
        vertical_offset: u32, // How many annotations on this line have to be displayed before this one
    },
    Multiline {
        start_rightmost_index: u32, // Index of this annotation of all starting or ending on the line this annotation starts on, from the right
        start_vertical_offset: u32, // How many annotations on the start line have to be displayed before this one
        continuing_bar_index: u32, // What position the `|` to the left of the source code is at
        end_rightmost_index: u32, // Index of this annotation of all starting or ending on the line this annotation ends on, from the right
        end_vertical_offset: u32, // How many annotations on the end line have to be displayed before this one
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

    max_nested_blocks: usize, line_digits: u32,
}

impl<'source, 'm> TerminalErrorRenderer<'source, 'm> {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, color_config: ColorConfig, source: &'source str, messages: &'m [MessageData]) -> Self {
        TerminalErrorRenderer {
            config, path,
            colors: color_config, lines: source.lines().collect(), line_col_lookup: LineColLookup::new(source), messages,
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

        let mut annotations = self.collect_annotations(message);
        annotations.sort_by(|a, b| a.start.line.cmp(&b.start.line).then_with(|| a.start.column.cmp(&b.start.column).reverse()));

        self.print_source_block_with_annotations(f, annotations)?;

        if message.suppressed_messages > 0 {
            writeln!(f, "... and {} more", message.suppressed_messages)?;
        }

        self.max_nested_blocks = 0;
        self.line_digits = 0;

        Ok(())
    }

    fn collect_annotations(&mut self, message: &MessageData) -> Vec<AnnotationData> {
        let mut annotations = Vec::with_capacity(message.additional_annotations.len() + 1);
        self.add_initial_annotation(&mut annotations, message.kind, true, message.span, message.primary_annotation.clone());

        for (span, label) in message.additional_annotations.iter() {
            self.add_initial_annotation(&mut annotations, message.kind, false, *span, label.clone());
        }

        annotations
    }

    fn add_initial_annotation(&mut self, annotations: &mut Vec<AnnotationData>, message_kind: MessageKind, primary: bool, span: Span, label: Option<String>) {
        let start = LineColumn::from_usize_lossy(self.line_col_lookup.get(span.start as usize));
        let mut end = LineColumn::from_usize_lossy(self.line_col_lookup.get(span.end as usize));

        if end.line > self.lines.len() as u32 {
            end.line = self.lines.len() as u32;
            end.column = self.lines[end.line as usize - 1].len() as u32;
        }

        annotations.push(AnnotationData {
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
                    start_rightmost_index: 0,
                    start_vertical_offset: 0,
                    end_rightmost_index: 0,
                    end_vertical_offset: 0,
                }
            },
        });
    }

    fn print_source_block_with_annotations(&mut self, f: &mut impl Write, mut annotations: Vec<AnnotationData>) -> std::fmt::Result {
        self.calculate_annotation_data(&mut annotations);

        let last_note_line = annotations.iter().map(|a| a.end.line).max()
            .expect("Error contains no annotations");

        let last_print_line = (last_note_line + self.config.error_surrounding_lines).min(self.lines.len() as u32);

        self.line_digits = last_print_line.ilog10() + 1;

        self.write_line_number(f, None, "-->")?;
        let primary_annotation = annotations.iter().find(|annotation| annotation.primary).expect("Error contains no primary annotation");
        writeln!(f, " {}:{}:{}", self.path.display(), primary_annotation.start.line, primary_annotation.start.column)?;

        {
            // for annotation in annotations.iter() {
            //     self.write_annotation_debug(f, annotation)?;
            // }
        }

        self.print_lines_with_annotations(f, annotations)?;
        Ok(())
    }

    fn calculate_annotation_data(&mut self, annotations: &mut Vec<AnnotationData>) {
        let mut max_nested_blocks = 0;
        let mut current_nested_blocks: Vec<&mut AnnotationData> = Vec::new();
        let mut last_line = 0;
        let mut next_rightmost_index = 0;

        for annotation in annotations.iter_mut() {
            match &mut annotation.display_data {
                AnnotationDisplayData::Singleline { rightmost_index, vertical_offset } => {
                    if annotation.start.line > last_line {
                        last_line = annotation.start.line;
                        next_rightmost_index = 0;
                    }

                    *rightmost_index = next_rightmost_index;
                    next_rightmost_index += 1;
                    *vertical_offset = *rightmost_index;
                },
                AnnotationDisplayData::Multiline { start_rightmost_index, start_vertical_offset, continuing_bar_index, end_rightmost_index: _, end_vertical_offset: _ } => {
                    for a in current_nested_blocks.iter_mut() {
                        if a.end.line < annotation.start.line {
                            if a.end.line > last_line {
                                last_line = a.end.line;
                                next_rightmost_index = 0;
                            }

                            match &mut a.display_data {
                                AnnotationDisplayData::Singleline { .. } => {}, // Can't happen
                                AnnotationDisplayData::Multiline { start_rightmost_index: _, start_vertical_offset: _, continuing_bar_index: _, end_rightmost_index: end_rightmost_index_2, end_vertical_offset: end_vertical_offset_2 } => {
                                    *end_rightmost_index_2 = next_rightmost_index;
                                    next_rightmost_index += 1;
                                    *end_vertical_offset_2 = *end_rightmost_index_2;
                                },
                            }
                        }
                    }

                    if annotation.start.line > last_line {
                        last_line = annotation.start.line;
                        next_rightmost_index = 0;
                    }

                    *start_rightmost_index = next_rightmost_index;
                    next_rightmost_index += 1;
                    *start_vertical_offset = *start_rightmost_index;

                    current_nested_blocks.retain(|a| a.end.line >= annotation.start.line);
                    *continuing_bar_index = current_nested_blocks.len() as u32;

                    current_nested_blocks.push(annotation);
                    max_nested_blocks = max_nested_blocks.max(current_nested_blocks.len());
                },
            }
        }

        for a in current_nested_blocks.iter_mut() {
            if a.end.line > last_line {
                last_line = a.end.line;
                next_rightmost_index = 0;
            }

            match &mut a.display_data {
                AnnotationDisplayData::Singleline { .. } => {}, // Can't happen
                AnnotationDisplayData::Multiline { start_rightmost_index: _, start_vertical_offset: _, continuing_bar_index: _, end_rightmost_index: end_rightmost_index_2, end_vertical_offset: end_vertical_offset_2 } => {
                    *end_rightmost_index_2 = next_rightmost_index;
                    next_rightmost_index += 1;
                    *end_vertical_offset_2 = *end_rightmost_index_2;
                },
            }
        }

        self.max_nested_blocks = max_nested_blocks;
    }

    fn print_lines_with_annotations(&mut self, f: &mut impl Write, annotations: Vec<AnnotationData>) -> std::fmt::Result {
        let mut already_printed_to = 0;
        let mut annotations_on_line = Vec::new();
        let mut last_line = 0;
        let mut min_index = 0;

        'outer: loop {
            last_line = match annotations.iter().skip(min_index).next() {
                None => break,
                Some(annotation) => (last_line + 1).max(annotation.start.line),
            };

            for (i, annotation) in annotations.iter().enumerate().skip(min_index) {
                if annotation.start.line > last_line && annotation.end.line > last_line {
                    break;
                } else if annotation.end.line < last_line && annotation.start.line < last_line {
                    min_index = i + 1;
                    continue;
                }

                if annotation.start.line == last_line || annotation.end.line == last_line {
                    annotations_on_line.push(annotation);
                }
            }

            if last_line != 0 && !annotations_on_line.is_empty() {
                self.print_part_lines(f, last_line, &annotations_on_line, &mut already_printed_to)?;
                annotations_on_line.clear();
            }
        }

        Ok(())
    }

    fn print_part_lines(&mut self, f: &mut impl Write, main_line: u32, annotations: &[&AnnotationData], already_printed_to: &mut u32) -> std::fmt::Result {
        let first_print_line = self.get_start_print_line(main_line).max(*already_printed_to + 1);
        let last_print_line = self.get_last_print_line(main_line);

        if first_print_line > *already_printed_to + 1 {
            self.write_line_number(f, None, "...")?;
            writeln!(f)?;
        }

        for line in first_print_line..=last_print_line {
            self.print_single_source_line(f, line, main_line, annotations)?;
            *already_printed_to = line;
        }

        Ok(())
    }

    fn print_single_source_line(&mut self, f: &mut impl Write, line: u32, _main_line: u32, annotations: &[&AnnotationData]) -> std::fmt::Result {
        self.write_line_number(f, Some(line), " | ")?;
        write!(f, "{:>nested_blocks$}", "", nested_blocks = 2 * self.max_nested_blocks as usize)?;
        writeln!(f, "{}", self.colors.source(self.lines[line as usize - 1]))?;

        for annotation in annotations.iter() {
            if annotation.start.line != line && annotation.end.line != line {
                continue;
            }

            self.write_annotation_debug(f, annotation)?;
        }

        Ok(())
    }

    fn write_line_number(&mut self, f: &mut impl Write, line: Option<u32>, separator: &str) -> std::fmt::Result {
        if let Some(line) = line {
            write!(f, "{:>fill$}", self.colors.line_number(&line.to_string()), fill = self.line_digits as usize)?;
        } else {
            write!(f, "{:>fill$}", "", fill = self.line_digits as usize)?;
        }

        write!(f, "{}", self.colors.line_number_separator(separator))
    }

    fn get_start_print_line(&self, line: u32) -> u32 {
        if self.config.error_surrounding_lines >= line {
            1
        } else {
            line - self.config.error_surrounding_lines
        }
    }

    fn get_last_print_line(&self, line: u32) -> u32 {
        (line + self.config.error_surrounding_lines).min(self.lines.len() as u32)
    }

    fn write_annotation_debug(&self, f: &mut impl Write, annotation: &AnnotationData) -> std::fmt::Result {
        write!(f, "[debug] annotation \"{}\"; primary = {:>5}, display kind = {}, ", annotation.label.as_ref().map::<&str, _>(|a| a).unwrap_or(""), annotation.primary, match &annotation.display_data {
            AnnotationDisplayData::Singleline { .. } => "singleline",
            AnnotationDisplayData::Multiline { .. } => "multiline",
        })?;

        match &annotation.display_data {
            AnnotationDisplayData::Singleline { rightmost_index, vertical_offset } => {
                writeln!(f, "rightmost index = {}, vertical_offset = {}", rightmost_index, vertical_offset)?;
            },
            AnnotationDisplayData::Multiline { start_rightmost_index, start_vertical_offset, continuing_bar_index, end_rightmost_index, end_vertical_offset } => {
                writeln!(f, "start rightmost index = {}, start vertical offset = {}, continuing bar index = {}, end rightmost index = {}, end vertical_offset = {}",
                    start_rightmost_index, start_vertical_offset, continuing_bar_index, end_rightmost_index, end_vertical_offset)?;
            },
        }

        Ok(())
    }
}
