#![deprecated(note = "This is the old error message renderer. Use diagnostic_render instead")]

use std::io::Write;
use std::path::PathBuf;
use std::rc::Rc;
use line_col::LineColLookup;
use termcolor::{BufferedStandardStream, Color, ColorChoice, ColorSpec, NoColor, WriteColor};
use crate::compiler::error::{DiagnosticData, Severity};
use crate::compiler::error::span::Span;
use crate::Config;

#[derive(Clone, Debug, PartialEq)]
pub enum ColorConfig {
    Default,
}

impl ColorConfig {
    pub fn reset(&self, f: &mut impl WriteColor) -> std::io::Result<()> {
        f.reset()
    }

    pub fn error(&self, f: &mut impl WriteColor) -> std::io::Result<()> {
        match self {
            Self::Default => f.set_color(ColorSpec::new().set_fg(Some(Color::Red))),
        }
    }

    pub fn warning(&self, f: &mut impl WriteColor) -> std::io::Result<()> {
        match self {
            Self::Default => f.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))),
        }
    }

    pub fn message(&self, f: &mut impl WriteColor, kind: Severity) -> std::io::Result<()> {
        match kind {
            Severity::Error => self.error(f),
            Severity::Warning => self.warning(f),
        }
    }

    pub fn description(&self, f: &mut impl WriteColor) -> std::io::Result<()> {
        match self {
            Self::Default => f.set_color(ColorSpec::new().set_bold(true)),
        }
    }

    pub fn line_number(&self, f: &mut impl WriteColor) -> std::io::Result<()> {
        match self {
            Self::Default => f.set_color(ColorSpec::new().set_fg(Some(Color::Blue)).set_intense(true).set_bold(true)),
        }
    }

    pub fn line_number_separator(&self, f: &mut impl WriteColor) -> std::io::Result<()> {
        match self {
            Self::Default => f.set_color(ColorSpec::new().set_fg(Some(Color::Blue)).set_intense(true)),
        }
    }

    pub fn additional_annotation(&self, f: &mut impl WriteColor) -> std::io::Result<()> {
        match self {
            Self::Default => f.set_color(ColorSpec::new().set_fg(Some(Color::Blue)).set_intense(true)),
        }
    }

    pub fn annotation(&self, f: &mut impl WriteColor, annotation: &AnnotationData) -> std::io::Result<()> {
        if annotation.primary {
            self.message(f, annotation.message_kind)
        } else {
            self.additional_annotation(f)
        }
    }

    pub fn source(&self, f: &mut impl WriteColor) -> std::io::Result<()> {
        match self {
            Self::Default => self.reset(f),
        }
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
    message_kind: Severity, primary: bool, // error / warning (for colors), primary (for color and underline char)
    label: Option<String>,
    start: LineColumn, end: LineColumn,
    display_data: AnnotationDisplayData,
}

pub struct TerminalErrorRenderer<'source, 'm> {
    config: Rc<Config>, path: Rc<PathBuf>, colors: ColorConfig,
    lines: Vec<&'source str>,
    line_col_lookup: LineColLookup<'source>,
    messages: &'m [DiagnosticData],

    max_nested_blocks: usize, line_digits: u32,
}

impl<'source, 'm> TerminalErrorRenderer<'source, 'm> {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, color_config: ColorConfig, source: &'source str, messages: &'m [DiagnosticData]) -> Self {
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

        let mut buf = BufferedStandardStream::stderr(ColorChoice::Auto);
        self.render_impl(&mut buf).expect("Failed to render error messages");
        buf.flush().expect("Failed to flush stderr");
    }

    pub fn render_to_string(&mut self) -> String {
        if self.messages.is_empty() {
            return String::new();
        }

        let mut buf = NoColor::new(std::io::Cursor::new(Vec::new()));
        self.render_impl(&mut buf).expect("Failed to render error messages");
        String::from_utf8_lossy(&buf.into_inner().into_inner()).into_owned()
    }

    fn render_impl(&mut self, f: &mut impl WriteColor) -> std::io::Result<()> {
        for (i, message) in self.messages.iter().enumerate() {
            self.print_message(f, message)?;

            if i < self.messages.len() - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }

    fn print_message(&mut self, f: &mut impl WriteColor, message: &DiagnosticData) -> std::io::Result<()> {
        self.colors.message(f, message.severity)?;
        write!(f, "{}", match message.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
        })?;
        // self.colors.reset(f)?;

        self.colors.message(f, message.severity)?;
        write!(f, "{}{}{}", "[", message.name, "]")?;
        self.colors.description(f)?;
        writeln!(f, ": {}", &message.message)?;
        self.colors.reset(f)?;

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

    fn collect_annotations(&mut self, message: &DiagnosticData) -> Vec<AnnotationData> {
        let mut annotations = Vec::with_capacity(message.additional_annotations.len() + 1);
        self.add_initial_annotation(&mut annotations, message.severity, true, message.span, message.primary_annotation.clone());

        for (span, label) in message.additional_annotations.iter() {
            self.add_initial_annotation(&mut annotations, message.severity, false, *span, label.clone());
        }

        annotations
    }

    fn add_initial_annotation(&mut self, annotations: &mut Vec<AnnotationData>, message_kind: Severity, primary: bool, span: Span, label: Option<String>) {
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

    fn print_source_block_with_annotations(&mut self, f: &mut impl WriteColor, mut annotations: Vec<AnnotationData>) -> std::io::Result<()> {
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
        let mut current_line = 0;
        let mut next_rightmost_index = 0;

        for annotation in annotations.iter_mut() {
            match &mut annotation.display_data {
                AnnotationDisplayData::Singleline { rightmost_index, vertical_offset } => {
                    if annotation.start.line > current_line {
                        current_line = annotation.start.line;
                        next_rightmost_index = 0;
                    }

                    *rightmost_index = next_rightmost_index;
                    next_rightmost_index += 1;
                    *vertical_offset = *rightmost_index;
                },
                AnnotationDisplayData::Multiline { start_rightmost_index, start_vertical_offset, continuing_bar_index, end_rightmost_index: _, end_vertical_offset: _ } => {
                    for a in current_nested_blocks.iter_mut() {
                        if a.end.line < annotation.start.line {
                            if a.end.line > current_line {
                                current_line = a.end.line;
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

                    if annotation.start.line > current_line {
                        current_line = annotation.start.line;
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
            if a.end.line > current_line {
                current_line = a.end.line;
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

    fn print_lines_with_annotations(&mut self, f: &mut impl WriteColor, mut annotations: Vec<AnnotationData>) -> std::io::Result<()> {
        let mut already_printed_to = 0;
        let mut annotations_on_line_indices = Vec::new();
        let mut continuing_annotations_indices = Vec::new();
        let mut current_line = 0;
        let mut last_line = None;
        let mut min_index = 0;

        loop {
            current_line = match annotations.iter().skip(min_index).next() {
                None => break,
                Some(annotation) => (current_line + 1).max(annotation.start.line),
            };

            if current_line as usize > self.lines.len() {
                break;
            }

            for (i, annotation) in annotations.iter().enumerate().skip(min_index) {
                if annotation.start.line > current_line && annotation.end.line > current_line {
                    break;
                } else if annotation.end.line < current_line && annotation.start.line < current_line {
                    min_index = i + 1;
                    continue;
                }

                if annotation.start.line == current_line || annotation.end.line == current_line {
                    annotations_on_line_indices.push(i);
                } else if annotation.start.line < current_line && annotation.end.line >= current_line {
                    continuing_annotations_indices.push(i);
                }
            }

            if current_line != 0 && !annotations_on_line_indices.is_empty() {
                self.fix_connecting_annotations(current_line, &mut annotations, &annotations_on_line_indices);

                self.print_part_lines(f, current_line, last_line,
                    &annotations_on_line_indices.iter().map(|i| &annotations[*i]).collect::<Vec<_>>(),
                    &continuing_annotations_indices.iter().map(|i| &annotations[*i]).collect::<Vec<_>>(),
                    &mut already_printed_to)?;
                annotations_on_line_indices.clear();
                continuing_annotations_indices.clear();

                last_line = Some(current_line);
            }
        }

        if let Some(last_line) = last_line {
            if (last_line as usize) < self.lines.len() {
                self.print_post_surrounding_lines(f, self.lines.len() as u32 + 1, last_line, &[], &mut already_printed_to)?;
            }
        }

        Ok(())
    }

    fn fix_connecting_annotations(&mut self, line: u32, annotations: &mut Vec<AnnotationData>, indices: &[usize]) {
        if indices.len() <= 1 {
            return;
        }

        let mut side = Vec::with_capacity(indices.len());
        let mut should_fix = false;

        for i in indices.iter() {
            let annotation = &annotations[*i];
            // eprintln!("[debug] fix collect; index: {} / {}", i, indices.len());

            let side = if annotation.start.line == line {
                side.push(false);
                false
            } else if annotation.end.line == line {
                side.push(true);
                true
            } else {
                panic!("Annotation does not start or end on this line")
            };

            match &annotation.display_data {
                AnnotationDisplayData::Singleline { .. } => {},
                AnnotationDisplayData::Multiline { start_rightmost_index, start_vertical_offset, end_rightmost_index, end_vertical_offset, .. } => {
                    if side {
                        if *end_rightmost_index == 0 && *end_vertical_offset == 0 {
                            should_fix = true;
                        }
                    } else {
                        if *start_rightmost_index == 0 && *start_vertical_offset == 0 {
                            should_fix = true;
                        }
                    }
                },
            }
        }

        if should_fix {
            for (i, index) in indices.iter().enumerate() {
                // eprintln!("[debug] fix process; index: {} / {}", index, indices.len());

                let annotation = &mut annotations[*index];
                let side = side[i];

                match &mut annotation.display_data {
                    AnnotationDisplayData::Singleline { vertical_offset, .. } => {
                        *vertical_offset += 1;
                    },
                    AnnotationDisplayData::Multiline { start_vertical_offset, end_vertical_offset, .. } => {
                        if side {
                            *end_vertical_offset += 1;
                        } else {
                            *start_vertical_offset += 1;
                        }
                    },
                }
            }
        }
    }

    fn print_post_surrounding_lines(&mut self, f: &mut impl WriteColor, main_line: u32, last_line: u32,
                                    continuing_annotations: &[&AnnotationData], already_printed_to: &mut u32) -> std::io::Result<()> {
        // writeln!(f, "[debug] potentially printing post surrounding lines, last line: {}, already printed to: {}", last_line, *already_printed_to)?;

        if last_line >= *already_printed_to {
            let first_print_line = (last_line + 1).max(*already_printed_to + 1);
            let last_print_line = self.get_last_print_line(last_line).min(main_line - 1);

            // writeln!(f, "[debug] printing post surrounding lines, last line: {}, first: {}, last: {}", last_line, first_print_line, last_print_line)?;

            if last_print_line >= first_print_line {
                for line in first_print_line..=last_print_line {
                    self.print_single_source_line(f, line, last_line, &[], continuing_annotations)?;
                    *already_printed_to = line;
                }
            }
        }

        Ok(())
    }

    fn print_part_lines(&mut self, f: &mut impl WriteColor, main_line: u32, last_line: Option<u32>,
                        annotations: &[&AnnotationData], continuing_annotations: &[&AnnotationData],
                        already_printed_to: &mut u32) -> std::io::Result<()> {
        if let Some(last_line) = last_line {
            self.print_post_surrounding_lines(f, main_line, last_line, continuing_annotations, already_printed_to)?;
        }

        let first_print_line = self.get_start_print_line(main_line).max(*already_printed_to + 1);
        let last_print_line = main_line;

        // writeln!(f, "[debug] current line ({}); first = {}, last = {}", main_line, first_print_line, last_print_line)?;

        if first_print_line > *already_printed_to + 1 {
            self.write_line_number(f, None, "...")?;
            writeln!(f)?;
        }

        for line in first_print_line..=last_print_line {
            self.print_single_source_line(f, line, main_line, annotations, continuing_annotations)?;
            *already_printed_to = line;
        }

        Ok(())
    }

    fn print_single_source_line(&mut self, f: &mut impl WriteColor, line: u32, main_line: u32,
                                annotations: &[&AnnotationData], continuing_annotations: &[&AnnotationData]) -> std::io::Result<()> {
        self.write_line_begin(f, Some(line), " | ", continuing_annotations)?;

        self.colors.source(f)?;
        writeln!(f, "{}", &self.lines[line as usize - 1])?;
        self.colors.reset(f)?;

        if line != main_line {
            return Ok(());
        }

        self.write_single_source_annotations(f, line, annotations, continuing_annotations)
    }

    fn write_single_source_annotations(&mut self, f: &mut impl WriteColor, line: u32,
                                       annotations: &[&AnnotationData], continuing_annotations: &[&AnnotationData]) -> std::io::Result<()> {
        let mut underline_prototype = vec![0; self.lines[line as usize - 1].len()];
        let mut prototype_next_index = 0;
        let mut message_kind = Severity::Error;
        let mut connecting_annotations = Vec::new();

        for annotation in annotations.iter().rev() {
            message_kind = annotation.message_kind;

            if annotation.start.line != line && annotation.end.line != line {
                continue;
            }

            let start = annotation.start.line == line;
            let end = annotation.end.line == line;

            if start && end {
                let start_column = annotation.start.column;
                let end_column = annotation.end.column;

                underline_prototype[start_column as usize - 1 .. end_column as usize - 1].fill(if annotation.primary { 1 } else { 2 });
                prototype_next_index = prototype_next_index.max(end_column as usize - 1);
            } else {
                let column = if start { annotation.start.column } else { annotation.end.column };
                underline_prototype[column as usize - 1] = if annotation.primary { 1 } else { 2 };
                prototype_next_index = prototype_next_index.max(column as usize);

                connecting_annotations.push(*annotation);
            }

            // self.write_annotation_debug(f, annotation)?;
        }

        self.write_line_number(f, None, " | ")?;

        let connect_on_first_line = connecting_annotations.iter().find(|a| {
            match &a.display_data {
                AnnotationDisplayData::Singleline { .. } => false,
                AnnotationDisplayData::Multiline { start_vertical_offset, end_vertical_offset, .. } => {
                    if a.start.line == line {
                        *start_vertical_offset == 0
                    } else if a.end.line == line {
                        *end_vertical_offset == 0
                    } else {
                        false
                    }
                },
            }
        });
        let label_on_first_line = annotations.iter().find(|a| {
            match &a.display_data {
                AnnotationDisplayData::Singleline { vertical_offset, .. } => *vertical_offset == 0,
                AnnotationDisplayData::Multiline { end_vertical_offset, .. } => {
                    if a.end.line == line {
                        *end_vertical_offset == 0
                    } else if a.start.line == line {
                        false
                    } else {
                        false
                    }
                },
            }
        });

        for (i, annotation) in continuing_annotations.iter().enumerate() {
            self.colors.annotation(f, annotation)?;
            write!(f, "|")?;
            self.colors.reset(f)?;

            if connect_on_first_line.is_none() || annotation.start.line == line || i < connecting_annotations.len() - 1 {
                write!(f, " ")?;
            }
        }

        self.colors.reset(f)?;

        if let Some(annotation) = &connect_on_first_line {
            if annotation.start.line == line {
                write!(f, " ")?;
            }

            self.colors.annotation(f, annotation)?;
            write!(f, "{}", "_".repeat(2 * (self.max_nested_blocks - continuing_annotations.len()) + if annotation.start.line == line { 0 } else { 1 }))?;
            self.colors.reset(f)?;
        } else {
            write!(f, "{:>nested_blocks$}", "", nested_blocks = 2 * (self.max_nested_blocks - continuing_annotations.len()))?;
        }

        let mut encountered_first = false;

        for (i, c) in underline_prototype.into_iter().enumerate() {
            if i >= prototype_next_index {
                break;
            }

            match c {
                0 => {
                    if let (false, Some(annotation)) = (encountered_first, connect_on_first_line) {
                        self.colors.annotation(f, annotation)?;
                        write!(f, "_")?;
                        self.colors.reset(f)?;
                    } else {
                        write!(f, " ")?
                    }
                },
                1 => {
                    self.colors.message(f, message_kind)?;
                    write!(f, "^")?;
                    self.colors.reset(f)?;
                    encountered_first = true;
                },
                2 => {
                    self.colors.additional_annotation(f)?;
                    write!(f, "-")?;
                    self.colors.reset(f)?;
                    encountered_first = true;
                },
                _ => {}, // Can't happen
            }
        }

        if let Some(annotation) = &label_on_first_line {
            if let Some(label) = annotation.label.as_ref() {
                write!(f, " ")?;
                self.colors.annotation(f, annotation)?;
                write!(f, "{}", label)?;
                self.colors.reset(f)?;
            }
        }

        writeln!(f)
    }

    fn write_line_number(&self, f: &mut impl WriteColor, line: Option<u32>, separator: &str) -> std::io::Result<()> {
        if let Some(line) = line {
            self.colors.line_number(f)?;
            write!(f, "{:>fill$}", line, fill = self.line_digits as usize)?;
        } else {
            write!(f, "{:>fill$}", "", fill = self.line_digits as usize)?;
        }

        self.colors.line_number_separator(f)?;
        write!(f, "{}", separator)?;
        self.colors.reset(f)?;
        Ok(())
    }

    fn write_line_begin(&self, f: &mut impl WriteColor, line: Option<u32>, separator: &str, continuing_annotations: &[&AnnotationData]) -> std::io::Result<()> {
        self.write_line_number(f, line, separator)?;

        for annotation in continuing_annotations.iter() {
            self.colors.annotation(f, annotation)?;
            write!(f, "| ")?;
        }

        self.colors.reset(f)?;
        write!(f, "{:>nested_blocks$}", "", nested_blocks = 2 * (self.max_nested_blocks - continuing_annotations.len()))?;
        Ok(())
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

    #[allow(unused)]
    fn write_annotation_debug(&self, f: &mut impl WriteColor, annotation: &AnnotationData) -> std::io::Result<()> {
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
