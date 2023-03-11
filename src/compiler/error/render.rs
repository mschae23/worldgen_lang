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

    pub fn description(&self, string: &str) -> colored::ColoredString {
        match self {
            ColorConfig::Uncolored => colored::ColoredString::from(string),
            ColorConfig::ColoredDefault => string.bold(),
        }
    }
}

pub trait ErrorRenderer {
    fn render(config: Rc<Config>, path: Rc<PathBuf>, color_config: ColorConfig, source: &str, messages: &[MessageData]);
}

pub struct TerminalErrorRenderer<'source, 'm> {
    config: Rc<Config>, path: Rc<PathBuf>, colors: ColorConfig,
    lines: Vec<&'source str>,
    line_col_lookup: LineColLookup<'source>,
    messages: &'m [MessageData],
}

impl<'source, 'm> TerminalErrorRenderer<'source, 'm> {
    pub fn new(config: Rc<Config>, path: Rc<PathBuf>, color_config: ColorConfig, source: &'source str, messages: &'m [MessageData]) -> Self {
        TerminalErrorRenderer {
            config, path,
            colors: color_config, lines: source.lines().collect(), line_col_lookup: LineColLookup::new(source), messages,
        }
    }

    pub fn render(&mut self) {
        if self.messages.is_empty() {
            return;
        }

        let mut buf = String::new();
        self.render_impl(&mut buf).expect("Failed to render error messages");
        eprint!("{}", buf);
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
        write!(f, "{}", match message.kind {
            MessageKind::Error => self.colors.error("error"),
            MessageKind::Warning => self.colors.warning("warning"),
        })?;

        write!(f, "{}{}{}: ", self.colors.error("["), self.colors.error(message.name), self.colors.error("]"))?;
        writeln!(f, "{}", self.colors.description(&message.description))?;

        let mut notes = message.additional_inline_notes.clone();
        notes.sort_unstable_by(|(a, _), (b, _)| a.start.cmp(&b.start));

        let main_note_index = match notes.binary_search_by(|(a, _)| a.start.cmp(&message.span.start)) {
            Ok(pos) => pos, // element already in notes, at pos
            Err(pos) => {
                notes.insert(pos, (message.span, message.main_inline_note.clone()));
                pos
            },
        };

        Self::print_source_block_with_notes(f, Rc::clone(&self.config), Rc::clone(&self.path), &self.lines, &mut self.line_col_lookup, notes, main_note_index)?;

        if message.suppressed_messages > 0 {
            writeln!(f, "... and {} more", message.suppressed_messages)?;
        }

        Ok(())
    }

    fn print_source_block_with_notes(f: &mut impl Write, config: Rc<Config>, path: Rc<PathBuf>,
                                     lines: &[&'source str], line_col_lookup: &mut LineColLookup<'source>,
                                     notes: Vec<(Span, Option<String>)>, main_note_index: usize) -> std::fmt::Result {
        let mut notes: Vec<_> = notes.into_iter()
            .map(|(span, note)|
                ((line_col_lookup.get(span.start as usize), line_col_lookup.get(span.end as usize)), note))
            .collect();

        let max_nested_blocks = {
            let mut max_nested_blocks = 0;
            let mut current_nested_blocks = Vec::new();

            for (((start_line, _), (end_line, _)), _) in notes.iter() {
                if start_line == end_line {
                    continue;
                }

                current_nested_blocks.retain(|end| end >= start_line);
                current_nested_blocks.push(*end_line);
                max_nested_blocks = max_nested_blocks.max(current_nested_blocks.len());
            }

            max_nested_blocks
        };

        let last_note_line = notes.last()
            .map(|((_, (end_line, _)), _)| *end_line).expect("Error contains no notes") as u32;

        let last_print_line = (last_note_line).min(lines.len() as u32);

        let line_digits = last_print_line.ilog10() + 1;
        let empty_line_number = " ".repeat(line_digits as usize);

        let main_note = &notes[main_note_index].0.0;
        writeln!(f, "{}{} {}:{}:{}", &empty_line_number, "-->".bright_blue(), path.display(), main_note.0, main_note.1)?;

        notes.reverse(); // First span is now the last element, and vice versa
        let main_note_index = notes.len() - 1 - main_note_index;

        {
            let mut current_line = None;
            let mut already_printed_to_line = 0;
            let mut notes_from_current_line = Vec::new();
            let mut continuing_notes = Vec::new();

            while let Some((((start_line, start_column), (end_line, end_column)), note)) = notes.pop() {
                if current_line.as_ref().map(|(current_line, _)| start_line > *current_line).unwrap_or(false) {
                    // notes.last() begins on the next line
                    // Ignore notes.last() and print current_line

                    let line = current_line.take().expect("Current line number doesn't exist (this shouldn't have happened)").0 as u32;
                    Self::print_part_lines(f, Rc::clone(&config), line, Some(start_line as u32),
                        &mut notes_from_current_line, &mut continuing_notes,
                        max_nested_blocks, line_digits, lines, &mut already_printed_to_line)?;
                }

                if current_line.is_none() {
                    current_line = Some((start_line, end_line));
                }

                let main = notes.len() == main_note_index;

                let pos = notes_from_current_line.binary_search_by(|(_, _, ((_, start_column_2), _))| start_column_2.cmp(&(start_column as u32)).reverse()).unwrap_or_else(|e| e);
                notes_from_current_line.insert(pos, (main, note, ((start_line as u32, start_column as u32), (end_line as u32, end_column as u32))));
            }

            if let Some((line, to_line)) = current_line.take() {
                let line = line as u32;
                let to_line = (to_line.min(lines.len())) as u32;

                Self::print_part_lines(f, Rc::clone(&config), line, None,
                    &mut notes_from_current_line, &mut continuing_notes,
                    max_nested_blocks, line_digits, lines, &mut already_printed_to_line)?;

                Self::print_part_lines(f, Rc::clone(&config), to_line, None,
                    &mut notes_from_current_line, &mut continuing_notes,
                    max_nested_blocks, line_digits, lines, &mut already_printed_to_line)?;
            }
        }

        Ok(())
    }

    fn print_part_lines(f: &mut impl Write, config: Rc<Config>, line: u32, next_start_line: Option<u32>,
                        notes_from_current_line: &mut Vec<(bool, Option<String>, ((u32, u32), (u32, u32)))>,
                        continuing_notes: &mut Vec<(bool, Option<String>, (u32, u32))>,
                        max_nested_blocks: usize,
                        line_digits: u32, lines: &[&'source str], already_printed_to_line: &mut u32) -> std::fmt::Result {
        let first_print_line = if config.error_surrounding_lines > line - 1 { 1 } else { line - config.error_surrounding_lines }
            .max(*already_printed_to_line + 1);

        if first_print_line > *already_printed_to_line + 1 {
            write!(f, "{:>fill$} {} ", "", "|".bright_blue(), fill = line_digits as usize)?;

            for (main, _, _) in continuing_notes.iter() {
                write!(f, "{} ", if *main { "|".red() } else { "|".bright_blue() })?;
            }

            writeln!(f, "{}...", " ".repeat(2 * (max_nested_blocks - continuing_notes.len())))?;
        }

        for print_line in first_print_line..line {
            Self::print_single_source_line(f, Rc::clone(&config), print_line, line, notes_from_current_line, continuing_notes, max_nested_blocks, line_digits, lines, lines[print_line as usize - 1])?;
        }

        Self::print_single_source_line(f, Rc::clone(&config), line, line, notes_from_current_line, continuing_notes, max_nested_blocks, line_digits, lines, lines[line as usize - 1])?;
        *already_printed_to_line = line;

        if line as usize + 1 <= lines.len() {
            for print_line in line + 1..=line + config.error_surrounding_lines {
                if print_line as usize > lines.len()
                    || next_start_line.as_ref().map(|next_start_line| print_line > *next_start_line).unwrap_or(false) {
                    break;
                }

                Self::print_single_source_line(f, Rc::clone(&config), print_line, line, notes_from_current_line, continuing_notes, max_nested_blocks, line_digits, lines, lines[print_line as usize - 1])?;
                *already_printed_to_line = print_line;
            }
        }

        Ok(())
    }

    fn print_single_source_line(f: &mut impl Write, _config: Rc<Config>, line: u32, main_line: u32,
                                notes_from_current_line: &mut Vec<(bool, Option<String>, ((u32, u32), (u32, u32)))>,
                                continuing_notes: &mut Vec<(bool, Option<String>, (u32, u32))>,
                                max_nested_blocks: usize,
                                line_digits: u32, lines: &[&'source str], line_str: &str) -> std::fmt::Result {
        write!(f, "{:>fill$} {} ", line.to_string().bright_blue().bold(), "|".bright_blue(), fill = line_digits as usize)?;

        for (main, _, _) in continuing_notes.iter() {
            write!(f, "{} ", if *main { "|".red() } else { "|".bright_blue() })?;
        }

        write!(f, "{}", " ".repeat(2 * (max_nested_blocks - continuing_notes.len())))?;
        writeln!(f, "{}", line_str)?;

        let mut done_note_indices = Vec::new();

        for (i, (main, label, ((start_line, start_column), (_, end_column)))) in notes_from_current_line.iter().filter(|(_, _, (start, end))| start.0 == end.0).enumerate() {
            if *start_line > line {
                continue;
            }

            write!(f, "{:>fill$} {} ", "", "|".bright_blue(), fill = line_digits as usize)?;

            for (main2, _, _) in continuing_notes.iter() {
                write!(f, "{} ", if *main2 { "|".red() } else { "|".bright_blue() })?;
            }

            write!(f, "{}", " ".repeat(2 * (max_nested_blocks - continuing_notes.len())))?;
            write!(f, "{}", " ".repeat(*start_column as usize - 1))?;

            let annotation = if *main { "^" } else { "-" }.repeat(*end_column as usize - *start_column as usize);
            write!(f, "{}", if *main { annotation.red() } else { annotation.bright_blue() })?;

            if let Some(label) = label {
                if i == 0 { // The note with the rightmost start column
                    writeln!(f, " {}", if *main { label.red() } else { label.bright_blue() })?;
                } else {
                    writeln!(f)?;

                    write!(f, "{:>fill$} {} ", "", "|".bright_blue(), fill = line_digits as usize)?;

                    for (main2, _, _) in continuing_notes.iter() {
                        write!(f, "{} ", if *main2 { "|".red() } else { "|".bright_blue() })?;
                    }

                    write!(f, "{}", " ".repeat(2 * (max_nested_blocks - continuing_notes.len())))?;
                    writeln!(f, "{}{}", " ".repeat(*start_column as usize - 1), if *main { "|".red() } else { "|".bright_blue() })?;

                    write!(f, "{:>fill$} {} ", "", "|".bright_blue(), fill = line_digits as usize)?;

                    for (main2, _, _) in continuing_notes.iter() {
                        write!(f, "{} ", if *main2 { "|".red() } else { "|".bright_blue() })?;
                    }

                    write!(f, "{}", " ".repeat(2 * (max_nested_blocks - continuing_notes.len())))?;
                    writeln!(f, "{} {}", " ".repeat(*start_column as usize - 1), if *main { label.red() } else { label.bright_blue() })?;
                }
            } else {
                writeln!(f)?;
            }

            done_note_indices.push(i);
        }

        for (i, index) in done_note_indices.into_iter().enumerate() {
            notes_from_current_line.remove(index - i);
        }

        let mut done_note_indices = Vec::new();

        for (i, (main, label, (end_line, end_column))) in continuing_notes.iter().enumerate() {
            // writeln!(f, "[debug] continuing note end (line {} (at {}, main line: {}) of {})", end_line, line, main_line, lines.len())?;

            if *end_line > line && (*end_line as usize <= lines.len() || line as usize != lines.len()) {
                continue;
            }

            if !(*end_line as usize > lines.len() && line as usize == lines.len()) {
                assert_eq!(line, *end_line);
            }

            write!(f, "{:>fill$} {} ", "", "|".bright_blue(), fill = line_digits as usize)?;

            for (j, (main2, _, _)) in continuing_notes.iter().enumerate() {
                if j >= i {
                    break;
                }

                write!(f, "{} ", if *main2 { "|".red() } else { "|".bright_blue() })?;
            }

            write!(f, "{}", if *main { "|".red() } else { "|".bright_blue() })?;
            let underscores = "_".repeat(2 * (max_nested_blocks - continuing_notes.len() - i) + *end_column as usize /* - 1 + 1 */);

            write!(f, "{}{}", if *main { underscores.red() } else { underscores.bright_blue() },
                if *main { "^".red() } else { "-".bright_blue() })?;

            if let Some(label) = label {
                writeln!(f, " {}", if *main { label.red() } else { label.bright_blue() })?;
            } else {
                writeln!(f, )?;
            }

            done_note_indices.push(i);
        }

        for (i, index) in done_note_indices.into_iter().enumerate() {
            continuing_notes.remove(index - i);
        }

        for (main, _, ((start_line, start_column), _)) in notes_from_current_line.iter().filter(|(_, _, (start, end))| start.0 < end.0) {
            if *start_line > line {
                continue;
            }

            write!(f, "{:>fill$} {} ", "", "|".bright_blue(), fill = line_digits as usize)?;

            for (main2, _, _) in continuing_notes.iter() {
                write!(f, "{} ", if *main2 { "|".red() } else { "|".bright_blue() })?;
            }

            write!(f, " ")?;
            let underscores = "_".repeat(*start_column as usize /* - 1 + 1 */);

            writeln!(f, "{}{}", if *main { underscores.red() } else { underscores.bright_blue() },
                if *main { "^".red() } else { "-".bright_blue() })?;
        }

        if line == main_line {
            continuing_notes.extend(notes_from_current_line.drain(0..notes_from_current_line.len())
                .map(|(main, label, (_, end))| (main, label, end)));
            notes_from_current_line.clear();
        }

        Ok(())
    }
}

impl<'source, 'm> ErrorRenderer for TerminalErrorRenderer<'source, 'm> {
    fn render(config: Rc<Config>, path: Rc<PathBuf>, color_config: ColorConfig, source: & str, messages: &[MessageData]) {
        let mut renderer = TerminalErrorRenderer::new(config, path, color_config, source, messages);
        renderer.render();
    }
}
