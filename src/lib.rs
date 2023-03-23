pub mod util;
pub mod compiler;

use std::path::PathBuf;
use std::rc::Rc;
use clap::Parser as ClapParser;
use crate::compiler::error::ErrorReporting;
use crate::compiler::pipeline::CompileState;

#[derive(ClapParser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Config {
    #[arg(default_value = "main.densityfunction", help = "Main input file")]
    pub input: PathBuf,
    #[arg(long, default_value = "target", help = "Target directory", value_name = "DIR")]
    pub target_dir: PathBuf,

    #[arg(short, long, help = "Print verbose log output")]
    pub verbose: bool,
    #[arg(long, default_value = "1", help = "Surrounding lines in error messages for context", value_name = "LINES")]
    pub error_surrounding_lines: u32,

    #[arg(long, help = "Disable pretty printing")]
    pub no_pretty_print: bool,
    #[arg(long, default_value = "    ", help = "Indentation string for pretty printing", value_name = "STRING")]
    pub indentation: String,
}

pub fn run() -> Result<(), std::io::Error> {
    let config: Rc<Config> = Rc::new(Config::parse());

    let mut reporting = ErrorReporting::new(Rc::clone(&config));

    let input = Rc::new(config.input.clone());
    let source = std::fs::read_to_string(input.as_path())?;
    let pipeline = CompileState::new(Rc::clone(&config), &source, Rc::clone(&input))
        .tokenize()
        .parse(&mut reporting);

    // compile

    std::fs::create_dir_all(&config.target_dir)?;
    // let mut writer = JsonWriter::new(config.indentation.to_owned(), !config.no_pretty_print);

    if reporting.has_diagnostics() {
        reporting.print_diagnostics_to_stderr();
    } else {
        for decl in &pipeline.declarations {
            eprintln!("{:#?}", decl);
        }
    }

    Ok(())
}
