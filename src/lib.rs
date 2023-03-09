pub mod util;
pub mod compiler;

use std::path::PathBuf;
use std::rc::Rc;
use clap::Parser as ClapParser;

#[derive(ClapParser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Config {
    #[clap(default_value = "main.densityfunction", help = "Main input file")]
    pub input: PathBuf,
    #[clap(long, default_value = "target", help = "Target directory")]
    pub target_dir: PathBuf,

    #[clap(short, long, help = "Print verbose log output")]
    pub verbose: bool,

    #[clap(long, help = "Disable pretty printing")]
    pub no_pretty_print: bool,
    #[clap(long, default_value = "    ", help = "Indentation string for pretty printing")]
    pub indentation: String,
}

pub fn run() -> Result<(), std::io::Error> {
    let config: Rc<Config> = Rc::new(Config::parse());

    // compile

    std::fs::create_dir_all(&config.target_dir)?;
    // let mut writer = JsonWriter::new(config.indentation.to_owned(), !config.no_pretty_print);

    Ok(())
}
