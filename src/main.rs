fn main() {
    match worldgen_lang::run() {
        Ok(_) => {}
        Err(_) => std::process::exit(1),
    }
}
