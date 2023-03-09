pub struct ErrorReporting<'source> {
    lines: Vec<&'source str>,
}

pub struct ErrorReportingFrame {

}

impl<'source> ErrorReporting<'source> {
    pub fn new(source: &'source str) -> Self {
        ErrorReporting {
            lines: source.lines().collect(),
        }
    }
}
