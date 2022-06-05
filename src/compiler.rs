use crate::ast;

#[derive(Debug)]
pub enum CompilerError {
    // Unknown,
}

pub struct Compiler<'a> {
    lines: Vec<ast::Line<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(lines: Vec<ast::Line<'a>>) -> Self {
        Self { lines }
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        for line in &self.lines {
            println!("{}", line);
        }

        Ok(())
    }
}
