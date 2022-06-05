use crate::ast;
use std::collections::HashMap;

#[derive(Debug)]
pub enum CompilerError {
    // Unknown,
}

pub struct Compiler<'a> {
    lines: Vec<ast::Line<'a>>,
}

#[derive(Debug)]
struct Output<'a, 'b> {
    address: u32,
    bytes: Vec<u8>,
    line: &'b ast::Line<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new(lines: Vec<ast::Line<'a>>) -> Self {
        Self { lines }
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let mut outputs: Vec<Output<'a, '_>> = vec![];
        let mut labels: HashMap<&'a str, usize> = HashMap::new();

        for line in &self.lines {
            match line {
                ast::Line::Label(label) => {
                    labels.insert(label, outputs.len());
                }
                line @ ast::Line::Instruction(_) => outputs.push(Output {
                    address: 0,
                    bytes: vec![],
                    line,
                }),
                line @ ast::Line::Data(data) => outputs.push(Output {
                    address: 0,
                    bytes: data.clone(),
                    line,
                }),
                ast::Line::Constant(_) => {}
                line @ ast::Line::Times(_) => outputs.push(Output {
                    address: 0,
                    bytes: vec![],
                    line,
                }),
            }
        }

        for output in &outputs {
            println!("{:?}", output);
        }

        Ok(())
    }
}
