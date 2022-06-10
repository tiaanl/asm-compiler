use crate::ast;
use std::collections::HashMap;

#[derive(Debug)]
pub enum CompilerError<'a> {
    InvalidOperation(ast::Operation),
    InvalidOperands(ast::Operands<'a>),
}

pub struct Compiler<'a> {
    lines: Vec<ast::Line<'a>>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct Output<'a, 'b> {
    address: u32,
    bytes: Vec<u8>,
    line: &'b ast::Line<'a>,
}

fn encode_instruction<'a>(
    instruction: &ast::Instruction<'a>,
) -> Result<Vec<u8>, CompilerError<'a>> {
    match instruction.operation {
        ast::Operation::STI => Ok(vec![]),
        ast::Operation::JMP => match &instruction.operands {
            // Operands::Destination(destination) => Ok(vec![]),
            _ => Err(CompilerError::InvalidOperands(instruction.operands.clone())),
        },
        _ => Err(CompilerError::InvalidOperation(instruction.operation)),
    }
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
                line @ ast::Line::Instruction(instruction) => outputs.push(Output {
                    address: 0,
                    bytes: encode_instruction(instruction)?,
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
