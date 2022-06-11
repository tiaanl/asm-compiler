use crate::ast;
use crate::lexer::Span;
use std::collections::HashMap;

#[derive(Debug)]
pub enum CompilerError<'a> {
    InvalidOperation(Span, ast::Operation),
    InvalidOperands(Span, ast::Operands<'a>),
}

impl<'a> CompilerError<'a> {
    pub fn span(&self) -> &Span {
        match self {
            CompilerError::InvalidOperation(span, _) | CompilerError::InvalidOperands(span, _) => {
                span
            }
        }
    }
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
    span: Span,
    instruction: &ast::Instruction<'a>,
) -> Result<Vec<u8>, CompilerError<'a>> {
    match instruction.operation {
        ast::Operation::STI => Ok(vec![]),
        ast::Operation::JMP => match &instruction.operands {
            ast::Operands::Destination(ref span, _) => Err(CompilerError::InvalidOperands(
                span.start..span.end,
                instruction.operands.clone(),
            )),
            _ => Err(CompilerError::InvalidOperands(
                span,
                instruction.operands.clone(),
            )),
        },
        _ => Err(CompilerError::InvalidOperation(0..0, instruction.operation)),
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
                ast::Line::Label(_, label) => {
                    labels.insert(label, outputs.len());
                }
                line @ ast::Line::Instruction(ref span, instruction) => outputs.push(Output {
                    address: 0,
                    bytes: encode_instruction(span.start..span.end, instruction)?,
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
