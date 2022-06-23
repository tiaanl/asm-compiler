#![allow(unused)]

use crate::ast;
use crate::ast::{Expression, Line, Operands, Operator, Value};
use crate::encoder::EncodeError;
use crate::parser::LineConsumer;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum CompilerError {
    EncodeError(EncodeError),
    UnknownLabel(String),
}

impl From<EncodeError> for CompilerError {
    fn from(other: EncodeError) -> Self {
        Self::EncodeError(other)
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EncodeError(err) => {
                write!(f, "{}", err)
            }
            Self::UnknownLabel(label) => {
                write!(f, "Unknown label: {}", label)
            }
        }
    }
}

impl CompilerError {
    pub fn span(&self) -> &ast::Span {
        match self {
            CompilerError::EncodeError(err) => err.span(),
            CompilerError::UnknownLabel(_) => &(0..0),
        }
    }
}

#[derive(Default)]
pub struct CompilerSession<'a> {
    pub lines: Vec<ast::Line>,
    pub labels: HashMap<&'a str, usize>,
}

#[derive(Debug, Default)]
pub struct Compiler {
    lines: Vec<ast::Line>,
    labels: HashMap<String, usize>,
}

impl<'a> LineConsumer<'a> for Compiler {
    fn consume(&mut self, line: Line) {
        self.lines.push(line);
    }
}

#[allow(dead_code)]
#[derive(Debug)]
struct Output {
    address: u32,
    bytes: Vec<u8>,
    line_num: usize,
}

fn encode_instruction(
    _span: ast::Span,
    instruction: &ast::Instruction,
) -> Result<Vec<u8>, CompilerError> {
    match crate::encoder::encode(instruction) {
        Ok(bytes) => Ok(bytes),
        Err(err) => Err(CompilerError::EncodeError(err)),
    }
}

impl Compiler {
    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let mut outputs: Vec<Output> = vec![];

        let mut address = 0;
        for (line_num, line) in self.lines.iter().enumerate() {
            match line {
                ast::Line::Label(_, label) => {
                    self.labels.insert(label.clone(), outputs.len());
                }

                ast::Line::Instruction(ref span, instruction) => outputs.push(Output {
                    address,
                    bytes: vec![],
                    line_num,
                }),

                line @ ast::Line::Data(ref span, data) => outputs.push(Output {
                    address,
                    bytes: data.clone(),
                    line_num,
                }),

                ast::Line::Constant(_, _) => {}

                line @ ast::Line::Times(_) => outputs.push(Output {
                    address,
                    bytes: vec![],
                    line_num,
                }),
            }

            //address += line.size_in_bytes()?;
        }

        let mut lines = unsafe {
            &mut *std::ptr::slice_from_raw_parts_mut(self.lines.as_mut_ptr(), self.lines.len())
        };

        for (line_num, line) in lines.iter_mut().enumerate() {
            if let ast::Line::Instruction(_, ast::Instruction { operands, .. }) = line {
                // Process the line.
            }
        }

        Ok(())
    }
}
