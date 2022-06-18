use crate::ast;
use crate::ast::Line;
use crate::encoder::EncodeError;
use crate::lexer::Span;
use crate::parser::LineConsumer;
use std::collections::HashMap;
use std::fmt::Formatter;

impl<'a> ast::Line<'a> {
    fn size_in_bytes(&self) -> Result<u32, EncodeError> {
        match self {
            ast::Line::Instruction(_, instruction) => crate::encoder::size_in_bytes(instruction),
            ast::Line::Data(data) => Ok(data.len() as u32),
            ast::Line::Times(_) => todo!(),

            // Other line types does not take up any bytes.
            _ => Ok(0),
        }
    }
}

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
    pub fn span(&self) -> &Span {
        match self {
            CompilerError::EncodeError(err) => err.span(),
            CompilerError::UnknownLabel(_) => &(0..0),
        }
    }
}

#[derive(Default)]
pub struct CompilerSession<'a> {
    pub lines: Vec<ast::Line<'a>>,
    pub labels: HashMap<&'a str, usize>,
}

#[derive(Debug)]
pub struct Compiler<'a> {
    lines: Vec<ast::Line<'a>>,
    labels: HashMap<&'a str, usize>,
}

impl<'a> LineConsumer<'a> for Compiler<'a> {
    fn consume(&mut self, line: Line<'a>) {
        self.lines.push(line);
    }
}

#[allow(dead_code)]
#[derive(Debug)]
struct Output<'a, 'b> {
    address: u32,
    bytes: Vec<u8>,
    line: &'b ast::Line<'a>,
}

fn encode_instruction(
    _span: Span,
    instruction: &ast::Instruction,
) -> Result<Vec<u8>, CompilerError> {
    match crate::encoder::encode(instruction) {
        Ok(bytes) => Ok(bytes),
        Err(err) => Err(CompilerError::EncodeError(err)),
    }
}

impl<'a> Compiler<'a> {
    pub fn new(lines: Vec<ast::Line<'a>>) -> Self {
        Self {
            lines,
            labels: HashMap::new(),
        }
    }

    pub fn compile(&mut self) -> Result<Vec<u8>, CompilerError> {
        let mut outputs: Vec<Output<'a, '_>> = vec![];

        let mut address = 0;
        for line in &self.lines {
            match line {
                ast::Line::Label(_, label) => {
                    self.labels.insert(label, outputs.len());
                }
                ast::Line::Instruction(ref span, instruction) => outputs.push(Output {
                    address,
                    bytes: vec![],
                    line,
                }),
                line @ ast::Line::Data(data) => outputs.push(Output {
                    address,
                    bytes: data.clone(),
                    line,
                }),
                ast::Line::Constant(_) => {}
                line @ ast::Line::Times(_) => outputs.push(Output {
                    address,
                    bytes: vec![],
                    line,
                }),
            }
            address += line.size_in_bytes()?;
        }

        Ok(vec![])
    }

    fn evaluate_instruction(
        &self,
        instruction: &mut ast::Instruction,
    ) -> Result<(), CompilerError> {
        self.evaluate_operands(&mut instruction.operands)
    }

    fn evaluate_operands(&self, operands: &mut ast::Operands) -> Result<(), CompilerError> {
        match operands {
            ast::Operands::None(_) => Ok(()),
            ast::Operands::Destination(span, destination) => self.evaluate_operand(destination),
            ast::Operands::DestinationAndSource(span, destination, source) => {
                self.evaluate_operand(source)?;
                self.evaluate_operand(destination)?;
                Ok(())
            }
        }
    }

    fn evaluate_operand(&self, operand: &mut ast::Operand) -> Result<(), CompilerError> {
        match operand {
            ast::Operand::Immediate(expr) => self.evaluate_expression(expr),
            ast::Operand::Address(_, expr, _) => self.evaluate_expression(expr),
            _ => Ok(()),
        }
    }

    fn evaluate_expression(&self, expression: &mut ast::Expression) -> Result<(), CompilerError> {
        match expression {
            _ => todo!(),
        }
    }
}
