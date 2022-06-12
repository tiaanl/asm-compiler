use crate::ast;
use crate::encoder::EncodeError;
use crate::lexer::Span;
use std::collections::HashMap;
use std::fmt::Formatter;

impl<'a> ast::Line<'a> {
    fn size_in_bytes(&self) -> Result<u32, crate::encoder::EncodeError> {
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
        }
    }
}

impl CompilerError {
    pub fn span(&self) -> &Span {
        match self {
            CompilerError::EncodeError(err) => err.span(),
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
        Self { lines }
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let mut outputs: Vec<Output<'a, '_>> = vec![];
        let mut labels: HashMap<&'a str, usize> = HashMap::new();

        let mut address = 0;
        for line in &self.lines {
            match line {
                ast::Line::Label(_, label) => {
                    labels.insert(label, outputs.len());
                }
                line @ ast::Line::Instruction(ref span, instruction) => outputs.push(Output {
                    address,
                    bytes: encode_instruction(span.start..span.end, instruction)?,
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

        for output in &outputs {
            println!("{:?}", output);
        }

        Ok(())
    }
}
