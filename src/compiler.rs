use crate::ast;
use crate::lexer::Span;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum CompilerError {
    EncodeError(Span, crate::encoder::EncodeError),
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EncodeError(_, err) => {
                write!(f, "{}", err)
            }
        }
    }
}

impl CompilerError {
    pub fn span(&self) -> &Span {
        match self {
            CompilerError::EncodeError(span, _) => span,
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
    span: Span,
    instruction: &ast::Instruction,
) -> Result<Vec<u8>, CompilerError> {
    match crate::encoder::encode(instruction) {
        Ok(bytes) => Ok(bytes),
        Err(err) => Err(CompilerError::EncodeError(span, err)),
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
