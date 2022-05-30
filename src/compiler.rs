use crate::parser::{Data, Line};
use std::collections::HashMap;

#[derive(Default)]
struct Compiler<'a> {
    bin: Vec<Vec<u8>>,
    labels: HashMap<&'a str, usize>,
}

impl<'a> Compiler<'a> {
    fn compile_lines(&mut self, lines: &[Line<'a>]) {
        for line in lines {
            match line {
                Line::Label(label) => {
                    self.labels.insert(label, self.bin.len());
                }

                Line::Data(data) => match data {
                    Data::Byte(d) | Data::Word(d) | Data::DoubleWord(d) => self.bin.push(d.clone()),
                },

                line => {
                    dbg!(line);
                    todo!()
                }
            }
        }
    }
}

pub fn compile<'a>(lines: &[Line<'a>]) -> Vec<Vec<u8>> {
    let mut compiler = Compiler::default();

    compiler.compile_lines(lines);

    todo!()
}
