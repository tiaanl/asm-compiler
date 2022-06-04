use crate::{
    ast,
    lexer::{Lexer, LexerError, LiteralKind, PunctuationKind, Token},
};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    Expected(usize, String),
}

impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> Self {
        ParserError::LexerError(err)
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    last_lexer_pos: usize,

    /// The current token we are working on.
    token: Token<'a>,

    block: ast::Block<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            last_lexer_pos: 0,
            token: Token::EndOfFile,
            block: ast::Block {
                lines: Vec::new(),
                labels: HashMap::new(),
            },
        }
    }

    pub fn parse(&mut self) -> Result<(), ParserError> {
        self.next_token()?;

        loop {
            // Skip blank lines.
            while let Token::NewLine = self.token {
                self.next_token()?;
            }

            if !self.parse_line()? {
                break;
            }
        }

        Ok(())
    }

    fn next_token(&mut self) -> Result<(), LexerError> {
        self.last_lexer_pos = self.lexer.pos();

        loop {
            match self.lexer.next_token()? {
                Token::Comment(_) | Token::Whitespace => {
                    self.last_lexer_pos = self.lexer.pos();
                    continue;
                }
                token => {
                    self.token = token;
                    break;
                }
            }
        }

        // match &self.token {
        //     Token::NewLine => {}
        //     _ => println!("next token: {:?}", self.token),
        // }

        Ok(())
    }

    /// The current token is required to be a new line.  If it is, then consume it, otherwise we
    /// report an error.
    fn require_new_line(&mut self) -> Result<(), ParserError> {
        if let Token::NewLine = self.token {
            self.next_token()?;
            Ok(())
        } else if let Token::EndOfFile = self.token {
            Ok(())
        } else {
            Err(self.expected("Expected new line".to_owned()))
        }
    }

    fn parse_line(&mut self) -> Result<bool, ParserError> {
        println!(
            "parse_line:\n{}",
            self.lexer
                .source_line_at_pos(self.last_lexer_pos, Some("mem"))
        );

        loop {
            match self.token {
                Token::Identifier(identifier) => {
                    if let Some(operation) = ast::Operation::from_str(identifier) {
                        self.next_token()?;
                        let instruction = self.parse_instruction(operation)?;
                        self.block.lines.push(ast::Line::Instruction(instruction));
                        return Ok(true);
                    } else {
                        match identifier.to_lowercase().as_str() {
                            "equ" => {
                                self.next_token()?;
                                self.parse_equ()?;
                            }

                            "db" => {
                                self.next_token()?;
                                let line = self.parse_data::<u8>()?;
                                self.block.lines.push(line);
                                return Ok(true);
                            }

                            "dw" => {
                                self.next_token()?;
                                let line = self.parse_data::<u16>()?;
                                self.block.lines.push(line);
                                return Ok(true);
                            }

                            "dd" => {
                                self.next_token()?;
                                let line = self.parse_data::<u32>()?;
                                self.block.lines.push(line);
                                return Ok(true);
                            }

                            _ => {
                                // Consume the token that holds the label.
                                self.next_token()?;

                                let label = self.parse_label(identifier)?;
                                self.block.labels.insert(label, self.block.lines.len());

                                return Ok(true);
                            }
                        }
                    }
                }

                Token::EndOfFile => return Ok(false),

                _ => {
                    return Err(self.expected(format!(
                        "Identifier expected at the start of a new line. Found {:?}\n{}",
                        self.token,
                        self.lexer.source_line_current(Some("mem"))
                    )))
                }
            }
        }
    }

    fn parse_label(&mut self, name: &'a str) -> Result<&'a str, ParserError> {
        // Skip the optional colon after a label.
        if matches!(self.token, Token::Punctuation(PunctuationKind::Colon)) {
            self.next_token()?;

            // If the token after the ":" is a new_line, then we should consume it as well.
            if matches!(self.token, Token::NewLine) {
                self.next_token()?;
            }
        }

        Ok(name)
    }

    fn parse_instruction(
        &mut self,
        operation: ast::Operation,
    ) -> Result<ast::Instruction<'a>, ParserError> {
        let operands = self.parse_operands()?;

        self.require_new_line()?;

        Ok(ast::Instruction {
            operation,
            operands,
        })
    }

    fn parse_operands(&mut self) -> Result<ast::Operands<'a>, ParserError> {
        if matches!(self.token, Token::Comment(_)) {
            self.next_token()?;
        }

        if matches!(self.token, Token::NewLine | Token::EndOfFile) {
            Ok(ast::Operands::None)
        } else {
            let destination = self.parse_operand()?;

            match self.token {
                Token::NewLine | Token::EndOfFile => Ok(ast::Operands::Destination(destination)),
                Token::Punctuation(PunctuationKind::Comma) => {
                    self.next_token()?;
                    let source = self.parse_operand()?;

                    Ok(ast::Operands::DestinationAndSource(destination, source))
                }

                _ => {
                    let source = self.lexer.source_line_current(Some("mem"));
                    println!("{}", source);
                    Err(self.expected(format!("An unexpected token was encountered: {:?}", source)))
                }
            }
        }
    }

    fn parse_operand(&mut self) -> Result<ast::Operand<'a>, ParserError> {
        match self.token {
            Token::Literal(LiteralKind::Number(number)) => {
                self.next_token()?;
                Ok(ast::Operand::Immediate(number))
            }

            Token::Identifier(identifier) => {
                if let Some(data_size) = ast::DataSize::from_str(identifier) {
                    // Consume the data size token.
                    self.next_token()?;
                    self.parse_memory_operand(Some(data_size))
                } else {
                    self.next_token()?;
                    Ok(ast::Operand::Register(identifier))
                }
            }

            Token::Punctuation(PunctuationKind::OpenBracket) => self.parse_memory_operand(None),

            _ => Err(self.expected("operand expected".to_owned())),
        }
    }

    fn parse_memory_operand(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand<'a>, ParserError> {
        if !matches!(self.token, Token::Punctuation(PunctuationKind::OpenBracket)) {
            return Err(self.expected("opening bracket for direct memory address".to_owned()));
        }

        self.next_token()?;

        let mut segment_override = None;

        let address_or_label = match self.token {
            Token::Identifier(identifier) => {
                // If the first identifier is a segment, then we have an override.
                if let Some(segment) = ast::Segment::from_str(identifier) {
                    segment_override = Some(segment);
                    self.next_token()?;

                    if matches!(self.token, Token::Punctuation(PunctuationKind::Colon)) {
                        self.next_token()?;
                    } else {
                        return Err(self.expected("colon after segment override".to_owned()));
                    }
                }

                self.next_token()?;
                ast::AddressOrLabel::Label(identifier)
            }
            _ => todo!(),
        };

        loop {
            match self.token {
                Token::Punctuation(PunctuationKind::CloseBracket) => break,

                Token::Punctuation(PunctuationKind::Plus)
                | Token::Punctuation(PunctuationKind::Minus) => {
                    self.next_token()?;
                    self.next_token()?;
                }

                _ => {
                    return Err(self.expected("closing bracket for memory address".to_owned()));
                }
            }
        }

        self.next_token()?;

        Ok(ast::Operand::DirectAddress(
            data_size,
            address_or_label,
            segment_override,
            0,
        ))
    }

    fn parse_equ(&mut self) -> Result<(), ParserError> {
        let value = match self.token {
            Token::Literal(LiteralKind::Number(number)) => {
                self.next_token()?;
                number
            }

            _ => return Err(self.expected("Constant value expected.".to_owned())),
        };

        self.require_new_line()?;

        self.block.lines.push(ast::Line::Constant(value));

        Ok(())
    }

    fn parse_data<T>(&mut self) -> Result<ast::Line<'a>, ParserError> {
        let mut data = Vec::<u8>::new();

        loop {
            match self.token {
                Token::Punctuation(PunctuationKind::Comma) => {
                    self.next_token()?;
                    continue;
                }

                Token::Literal(LiteralKind::String(s)) => {
                    self.next_token()?;
                    for b in s.as_bytes() {
                        data.push(*b);
                    }
                }

                Token::Literal(LiteralKind::Number(number)) => {
                    self.next_token()?;
                    for b in number.to_le_bytes() {
                        data.push(b);
                    }
                }

                _ => {
                    break;
                }
            }
        }

        self.require_new_line()?;

        match std::mem::size_of::<T>() {
            1 => Ok(ast::Line::Data(ast::Data::Byte(data as Vec<u8>))),
            2 => Ok(ast::Line::Data(ast::Data::Word(data as Vec<u8>))),
            4 => Ok(ast::Line::Data(ast::Data::DoubleWord(data as Vec<u8>))),
            _ => unreachable!("Invalid data size"),
        }
    }

    fn expected(&self, message: String) -> ParserError {
        ParserError::Expected(self.last_lexer_pos, message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn parse(input: &str) -> Result<ast::Block, ParserError> {
        let mut parser = Parser::new(input);
        parser.parse()?;
        Ok(parser.block)
    }

    #[test]
    fn blank_lines() {
        let ast::Block { lines, .. } = parse("").unwrap();
        assert_eq!(lines, vec![]);

        let ast::Block { lines, .. } = parse("\n\n").unwrap();
        assert_eq!(lines, vec![]);
    }

    #[test]
    fn label_and_instruction() {
        let ast::Block { lines, labels } = parse("start hlt").unwrap();
        assert_eq!(
            lines,
            vec![ast::Line::Instruction(ast::Instruction {
                operation: ast::Operation::HLT,
                operands: ast::Operands::None,
            })]
        );
        assert_eq!(labels, HashMap::from([("start", 0)]),);
    }

    #[test]
    fn multiple_labels() {
        let ast::Block { lines, labels } = parse("start begin: begin2:hlt").unwrap();
        assert_eq!(
            lines,
            vec![ast::Line::Instruction(ast::Instruction {
                operation: ast::Operation::HLT,
                operands: ast::Operands::None,
            })]
        );
        assert_eq!(
            labels,
            HashMap::from([("start", 0), ("begin", 0), ("begin2", 0)]),
        );
    }

    #[test]
    fn constants() {
        let block = parse("label equ 42").unwrap();
        assert_eq!(block.lines, vec![ast::Line::Constant(42)]);
        assert_eq!(block.labels, HashMap::from([("label", 0)]));
    }
}
