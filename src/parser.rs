use crate::{
    ast,
    lexer::{Lexer, LexerError, LiteralKind, PunctuationKind, Token},
};

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    Expected(usize, &'static str),
}

impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> Self {
        ParserError::LexerError(err)
    }
}

#[derive(Debug)]
pub enum Data {
    Byte(Vec<u8>),
    Word(Vec<u8>),
    DoubleWord(Vec<u8>),
}

#[derive(Debug)]
pub enum Line<'a> {
    Label(&'a str),
    Equ(i32),
    Instruction(ast::Instruction<'a>),
    Data(Data),
    EndOfFile,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    /// The current token we are working on.
    token: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            token: Token::EndOfFile,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Line<'a>>, ParserError> {
        self.next_token()?;

        let mut lines = vec![];

        loop {
            // Skip blank lines.
            while let Token::NewLine = self.token {
                self.next_token()?;
            }

            match self.parse_line()? {
                Line::EndOfFile => break,

                l => {
                    lines.push(l);
                }
            }
        }

        Ok(lines)
    }

    fn next_token(&mut self) -> Result<(), LexerError> {
        loop {
            match self.lexer.next_token()? {
                Token::Comment(_) | Token::Whitespace => {
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

    // fn peek_token(&self) -> Result<Token<'a>, LexerError> {
    //     let mut lexer = self.lexer;
    //     loop {
    //         match lexer.next_token() {
    //             Ok(Token::Whitespace) | Ok(Token::Comment(_)) => continue,
    //             r => return r,
    //         }
    //     }
    // }

    /// The current token should be a new line and if it is, consume it.
    fn expect_new_line(&mut self) -> Result<(), ParserError> {
        if let Token::NewLine | Token::EndOfFile = self.token {
            Ok(())
        } else {
            Err(ParserError::Expected(self.lexer.pos(), "Expected new line"))
        }
    }

    fn parse_line(&mut self) -> Result<Line<'a>, ParserError> {
        match self.token {
            Token::Identifier(identifier) => {
                if let Some(operation) = ast::Operation::from_str(identifier) {
                    self.next_token()?;
                    let instruction = self.parse_instruction(operation)?;
                    Ok(Line::Instruction(instruction))
                } else {
                    match identifier.to_lowercase().as_str() {
                        "equ" => {
                            self.next_token()?;
                            self.parse_equ()
                        }

                        "db" => {
                            self.next_token()?;
                            self.parse_data::<u8>()
                        }

                        "dw" => {
                            self.next_token()?;
                            self.parse_data::<u16>()
                        }

                        "dd" => {
                            self.next_token()?;
                            self.parse_data::<u32>()
                        }

                        _ => {
                            self.next_token()?;
                            self.parse_label(identifier)
                        }
                    }
                }
            }

            Token::EndOfFile => Ok(Line::EndOfFile),

            _ => Err(ParserError::Expected(
                self.lexer.pos(),
                "Identifier expected to start a new line",
            )),
        }
    }

    fn parse_label(&mut self, name: &'a str) -> Result<Line<'a>, ParserError> {
        // Skip the optional colon after a label.
        if matches!(self.token, Token::Punctuation(PunctuationKind::Colon)) {
            self.next_token()?;
        }

        Ok(Line::Label(name))
    }

    fn parse_instruction(
        &mut self,
        operation: ast::Operation,
    ) -> Result<ast::Instruction<'a>, ParserError> {
        let operands = self.parse_operands()?;

        self.expect_new_line()?;

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

                _ => Err(ParserError::Expected(self.lexer.pos(), "something here?")),
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

            _ => Err(ParserError::Expected(self.lexer.pos(), "operand expected")),
        }
    }

    fn parse_memory_operand(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand<'a>, ParserError> {
        if !matches!(self.token, Token::Punctuation(PunctuationKind::OpenBracket)) {
            return Err(ParserError::Expected(
                self.lexer.pos(),
                "opening bracket for direct memory address",
            ));
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
                        return Err(ParserError::Expected(
                            self.lexer.pos(),
                            "colon after segment override",
                        ));
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
                    return Err(ParserError::Expected(
                        self.lexer.pos(),
                        "closing bracket for memory address",
                    ));
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

    fn parse_equ(&mut self) -> Result<Line<'a>, ParserError> {
        let value = match self.token {
            Token::Literal(LiteralKind::Number(number)) => {
                self.next_token()?;
                number
            }

            _ => {
                return Err(ParserError::Expected(
                    self.lexer.pos(),
                    "constant value expected.",
                ))
            }
        };

        self.expect_new_line()?;

        Ok(Line::Equ(value))
    }

    fn parse_data<T>(&mut self) -> Result<Line<'a>, ParserError> {
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

        self.expect_new_line()?;

        match std::mem::size_of::<T>() {
            1 => Ok(Line::Data(Data::Byte(data as Vec<u8>))),
            2 => Ok(Line::Data(Data::Word(data as Vec<u8>))),
            4 => Ok(Line::Data(Data::DoubleWord(data as Vec<u8>))),
            _ => unreachable!("Invalid data size"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blank_lines() {
        Parser::new("").parse();
    }
}
