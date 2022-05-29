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

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    /// The current token we are working on.
    token: Token<'a>,

    instructions: Vec<ast::Instruction<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            token: Token::EndOfFile,
            instructions: vec![],
        }
    }

    pub fn parse(&mut self) -> Result<(), ParserError> {
        self.next_token()?;

        loop {
            if !self.parse_line()? {
                break;
            }
        }

        Ok(())
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

    fn peek_token(&self) -> Result<Token<'a>, LexerError> {
        let mut lexer = self.lexer;
        loop {
            match lexer.next_token() {
                Ok(Token::Whitespace) | Ok(Token::Comment(_)) => continue,
                r => return r,
            }
        }
    }

    /// The current token should be a new line and if it is, consume it.
    fn expect_new_line(&mut self) -> Result<(), ParserError> {
        if let Token::NewLine | Token::EndOfFile = self.token {
            Ok(())
        } else {
            Err(ParserError::Expected(self.lexer.pos(), "Expected new line"))
        }
    }

    fn parse_line(&mut self) -> Result<bool, ParserError> {
        match self.token {
            Token::NewLine => {
                // We encountered a blank line or a line that only contained a
                // comment, so we just continue parsing.
                self.next_token()?;
                Ok(true)
            }

            Token::Identifier(identifier) => {
                if let Some(operation) = ast::Operation::from_str(identifier) {
                    self.next_token()?;
                    let instruction = self.parse_instruction(operation)?;
                    self.instructions.push(instruction);
                    Ok(true)
                } else {
                    // We found an identifier, so let's check the following token
                    // to determine if it is a label, mnemonic, etc.
                    match self.peek_token()? {
                        Token::Punctuation(PunctuationKind::Colon) => {
                            // If we encounter a label, then we add the label to the parser and
                            // move onto the next token.
                            self.parse_label()?;
                            Ok(true)
                        }

                        Token::Identifier(identifier) => match identifier.to_lowercase().as_str() {
                            "equ" => {
                                self.parse_equ()?;
                                Ok(true)
                            }

                            "db" => {
                                self.parse_data_bytes()?;
                                Ok(true)
                            }

                            "dw" => {
                                self.parse_data_words()?;
                                Ok(true)
                            }

                            "dd" => {
                                self.parse_data_double_words()?;
                                Ok(true)
                            }

                            _ => Err(ParserError::Expected(
                                self.lexer.pos(),
                                "equ, db, dw expected",
                            )),
                        },

                        _ => Err(ParserError::Expected(
                            self.lexer.pos(),
                            "label or instruction expected",
                        )),
                    }
                }
            }

            Token::EndOfFile => Ok(false),

            _ => Err(ParserError::Expected(
                self.lexer.pos(),
                "Identifier expected to start a new line",
            )),
        }
    }

    fn add_label(&mut self, name: &'a str) {
        println!("Adding label: {}", name);
    }

    fn parse_label(&mut self) -> Result<(), ParserError> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));

        if let Token::Identifier(identifier) = self.token {
            self.add_label(identifier);
        }
        // Consume the identifer and the ':'.
        self.next_token()?;
        self.next_token()?;

        Ok(())
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

    fn parse_equ(&mut self) -> Result<(), ParserError> {
        let name = self.token;

        debug_assert!(matches!(self.token, Token::Identifier(_)));
        self.next_token()?;

        debug_assert!(matches!(self.token, Token::Identifier("equ")));
        self.next_token()?;

        match self.token {
            Token::Literal(LiteralKind::Number(number)) => {
                println!("EQU: {:?} = {}", name, number);
                self.next_token()?;
            }
            _ => todo!(),
        }

        self.expect_new_line()?;

        Ok(())
    }

    fn parse_data_bytes(&mut self) -> Result<(), ParserError> {
        let name = self.token;
        self.next_token()?;

        debug_assert!(matches!(self.token, Token::Identifier(_)));
        self.next_token()?;

        match self.token {
            Token::Literal(LiteralKind::Number(value)) => {
                self.next_token()?;
                println!("data bytes: {:?} {}", name, value);
            }

            _ => {
                return Err(ParserError::Expected(
                    self.lexer.pos(),
                    "constant byte value",
                ))
            }
        }

        self.expect_new_line()?;

        Ok(())
    }

    fn parse_data_words(&mut self) -> Result<(), ParserError> {
        let name = self.token;
        self.next_token()?;

        debug_assert!(matches!(self.token, Token::Identifier(_)));
        self.next_token()?;

        match self.token {
            Token::Literal(LiteralKind::Number(value)) => {
                self.next_token()?;
                println!("data bytes: {:?} {}", name, value);
            }

            _ => {
                return Err(ParserError::Expected(
                    self.lexer.pos(),
                    "constant byte value",
                ))
            }
        }

        self.expect_new_line()?;

        Ok(())
    }

    fn parse_data_double_words(&mut self) -> Result<(), ParserError> {
        let name = self.token;
        self.next_token()?;

        debug_assert!(matches!(self.token, Token::Identifier(_)));
        self.next_token()?;

        match self.token {
            Token::Literal(LiteralKind::Number(value)) => {
                self.next_token()?;
                println!("data bytes: {:?} {}", name, value);
            }

            _ => {
                return Err(ParserError::Expected(
                    self.lexer.pos(),
                    "constant byte value",
                ))
            }
        }

        self.expect_new_line()?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instruction_none() {
        assert_eq!(
            Parser::new("hlt ; no operands")
                .parse_instruction()
                .unwrap(),
            ast::Instruction {
                mnemonic: "hlt",
                operands: ast::Operands::None,
            }
        );
    }

    #[test]
    fn instruction_destination() {
        assert_eq!(
            Parser::new("int 0x21 ; dos interrupt")
                .parse_instruction()
                .unwrap(),
            ast::Instruction {
                mnemonic: "int",
                operands: ast::Operands::Destination(ast::Operand::Immediate(33)),
            }
        );
    }

    #[test]
    fn instruction_destination_and_source() {
        assert_eq!(
            Parser::new("out 0xA0, 0x00 ; write to some port")
                .parse_instruction()
                .unwrap(),
            ast::Instruction {
                mnemonic: "out",
                operands: ast::Operands::DestinationAndSource(
                    ast::Operand::Immediate(160),
                    ast::Operand::Immediate(0)
                ),
            }
        );
    }
}
