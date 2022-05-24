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
    lexer: crate::lexer::Lexer<'a>,

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

        println!("next token: {:?}", self.token);

        Ok(())
    }

    fn peek_token(&self) -> Result<Token<'a>, LexerError> {
        let mut lexer = self.lexer;
        lexer.next_token()
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
                return Ok(true);
            }

            Token::Identifier(_) => {
                // We found an identifier, so let's check the following token
                // to determine if it is a label, mnemonic, etc.
                match self.peek_token()? {
                    Token::Punctuation(PunctuationKind::Colon) => {
                        // If we encounter a label, then we add the label to the parser and
                        // move onto the next token.
                        self.parse_label()?;
                        return Ok(true);
                    }

                    Token::Identifier(identifier) if identifier == "equ" => {
                        self.parse_equ()?;
                        return Ok(true);
                    }

                    _ => {}
                }
            }

            Token::EndOfFile => return Ok(false),

            _ => {
                return Err(ParserError::Expected(
                    self.lexer.pos(),
                    "Identifier expected to start a new line",
                ))
            }
        }

        let instruction = self.parse_instruction()?;
        self.instructions.push(instruction);

        Ok(true)
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

    fn parse_instruction(&mut self) -> Result<ast::Instruction<'a>, ParserError> {
        match self.token {
            Token::Identifier(identifier) => {
                let mnemonic = identifier;

                // Found a mnemonic, so move to the next token.
                self.next_token()?;

                let operands = self.parse_operands()?;

                self.expect_new_line()?;

                Ok(ast::Instruction { mnemonic, operands })
            }

            _ => Err(ParserError::Expected(self.lexer.pos(), "mnemonic")),
        }
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
                Token::Comment(_) | Token::NewLine | Token::EndOfFile => {
                    Ok(ast::Operands::Destination(destination))
                }
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
                if identifier == "byte" || identifier == "word" {
                    self.parse_memory_operand()
                } else {
                    self.next_token()?;
                    Ok(ast::Operand::Register(identifier))
                }
            }

            Token::Punctuation(PunctuationKind::OpenBracket) => self.parse_memory_operand(),

            _ => Err(ParserError::Expected(self.lexer.pos(), "operand")),
        }
    }

    fn parse_memory_operand(&mut self) -> Result<ast::Operand<'a>, ParserError> {
        let data_size = if let Token::Identifier(identifer) = self.token {
            self.next_token()?;

            Some(match identifer {
                "byte" => ast::DataSize::Byte,
                "word" => ast::DataSize::Word,
                _ => return Err(ParserError::Expected(self.lexer.pos(), "data size keyword")),
            })
        } else {
            None
        };

        if !matches!(self.token, Token::Punctuation(PunctuationKind::OpenBracket)) {
            return Err(ParserError::Expected(
                self.lexer.pos(),
                "opening bracket for direct memory address",
            ));
        }

        self.next_token()?;

        let address_or_label = match self.token {
            Token::Identifier(identifier) => {
                self.next_token()?;
                ast::AddressOrLabel::Label(identifier)
            }
            _ => todo!(),
        };

        if !matches!(
            self.token,
            Token::Punctuation(PunctuationKind::CloseBracket)
        ) {
            return Err(ParserError::Expected(
                self.lexer.pos(),
                "closing bracket for memory address",
            ));
        }

        self.next_token()?;

        Ok(ast::Operand::DirectAddress(data_size, address_or_label))
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

        println!("equ: {:?}", name);

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
