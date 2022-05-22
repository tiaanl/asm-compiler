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
}

enum Line<'a> {
    Instruction(ast::Instruction<'a>),
    EnfOfFile,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            token: Token::EndOfFile,
        }
    }

    fn skip_comments_and_new_lines(&mut self) -> Result<(), ParserError> {
        loop {
            if matches!(self.token, Token::NewLine | Token::Comment(_)) {
                self.next_token()?;
            } else {
                break;
            }
        }
        Ok(())
    }

    pub fn parse(&mut self) -> Result<ast::Block, ParserError> {
        let mut block = ast::Block {
            origin: 0,
            instructions: vec![],
        };

        self.next_token()?;
        self.skip_comments_and_new_lines()?;

        loop {
            match self.parse_line()? {
                Line::Instruction(instruction) => block.instructions.push(dbg!(instruction)),
                Line::EnfOfFile => break,
            }
        }

        Ok(block)
    }

    fn next_token(&mut self) -> Result<(), LexerError> {
        match self.lexer.next_token() {
            Ok(token) => {
                // self.token = dbg!(token);
                self.token = token;
                Ok(())
            }
            Err(err) => Err(err),
        }
    }

    fn parse_line(&mut self) -> Result<Line<'a>, ParserError> {
        Ok(match self.token {
            Token::EndOfFile => Line::EnfOfFile,
            _ => Line::Instruction(self.parse_instruction()?),
        })
    }

    fn parse_instruction(&mut self) -> Result<ast::Instruction<'a>, ParserError> {
        match self.token {
            Token::Identifier(identifier) => {
                let mnemonic = identifier;

                // Found a mnemonic, so move to the next token.
                self.next_token()?;

                let operands = self.parse_operands()?;

                self.skip_comments_and_new_lines()?;

                Ok(ast::Instruction { mnemonic, operands })
            }

            _ => Err(ParserError::Expected(self.lexer.pos(), "mnemonic")),
        }
    }

    fn parse_operands(&mut self) -> Result<ast::Operands, ParserError> {
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

    fn parse_operand(&mut self) -> Result<ast::Operand, ParserError> {
        match self.token {
            Token::Literal(LiteralKind::Number(number)) => {
                self.next_token()?;
                Ok(ast::Operand::Immediate(number))
            }

            _ => Err(ParserError::Expected(self.lexer.pos(), "operand")),
        }
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
