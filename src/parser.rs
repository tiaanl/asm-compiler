use crate::ast;
use crate::lexer::{Lexer, LiteralKind, PunctuationKind, Token};

#[derive(Debug)]
pub enum ParserError {
    Expected(usize, String),
}

pub fn parse(source: &str) -> Result<Vec<ast::Line>, ParserError> {
    let mut parser = Parser::new(source);
    let _ = parser.parse()?;
    Ok(parser.lines)
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    token_pos: usize,

    /// The current token we are working on.
    token: Token<'a>,

    lines: Vec<ast::Line<'a>>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            token_pos: 0,
            token: Token::EndOfFile,
            lines: vec![],
        }
    }

    fn parse(&mut self) -> Result<(), ParserError> {
        // Pull in the first token from the lexer.
        self.next_token();

        loop {
            match self.token {
                Token::NewLine => {
                    // If the line starts with a new line, we just skip it.
                    self.next_token();
                }

                Token::Identifier(identifier) => {
                    if let Some(operation) = ast::Operation::from_str(identifier) {
                        self.next_token();
                        let instruction = self.parse_instruction(operation)?;
                        self.lines.push(ast::Line::Instruction(instruction));
                        continue;
                    } else {
                        match identifier.to_lowercase().as_str() {
                            "equ" => {
                                let line = self.parse_equ()?;
                                self.lines.push(line);
                                continue;
                            }

                            "db" => {
                                let line = self.parse_data::<u8>()?;
                                self.lines.push(line);
                                continue;
                            }

                            "dw" => {
                                let line = self.parse_data::<u16>()?;
                                self.lines.push(line);
                                continue;
                            }

                            "dd" => {
                                let line = self.parse_data::<u32>()?;
                                self.lines.push(line);
                                continue;
                            }

                            "times" => {
                                let line = self.parse_times()?;
                                self.lines.push(line);
                                continue;
                            }

                            _ => {
                                // If no known keyword is found, we assume the identifier is a
                                // label.
                                let line = self.parse_label(identifier)?;
                                self.lines.push(line);
                                continue;
                            }
                        }
                    }
                }

                Token::EndOfFile => break,

                _ => {
                    return Err(self.expected(format!(
                        "Identifier expected at the start of a new line. Found {:?}",
                        self.token,
                    )))
                }
            }
        }

        Ok(())
    }

    fn next_token(&mut self) {
        // Store the position of the lexer to that we know where the token we are consuming starts.
        self.token_pos = self.lexer.pos();

        loop {
            match self.lexer.next_token() {
                Token::Comment(_) | Token::Whitespace => {
                    self.token_pos = self.lexer.pos();
                    continue;
                }
                token => {
                    self.token = token;
                    break;
                }
            }
        }
    }

    /// The current token is required to be a new line.  If it is, then consume it, otherwise we
    /// report an error.
    fn require_new_line(&mut self) -> Result<(), ParserError> {
        if let Token::NewLine = self.token {
            self.next_token();
            Ok(())
        } else if let Token::EndOfFile = self.token {
            Ok(())
        } else {
            Err(self.expected("Expected new line".to_owned()))
        }
    }

    fn parse_label(&mut self, name: &'a str) -> Result<ast::Line<'a>, ParserError> {
        // Consume the token that holds the label.
        self.next_token();

        // Skip the optional colon after a label.
        if matches!(self.token, Token::Punctuation(PunctuationKind::Colon)) {
            self.next_token();

            // If the token after the ":" is a new_line, then we should consume it as well.
            if matches!(self.token, Token::NewLine) {
                self.next_token();
            }
        } else if matches!(self.token, Token::NewLine) {
            // A new line after the label should also be consumed.
            self.next_token();
        }

        Ok(ast::Line::Label(name))
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
            self.next_token();
        }

        if matches!(self.token, Token::NewLine | Token::EndOfFile) {
            Ok(ast::Operands::None)
        } else {
            let destination = self.parse_operand(None)?;

            match self.token {
                Token::NewLine | Token::EndOfFile => Ok(ast::Operands::Destination(destination)),
                Token::Punctuation(PunctuationKind::Comma) => {
                    self.next_token();
                    let source = self.parse_operand(None)?;

                    Ok(ast::Operands::DestinationAndSource(destination, source))
                }

                _ => Err(self.expected("An unexpected token was encountered.".to_owned())),
            }
        }
    }

    fn parse_operand(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand<'a>, ParserError> {
        match self.token {
            Token::Punctuation(PunctuationKind::OpenBracket) => {
                self.next_token();
                self.parse_memory_operand(data_size)
            }

            Token::Identifier(identifier) => {
                if let Some(register) = ast::Register::from_str(identifier) {
                    self.next_token();
                    Ok(ast::Operand::Register(register))
                } else if let Some(data_size) = ast::DataSize::from_str(identifier) {
                    self.next_token();
                    self.parse_operand(Some(data_size))
                } else {
                    let expression = self.parse_expression(None)?;
                    Ok(ast::Operand::Immediate(expression))
                }
            }

            _ => {
                let expression = self.parse_expression(None)?;
                Ok(ast::Operand::Immediate(expression))
            }
        }
    }

    fn parse_expression(
        &mut self,
        left: Option<Box<ast::Expression<'a>>>,
    ) -> Result<Box<ast::Expression<'a>>, ParserError> {
        match self.token {
            Token::Literal(_) | Token::Identifier(_) => {
                let value_or_label = self.parse_value_or_label()?;
                return self
                    .parse_expression(Some(Box::new(ast::Expression::Value(value_or_label))));
            }

            Token::Punctuation(PunctuationKind::Plus) if left.is_some() => {
                self.next_token();

                let right = self.parse_value_or_label()?;

                let left = Box::new(ast::Expression::Add(
                    left.unwrap(),
                    Box::new(ast::Expression::Value(right)),
                ));

                self.parse_expression(Some(left))
            }

            Token::Punctuation(PunctuationKind::Minus) if left.is_some() => {
                self.next_token();

                let right = self.parse_value_or_label()?;

                let left = Box::new(ast::Expression::Subtract(
                    left.unwrap(),
                    Box::new(ast::Expression::Value(right)),
                ));

                self.parse_expression(Some(left))
            }

            Token::Punctuation(PunctuationKind::Star) if left.is_some() => {
                self.next_token();

                let right = self.parse_value_or_label()?;

                let left = Box::new(ast::Expression::Multiply(
                    left.unwrap(),
                    Box::new(ast::Expression::Value(right)),
                ));

                self.parse_expression(Some(left))
            }

            Token::Punctuation(PunctuationKind::ForwardSlash) if left.is_some() => {
                self.next_token();

                let right = self.parse_value_or_label()?;

                let left = Box::new(ast::Expression::Divide(
                    left.unwrap(),
                    Box::new(ast::Expression::Value(right)),
                ));

                self.parse_expression(Some(left))
            }

            _ => {
                if let Some(expression) = left {
                    return Ok(expression);
                } else {
                    todo!()
                }
            }
        }
    }

    fn parse_value_or_label(&mut self) -> Result<ast::ValueOrLabel<'a>, ParserError> {
        match self.token {
            Token::Literal(LiteralKind::Number(value)) => {
                self.next_token();
                Ok(ast::ValueOrLabel::Value(value))
            }

            Token::Literal(LiteralKind::String(s, terminated)) => {
                if !terminated {
                    Err(self.expected("Unterminated string literal".to_owned()))
                } else if s.len() != 1 {
                    Err(self.expected("Only character literals allowed as operands".to_owned()))
                } else {
                    // Consume the literal.
                    self.next_token();

                    let value = s.chars().next().unwrap() as i32;

                    Ok(ast::ValueOrLabel::Value(value))
                }
            }

            Token::Identifier(identifier) => {
                self.next_token();
                Ok(ast::ValueOrLabel::Label(identifier))
            }

            _ => Err(self.expected("Value or label expected.".to_owned())),
        }
    }

    fn parse_memory_operand(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand<'a>, ParserError> {
        let mut segment_override = None;

        let expression = match self.token {
            Token::Identifier(identifier) => {
                // If the first identifier is a segment, then we have an override.
                if let Some(segment) = ast::Segment::from_str(identifier) {
                    segment_override = Some(segment);
                    self.next_token();

                    if matches!(self.token, Token::Punctuation(PunctuationKind::Colon)) {
                        self.next_token();
                    } else {
                        return Err(
                            self.expected("Colon (:) required after segment override.".to_owned())
                        );
                    }
                }

                self.parse_expression(None)?
            }
            _ => todo!(),
        };

        if matches!(
            self.token,
            Token::Punctuation(PunctuationKind::CloseBracket)
        ) {
            self.next_token();
        } else {
            return Err(self.expected("closing bracket for memory address".to_owned()));
        }

        Ok(ast::Operand::DirectAddress(
            data_size,
            expression,
            segment_override,
        ))
    }

    fn parse_equ(&mut self) -> Result<ast::Line<'a>, ParserError> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));

        // Consume the "equ" keyword.
        self.next_token();

        let expression = self.parse_expression(None)?;

        self.require_new_line()?;

        Ok(ast::Line::Constant(expression))
    }

    fn parse_data<T>(&mut self) -> Result<ast::Line<'a>, ParserError> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));

        // Consume the "Dx" keyword.
        self.next_token();

        let mut data = Vec::<u8>::new();

        loop {
            match self.token {
                Token::Punctuation(PunctuationKind::Comma) => {
                    self.next_token();
                    continue;
                }

                Token::Literal(LiteralKind::String(s, _)) => {
                    self.next_token();
                    for b in s.as_bytes() {
                        data.push(*b);
                    }
                }

                Token::Literal(LiteralKind::Number(number)) => {
                    self.next_token();
                    for b in number.to_le_bytes() {
                        data.push(b);
                    }
                }

                _ => break,
            }
        }

        self.require_new_line()?;

        Ok(ast::Line::Data(data))
    }

    fn parse_times(&mut self) -> Result<ast::Line<'a>, ParserError> {
        self.next_token();

        let expression = self.parse_expression(None)?;

        Ok(ast::Line::Times(expression))
    }

    fn expected(&self, message: String) -> ParserError {
        ParserError::Expected(self.token_pos, message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blank_lines() {
        let lines = parse("").unwrap();
        assert_eq!(lines, vec![]);

        let lines = parse("\n\n").unwrap();
        assert_eq!(lines, vec![]);
    }

    #[test]
    fn label_and_instruction() {
        let lines = parse("start hlt").unwrap();
        assert_eq!(
            lines,
            vec![
                ast::Line::Label("start"),
                ast::Line::Instruction(ast::Instruction {
                    operation: ast::Operation::HLT,
                    operands: ast::Operands::None,
                })
            ]
        );
    }

    #[test]
    fn multiple_labels() {
        let lines = parse("start begin: begin2:hlt").unwrap();
        assert_eq!(
            lines,
            vec![
                ast::Line::Label("start"),
                ast::Line::Label("begin"),
                ast::Line::Label("begin2"),
                ast::Line::Instruction(ast::Instruction {
                    operation: ast::Operation::HLT,
                    operands: ast::Operands::None,
                })
            ]
        );
    }

    #[test]
    fn constants() {
        let lines = parse("label equ 42").unwrap();
        assert_eq!(
            lines,
            vec![ast::Line::Label("label"), ast::Line::Constant(42)]
        );

        let lines =
            parse("first equ 10 ; first value\n\nsecond equ 20 ; second value\n\n").unwrap();
        assert_eq!(
            lines,
            vec![
                ast::Line::Label("first"),
                ast::Line::Constant(10),
                ast::Line::Label("second"),
                ast::Line::Constant(20),
            ]
        );
    }
}
