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
    token: Token,

    lines: Vec<ast::Line<'a>>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(source),
            token_pos: 0,
            token: Token::EndOfFile(0..0),
            lines: vec![],
        };

        // Initialize the current token with the first token that we can fetch from the lexer.
        parser.next_token();

        parser
    }

    fn parse(&mut self) -> Result<(), ParserError> {
        loop {
            match self.token {
                Token::NewLine(_) => {
                    // If the line starts with a new line, we just skip it.
                    self.next_token();
                }

                Token::Identifier(ref span) => {
                    let identifier = self.lexer.source_at(span);
                    if let Some(operation) = ast::Operation::from_str(identifier) {
                        let start = self.token.span().start;
                        self.next_token();
                        let instruction = self.parse_instruction(operation)?;
                        let end = self.token.span().end;
                        self.lines
                            .push(ast::Line::Instruction(start..end, instruction));
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

                Token::EndOfFile(_) => break,

                _ => {
                    return Err(self.expected(format!(
                        "Identifier or label expected, found {:?}",
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
            let token = self.lexer.next_token();
            match token {
                Token::Comment(_) | Token::Whitespace(_) => {
                    self.token_pos = self.lexer.pos();
                    continue;
                }
                _ => {
                    self.token = token;
                    break;
                }
            }
        }
    }

    /// The current token is required to be a new line.  If it is, then consume it, otherwise we
    /// report an error.
    fn require_new_line(&mut self) -> Result<(), ParserError> {
        if let Token::NewLine(_) = self.token {
            self.next_token();
            Ok(())
        } else if let Token::EndOfFile(_) = self.token {
            Ok(())
        } else {
            Err(self.expected("Expected new line".to_owned()))
        }
    }

    fn parse_label(&mut self, name: &'a str) -> Result<ast::Line<'a>, ParserError> {
        let start = self.token.span().start;

        // Consume the token that holds the label.
        self.next_token();

        // Skip the optional colon after a label.
        if matches!(self.token, Token::Punctuation(_, PunctuationKind::Colon)) {
            self.next_token();

            // If the token after the ":" is a new_line, then we should consume it as well.
            if matches!(self.token, Token::NewLine(_)) {
                self.next_token();
            }
        } else if matches!(self.token, Token::NewLine(_)) {
            // A new line after the label should also be consumed.
            self.next_token();
        }

        let end = self.token.span().end;

        Ok(ast::Line::Label(start..end, name))
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
        let start = self.token.span().start;

        if matches!(self.token, Token::Comment(_)) {
            self.next_token();
        }

        if matches!(self.token, Token::NewLine(_) | Token::EndOfFile(_)) {
            Ok(ast::Operands::None(start..start))
        } else {
            let destination = self.parse_operand(None)?;

            match self.token {
                Token::NewLine(_) | Token::EndOfFile(_) => Ok(ast::Operands::Destination(
                    start..self.token.span().end,
                    destination,
                )),
                Token::Punctuation(_, PunctuationKind::Comma) => {
                    self.next_token();
                    let source = self.parse_operand(None)?;

                    Ok(ast::Operands::DestinationAndSource(
                        start..self.token.span().end,
                        destination,
                        source,
                    ))
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
            Token::Punctuation(_, PunctuationKind::OpenBracket) => {
                self.next_token();
                self.parse_memory_operand(data_size)
            }

            Token::Identifier(ref span) => {
                let identifier = self.lexer.source_at(span);
                if let Some(register) = ast::Register::from_str(identifier) {
                    self.next_token();
                    Ok(ast::Operand::Register(register))
                } else if let Some(segment) = ast::Segment::from_str(identifier) {
                    self.next_token();
                    Ok(ast::Operand::Segment(segment))
                } else if let Some(data_size) = ast::DataSize::from_str(identifier) {
                    self.next_token();
                    self.parse_operand(Some(data_size))
                } else {
                    let expression = self.parse_expression()?;
                    Ok(ast::Operand::Immediate(expression))
                }
            }

            _ => {
                let expression = self.parse_expression()?;
                Ok(ast::Operand::Immediate(expression))
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Box<ast::Expression<'a>>, ParserError> {
        let mut left = Box::new(ast::Expression::Term(self.parse_value()?));

        macro_rules! op {
            ($operator:ident) => {{
                // Consume the operator.
                self.next_token();

                let right = self.parse_expression()?;
                left = Box::new(ast::Expression::$operator(left, right));
            }};
        }

        loop {
            match self.token {
                Token::Punctuation(_, PunctuationKind::Plus) => op!(Add),
                Token::Punctuation(_, PunctuationKind::Minus) => op!(Subtract),
                Token::Punctuation(_, PunctuationKind::Star) => op!(Multiply),
                Token::Punctuation(_, PunctuationKind::ForwardSlash) => op!(Divide),

                _ => return Ok(left),
            }
        }
    }

    fn parse_value(&mut self) -> Result<ast::Value<'a>, ParserError> {
        match self.token {
            Token::Literal(_, LiteralKind::Number(value)) => {
                self.next_token();
                Ok(ast::Value::Constant(value))
            }

            Token::Literal(ref span, LiteralKind::String(terminated)) => {
                let literal = self.lexer.source_at(span);
                if !terminated {
                    Err(self.expected("Unterminated string literal".to_owned()))
                } else if literal.len() != 1 {
                    Err(self.expected("Only character literals allowed.".to_owned()))
                } else {
                    // Consume the literal.
                    self.next_token();

                    let value = literal.chars().next().unwrap() as i32;

                    Ok(ast::Value::Constant(value))
                }
            }

            Token::Identifier(ref span) => {
                let identifier = self.lexer.source_at(span);

                self.next_token();

                if let Some(register) = ast::Register::from_str(identifier) {
                    Ok(ast::Value::Register(register))
                } else {
                    Ok(ast::Value::Label(identifier))
                }
            }

            _ => Err(self.expected("Constant, label or register expected.".to_owned())),
        }
    }

    fn parse_memory_operand(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand<'a>, ParserError> {
        let mut segment_override = None;

        let expression = match self.token {
            Token::Identifier(ref span) => {
                let identifier = self.lexer.source_at(span);
                // If the first identifier is a segment, then we have an override.
                if let Some(segment) = ast::Segment::from_str(identifier) {
                    segment_override = Some(segment);
                    self.next_token();

                    if matches!(self.token, Token::Punctuation(_, PunctuationKind::Colon)) {
                        self.next_token();
                    } else {
                        return Err(
                            self.expected("Colon (:) required after segment override.".to_owned())
                        );
                    }
                }

                self.parse_expression()?
            }
            _ => todo!(),
        };

        if matches!(
            self.token,
            Token::Punctuation(_, PunctuationKind::CloseBracket)
        ) {
            self.next_token();
        } else {
            return Err(self.expected("closing bracket for memory address".to_owned()));
        }

        Ok(ast::Operand::Address(
            data_size,
            expression,
            segment_override,
        ))
    }

    fn parse_equ(&mut self) -> Result<ast::Line<'a>, ParserError> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));

        // Consume the "equ" keyword.
        self.next_token();

        let expression = self.parse_expression()?;

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
                Token::Punctuation(_, PunctuationKind::Comma) => {
                    self.next_token();
                    continue;
                }

                Token::Literal(ref span, LiteralKind::String(_)) => {
                    let source = self.lexer.source_at(span);
                    self.next_token();
                    for b in source.as_bytes() {
                        data.push(*b);
                    }
                }

                Token::Literal(_, LiteralKind::Number(number)) => {
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

        let expression = self.parse_expression()?;

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
            vec![
                ast::Line::Label("label"),
                ast::Line::Constant(Box::new(ast::Expression::Term(ast::Value::Constant(42))))
            ]
        );

        let lines =
            parse("first equ 10 ; first value\n\nsecond equ 20 ; second value\n\n").unwrap();
        assert_eq!(
            lines,
            vec![
                ast::Line::Label("first"),
                ast::Line::Constant(Box::new(ast::Expression::Term(ast::Value::Constant(10)))),
                ast::Line::Label("second"),
                ast::Line::Constant(Box::new(ast::Expression::Term(ast::Value::Constant(20)))),
            ]
        );
    }

    #[test]
    fn expressions() {
        let mut parser = Parser::new("2 + 3 * 4 + 5");
        println!("{}", parser.parse_expression().unwrap());
    }
}
