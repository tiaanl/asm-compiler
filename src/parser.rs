use crate::instructions::{str_to_operation, Operation};
use crate::lexer::{Lexer, LiteralKind, PunctuationKind, Token};
use crate::{ast, Span};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum ParserError {
    Expected(usize, String),
    InvalidPrefixOperator,
}

struct FoundToken<'a>(Token, &'a str);

impl<'a> Display for FoundToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Token::Whitespace(_) => write!(f, "whitespace"),
            Token::Identifier(_) => {
                write!(f, "identifier \"{}\"", self.1)
            }
            Token::Literal(_, literal_kind) => match literal_kind {
                LiteralKind::Number(value) => write!(f, "number \"{}\"", value),
                LiteralKind::String(terminated) => {
                    if *terminated {
                        write!(f, "string \"{}\"", self.1)
                    } else {
                        write!(f, "unterminated string \"{}\"", self.1)
                    }
                }
            },
            Token::Punctuation(_, _) => {
                write!(f, "punctuation \"{}\"", self.1)
            }
            Token::Comment(_) => write!(f, "comment \"{}\"", self.1),
            Token::NewLine(_) => write!(f, "new line"),
            Token::EndOfFile(_) => write!(f, "end of file"),
            Token::Invalid(_, c) => write!(f, "invalid character \"{}\"", c),
        }
    }
}

pub trait LineConsumer<'a> {
    fn consume(&mut self, line: ast::Line<'a>);
}

#[inline]
pub fn parse<'a>(source: &'a str, consumer: &mut impl LineConsumer<'a>) -> Result<(), ParserError> {
    Parser::new(source).parse(consumer)
}

struct Parser<'a> {
    source: &'a str,
    source_pos: usize,

    /// The position of the current token.
    token_pos: usize,

    /// The current token we are parsing.
    token: Token,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        let mut parser = Self {
            source,
            source_pos: 0,
            token_pos: 0,
            token: Token::EndOfFile(0),
        };

        // Initialize the current token with the first token that we can fetch from the lexer.
        parser.next_token();

        parser
    }

    fn token_source(&self) -> &'a str {
        &self.source[self.token_pos..self.token_pos + self.token.len()]
    }

    fn parse(&mut self, consumer: &mut impl LineConsumer<'a>) -> Result<(), ParserError> {
        loop {
            match self.token {
                Token::NewLine(_) => {
                    // If the line starts with a new line, we just skip it.
                    self.next_token();
                }

                Token::Identifier(ref span) => {
                    let identifier = self.token_source();
                    if let Some(operation) = str_to_operation(identifier) {
                        let instruction = self.parse_instruction(operation)?;
                        consumer.consume(instruction);
                    } else {
                        match identifier.to_lowercase().as_str() {
                            "equ" => {
                                let line = self.parse_equ()?;
                                consumer.consume(line);
                            }

                            "db" => {
                                let line = self.parse_data::<u8>()?;
                                consumer.consume(line);
                            }

                            "dw" => {
                                let line = self.parse_data::<u16>()?;
                                consumer.consume(line);
                            }

                            "dd" => {
                                let line = self.parse_data::<u32>()?;
                                consumer.consume(line);
                            }

                            "times" => {
                                let line = self.parse_times()?;
                                consumer.consume(line);
                            }

                            _ => {
                                // If no known keyword is found, we assume the identifier is a
                                // label.
                                let line = self.parse_label(identifier)?;
                                consumer.consume(line);
                            }
                        }
                    }
                }

                Token::EndOfFile(_) => break,

                _ => {
                    return Err(self.expected(format!(
                        "Identifier or label expected, found {}",
                        FoundToken(self.token.clone(), self.token_source()),
                    )))
                }
            }
        }

        Ok(())
    }

    fn next_token(&mut self) {
        // Store the position of the lexer to that we know where the token we are consuming starts.
        self.token_pos = self.source_pos;

        loop {
            let token = Lexer::new(&self.source[self.source_pos..]).next_token();
            self.source_pos += token.len();
            match token {
                Token::Comment(_) | Token::Whitespace(_) => {
                    self.token_pos = self.source_pos;
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
        let start = self.token_pos;

        // We only capture the label part.
        let end = self.token_pos + self.token.len();

        // Consume the token that holds the label.
        self.next_token();

        // Skip the optional colon after a label.
        if matches!(self.token, Token::Punctuation(_, PunctuationKind::Colon)) {
            self.next_token();
        }

        // We can have a label on a single line, so consume any new lines as well.
        if matches!(self.token, Token::NewLine(_)) {
            self.next_token();
        }

        Ok(ast::Line::Label(start..end, name))
    }

    fn parse_instruction(&mut self, operation: Operation) -> Result<ast::Line<'a>, ParserError> {
        let start = self.token_pos;

        // Consume the operation.
        self.next_token();

        let operands = self.parse_operands()?;

        let end = self.token_pos + self.token.len();

        self.require_new_line()?;

        Ok(ast::Line::Instruction(
            start..end,
            ast::Instruction {
                operation,
                operands,
            },
        ))
    }

    fn parse_operands(&mut self) -> Result<ast::Operands, ParserError> {
        let start = self.token_pos;

        if matches!(self.token, Token::NewLine(_) | Token::EndOfFile(_)) {
            Ok(ast::Operands::None(
                start..self.token_pos + self.token.len(),
            ))
        } else {
            let destination = self.parse_operand(None)?;

            match self.token {
                Token::NewLine(_) | Token::EndOfFile(_) => Ok(ast::Operands::Destination(
                    start..self.token_pos + self.token.len(),
                    destination,
                )),
                Token::Punctuation(_, PunctuationKind::Comma) => {
                    self.next_token();
                    let source = self.parse_operand(None)?;

                    Ok(ast::Operands::DestinationAndSource(
                        start..self.token_pos + self.token.len(),
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
    ) -> Result<ast::Operand, ParserError> {
        match self.token {
            Token::Punctuation(_, PunctuationKind::OpenBracket) => {
                self.next_token();
                self.parse_memory_operand(data_size)
            }

            Token::Identifier(ref span) => {
                let identifier = self.token_source();
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

    fn parse_expression(&mut self) -> Result<ast::Expression, ParserError> {
        self.parse_expression_with_binding_power(0)
    }

    fn parse_memory_operand(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand, ParserError> {
        let mut segment_override = None;

        let expression = match self.token {
            Token::Identifier(ref span) => {
                let identifier = self.token_source();
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
                    let source = self.token_source();
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

/// Utility functions for tokens.
impl Token {
    fn operator(&self) -> Option<ast::Operator> {
        Some(match self {
            Token::Punctuation(_, punctuation) => match punctuation {
                PunctuationKind::Plus => ast::Operator::Add,
                PunctuationKind::Minus => ast::Operator::Subtract,
                PunctuationKind::Star => ast::Operator::Multiply,
                PunctuationKind::ForwardSlash => ast::Operator::Divide,
                _ => return None,
            },
            _ => return None,
        })
    }
}

impl<'a> Parser<'a> {
    fn prefix_binding_power(operator: ast::Operator) -> Result<((), u8), ParserError> {
        Ok(match operator {
            ast::Operator::Add | ast::Operator::Subtract => ((), 5),
            _ => return Err(ParserError::InvalidPrefixOperator),
        })
    }

    fn infix_binding_power(operator: ast::Operator) -> (u8, u8) {
        match operator {
            ast::Operator::Add | ast::Operator::Subtract => (1, 2),
            ast::Operator::Multiply | ast::Operator::Divide => (3, 4),
        }
    }

    fn parse_expression_with_binding_power(
        &mut self,
        binding_power: u8,
    ) -> Result<ast::Expression, ParserError> {
        let mut left = match &self.token {
            Token::Punctuation(_, PunctuationKind::OpenParenthesis) => {
                self.next_token();

                let left = self.parse_expression_with_binding_power(0)?;

                if let Token::Punctuation(_, PunctuationKind::CloseParenthesis) = self.token {
                    self.next_token();

                    left
                } else {
                    return Err(
                        self.expected(format!("closing parenthesis, found {:?}", self.token))
                    );
                }
            }

            _ => {
                if let Some(operator) = self.token.operator() {
                    let ((), right_binding_power) = Self::prefix_binding_power(operator)?;
                    let right = self.parse_expression_with_binding_power(right_binding_power)?;
                    ast::Expression::PrefixOperator(operator, Box::new(right))
                } else {
                    ast::Expression::Term(self.parse_atom()?)
                }
            }
        };

        loop {
            let operator = match self.token {
                Token::NewLine(_) | Token::EndOfFile(_) => break,

                _ => {
                    if let Some(operator) = self.token.operator() {
                        operator
                    } else {
                        break;
                        //return Err(
                        //    self.expected(format!("Operator expected, found {:?}", self.token))
                        //);
                    }
                }
            };

            let (left_binding_power, right_binding_power) = Parser::infix_binding_power(operator);

            if left_binding_power < binding_power {
                break;
            }

            self.next_token();

            let right = self.parse_expression_with_binding_power(right_binding_power)?;

            left = ast::Expression::InfixOperator(operator, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_atom(&mut self) -> Result<ast::Value, ParserError> {
        match self.token {
            Token::Literal(_, LiteralKind::Number(value)) => {
                self.next_token();
                Ok(ast::Value::Constant(value))
            }

            Token::Literal(ref span, LiteralKind::String(terminated)) => {
                let literal = self.token_source();
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
                let identifier = self.token_source();

                self.next_token();

                if let Some(register) = ast::Register::from_str(identifier) {
                    Ok(ast::Value::Register(register))
                } else {
                    Ok(ast::Value::Label(identifier.to_owned()))
                }
            }

            _ => Err(self.expected("Constant, label or register expected.".to_owned())),
        }
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
                ast::Line::Label(0..5, "start"),
                ast::Line::Instruction(
                    6..9,
                    ast::Instruction {
                        operation: Operation::Hlt,
                        operands: ast::Operands::None(9..9),
                    }
                )
            ]
        );
    }

    #[test]
    fn multiple_labels() {
        let lines = parse("start begin: begin2:hlt").unwrap();
        assert_eq!(
            lines,
            vec![
                ast::Line::Label(0..5, "start"),
                ast::Line::Label(6..11, "begin"),
                ast::Line::Label(13..19, "begin2"),
                ast::Line::Instruction(
                    20..23,
                    ast::Instruction {
                        operation: Operation::Hlt,
                        operands: ast::Operands::None(23..23),
                    }
                )
            ]
        );
    }

    #[test]
    fn constants() {
        let lines = parse("label equ 42").unwrap();
        assert_eq!(
            lines,
            vec![
                ast::Line::Label(0..5, "label"),
                ast::Line::Constant(ast::Expression::Term(ast::Value::Constant(42)))
            ]
        );

        let lines =
            parse("first equ 10 ; first value\n\nsecond equ 20 ; second value\n\n").unwrap();
        assert_eq!(
            lines,
            vec![
                ast::Line::Label(0..5, "first"),
                ast::Line::Constant(ast::Expression::Term(ast::Value::Constant(10))),
                ast::Line::Label(28..34, "second"),
                ast::Line::Constant(ast::Expression::Term(ast::Value::Constant(20))),
            ]
        );
    }

    #[test]
    fn expressions() {
        let mut parser = Parser::new("2 + 3 * 4 + 5");
        let expr = parser.parse_expression().unwrap();
        assert_eq!(
            expr,
            ast::Expression::InfixOperator(
                ast::Operator::Add,
                Box::new(ast::Expression::InfixOperator(
                    ast::Operator::Add,
                    Box::new(ast::Expression::Term(ast::Value::Constant(2))),
                    Box::new(ast::Expression::InfixOperator(
                        ast::Operator::Multiply,
                        Box::new(ast::Expression::Term(ast::Value::Constant(3))),
                        Box::new(ast::Expression::Term(ast::Value::Constant(4))),
                    )),
                )),
                Box::new(ast::Expression::Term(ast::Value::Constant(5))),
            )
        );
    }
}
