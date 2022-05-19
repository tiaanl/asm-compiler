#[derive(Debug)]
pub enum LiteralKind<'a> {
    Number(i32),
    String(&'a str),
}

#[derive(Debug)]
pub enum PunctuationKind {
    Colon,
    Comma,
    Dot,
    OpenBracket,
    CloseBracket,
    OpenParenthesis,
    CloseParenthesis,
    Plus,
    Minus,
    Multiply,
    QuestionMark,
}

#[derive(Debug)]
pub enum Token<'a> {
    Identifier(&'a str),
    Literal(LiteralKind<'a>),
    Punctuation(PunctuationKind),
    Comment(&'a str),
    NewLine,
    EndOfFile,
}

#[derive(Debug)]
pub enum LexerErrorKind {
    InvalidToken(char),
    UnterminatedStringLiteral,
}

impl std::fmt::Display for LexerErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerErrorKind::InvalidToken(c) => write!(f, "Invalid token found: \"{}\"", c),
            LexerErrorKind::UnterminatedStringLiteral => write!(f, "Unterminated string literal"),
        }
    }
}

#[derive(Debug)]
pub struct LexerError {
    pub pos: usize,
    pub kind: LexerErrorKind,
}

impl LexerError {
    pub fn new(pos: usize, kind: LexerErrorKind) -> Self {
        Self { pos, kind }
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        if self.pos >= self.source.len() {
            return Ok(Token::EndOfFile);
        }

        // Skip any preceeding whitespace.
        loop {
            match self.source[self.pos..].chars().next() {
                Some(c) if is_whitespace(c) => {
                    self.pos += 1;
                    continue;
                }

                None => return Ok(Token::EndOfFile),

                _ => break,
            }
        }

        let mut chars = self.source[self.pos..].chars();

        let first = match chars.next() {
            Some(c) => c,
            None => return Ok(Token::EndOfFile),
        };

        Ok(match first {
            ';' => {
                let start = self.pos;
                match self.source[start..].find(|c| c == '\n') {
                    Some(found) => {
                        self.pos += found;
                        Token::Comment(&self.source[start..self.pos])
                    }
                    None => {
                        // If a new line was not found, we have to take the rest of the source.
                        Token::Comment(&self.source[start..])
                    }
                }
            }

            c if is_identifier_first(c) => {
                let start = self.pos;

                // Okay to unwrap here, because we already know [is_identifier_first] succeeded and
                // [is_identifier] is a superset of it.
                let found = self.source[self.pos..].find(|c| !is_identifier(c)).unwrap();

                self.pos += found;

                Token::Identifier(&self.source[start..self.pos])
            }

            c if is_number(c) => {
                let start = self.pos;

                if let Some(second) = chars.next() {
                    match second {
                        'x' => {
                            self.pos += 2;
                            self.hexadecimal_number(start + 2)
                        }
                        'b' => {
                            self.pos += 2;
                            self.binary_number(start + 2)
                        }
                        _ => self.decimal_number(start),
                    }
                } else {
                    // End of stream reached, so we handle only the single first character as a number
                    self.decimal_number(start)
                }
            }

            '\'' => {
                return self.string_literal();
            }

            '\n' => {
                self.pos += 1;

                Token::NewLine
            }

            ':' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::Colon)
            }

            ',' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::Comma)
            }

            '.' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::Dot)
            }

            '[' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::OpenBracket)
            }

            ']' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::CloseBracket)
            }

            '(' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::OpenParenthesis)
            }

            ')' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::CloseParenthesis)
            }

            '+' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::Plus)
            }

            '-' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::Minus)
            }

            '*' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::Multiply)
            }

            '?' => {
                self.pos += 1;
                Token::Punctuation(PunctuationKind::QuestionMark)
            }

            c => {
                return Err(LexerError::new(self.pos, LexerErrorKind::InvalidToken(c)));
            }
        })
    }

    fn decimal_number(&mut self, start: usize) -> Token {
        // Okay to unwrap here, because we already know [is_identifier_first] succeeded and
        // [is_identifier] is a superset of it.
        let found = self.source[start..].find(|c| !is_number(c)).unwrap();

        self.pos += found;

        let value = self.source[start..self.pos].parse().unwrap();

        Token::Literal(LiteralKind::Number(value))
    }

    fn hexadecimal_number(&mut self, start: usize) -> Token {
        // Okay to unwrap here, because we already know [is_identifier_first] succeeded and
        // [is_identifier] is a superset of it.
        let found = self.source[start..]
            .find(|c| !is_hexadecimal_digit(c))
            .unwrap();

        self.pos += found;

        let value = i32::from_str_radix(&self.source[start..self.pos], 16).unwrap();

        Token::Literal(LiteralKind::Number(value))
    }

    fn binary_number(&mut self, start: usize) -> Token {
        // Okay to unwrap here, because we already know [is_identifier_first] succeeded and
        // [is_identifier] is a superset of it.
        let found = self.source[start..].find(|c| !is_number(c)).unwrap();

        self.pos += found;

        let value = i32::from_str_radix(&self.source[start..self.pos], 1).unwrap();

        Token::Literal(LiteralKind::Number(value))
    }

    fn string_literal(&mut self) -> Result<Token, LexerError> {
        let start = self.pos;

        let mut chars = self.source[(start + 1)..].chars();
        loop {
            match chars.next() {
                Some(c) if c == '\n' => {
                    return Err(LexerError::new(
                        start,
                        LexerErrorKind::UnterminatedStringLiteral,
                    ));
                }

                Some(c) if c == '\'' => {
                    let len = self.source.len() - start - chars.as_str().len();
                    let lit = &self.source[start..start + len];
                    self.pos += len;
                    return Ok(Token::Literal(LiteralKind::String(lit)));
                }

                None => todo!(),

                _ => {}
            }
        }
    }
}

#[inline(always)]
fn is_identifier_first(c: char) -> bool {
    ('a'..='z').contains(&c) | ('A'..='Z').contains(&c) || c == '_'
}

fn is_identifier(c: char) -> bool {
    is_identifier_first(c) || is_number(c)
}

#[inline(always)]
fn is_number(c: char) -> bool {
    ('0'..='9').contains(&c)
}

#[inline(always)]
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r'
}

#[inline(always)]
fn is_hexadecimal_digit(c: char) -> bool {
    is_number(c) || ('a'..='f').contains(&c) || ('A'..='F').contains(&c)
}
