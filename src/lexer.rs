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
        if self.skip_whitespace() {
            return Ok(Token::EndOfFile);
        }

        let mut chars = self.source[self.pos..].chars();

        let first = match chars.next() {
            Some(c) => c,
            None => return Ok(Token::EndOfFile),
        };

        match first {
            ';' => {
                let start = self.pos;
                match self.source[start..].find(|c| c == '\n') {
                    Some(found) => {
                        self.pos += found;
                        Ok(Token::Comment(&self.source[start..self.pos]))
                    }
                    None => {
                        // If a new line was not found, we have to take the rest of the source.
                        Ok(Token::Comment(&self.source[start..]))
                    }
                }
            }

            c if is_identifier_first(c) => {
                let start = self.pos;

                // Okay to unwrap here, because we already know [is_identifier_first] succeeded and
                // [is_identifier] is a superset of it.
                self.pos += self.source[self.pos..].find(|c| !is_identifier(c)).unwrap();

                Ok(Token::Identifier(&self.source[start..self.pos]))
            }

            c if is_number(c) => {
                let start = self.pos;

                Ok(if let Some(second) = chars.next() {
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
                })
            }

            '\'' => self.string_literal(),

            '\n' => Ok(self.single_char_token(Token::NewLine)),

            ':' => Ok(self.punctuation(PunctuationKind::Colon)),
            ',' => Ok(self.punctuation(PunctuationKind::Comma)),
            '.' => Ok(self.punctuation(PunctuationKind::Dot)),
            '[' => Ok(self.punctuation(PunctuationKind::OpenBracket)),
            ']' => Ok(self.punctuation(PunctuationKind::CloseBracket)),
            '(' => Ok(self.punctuation(PunctuationKind::OpenParenthesis)),
            ')' => Ok(self.punctuation(PunctuationKind::CloseParenthesis)),
            '+' => Ok(self.punctuation(PunctuationKind::Plus)),
            '-' => Ok(self.punctuation(PunctuationKind::Minus)),
            '*' => Ok(self.punctuation(PunctuationKind::Multiply)),
            '?' => Ok(self.punctuation(PunctuationKind::QuestionMark)),

            c => Err(LexerError::new(self.pos, LexerErrorKind::InvalidToken(c))),
        }
    }

    #[inline(always)]
    fn single_char_token(&mut self, token: Token<'a>) -> Token<'a> {
        self.pos += 1;
        token
    }

    #[inline(always)]
    fn punctuation(&mut self, punctuation: PunctuationKind) -> Token<'a> {
        self.single_char_token(Token::Punctuation(punctuation))
    }

    /// Advances the [pos] past any whitespace characters.  Return true if the
    /// end of the file was reached.
    fn skip_whitespace(&mut self) -> bool {
        loop {
            match self.source[self.pos..].chars().next() {
                Some(c) if is_whitespace(c) => {
                    self.pos += 1;
                    continue;
                }

                None => return true,

                _ => break,
            }
        }

        false
    }

    fn decimal_number(&mut self, start: usize) -> Token {
        self.pos += self.source[start..].find(|c| !is_number(c)).unwrap();

        Token::Literal(LiteralKind::Number(
            self.source[start..self.pos].parse().unwrap(),
        ))
    }

    fn hexadecimal_number(&mut self, start: usize) -> Token {
        self.pos += self.source[start..]
            .find(|c| !is_hexadecimal_digit(c))
            .unwrap();

        Token::Literal(LiteralKind::Number(
            i32::from_str_radix(&self.source[start..self.pos], 16).unwrap(),
        ))
    }

    fn binary_number(&mut self, start: usize) -> Token {
        self.pos += self.source[start..].find(|c| !is_number(c)).unwrap();

        Token::Literal(LiteralKind::Number(
            i32::from_str_radix(&self.source[start..self.pos], 1).unwrap(),
        ))
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

                None => {
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

                _ => {}
            }
        }
    }
}

#[inline(always)]
fn is_identifier_first(c: char) -> bool {
    ('a'..='z').contains(&c) | ('A'..='Z').contains(&c) || c == '_'
}

#[inline(always)]
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
