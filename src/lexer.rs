#[derive(Clone, Copy, Debug)]
pub enum LiteralKind<'a> {
    Number(i32),
    String(&'a str),
}

#[derive(Clone, Copy, Debug)]
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
}

#[derive(Clone, Copy, Debug)]
pub enum Token<'a> {
    Whitespace,
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

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    #[inline(always)]
    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn next_token(&mut self) -> Result<Token<'a>, LexerError> {
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
                        self.pos += self.source[start..].len();
                        Ok(Token::Comment(&self.source[start..]))
                    }
                }
            }

            c if is_whitespace(c) => {
                self.pos += match self.source[self.pos..].find(|c| !is_whitespace(c)) {
                    Some(distance) => distance,
                    None => self.source[self.pos..].len(),
                };

                Ok(Token::Whitespace)
            }

            c if is_identifier_first(c) => {
                let start = self.pos;

                // If we can't find another character that is not an
                // identifier, it means the identifier fills the rest of the
                // source up to the end of the file.
                self.pos += match self.source[self.pos..].find(|c| !is_identifier(c)) {
                    Some(distance) => distance,
                    None => self.source[self.pos..].len(),
                };

                Ok(Token::Identifier(&self.source[start..self.pos]))
            }

            c if is_number(c) => Ok(self.number(c)),

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

    fn decimal_number(&mut self, start: usize) -> Token<'a> {
        self.pos += match self.source[start..].find(|c| !is_number(c)) {
            Some(found) => found,
            None => self.source.len() - start,
        };

        Token::Literal(LiteralKind::Number(
            self.source[start..self.pos].parse().unwrap(),
        ))
    }

    fn hexadecimal_number(&mut self, start: usize) -> Token<'a> {
        self.pos += match self.source[start..].find(|c| !is_hexadecimal_digit(c)) {
            Some(found) => found,
            None => self.source.len() - start,
        };

        Token::Literal(LiteralKind::Number(
            i32::from_str_radix(&self.source[start..self.pos], 16).unwrap(),
        ))
    }

    fn binary_number(&mut self, start: usize) -> Token<'a> {
        self.pos += match self.source[start..].find(|c| !is_number(c)) {
            Some(found) => found,
            None => self.source.len() - start,
        };

        Token::Literal(LiteralKind::Number(
            i32::from_str_radix(&self.source[start..self.pos], 2).unwrap(),
        ))
    }

    fn number(&mut self, first_char: char) -> Token<'a> {
        let start = self.pos;
        self.pos += 1;

        if let Some(second) = self.source[self.pos..].chars().next() {
            match dbg!(second) {
                'x' => {
                    self.pos += 1;
                    self.hexadecimal_number(start + 2)
                }

                'b' => {
                    self.pos += 1;
                    self.binary_number(start + 2)
                }

                _ => self.decimal_number(start),
            }
        } else {
            // End of stream reached, so we handle only the single first character as a number
            self.decimal_number(start)
        }
    }

    fn string_literal(&mut self) -> Result<Token<'a>, LexerError> {
        let start = self.pos;

        let first_terminator = self.source[(start + 1)..].find('\'');
        let first_new_line = self.source[(start + 1)..].find('\n');

        match (first_terminator, first_new_line) {
            (None, None) | (None, Some(_)) => Err(LexerError::new(
                self.pos,
                LexerErrorKind::UnterminatedStringLiteral,
            )),

            (Some(terminator), None) => {
                self.pos += terminator + 2;
                Ok(Token::Literal(LiteralKind::String(
                    &self.source[(start + 1)..(self.pos - 1)],
                )))
            }

            (Some(terminator), Some(new_line)) => {
                if new_line < terminator {
                    Err(LexerError::new(
                        self.pos,
                        LexerErrorKind::UnterminatedStringLiteral,
                    ))
                } else {
                    self.pos += terminator + 2;
                    Ok(Token::Literal(LiteralKind::String(
                        &self.source[(start + 1)..(self.pos - 1)],
                    )))
                }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn end_of_file() {
        let mut lexer = Lexer::new("");
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));

        let mut lexer = Lexer::new("test    ");
        assert!(matches!(lexer.next_token(), Ok(_)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
    }

    #[test]
    fn skips_whitespace() {
        let mut lexer = Lexer::new(" \t test \rtest2");
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier("test"))));
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier("test2"))));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
    }

    #[test]
    fn comments() {
        let mut lexer = Lexer::new("comment ; this is a comment");
        assert!(matches!(lexer.next_token(), Ok(_)));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Comment("; this is a comment"))
        ));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));

        let mut lexer = Lexer::new("comment ; this is a comment with newline\nid");
        assert!(matches!(lexer.next_token(), Ok(_)));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Comment("; this is a comment with newline"))
        ));
        assert!(matches!(lexer.next_token(), Ok(_)));
        assert!(matches!(lexer.next_token(), Ok(_)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
    }

    #[test]
    fn string_literals() {
        let mut lexer = Lexer::new("  'a string literal ;; 123'");
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::String(
                "a string literal ;; 123"
            )))
        ));

        let mut lexer = Lexer::new("  'a string literal  ");
        assert!(matches!(
            lexer.next_token(),
            Err(LexerError {
                pos: 2,
                kind: LexerErrorKind::UnterminatedStringLiteral
            })
        ));

        let mut lexer = Lexer::new("  'a string literal\n'  ");
        assert!(matches!(
            lexer.next_token(),
            Err(LexerError {
                pos: 2,
                kind: LexerErrorKind::UnterminatedStringLiteral
            })
        ));
    }

    #[test]
    fn number_literals() {
        let mut lexer = Lexer::new(" 10 0x10 0b10 0X10 0B10 0x ");
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(10)))
        ));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(16)))
        ));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(2)))
        ));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(0)))
        ));
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier("X10"))));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(0)))
        ));
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier("B10"))));

        let mut lexer = Lexer::new("10");
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(10)))
        ));

        let mut lexer = Lexer::new("0x10");
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(16)))
        ));

        let mut lexer = Lexer::new("0b10");
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(2)))
        ));
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("test _te_st test123 1tst");
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier("test"))));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Identifier("_te_st"))
        ));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Identifier("test123"))
        ));
        assert!(!matches!(lexer.next_token(), Ok(Token::Identifier(_))));
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier("tst"))));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
    }
}
