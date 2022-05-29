#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LiteralKind<'a> {
    Number(i32),
    String(&'a str),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
#[cfg_attr(test, derive(Eq, PartialEq))]
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
#[cfg_attr(test, derive(Eq, PartialEq))]
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

        let token = match first {
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

            c if is_number(c) => Ok(self.number(c)),

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
        };

        // dbg!(token)
        token
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

    // fn decimal_number(&mut self, start: usize) -> Token<'a> {
    //     self.pos += match self.source[start..].find(|c| !is_number(c)) {
    //         Some(found) => found,
    //         None => self.source.len() - start,
    //     };
    //
    //     // dbg!(&self.source[start..self.pos]);
    //
    //     Token::Literal(LiteralKind::Number(
    //         self.source[start..self.pos].parse().unwrap(),
    //     ))
    // }

    // fn hexadecimal_number(&mut self, start: usize) -> Token<'a> {
    //     self.pos += match self.source[start..].find(|c| !is_hexadecimal_digit(c)) {
    //         Some(found) => found,
    //         None => self.source.len() - start,
    //     };
    //
    //     Token::Literal(LiteralKind::Number(
    //         i32::from_str_radix(&self.source[start..self.pos], 16).unwrap(),
    //     ))
    // }

    // fn binary_number(&mut self, start: usize) -> Token<'a> {
    //     self.pos += match self.source[start..].find(|c| !is_number(c)) {
    //         Some(found) => found,
    //         None => self.source.len() - start,
    //     };
    //
    //     Token::Literal(LiteralKind::Number(
    //         i32::from_str_radix(&self.source[start..self.pos], 2).unwrap(),
    //     ))
    // }

    fn number(&mut self, first_char: char) -> Token<'a> {
        // The caller checked the first character and passed it to us and should be a number.
        debug_assert!(is_number(first_char));

        if first_char == '0' {
            if let Some(second) = self.source[self.pos + 1..].chars().next() {
                let (begin, end, radix) = match second {
                    'x' => self.number_literal_with_prefix(second),
                    'b' => self.number_literal_with_prefix(second),
                    _ => self.number_literal_with_suffix(),
                };

                if begin == end {
                    Token::Literal(LiteralKind::Number(0))
                } else {
                    Token::Literal(LiteralKind::Number(
                        i32::from_str_radix(&self.source[begin..end], radix).unwrap(),
                    ))
                }
            } else {
                // An [EndOfFile] here means that one the 0 is available.
                Token::Literal(LiteralKind::Number(0))
            }
        } else {
            let start = self.pos;

            let found = if let Some(found) = self.source[self.pos..].find(|c| !is_number(c)) {
                found
            } else {
                1
            };

            self.pos += found;

            Token::Literal(LiteralKind::Number(
                self.source[start..self.pos].parse().unwrap(),
            ))
        }
    }

    fn number_literal_with_prefix(&mut self, prefix: char) -> (usize, usize, u32) {
        let start = self.pos;

        // Consume the "0" of the prefix.
        self.pos += 1;

        let radix = match prefix {
            'x' => 16,
            'b' => 2,
            _ => 10,
        };

        // Search for anything that is not a number after the prefix.
        if let Some(found) = self.source[self.pos + 1..].find(|c| !is_hexadecimal_digit(c)) {
            if found > 0 {
                self.pos += found + 1;
                (start + 2, self.pos, radix)
            } else {
                (start, start, radix)
            }
        } else {
            // If no more digits were found, then we return an empty range.
            (start, start, radix)
        }
    }

    fn number_literal_with_suffix(&mut self) -> (usize, usize, u32) {
        debug_assert!(self.source[self.pos..].starts_with('0'));

        let start = self.pos;

        // The first character is a 0 and the next is also a number, so we parse
        // anything that is a hexadecimal digit and then look for a suffix.
        if let Some(found) = self.source[self.pos + 1..].find(|c| !is_hexadecimal_digit(c)) {
            self.pos += found + 1;
        } else {
            // We didn't find any other digits, so it is just the 0.
            // return Token::Literal(LiteralKind::Number(0));
            todo!()
        };

        // Check for a suffix.
        if let Some(suffix) = self.source[self.pos..].chars().next() {
            match suffix {
                'H' => {
                    self.pos += 1;
                    (start, self.pos - 1, 16)
                }
                // 'B' => {
                //     self.pos += 1;
                //     (start, self.pos - 1, 2)
                // }
                _ => (start, self.pos, 10),
            }
        } else {
            todo!()
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
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier(_))));
        assert!(matches!(lexer.next_token(), Ok(Token::Whitespace)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
    }

    #[test]
    fn skips_whitespace() {
        let mut lexer = Lexer::new(" \t test \rtest2");
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(lexer.next_token().unwrap(), Token::Identifier("test"));
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(lexer.next_token().unwrap(), Token::Identifier("test2"));
        assert_eq!(lexer.next_token().unwrap(), Token::EndOfFile);
    }

    #[test]
    fn comments() {
        let mut lexer = Lexer::new("comment ; this is a comment");
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier(_))));
        assert!(matches!(lexer.next_token(), Ok(Token::Whitespace)));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Comment("; this is a comment"))
        ));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));

        let mut lexer = Lexer::new("comment ; this is a comment with newline\nid");
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier(_))));
        assert!(matches!(lexer.next_token(), Ok(Token::Whitespace)));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Comment("; this is a comment with newline"))
        ));
        assert!(matches!(lexer.next_token(), Ok(Token::NewLine)));
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier(_))));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
    }

    #[test]
    fn string_literals() {
        let mut lexer = Lexer::new("  'a string literal ;; 123'");
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Literal(LiteralKind::String("a string literal ;; 123"))
        );

        let mut lexer = Lexer::new("  'a string literal  ");
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(
            lexer.next_token().err().unwrap(),
            LexerError {
                pos: 2,
                kind: LexerErrorKind::UnterminatedStringLiteral
            }
        );

        let mut lexer = Lexer::new("  'a string literal\n'  ");
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(
            lexer.next_token().err().unwrap(),
            LexerError {
                pos: 2,
                kind: LexerErrorKind::UnterminatedStringLiteral
            }
        );
    }

    #[test]
    fn number_literals() {
        let mut lexer = Lexer::new(" 10 ");
        assert!(matches!(lexer.next_token(), Ok(Token::Whitespace)));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Literal(LiteralKind::Number(10)))
        ));
        assert!(matches!(lexer.next_token(), Ok(Token::Whitespace)));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));

        let mut lexer = Lexer::new(" 0x10 010H 010 0X10 0x ");
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Literal(LiteralKind::Number(16))
        );
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Literal(LiteralKind::Number(16))
        );
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Literal(LiteralKind::Number(10))
        );
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Literal(LiteralKind::Number(0))
        );
        assert_eq!(lexer.next_token().unwrap(), Token::Identifier("X10"));
        assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Literal(LiteralKind::Number(0))
        );
        assert_eq!(lexer.next_token().unwrap(), Token::Identifier("x"));

        // let mut lexer = Lexer::new(" 0b10 010B 010 0B10 0b ");
        // assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        // assert_eq!(
        //     lexer.next_token().unwrap(),
        //     Token::Literal(LiteralKind::Number(2))
        // );
        // assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        // assert_eq!(
        //     lexer.next_token().unwrap(),
        //     Token::Literal(LiteralKind::Number(2))
        // );
        // assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        // assert_eq!(
        //     lexer.next_token().unwrap(),
        //     Token::Literal(LiteralKind::Number(2))
        // );
        // assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        // assert_eq!(
        //     lexer.next_token().unwrap(),
        //     Token::Literal(LiteralKind::Number(0))
        // );
        // assert_eq!(lexer.next_token().unwrap(), Token::Identifier("B10"));
        // assert_eq!(lexer.next_token().unwrap(), Token::Whitespace);
        // assert_eq!(
        //     lexer.next_token().unwrap(),
        //     Token::Literal(LiteralKind::Number(0))
        // );
        // assert_eq!(lexer.next_token().unwrap(), Token::Identifier("b"));
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("test _te_st test123 1tst");
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier("test"))));
        assert!(matches!(lexer.next_token(), Ok(Token::Whitespace)));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Identifier("_te_st"))
        ));
        assert!(matches!(lexer.next_token(), Ok(Token::Whitespace)));
        assert!(matches!(
            lexer.next_token(),
            Ok(Token::Identifier("test123"))
        ));
        assert!(matches!(lexer.next_token(), Ok(Token::Whitespace)));
        assert!(!matches!(lexer.next_token(), Ok(Token::Identifier(_))));
        assert!(matches!(lexer.next_token(), Ok(Token::Identifier("tst"))));
        assert!(matches!(lexer.next_token(), Ok(Token::EndOfFile)));
    }
}
