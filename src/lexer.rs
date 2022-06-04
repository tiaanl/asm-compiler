#[repr(u8)]
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Base {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

impl Base {
    #[inline(always)]
    fn try_from_char(c: char) -> Result<Self, ()> {
        Ok(match c {
            'b' | 'B' => Self::Binary,
            'o' | 'O' => Self::Octal,
            'd' | 'D' => Self::Decimal,
            'h' | 'H' => Self::Hexadecimal,
            _ => return Err(()),
        })
    }

    fn detect_highest_for(s: &str) -> Self {
        let mut base = Self::Binary;

        for c in s.chars() {
            if c > '1' {
                base = Self::Octal;
            } else if c > '8' {
                base = Self::Decimal
            } else if c > '9' {
                base = Self::Hexadecimal
            }
        }

        base
    }
}

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

macro_rules! number_with_base {
    ($self:expr, $prefix:expr, $is_digit:ident, $base:expr) => {{
        debug_assert!($self.source[$self.pos..].starts_with('0'));
        debug_assert!($self.source[$self.pos + 1..].starts_with($prefix));

        // Only consume the '0'.
        $self.pos += 1;
        let start = $self.pos;

        let found = match $self.source[$self.pos + 1..].find(|c| !$is_digit(c)) {
            Some(found) => found,
            None => $self.source.len() - $self.pos - 1,
        };

        // If no additional characters were found, then we return the '0' character as the literal
        // only.
        if found == 0 {
            return Token::Literal(LiteralKind::Number(0));
        }

        // We have additional characters, so we can consume the prefix character and the found
        // digits.
        $self.pos += found + 1;

        // We can unwrap here, because we already made sure we only have valid characters.
        #[allow(clippy::from_str_radix_10)]
        let value = i32::from_str_radix(&$self.source[start + 1..$self.pos], $base).unwrap();
        Token::Literal(LiteralKind::Number(value))
    }};
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
        let first = match self.source[self.pos..].chars().next() {
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

            c if is_decimal_digit(c) => Ok(self.number(c)),

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

    fn number(&mut self, first_char: char) -> Token<'a> {
        debug_assert!(is_decimal_digit(first_char));

        if first_char == '0' {
            self.number_literal_with_prefix()
        } else {
            self.number_literal_with_suffix()
        }
    }

    fn number_literal_with_prefix(&mut self) -> Token<'a> {
        if let Some(second_char) = self.source[self.pos + 1..].chars().next() {
            // If we have a prefix at this point, then we can handle it immediately.
            match second_char {
                c @ 'b' => return number_with_base!(self, c, is_binary_digit, 2),
                c @ 'o' => return number_with_base!(self, c, is_octal_digit, 8),
                c @ 'd' => return number_with_base!(self, c, is_decimal_digit, 10),
                c @ 'x' => return number_with_base!(self, c, is_hexadecimal_digit, 16),

                // [second_char] is not a valid prefix, so fall through to handle other formats.
                _ => self.number_literal_with_suffix(),
            }
        } else {
            // We reached the end of the source and we only have the '0' character.
            return Token::Literal(LiteralKind::Number(0));
        }
    }

    fn number_literal_with_suffix(&mut self) -> Token<'a> {
        debug_assert!(is_decimal_digit(
            self.source[self.pos..].chars().next().unwrap()
        ));

        let start = self.pos;

        // Consume as many of the highest base (hexadecimal) characters as we can.
        match self.source[self.pos..].find(|c| !is_hexadecimal_digit(c)) {
            Some(found) => self.pos += found,
            None => self.pos = self.source.len(),
        };

        let mut s = &self.source[start..self.pos];

        // We can unwrap here, because we are guaranteed to have at least a single numeric
        // character.
        let suffix = s.chars().last().unwrap();

        let base = match Base::try_from_char(suffix) {
            Ok(base) => {
                // If we found a suffix included in the string already, we have to cut it off.
                s = &s[..s.len() - 1];

                base
            }
            Err(_) => {
                // Consume another character to see if there is a suffix that is not a valid
                // hexadecimal character as well.
                match self.source[self.pos..].chars().next() {
                    Some(c) => match Base::try_from_char(c) {
                        Ok(base) => {
                            // Consume the suffix.
                            self.pos += 1;

                            base
                        }
                        Err(_) => Base::Decimal,
                    },
                    None => Base::Decimal,
                }
            }
        };

        let highest_base = Base::detect_highest_for(s);

        if highest_base > base {
            todo!("The requested base in lower than the detected base")
        }

        // We can unwrap here, because we already checked that all the characters are valid for the
        // base.
        let value = i32::from_str_radix(s, base as u32).unwrap();

        Token::Literal(LiteralKind::Number(value))
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

    pub fn source_line_current(&self, path: Option<&str>) -> String {
        self.source_line_at_pos(self.pos, path)
    }

    pub fn source_line_at_pos(&self, pos: usize, path: Option<&str>) -> String {
        let prev_new_line = if let Some(found) = self.source[..pos].rfind('\n') {
            found + 1
        } else {
            0
        };

        let next_new_line = if let Some(found) = self.source[pos..].find('\n') {
            pos + found
        } else {
            self.source.len()
        };

        let fragment = &self.source[prev_new_line..next_new_line];

        let line = self.source[0..pos].matches('\n').count() + 1;
        let column = pos - prev_new_line;

        let mut result = String::new();

        if let Some(path) = path {
            result += format!("{}:{}:{}\n", path, line, column + 1).as_str();
        }

        result += fragment;
        result += "\n";
        for _ in 0..column {
            result += " ";
        }
        result += "^";

        result
    }
}

#[inline(always)]
fn is_identifier_first(c: char) -> bool {
    ('a'..='z').contains(&c) | ('A'..='Z').contains(&c) || c == '_'
}

#[inline(always)]
fn is_identifier(c: char) -> bool {
    is_identifier_first(c) || is_decimal_digit(c)
}

#[inline(always)]
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r'
}

#[inline(always)]
fn is_binary_digit(c: char) -> bool {
    c == '0' || c == '1'
}

#[inline(always)]
fn is_octal_digit(c: char) -> bool {
    ('0'..='7').contains(&c)
}

#[inline(always)]
fn is_decimal_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

#[inline(always)]
fn is_hexadecimal_digit(c: char) -> bool {
    is_decimal_digit(c) || ('a'..='f').contains(&c) || ('A'..='F').contains(&c)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_next_token {
        ($lexer:expr, $token:expr) => {
            assert_eq!($lexer.next_token().unwrap(), $token);
        };
    }

    macro_rules! assert_next_token_err {
        ($lexer:expr, $err:expr) => {
            assert_eq!($lexer.next_token().err().unwrap(), $err);
        };
    }

    macro_rules! assert_parse {
        ($source:literal, $token:expr) => {
            let mut lexer = Lexer::new($source);
            assert_next_token!(lexer, $token);
        };
    }

    #[test]
    fn end_of_file() {
        let mut lexer = Lexer::new("");
        assert_next_token!(lexer, Token::EndOfFile);
        assert_next_token!(lexer, Token::EndOfFile);
        assert_next_token!(lexer, Token::EndOfFile);

        let mut lexer = Lexer::new("test    ");
        assert_next_token!(lexer, Token::Identifier("test"));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::EndOfFile);
        assert_next_token!(lexer, Token::EndOfFile);
        assert_next_token!(lexer, Token::EndOfFile);
    }

    #[test]
    fn skips_whitespace() {
        assert_parse!("", Token::EndOfFile);

        let mut lexer = Lexer::new(" \t test \rtest2");
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Identifier("test"));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Identifier("test2"));
        assert_next_token!(lexer, Token::EndOfFile);
    }

    #[test]
    fn comments() {
        let mut lexer = Lexer::new("comment ; this is a comment");
        assert_next_token!(lexer, Token::Identifier("comment"));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Comment("; this is a comment"));
        assert_next_token!(lexer, Token::EndOfFile);

        let mut lexer = Lexer::new("comment ; this is a comment with newline\nid");
        assert_next_token!(lexer, Token::Identifier("comment"));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Comment("; this is a comment with newline"));
        assert_next_token!(lexer, Token::NewLine);
        assert_next_token!(lexer, Token::Identifier("id"));
        assert_next_token!(lexer, Token::EndOfFile);
    }

    #[test]
    fn string_literals() {
        let mut lexer = Lexer::new("  'a string literal ;; 123'");
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(
            lexer,
            Token::Literal(LiteralKind::String("a string literal ;; 123"))
        );

        let mut lexer = Lexer::new("  'a string literal  ");
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token_err!(
            lexer,
            LexerError {
                pos: 2,
                kind: LexerErrorKind::UnterminatedStringLiteral
            }
        );

        let mut lexer = Lexer::new("  'a string literal\n'  ");
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token_err!(
            lexer,
            LexerError {
                pos: 2,
                kind: LexerErrorKind::UnterminatedStringLiteral
            }
        );
    }

    #[test]
    fn number_literals() {
        // binary
        assert_parse!("11001000b", Token::Literal(LiteralKind::Number(200)));
        assert_parse!("0b11001000", Token::Literal(LiteralKind::Number(200)));

        // octal
        assert_parse!("310o", Token::Literal(LiteralKind::Number(200)));
        assert_parse!("0o310", Token::Literal(LiteralKind::Number(200)));

        // decimal
        assert_parse!("200", Token::Literal(LiteralKind::Number(200)));
        assert_parse!("0200", Token::Literal(LiteralKind::Number(200)));
        assert_parse!("200d", Token::Literal(LiteralKind::Number(200)));
        assert_parse!("0200d", Token::Literal(LiteralKind::Number(200)));
        assert_parse!("0d200", Token::Literal(LiteralKind::Number(200)));

        // hex
        assert_parse!("0c8h", Token::Literal(LiteralKind::Number(200)));
        assert_parse!("0xc8", Token::Literal(LiteralKind::Number(200)));

        // ensure we process the suffixes
        let mut lexer = Lexer::new("10d\n10h\n10b\n");
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(10)));
        assert_next_token!(lexer, Token::NewLine);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(16)));
        assert_next_token!(lexer, Token::NewLine);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(2)));
        assert_next_token!(lexer, Token::NewLine);

        let mut lexer = Lexer::new(" 10 ");
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(10)));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::EndOfFile);

        let mut lexer = Lexer::new(" 0x10 010H 010 0X10 0x ");
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(16)));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(16)));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(10)));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(0)));
        assert_next_token!(lexer, Token::Identifier("X10"));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(0)));
        assert_next_token!(lexer, Token::Identifier("x"));
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("test _te_st test123 1tst");
        assert_next_token!(lexer, Token::Identifier("test"));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Identifier("_te_st"));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Identifier("test123"));
        assert_next_token!(lexer, Token::Whitespace);
        assert_next_token!(lexer, Token::Literal(LiteralKind::Number(1)));
        assert_next_token!(lexer, Token::Identifier("tst"));
        assert_next_token!(lexer, Token::EndOfFile);
    }
}
