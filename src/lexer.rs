use std::ops::Range;

#[repr(u8)]
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Base {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

impl Base {
    #[inline]
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
pub enum LiteralKind {
    Number(i32),
    String(bool),
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
    Star,
    ForwardSlash,
}

pub type Span = Range<usize>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Whitespace(usize),
    Identifier(usize),
    Literal(usize, LiteralKind),
    Punctuation(usize, PunctuationKind),
    Comment(usize),
    NewLine(usize),
    EndOfFile(usize),
    Invalid(usize, char),
}

impl Token {
    pub fn len(&self) -> usize {
        match self {
            Token::Whitespace(len)
            | Token::Identifier(len)
            | Token::Literal(len, _)
            | Token::Punctuation(len, _)
            | Token::Comment(len)
            | Token::NewLine(len)
            | Token::EndOfFile(len)
            | Token::Invalid(len, _) => *len,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    source: &'a str,
}

macro_rules! first_not_of {
    ($self:expr, $predicate:ident) => {{
        first_not_of!($self, $predicate, 0)
    }};

    ($self:expr, $predicate:ident, $offset:expr) => {{
        match $self.source[$offset..].find(|c| !$predicate(c)) {
            Some(found) => found,
            None => $self.source.len() - $offset,
        }
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    pub fn next_token(&mut self) -> Token {
        let first = match self.first() {
            Some(c) => c,
            None => return Token::EndOfFile(0),
        };

        match first {
            ';' => {
                match self.source.find(|c| c == '\n') {
                    Some(found) => Token::Comment(found),
                    None => {
                        // If a new line was not found, we have to take the rest of the source.
                        Token::Comment(self.source.len())
                    }
                }
            }

            c if is_whitespace(c) => Token::Whitespace(first_not_of!(self, is_whitespace)),

            c if is_decimal_digit(c) => self.number(c),

            c if is_identifier_first(c) => Token::Identifier(first_not_of!(self, is_identifier)),

            '\'' => self.string_literal(),

            '\n' => Token::NewLine(1),

            ':' => Token::Punctuation(1, PunctuationKind::Colon),
            ',' => Token::Punctuation(1, PunctuationKind::Comma),
            '.' => Token::Punctuation(1, PunctuationKind::Dot),
            '[' => Token::Punctuation(1, PunctuationKind::OpenBracket),
            ']' => Token::Punctuation(1, PunctuationKind::CloseBracket),
            '(' => Token::Punctuation(1, PunctuationKind::OpenParenthesis),
            ')' => Token::Punctuation(1, PunctuationKind::CloseParenthesis),
            '+' => Token::Punctuation(1, PunctuationKind::Plus),
            '-' => Token::Punctuation(1, PunctuationKind::Minus),
            '*' => Token::Punctuation(1, PunctuationKind::Star),
            '/' => Token::Punctuation(1, PunctuationKind::ForwardSlash),

            c => Token::Invalid(1, c),
        }
    }

    #[inline]
    fn first(&self) -> Option<char> {
        self.source.chars().next()
    }

    #[inline]
    fn second(&self) -> Option<char> {
        let mut chars = self.source.chars();

        if chars.next().is_some() {
            chars.next()
        } else {
            None
        }
    }

    #[inline]
    fn punctuation(&mut self, punctuation: PunctuationKind) -> Token {
        Token::Punctuation(1, punctuation)
    }

    fn number(&mut self, first_char: char) -> Token {
        debug_assert!(is_decimal_digit(first_char));

        if first_char == '0' {
            self.number_literal_with_prefix()
        } else {
            self.number_literal_with_suffix(0)
        }
    }

    fn number_literal_with_prefix(&mut self) -> Token {
        macro_rules! number_with_base {
            ($prefix:expr, $is_digit:ident, $base:expr) => {{
                debug_assert!(self.first().unwrap() == '0');
                debug_assert!(self.second().unwrap() == $prefix);

                let mut end = 0;

                let found = first_not_of!(self, $is_digit, 1);

                // If no additional characters were found, then we return the '0' character as the
                // literal only.
                if found == 0 {
                    return Token::Literal(1, LiteralKind::Number(0));
                }

                // Consume the prefix.
                end += 1;
                let decode_pos = end;

                // We have additional characters, so we can consume the prefix character and the found
                // digits.
                end += found;

                // We can unwrap here, because we already made sure we only have valid characters.
                #[allow(clippy::from_str_radix_10)]
                let value = i32::from_str_radix(&self.source[decode_pos..end], $base).unwrap();
                Token::Literal(end, LiteralKind::Number(value))
            }};
        }

        if let Some(second_char) = self.second() {
            // If we have a prefix at this point, then we can handle it immediately.
            match second_char {
                c @ 'b' => return number_with_base!(c, is_binary_digit, 2),
                c @ 'o' => return number_with_base!(c, is_octal_digit, 8),
                c @ 'd' => return number_with_base!(c, is_decimal_digit, 10),
                c @ 'x' => return number_with_base!(c, is_hexadecimal_digit, 16),

                // [second_char] is not a valid prefix, so fall through to handle other formats.
                _ => self.number_literal_with_suffix(2),
            }
        } else {
            // We reached the end of the source and we only have the '0' character.
            return Token::Literal(1, LiteralKind::Number(0));
        }
    }

    fn number_literal_with_suffix(&mut self, start: usize) -> Token {
        debug_assert!(is_decimal_digit(self.first().unwrap()));

        // Consume as many of the highest base (hexadecimal) characters as we can.
        let mut end = first_not_of!(self, is_hexadecimal_digit);

        let mut s = &self.source[start..end];

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
                match self.first() {
                    Some(c) => match Base::try_from_char(c) {
                        Ok(base) => {
                            // Consume the suffix.
                            end += 1;

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

        Token::Literal(end, LiteralKind::Number(value))
    }

    fn string_literal(&mut self) -> Token {
        // Consume the opening character.
        let mut end = 1;

        // The start is the first character after the opening character.
        let start = end;

        let first_terminator = self.source[end..].find('\'');
        let first_new_line = self.source[end..].find('\n');

        match (first_terminator, first_new_line) {
            (None, None) => {
                end = self.source.len();
                Token::Literal(end, LiteralKind::String(false))
            }

            (None, Some(new_line)) => {
                // Consume until the '\n' character.
                end += new_line;
                Token::Literal(end, LiteralKind::String(false))
            }

            (Some(terminator), None) => {
                // Consume the text and the terminator.
                end += terminator + 1;
                Token::Literal(end, LiteralKind::String(true))
            }

            (Some(terminator), Some(new_line)) => {
                if new_line < terminator {
                    end += new_line;

                    Token::Literal(end, LiteralKind::String(false))
                } else {
                    end += terminator + 1;

                    Token::Literal(end, LiteralKind::String(true))
                }
            }
        }
    }

    // pub fn _source_line_current(&self, path: Option<&str>) -> String {
    //     self._source_line_at_pos(self.pos, path)
    // }
    //
    // pub fn _source_line_at_pos(&self, pos: usize, path: Option<&str>) -> String {
    //     let prev_new_line = if let Some(found) = self.source[..pos].rfind('\n') {
    //         found + 1
    //     } else {
    //         0
    //     };
    //
    //     let next_new_line = if let Some(found) = self.source[pos..].find('\n') {
    //         pos + found
    //     } else {
    //         self.source.len()
    //     };
    //
    //     let fragment = &self.source[prev_new_line..next_new_line];
    //
    //     let line = self.source[0..pos].matches('\n').count() + 1;
    //     let column = pos - prev_new_line;
    //
    //     let mut result = String::new();
    //
    //     if let Some(path) = path {
    //         result += format!("{}:{}:{}\n", path, line, column + 1).as_str();
    //     }
    //
    //     result += fragment;
    //     result += "\n";
    //     for _ in 0..column {
    //         result += " ";
    //     }
    //     result += "^";
    //
    //     result
    // }
}

#[inline]
fn is_identifier_first(c: char) -> bool {
    ('a'..='z').contains(&c) | ('A'..='Z').contains(&c) || c == '_'
}

#[inline]
fn is_identifier(c: char) -> bool {
    is_identifier_first(c) || is_decimal_digit(c)
}

#[inline]
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r'
}

#[inline]
fn is_binary_digit(c: char) -> bool {
    c == '0' || c == '1'
}

#[inline]
fn is_octal_digit(c: char) -> bool {
    ('0'..='7').contains(&c)
}

#[inline]
fn is_decimal_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

#[inline]
fn is_hexadecimal_digit(c: char) -> bool {
    is_decimal_digit(c) || ('a'..='f').contains(&c) || ('A'..='F').contains(&c)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_next_token {
        ($source:literal, $token:expr, $match:literal) => {
            let source = $source;
            let token = Lexer::new(source).next_token();
            assert_eq!(token, $token);
            assert_eq!($match, &source[..token.len()]);
        };
    }

    // #[test]
    // fn snake() {
    //     let source = include_str!("../samples/snake.asm");
    //     let mut lexer = Lexer::new(source);
    //
    //     loop {
    //         let token = lexer.next_token();
    //         match token.kind {
    //             Token::EndOfFile => break,
    //             token => println!("{:?}", token),
    //         };
    //     }
    // }

    #[test]
    fn end_of_file() {
        assert_next_token!("", Token::EndOfFile(0), "");
    }

    #[test]
    fn whitespace() {
        assert_next_token!("  \r \t", Token::Whitespace(5), "  \r \t");
        assert_next_token!("  \r \ttest", Token::Whitespace(5), "  \r \t");
    }

    #[test]
    fn comments() {
        assert_next_token!(
            "; this is a comment",
            Token::Comment(19),
            "; this is a comment"
        );
        assert_next_token!(
            "; this is a comment\nid",
            Token::Comment(19),
            "; this is a comment"
        );
    }

    #[test]
    fn string_literals() {
        let mut lexer = Lexer::new("  'a string literal ;; 123'\ntest");
        assert_next_token!(Token::Whitespace(0..2), "  ");
        assert_next_token!(
            Token::Literal(3..26, LiteralKind::String(true)),
            "a string literal ;; 123"
        );
        assert_next_token!(Token::NewLine(27..28), "\n");
        assert_next_token!(Token::Identifier(28..32), "test");
        assert_next_token!(Token::EndOfFile(32..32), "");

        let mut lexer = Lexer::new("  'a string literal  ");
        assert_next_token!(Token::Whitespace(0..2), "  ");
        assert_next_token!(
            Token::Literal(3..21, LiteralKind::String(false)),
            "a string literal  "
        );
        assert_next_token!(Token::EndOfFile(21..21), "");

        let mut lexer = Lexer::new("  'a string literal\n'  ");
        assert_next_token!(Token::Whitespace(0..2), "  ");
        assert_next_token!(
            Token::Literal(3..19, LiteralKind::String(false)),
            "a string literal"
        );
        assert_next_token!(Token::NewLine(19..20), "\n");
        assert_next_token!(Token::Literal(21..23, LiteralKind::String(false)), "  ");
    }

    #[test]
    fn number_literals() {
        // binary
        assert_parse!(
            "11001000b",
            Token::Literal(0..9, LiteralKind::Number(200)),
            "11001000b"
        );
        assert_parse!(
            "0b11001000",
            Token::Literal(0..10, LiteralKind::Number(200)),
            "0b11001000"
        );

        // octal
        assert_parse!(
            "310o",
            Token::Literal(0..4, LiteralKind::Number(200)),
            "310o"
        );
        assert_parse!(
            "0o310",
            Token::Literal(0..5, LiteralKind::Number(200)),
            "0o310"
        );

        // decimal
        assert_parse!("200", Token::Literal(0..3, LiteralKind::Number(200)), "200");
        assert_parse!(
            "0200",
            Token::Literal(0..4, LiteralKind::Number(200)),
            "0200"
        );
        assert_parse!(
            "200d",
            Token::Literal(0..4, LiteralKind::Number(200)),
            "200d"
        );
        assert_parse!(
            "0200d",
            Token::Literal(0..5, LiteralKind::Number(200)),
            "0200d"
        );
        assert_parse!(
            "0d200",
            Token::Literal(0..5, LiteralKind::Number(200)),
            "0d200"
        );

        // hex
        assert_parse!(
            "0c8h",
            Token::Literal(0..4, LiteralKind::Number(200)),
            "0c8h"
        );
        assert_parse!(
            "0xc8",
            Token::Literal(0..4, LiteralKind::Number(200)),
            "0xc8"
        );

        // ensure we process the suffixes
        let mut lexer = Lexer::new("10d\n10h\n10b\n");
        assert_next_token!(Token::Literal(0..3, LiteralKind::Number(10)), "10d");
        assert_next_token!(Token::NewLine(3..4), "\n");
        assert_next_token!(Token::Literal(4..7, LiteralKind::Number(16)), "10h");
        assert_next_token!(Token::NewLine(7..8), "\n");
        assert_next_token!(Token::Literal(8..11, LiteralKind::Number(2)), "10b");
        assert_next_token!(Token::NewLine(11..12), "\n");
        assert_next_token!(Token::EndOfFile(12..12), "");

        let mut lexer = Lexer::new(" 10 ");
        assert_next_token!(Token::Whitespace(0..1), " ");
        assert_next_token!(Token::Literal(1..3, LiteralKind::Number(10)), "10");
        assert_next_token!(Token::Whitespace(3..4), " ");
        assert_next_token!(Token::EndOfFile(4..4), "");

        let mut lexer = Lexer::new(" 0x10 010H 010 0X10 0x ");
        assert_next_token!(Token::Whitespace(0..1), " ");
        assert_next_token!(Token::Literal(1..5, LiteralKind::Number(16)), "0x10");
        assert_next_token!(Token::Whitespace(5..6), " ");
        assert_next_token!(Token::Literal(6..10, LiteralKind::Number(16)), "010H");
        assert_next_token!(Token::Whitespace(10..11), " ");
        assert_next_token!(Token::Literal(11..14, LiteralKind::Number(10)), "010");
        assert_next_token!(Token::Whitespace(14..15), " ");
        assert_next_token!(Token::Literal(15..16, LiteralKind::Number(0)), "0");
        assert_next_token!(Token::Identifier(16..19), "X10");
        assert_next_token!(Token::Whitespace(19..20), " ");
        assert_next_token!(Token::Literal(20..21, LiteralKind::Number(0)), "0");
        assert_next_token!(Token::Identifier(21..22), "x");
        assert_next_token!(Token::Whitespace(22..23), " ");
        assert_next_token!(Token::EndOfFile(23..23), "");
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("test _te_st test123 1tst");
        assert_next_token!(Token::Identifier(0..4), "test");
        assert_next_token!(Token::Whitespace(4..5), " ");
        assert_next_token!(Token::Identifier(5..11), "_te_st");
        assert_next_token!(Token::Whitespace(11..12), " ");
        assert_next_token!(Token::Identifier(12..19), "test123");
        assert_next_token!(Token::Whitespace(19..20), " ");
        assert_next_token!(Token::Literal(20..21, LiteralKind::Number(1)), "1");
        assert_next_token!(Token::Identifier(21..24), "tst");
        assert_next_token!(Token::EndOfFile(24..24), "");
    }
}
