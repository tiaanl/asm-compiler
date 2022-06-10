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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
    Whitespace,
    Identifier,
    Literal(LiteralKind),
    Punctuation(PunctuationKind),
    Comment,
    NewLine,
    EndOfFile,
    Invalid(char),
}

type Span = Range<usize>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub span: Range<usize>,
    pub kind: TokenKind,
}

impl Token {
    pub fn whitespace(span: Span) -> Self {
        Self {
            span,
            kind: TokenKind::Whitespace,
        }
    }

    pub fn identifier(span: Span) -> Self {
        Self {
            span,
            kind: TokenKind::Identifier,
        }
    }

    pub fn number_literal(span: Span, value: i32) -> Self {
        Self {
            span,
            kind: TokenKind::Literal(LiteralKind::Number(value)),
        }
    }

    pub fn string_literal(span: Span, terminated: bool) -> Self {
        Self {
            span,
            kind: TokenKind::Literal(LiteralKind::String(terminated)),
        }
    }

    pub fn comment(span: Span) -> Self {
        Self {
            span,
            kind: TokenKind::Comment,
        }
    }

    pub fn end_of_file(span: Span) -> Self {
        Self {
            span,
            kind: TokenKind::EndOfFile,
        }
    }

    pub fn invalid(span: Span, c: char) -> Self {
        Self {
            span,
            kind: TokenKind::Invalid(c),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

macro_rules! first_not_of {
    ($self:expr, $predicate:ident) => {{
        first_not_of!($self, $predicate, 0)
    }};

    ($self:expr, $predicate:ident, $offset:expr) => {{
        match $self.source[$self.pos + $offset..].find(|c| !$predicate(c)) {
            Some(found) => found,
            None => $self.source.len() - $self.pos - $offset,
        }
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    #[inline]
    pub fn pos(&self) -> usize {
        self.pos
    }

    #[inline]
    pub fn source_at(&self, span: &Span) -> &'a str {
        &self.source[span.start..span.end]
    }

    pub fn next_token(&mut self) -> Token {
        let first = match self.first() {
            Some(c) => c,
            None => return Token::end_of_file(self.pos..self.pos),
        };

        match first {
            ';' => {
                let start = self.pos;
                match self.source[start..].find(|c| c == '\n') {
                    Some(found) => {
                        self.advance(found);
                        Token::comment(start..self.pos)
                    }
                    None => {
                        // If a new line was not found, we have to take the rest of the source.
                        self.advance(self.source.len() - start);
                        Token::comment(start..self.source.len())
                    }
                }
            }

            c if is_whitespace(c) => {
                let start = self.pos;
                self.advance(first_not_of!(self, is_whitespace));

                Token::whitespace(start..self.pos)
            }

            c if is_decimal_digit(c) => self.number(c),

            c if is_identifier_first(c) => {
                let start = self.pos;

                // If we can't find another character that is not an identifier, it means the
                // identifier fills the rest of the source up to the end of the file.
                self.advance(first_not_of!(self, is_identifier));

                Token::identifier(start..self.pos)
            }

            '\'' => self.string_literal(),

            '\n' => self.single_char_token(TokenKind::NewLine),

            ':' => self.punctuation(PunctuationKind::Colon),
            ',' => self.punctuation(PunctuationKind::Comma),
            '.' => self.punctuation(PunctuationKind::Dot),
            '[' => self.punctuation(PunctuationKind::OpenBracket),
            ']' => self.punctuation(PunctuationKind::CloseBracket),
            '(' => self.punctuation(PunctuationKind::OpenParenthesis),
            ')' => self.punctuation(PunctuationKind::CloseParenthesis),
            '+' => self.punctuation(PunctuationKind::Plus),
            '-' => self.punctuation(PunctuationKind::Minus),
            '*' => self.punctuation(PunctuationKind::Star),
            '/' => self.punctuation(PunctuationKind::ForwardSlash),

            c => Token::invalid(self.pos..self.pos + 1, c),
        }
    }

    #[inline]
    fn advance(&mut self, offset: usize) {
        self.pos += offset;
    }

    #[inline]
    fn first(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    #[inline]
    fn second(&self) -> Option<char> {
        let mut chars = self.source[self.pos..].chars();

        if chars.next().is_some() {
            chars.next()
        } else {
            None
        }
    }

    #[inline]
    fn single_char_token(&mut self, kind: TokenKind) -> Token {
        let start = self.pos;

        self.advance(1);

        Token {
            span: start..self.pos,
            kind,
        }
    }

    #[inline]
    fn punctuation(&mut self, punctuation: PunctuationKind) -> Token {
        self.single_char_token(TokenKind::Punctuation(punctuation))
    }

    fn number(&mut self, first_char: char) -> Token {
        debug_assert!(is_decimal_digit(first_char));

        let start = self.pos;

        if first_char == '0' {
            self.number_literal_with_prefix(start)
        } else {
            self.number_literal_with_suffix(start)
        }
    }

    fn number_literal_with_prefix(&mut self, start: usize) -> Token {
        macro_rules! number_with_base {
            ($prefix:expr, $is_digit:ident, $base:expr) => {{
                debug_assert!(self.first().unwrap() == '0');
                debug_assert!(self.second().unwrap() == $prefix);

                // Only consume the '0'.
                self.advance(1);

                let found = first_not_of!(self, $is_digit, 1);

                // If no additional characters were found, then we return the '0' character as the literal
                // only.
                if found == 0 {
                    return Token::number_literal(start..self.pos, 0);
                }

                // Consume the prefix.
                self.advance(1);
                let decode_pos = self.pos;

                // We have additional characters, so we can consume the prefix character and the found
                // digits.
                self.advance(found);

                // We can unwrap here, because we already made sure we only have valid characters.
                #[allow(clippy::from_str_radix_10)]
                let value = i32::from_str_radix(&self.source[decode_pos..self.pos], $base).unwrap();
                Token::number_literal(start..self.pos, value)
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
                _ => self.number_literal_with_suffix(start),
            }
        } else {
            // We reached the end of the source and we only have the '0' character.
            return Token::number_literal(start..self.pos, 0);
        }
    }

    fn number_literal_with_suffix(&mut self, start: usize) -> Token {
        debug_assert!(is_decimal_digit(self.first().unwrap()));

        // Consume as many of the highest base (hexadecimal) characters as we can.
        self.advance(first_not_of!(self, is_hexadecimal_digit));

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
                match self.first() {
                    Some(c) => match Base::try_from_char(c) {
                        Ok(base) => {
                            // Consume the suffix.
                            self.advance(1);

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

        Token::number_literal(start..self.pos, value)
    }

    fn string_literal(&mut self) -> Token {
        // Consume the opening character.
        self.advance(1);

        // The start is the first character after the opening character.
        let start = self.pos;

        let first_terminator = self.source[self.pos..].find('\'');
        let first_new_line = self.source[self.pos..].find('\n');

        match (first_terminator, first_new_line) {
            (None, None) => {
                self.pos = self.source.len();
                Token::string_literal(start..self.pos, false)
            }

            (None, Some(new_line)) => {
                // Consume until the '\n' character.
                self.advance(new_line);
                Token::string_literal(start..self.pos + new_line, false)
            }

            (Some(terminator), None) => {
                // Consume the text and the terminator.
                self.advance(terminator + 1);
                Token::string_literal(start..self.pos - 1, true)
            }

            (Some(terminator), Some(new_line)) => {
                if new_line < terminator {
                    self.advance(new_line);

                    Token::string_literal(start..self.pos, false)
                } else {
                    self.advance(terminator + 1);

                    Token::string_literal(start..self.pos - 1, true)
                }
            }
        }
    }

    pub fn _source_line_current(&self, path: Option<&str>) -> String {
        self._source_line_at_pos(self.pos, path)
    }

    pub fn _source_line_at_pos(&self, pos: usize, path: Option<&str>) -> String {
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
        ($lexer:expr, $token:expr, $text:literal) => {
            let token = $lexer.next_token();
            assert_eq!(token, $token);
            assert_eq!($text, $lexer.source_at(&token.span));
        };
    }

    macro_rules! assert_parse {
        ($source:literal, $token:expr, $text:literal) => {
            let mut lexer = Lexer::new($source);
            assert_next_token!(lexer, $token, $text);

            let pos = $token.span.start + $token.span.end;
            assert_next_token!(lexer, Token::end_of_file(pos..pos), "");
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
    //             TokenKind::EndOfFile => break,
    //             token => println!("{:?}", token),
    //         };
    //     }
    // }

    #[test]
    fn end_of_file() {
        let mut lexer = Lexer::new("");
        assert_next_token!(lexer, Token::end_of_file(0..0), "");
        assert_next_token!(lexer, Token::end_of_file(0..0), "");
        assert_next_token!(lexer, Token::end_of_file(0..0), "");

        let mut lexer = Lexer::new("test    ");
        assert_next_token!(lexer, Token::identifier(0..4), "test");
        assert_next_token!(lexer, Token::whitespace(4..8), "    ");
        assert_next_token!(lexer, Token::end_of_file(8..8), "");
        assert_next_token!(lexer, Token::end_of_file(8..8), "");
        assert_next_token!(lexer, Token::end_of_file(8..8), "");
    }

    #[test]
    fn skips_whitespace() {
        assert_parse!("", Token::end_of_file(0..0), "");

        let mut lexer = Lexer::new(" \t test \rtest2");
        assert_next_token!(lexer, Token::whitespace(0..3), " \t ");
        assert_next_token!(lexer, Token::identifier(3..7), "test");
        assert_next_token!(lexer, Token::whitespace(7..9), " \r");
        assert_next_token!(lexer, Token::identifier(9..14), "test2");
        assert_next_token!(lexer, Token::end_of_file(14..14), "");
    }

    #[test]
    fn comments() {
        let mut lexer = Lexer::new("comment ; this is a comment");
        assert_next_token!(lexer, Token::identifier(0..7), "comment");
        assert_next_token!(lexer, Token::whitespace(7..8), " ");
        assert_next_token!(lexer, Token::comment(8..27), "; this is a comment");
        assert_next_token!(lexer, Token::end_of_file(27..27), "");

        let mut lexer = Lexer::new("comment ; this is a comment with newline\nid");
        assert_next_token!(lexer, Token::identifier(0..7), "comment");
        assert_next_token!(lexer, Token::whitespace(7..8), " ");
        assert_next_token!(
            lexer,
            Token::comment(8..40),
            "; this is a comment with newline"
        );
        assert_next_token!(lexer, Token::new_line(40..41), "\n");
        assert_next_token!(lexer, Token::identifier(41..43), "id");
        assert_next_token!(lexer, Token::end_of_file(43..43), "");
    }

    #[test]
    fn string_literals() {
        let mut lexer = Lexer::new("  'a string literal ;; 123'\ntest");
        assert_next_token!(lexer, Token::whitespace(0..2), "  ");
        assert_next_token!(
            lexer,
            Token::string_literal(3..26, true),
            "a string literal ;; 123"
        );
        assert_next_token!(lexer, Token::new_line(27..28), "\n");
        assert_next_token!(lexer, Token::identifier(28..32), "test");
        assert_next_token!(lexer, Token::end_of_file(32..32), "");

        let mut lexer = Lexer::new("  'a string literal  ");
        assert_next_token!(lexer, Token::whitespace(0..2), "  ");
        assert_next_token!(
            lexer,
            Token::string_literal(3..21, false),
            "a string literal  "
        );
        assert_next_token!(lexer, Token::end_of_file(21..21), "");

        let mut lexer = Lexer::new("  'a string literal\n'  ");
        assert_next_token!(lexer, Token::whitespace(0..2), "  ");
        assert_next_token!(
            lexer,
            Token::string_literal(3..19, false),
            "a string literal"
        );
        assert_next_token!(lexer, Token::new_line(19..20), "\n");
        assert_next_token!(lexer, Token::string_literal(21..23, false), "  ");
    }

    #[test]
    fn number_literals() {
        // binary
        assert_parse!("11001000b", Token::number_literal(0..9, 200), "11001000b");
        assert_parse!(
            "0b11001000",
            Token::number_literal(0..10, 200),
            "0b11001000"
        );

        // octal
        assert_parse!("310o", Token::number_literal(0..4, 200), "310o");
        assert_parse!("0o310", Token::number_literal(0..5, 200), "0o310");

        // decimal
        assert_parse!("200", Token::number_literal(0..3, 200), "200");
        assert_parse!("0200", Token::number_literal(0..4, 200), "0200");
        assert_parse!("200d", Token::number_literal(0..4, 200), "200d");
        assert_parse!("0200d", Token::number_literal(0..5, 200), "0200d");
        assert_parse!("0d200", Token::number_literal(0..5, 200), "0d200");

        // hex
        assert_parse!("0c8h", Token::number_literal(0..4, 200), "0c8h");
        assert_parse!("0xc8", Token::number_literal(0..4, 200), "0xc8");

        // ensure we process the suffixes
        let mut lexer = Lexer::new("10d\n10h\n10b\n");
        assert_next_token!(lexer, Token::number_literal(0..3, 10), "10d");
        assert_next_token!(lexer, Token::new_line(3..4), "\n");
        assert_next_token!(lexer, Token::number_literal(4..7, 16), "10h");
        assert_next_token!(lexer, Token::new_line(7..8), "\n");
        assert_next_token!(lexer, Token::number_literal(8..11, 2), "10b");
        assert_next_token!(lexer, Token::new_line(11..12), "\n");
        assert_next_token!(lexer, Token::end_of_file(12..12), "");

        let mut lexer = Lexer::new(" 10 ");
        assert_next_token!(lexer, Token::whitespace(0..1), " ");
        assert_next_token!(lexer, Token::number_literal(1..3, 10), "10");
        assert_next_token!(lexer, Token::whitespace(3..4), " ");
        assert_next_token!(lexer, Token::end_of_file(4..4), "");

        let mut lexer = Lexer::new(" 0x10 010H 010 0X10 0x ");
        assert_next_token!(lexer, Token::whitespace(0..1), " ");
        assert_next_token!(lexer, Token::number_literal(1..5, 16), "0x10");
        assert_next_token!(lexer, Token::whitespace(5..6), " ");
        assert_next_token!(lexer, Token::number_literal(6..10, 16), "010H");
        assert_next_token!(lexer, Token::whitespace(10..11), " ");
        assert_next_token!(lexer, Token::number_literal(11..14, 10), "010");
        assert_next_token!(lexer, Token::whitespace(14..15), " ");
        assert_next_token!(lexer, Token::number_literal(15..16, 0), "0");
        assert_next_token!(lexer, Token::identifier(16..19), "X10");
        assert_next_token!(lexer, Token::whitespace(19..20), " ");
        assert_next_token!(lexer, Token::number_literal(20..21, 0), "0");
        assert_next_token!(lexer, Token::identifier(21..22), "x");
        assert_next_token!(lexer, Token::whitespace(22..23), " ");
        assert_next_token!(lexer, Token::end_of_file(23..23), "");
    }

    #[test]
    fn identifier() {
        let mut lexer = Lexer::new("test _te_st test123 1tst");
        assert_next_token!(lexer, Token::identifier(0..4), "test");
        assert_next_token!(lexer, Token::whitespace(4..5), " ");
        assert_next_token!(lexer, Token::identifier(5..11), "_te_st");
        assert_next_token!(lexer, Token::whitespace(11..12), " ");
        assert_next_token!(lexer, Token::identifier(12..19), "test123");
        assert_next_token!(lexer, Token::whitespace(19..20), " ");
        assert_next_token!(lexer, Token::number_literal(20..21, 1), "1");
        assert_next_token!(lexer, Token::identifier(21..24), "tst");
        assert_next_token!(lexer, Token::end_of_file(24..24), "");
    }
}
