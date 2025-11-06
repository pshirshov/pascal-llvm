/// Lexer for Pascal-like language
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Integer(i32),
    Real(f64),
    Char(char),
    String(String),
    Ident(String),

    // Keywords
    Program,
    Var,
    Val,
    Type,
    Def,
    End,
    If,
    Else,
    While,
    For,
    To,
    Of,
    Array,
    Record,

    // Types
    TInteger,
    TReal,
    TBoolean,
    TChar,
    TString,

    // Boolean literals
    True,
    False,

    // Operators
    And,
    Or,
    Not,
    Div,
    Mod,

    // I/O
    Writeln,
    Write,
    Readln,
    New,
    Return,

    // Symbols
    LParen,      // (
    RParen,      // )
    LBrack,      // [
    RBrack,      // ]
    LBrace,      // {
    RBrace,      // }
    Dot,         // .
    Comma,       // ,
    Colon,       // :
    Semicolon,   // ;
    Assign,      // :=
    DotDot,      // ..

    // Comparison
    Eq,          // =
    Ne,          // <>
    Lt,          // <
    Le,          // <=
    Gt,          // >
    Ge,          // >=

    // Arithmetic
    Plus,        // +
    Minus,       // -
    Star,        // *
    Slash,       // /
    Caret,       // ^
    At,          // @

    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Integer(n) => write!(f, "INTEGER({})", n),
            Token::Real(r) => write!(f, "REAL({})", r),
            Token::Char(c) => write!(f, "CHAR('{}')", c),
            Token::String(s) => write!(f, "STRING(\"{}\")", s),
            Token::Ident(s) => write!(f, "IDENT({})", s),
            _ => write!(f, "{:?}", self),
        }
    }
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();
        let current_char = chars.get(0).copied();
        Lexer {
            input: chars,
            position: 0,
            current_char,
        }
    }

    fn advance(&mut self) {
        self.position += 1;
        self.current_char = self.input.get(self.position).copied();
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.input.get(self.position + offset).copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        // Handle // style comments
        if self.current_char == Some('/') && self.peek(1) == Some('/') {
            while self.current_char.is_some() && self.current_char != Some('\n') {
                self.advance();
            }
            if self.current_char == Some('\n') {
                self.advance();
            }
            return;
        }

        // Handle /* */ style comments
        if self.current_char == Some('/') && self.peek(1) == Some('*') {
            self.advance(); // skip /
            self.advance(); // skip *
            while self.current_char.is_some() {
                if self.current_char == Some('*') && self.peek(1) == Some('/') {
                    self.advance(); // skip *
                    self.advance(); // skip /
                    break;
                }
                self.advance();
            }
        }
    }

    fn read_number(&mut self) -> Token {
        let mut num_str = String::new();
        let mut is_real = false;

        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.advance();
            } else if ch == '.' && self.peek(1).map_or(false, |c| c.is_ascii_digit()) {
                is_real = true;
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if is_real {
            Token::Real(num_str.parse().unwrap())
        } else {
            Token::Integer(num_str.parse().unwrap())
        }
    }

    fn read_identifier(&mut self) -> Token {
        let mut ident = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check if it's a keyword
        match ident.to_lowercase().as_str() {
            "program" => Token::Program,
            "var" => Token::Var,
            "val" => Token::Val,
            "type" => Token::Type,
            "def" => Token::Def,
            "end" => Token::End,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "to" => Token::To,
            "of" => Token::Of,
            "array" => Token::Array,
            "record" => Token::Record,
            "integer" => Token::TInteger,
            "real" => Token::TReal,
            "boolean" => Token::TBoolean,
            "char" => Token::TChar,
            "string" => Token::TString,
            "true" => Token::True,
            "false" => Token::False,
            "and" => Token::And,
            "or" => Token::Or,
            "not" => Token::Not,
            "div" => Token::Div,
            "mod" => Token::Mod,
            "writeln" => Token::Writeln,
            "write" => Token::Write,
            "readln" => Token::Readln,
            "new" => Token::New,
            "return" => Token::Return,
            _ => Token::Ident(ident),
        }
    }

    fn read_string(&mut self) -> Token {
        self.advance(); // skip opening '
        let mut string = String::new();

        while let Some(ch) = self.current_char {
            if ch == '\'' {
                self.advance(); // skip closing '
                break;
            }
            string.push(ch);
            self.advance();
        }

        if string.len() == 1 {
            Token::Char(string.chars().next().unwrap())
        } else {
            Token::String(string)
        }
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            self.skip_whitespace();

            if self.current_char.is_none() {
                return Token::Eof;
            }

            // Try to skip comments
            let pos_before = self.position;
            self.skip_comment();
            if self.position != pos_before {
                continue; // Comment was skipped, check for whitespace again
            }

            match self.current_char.unwrap() {
                ch if ch.is_ascii_digit() => return self.read_number(),
                ch if ch.is_alphabetic() => return self.read_identifier(),
                '\'' => return self.read_string(),
                '(' => {
                    self.advance();
                    return Token::LParen;
                }
                ')' => {
                    self.advance();
                    return Token::RParen;
                }
                '[' => {
                    self.advance();
                    return Token::LBrack;
                }
                ']' => {
                    self.advance();
                    return Token::RBrack;
                }
                '{' => {
                    self.advance();
                    return Token::LBrace;
                }
                '}' => {
                    self.advance();
                    return Token::RBrace;
                }
                '.' => {
                    if self.peek(1) == Some('.') {
                        self.advance();
                        self.advance();
                        return Token::DotDot;
                    }
                    self.advance();
                    return Token::Dot;
                }
                ',' => {
                    self.advance();
                    return Token::Comma;
                }
                ':' => {
                    if self.peek(1) == Some('=') {
                        self.advance();
                        self.advance();
                        return Token::Assign;
                    }
                    self.advance();
                    return Token::Colon;
                }
                ';' => {
                    self.advance();
                    return Token::Semicolon;
                }
                '=' => {
                    self.advance();
                    return Token::Eq;
                }
                '<' => {
                    if self.peek(1) == Some('>') {
                        self.advance();
                        self.advance();
                        return Token::Ne;
                    } else if self.peek(1) == Some('=') {
                        self.advance();
                        self.advance();
                        return Token::Le;
                    }
                    self.advance();
                    return Token::Lt;
                }
                '>' => {
                    if self.peek(1) == Some('=') {
                        self.advance();
                        self.advance();
                        return Token::Ge;
                    }
                    self.advance();
                    return Token::Gt;
                }
                '+' => {
                    self.advance();
                    return Token::Plus;
                }
                '-' => {
                    self.advance();
                    return Token::Minus;
                }
                '*' => {
                    self.advance();
                    return Token::Star;
                }
                '/' => {
                    self.advance();
                    return Token::Slash;
                }
                '^' => {
                    self.advance();
                    return Token::Caret;
                }
                '@' => {
                    self.advance();
                    return Token::At;
                }
                _ => {
                    panic!("Unexpected character: {}", self.current_char.unwrap());
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token == Token::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("program Test; var x: integer;");
        assert_eq!(lexer.next_token(), Token::Program);
        assert_eq!(lexer.next_token(), Token::Ident("Test".to_string()));
        assert_eq!(lexer.next_token(), Token::Semicolon);
        assert_eq!(lexer.next_token(), Token::Var);
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Colon);
        assert_eq!(lexer.next_token(), Token::TInteger);
        assert_eq!(lexer.next_token(), Token::Semicolon);
    }

    #[test]
    fn test_inline_declarations() {
        let mut lexer = Lexer::new("var x: integer := 10; val y: integer = 20;");
        assert_eq!(lexer.next_token(), Token::Var);
        lexer.next_token(); // x
        lexer.next_token(); // :
        lexer.next_token(); // integer
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Integer(10));
        lexer.next_token(); // ;
        assert_eq!(lexer.next_token(), Token::Val);
    }
}
