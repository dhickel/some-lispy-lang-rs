use std::iter::Peekable;
use std::str::Chars;
use crate::parse::{Lex, Lit, match_single_token, Op, Syn, Token, token, TokenData, TokenType};
use crate::parse::Lit::Identifier;
use crate::parse::TokenType::{Lexical, Literal, Syntactic, Operation};

struct LexicalState<'a> {
    pub tokens: Vec<Token>,
    pub line_num: i32,
    char_iter: Peekable<Chars<'a>>,
    pub curr_char: char,
    pub last_char: char,

}


impl<'a> LexicalState<'a> {
    pub fn new(source: &'a String) -> Self {
        let char_iter = source.chars().peekable();
        LexicalState {
            tokens: Vec::new(),
            line_num: 0,
            char_iter: char_iter,
            curr_char: '\0',
            last_char: '\0',
        }
    }

    pub fn have_next(&mut self) -> bool {
        return !self.char_iter.peek().is_none();
    }

    pub fn advance(&mut self) -> char {
        self.last_char = self.curr_char;
        self.curr_char = self.char_iter.next().expect("Advanced past end");
        self.curr_char
    }

    pub fn peek(&mut self) -> char {
        return *self.char_iter.peek().expect("Advanced past end");
    }

    pub fn add_token(&mut self, token_type: TokenType) -> bool {
        let token = Token {
            token_type,
            data: None,
            line: self.line_num,
        };
        self.tokens.push(token);
        true
    }

    pub fn add_token_data(&mut self, token_type: TokenType, data: TokenData) -> bool {
        let token = Token {
            token_type,
            data: Some(data),
            line: self.line_num,
        };
        self.tokens.push(token);
        true
    }
}


pub fn process(input: String) -> Result<Vec<Token>, String> {
    let mut state = LexicalState::new(&input);

    while state.have_next() {
        let c = state.advance();

        if matches!(c, ' ' | '\t' | '\r') { continue; }

        if c == '\n' {
            state.line_num += 1;
            continue;
        }

        if lex_double_token(&mut state) { continue; }
        if lex_single_token(&mut state) { continue; }
        if is_numeric(c) && lex_number_token(&mut state) { continue; }
        if lex_word_token(&mut state) { continue; }

        return Err(format!("Error while lexing line {}, char {}", state.line_num, c));
    }
    state.add_token(Lexical(Lex::EOF));
    Ok(state.tokens)
}


fn lex_double_token(state: &mut LexicalState) -> bool {
    if !state.have_next() { return false; }

    match token::match_double_token((state.curr_char, state.peek())) {
        None => false,
        Some(value) => {
            state.advance(); // TODO untested change might breal
            if matches!(value, Syntactic(Syn::Type)) {
                lex_type(state)
            } else { state.add_token(value) }
        }
    }
}

fn lex_single_token(state: &mut LexicalState) -> bool {
    match match_single_token(state.curr_char) {
        None => false,
        Some(value) => {
            if matches!(value, Syntactic(Syn::Ampersand)) {
                lex_modifier(state)
            } else if matches!(value, Lexical(Lex::DoubleQuote)) {
                lex_string_literal(state)
            } else { state.add_token(value) }
        }
    }
}

fn lex_word_token(state: &mut LexicalState) -> bool {
    let string_data = read_data_to_string(state);
    match token::match_word_token(&string_data) {
        None => state.add_token_data(Literal(Identifier), TokenData::String(string_data)),
        Some(value) => state.add_token_data(value, TokenData::String(string_data))
    }
}


fn lex_type(state: &mut LexicalState) -> bool {
    state.advance(); // skip the extra  :
    let type_string: String = read_data_to_string(state);
    state.add_token_data(Syntactic(Syn::Type), TokenData::String(type_string));
    true
}

fn lex_modifier(state: &mut LexicalState) -> bool {
    let mod_string: String = read_data_to_string(state);
    match token::match_modifier_token(&mod_string) {
        None => false,
        Some(value) => {
            state.add_token(value)
        }
    }
}

// TODO handle nested strings
fn lex_string_literal(state: &mut LexicalState) -> bool {
    let mut string_data = String::new();

    while state.have_next() && state.peek() != '"' {
        string_data.push(state.advance());
    }

    state.advance(); // Consume closing "
    state.add_token_data(Literal(Lit::String), TokenData::String(string_data));
    true
}

fn read_data_to_string(state: &mut LexicalState) -> String {
    let mut string_data = String::new();
    string_data.push(state.curr_char);

    let mut c: char = state.peek();
    while state.have_next() && !is_def_end(c) && is_alpha_numeric(c) {
        string_data.push(state.advance());
        c = state.peek()
    }
    string_data
}


fn lex_number_token(state: &mut LexicalState) -> bool {
    let mut is_float: bool = false;
    let mut is_neg: bool = false;

    if matches!(state.tokens.last().unwrap().token_type, Operation(Op::Minus)) {
        is_neg = true;
        state.tokens.remove(state.tokens.len() - 1);
    }

    let mut num_string = String::new();
    num_string.push(state.curr_char);

    if state.peek() == '.' {
        is_float = true;
        num_string.push(state.advance());
    }

    let mut c: char = state.peek();
    while state.have_next() && !is_def_end(c) && (is_numeric(c) || c == '.') {
        if c == '.' { is_float = true; }
        num_string.push(state.advance());
        c = state.peek();
    }

    if is_float {
        let number: f64 = num_string.parse::<f64>().expect("Failed parsing number");
        state.add_token_data(
            Literal(Lit::Float),
            TokenData::Float(if is_neg { -number } else { number }),
        )
    } else {
        let number: i64 = num_string.parse::<i64>().expect("Failed parsing number");
        state.add_token_data(
            Literal(Lit::Int),
            TokenData::Integer(if is_neg { -number } else { number }),
        )
    }
}


fn is_def_end(c: char) -> bool {
    matches!(c, ' ' | ')' | '(' | '\r' | '\n' | '\t')
}

fn is_numeric(c: char) -> bool {
    c.is_ascii_digit()
}

fn is_alpha(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '-')
}

fn is_alpha_numeric(c: char) -> bool {
    is_numeric(c) || is_alpha(c)
}