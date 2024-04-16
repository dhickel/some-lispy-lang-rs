use std::iter::Peekable;
use std::str::Chars;
use crate::token::{Lex, Lit, Op, Syn, Token, TokenData, TokenType};
use crate::token::TokenType::{TLexical, TLiteral, TOperation, TSyntactic};
use crate::{token, util};
use crate::util::SCACHE;


struct LexicalState<'a> {
    pub tokens: Vec<Token>,
    pub line_num: i32,
    pub line_char: i32,
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
            line_char: 0,
            char_iter: char_iter,
            curr_char: '\0',
            last_char: '\0',
        }
    }

    pub fn have_next(&mut self) -> bool {
        return !self.char_iter.peek().is_none();
    }

    pub fn inc_line_num(&mut self) {
        self.line_num += 1;
        self.line_char = 0;
    }

    pub fn advance(&mut self) -> char {
        self.last_char = self.curr_char;
        self.curr_char = self.char_iter.next().expect("Advanced past end");
        self.line_char += 1;
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
            char: self.line_char,
        };
        self.tokens.push(token);
        true
    }

    pub fn add_token_data(&mut self, token_type: TokenType, data: TokenData) -> bool {
        let token = Token {
            token_type,
            data: Some(data),
            line: self.line_num,
            char: self.line_char,
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
            state.inc_line_num();
            continue;
        }

        if lex_double_token(&mut state) { continue; }
        if lex_single_token(&mut state) { continue; }
        if is_numeric(c) && lex_number_token(&mut state) { continue; }
        if lex_word_token(&mut state) { continue; }

        return Err(format!("Error while lexing line {}, char {}", state.line_num, c));
    }
    state.add_token(TLexical(Lex::EOF));
    Ok(state.tokens)
}


fn lex_double_token(state: &mut LexicalState) -> bool {
    if !state.have_next() { return false; }

    match token::match_double_token((state.curr_char, state.peek())) {
        None => false,
        Some(value) => {
            state.advance();
            state.add_token(value)
        }
    }
}


fn lex_single_token(state: &mut LexicalState) -> bool {
    match token::match_single_token(state.curr_char) {
        None => false,
        Some(value) => {
            match value {
                TLexical(Lex::DoubleQuote) => lex_string_literal(state),
                TSyntactic(Syn::Ampersand) => lex_modifier(state),
                TSyntactic(Syn::At) => lex_instance(state),
                TOperation(Op::Minus) => {
                    if state.peek().is_numeric() {
                        lex_number_token(state)
                    } else { state.add_token(value) }
                }
                _ => state.add_token(value)
            }
        }
    }
}


fn lex_word_token(state: &mut LexicalState) -> bool {
    let string_data = read_data_to_string(state);
    match token::match_word_token(util::SCACHE.resolve(string_data)) {
        None => state.add_token_data(TLiteral(Lit::Identifier), TokenData::String(string_data)),
        Some(value) => state.add_token_data(value, TokenData::String(string_data))
    }
}


fn lex_type(state: &mut LexicalState) -> bool {
    state.advance(); // skip the extra  :
    let type_string = read_data_to_string(state);
    state.add_token_data(TSyntactic(Syn::DoubleColon), TokenData::String(type_string));
    true
}


fn lex_modifier(state: &mut LexicalState) -> bool {
    let mod_string = read_data_to_string(state);
    match token::match_modifier_token(SCACHE.resolve(mod_string)) {
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

    let u64 = util::SCACHE.intern(string_data);
    state.advance(); // Consume closing "
    state.add_token_data(TLiteral(Lit::String), TokenData::String(u64));
    true
}


fn lex_instance(state: &mut LexicalState) -> bool {
    state.advance(); // skip @ symbol
    let instance_id = read_data_to_string(state);
    state.add_token_data(TLiteral(Lit::Instance), TokenData::String(instance_id));
    true
}

// fn lex_quote(state: &mut LexicalState) {
//     let mut data = String::new();
//     if state.peek() == '(' {
//         let mut depth = 1;
//         state.advance(); // skip known paren
//         while depth != 0 && state.have_next() {
//             if state.peek() == '(' { depth += 1; }
//             if state.peek() == ')' { depth -= 1; }
//             data.push(state.peek());
//             state.advance();
//         }
//     } else {
//         data = read_data_to_string(state);
//     }
//     state.add_token_data(Literal(Lit::Quote), TokenData::String(data));
// }

fn read_data_to_string(state: &mut LexicalState) -> u64 {
    let mut string_data = String::new();
    string_data.push(state.curr_char);
    
    while state.have_next() {
        let c = state.peek();
        if !is_def_end(c) && is_alpha_numeric(c) {
            string_data.push(state.advance());
        } else { break; }
    }
    util::SCACHE.intern(string_data)
}


fn lex_number_token(state: &mut LexicalState) -> bool {
    let mut is_float: bool = false;
    let mut is_neg: bool = false;

    if state.curr_char == '-' {
        is_neg = true;
        state.advance();
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
            TLiteral(Lit::Float),
            TokenData::Float(if is_neg { -number } else { number }),
        )
    } else {
        let number: i64 = num_string.parse::<i64>().expect("Failed parsing number");
        state.add_token_data(
            TLiteral(Lit::Int),
            TokenData::Integer(if is_neg { -number } else { number }),
        )
    }
}


fn is_def_end(c: char) -> bool {
    matches!(c, ' ' | ')' | '(' | '[' | ']' | '\r' | '\n' | '\t')
}


fn is_numeric(c: char) -> bool {
    c.is_ascii_digit()
}


fn is_alpha(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '-' )
}


fn is_alpha_numeric(c: char) -> bool {
    is_numeric(c) || is_alpha(c)
}