use crate::errors::{ParserError, TokenizerError};
use crate::errors::PrettyPrint;
use crate::tokenizer::Token;
use crate::tokenizer::Keyword;

impl ParserError {
    pub fn new<Keywords: Keyword>(msg: &str, token: &Token<Keywords>) -> Self {
        Self {
            start_index: token.start_index,
            end_index: token.end_index,
            msg: String::from(msg),
            file: token.file.clone()
        }
    }

    pub fn eof(file: Option<String>) -> Self {
        Self {
            start_index: 0,
            end_index: 0,
            msg: String::from("Reached EOF before finishing parse"),
            file
        }
    }
}

pub fn can_see(c: char) -> bool {
    !(c.is_control() || c.is_whitespace() && c != ' ')
}

pub fn context(str: &str, start: usize) -> &str {
    let start = start as isize;
    let mut amount_left = 0usize;
    let mut amount_right = 0usize;

    for c in str[start as usize..].chars() {
        if can_see(c) {
            amount_right += c.len_utf8();
        } else {
            break;
        }
    }

    for c in str[..start as usize].chars().rev() {
        if can_see(c) {
            amount_left += c.len_utf8();
        } else {
            break;
        }
    }

    let begin_index = core::cmp::max(start - amount_left as isize, 0) as usize;
    let end_index = core::cmp::min(start + amount_right as isize, str.len() as isize - 1) as usize;

    &str[begin_index..end_index]
}

pub fn count_lines(str: &str, end: usize) -> usize {
    let mut size = 0;
    for c in str[0..end].chars() {
        match c {
            '\n' => size += 1,
            _ => continue,
        }
    }
    size
}

pub fn count_column(str: &str, end: usize) -> usize {
    let mut size = 0;
    for c in str[0..end].chars().rev() {
        match c {
            '\n' => break,
            _ => size += 1,
        }
    }
    size
}

fn format_string(str: &str) -> String {
    let mut ret = String::new();
    ret.reserve(str.len());
    for c in str.chars() {
        if can_see(c) {
            ret.push(c);
        } else {
            ret.push_str(&format!("\\u{{{:04X}}}", c as u32))
        }
    }
    ret
}
impl PrettyPrint for Vec<TokenizerError> {
    fn pretty_print(&self, str: &str) {
        for error in self {
            let ctx = context(str, error.token_start_index);
            let line = count_lines(str, error.token_start_index);
            let column = count_column(str, error.token_start_index);
            println!(
                "An error occurred, unrecognized symbol '{}' at {}:{} in file {}",
                format_string(&str[error.token_start_index..error.token_end_index]),
                line,
                column,
                error.file.clone().unwrap_or_default()
            );
            println!("  Near: '{}'", format_string(ctx));
            println!("  Context: {}", error.msg)
        }
    }
}

impl PrettyPrint for Vec<ParserError> {
    fn pretty_print(&self, str: &str) {
        for error in self {
            let ctx = context(str, error.start_index);
            let line = count_lines(str, error.start_index);
            let column = count_column(str, error.start_index);
            println!(
                "An error occurred. I do not understand '{}' at {}:{} in file {}",
                format_string(&str[error.start_index..error.end_index]),
                line,
                column,
                error.file.clone().unwrap_or_default()
            );
            println!("  Near '{}'", format_string(ctx));
            println!("  Context: {}", error.msg)
        }
    }
}
