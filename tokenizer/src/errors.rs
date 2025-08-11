
mod errors_impl;

pub trait PrettyPrint {
    fn pretty_print(&self, str: &str);
}

#[derive(Debug, Clone)]
pub struct TokenizerError {
    pub token_start_index: usize,
    pub token_end_index: usize,
    pub msg: String,
    pub file: Option<String>
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub start_index: usize,
    pub end_index: usize,
    pub msg: String,
    pub file: Option<String>
}