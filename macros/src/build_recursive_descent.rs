use proc_macro::TokenStream;
use std::collections::{HashMap, HashSet};
use std::slice::Iter;
use proc_macro2::Ident;
use quote::quote;
use syn::parse_macro_input;
use crate::build_recursive_descent::basic_bnf::Lexeme;

mod basic_bnf;

struct ParseState {
    grammar: basic_bnf::Grammar,

}

fn what(production_iters: &mut HashMap<&Ident, Vec<Iter<basic_bnf::Lexeme>>>) {
    for (ident, productions) in production_iters {
        let mut vec = HashMap::new();
        for production in productions {
            if let Some(v) = production.next() {
                let g = vec.entry(v).or_insert(Vec::new());
                g.push(production)
            }
        }

    }
}

pub fn build_recursive_descent(input: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(input as basic_bnf::Grammar);

    let roots = grammar.find_roots();

    let mut production_iters = HashMap::new();
    for (ident, productions) in &grammar.rules {
        for production in productions {
            let e = production_iters.entry(ident).or_insert(Vec::new());
            e.push(production.lexemes.iter());
        }
    }

    what(&mut production_iters);


    quote! {
        pub struct Parser<'a, Keywords: Keyword> {
            tokens: &'a Vec<Token<Keywords>>,
            errors: Vec<ParserError>,
            current_token: usize,
            file: Option<String>,
        }

        impl<'a, Keywords: Keyword> Parser<'a, Keywords> {
            pub fn new(tokens: &'a Vec<Token<Keywords>>, file: Option<String>) -> Self {
                Self {
                    errors: Vec::new(),
                    tokens,
                    current_token: 0,
                    file,
                }
            }

            pub fn peek_token(&mut self) -> &Token<Keywords> {
                &self.tokens[self.current_token]
            }

            pub fn next_token(&mut self) -> &Token<Keywords> {
                let token = &self.tokens[self.current_token];
                self.advance();
                token
            }

            pub fn has_token(&self) -> bool {
                self.current_token < self.tokens.len()
            }

            pub fn advance(&mut self) {
                self.current_token += 1;
            }

            pub fn sync_statement(&mut self) {
                while (self.peek_token().token_type != Lexeme::Semicolon) {
                    self.next_token();
                }
            }

            pub fn sync_scope(&mut self) {
                while (self.peek_token().token_type != Lexeme::CloseCurly) {
                    self.next_token();
                }
            }
        }

        trait Parse<Keywords: keyword> {
            type AstNode;
            fn parse(&self, &mut Parser<Keywords>) -> AstNode;
        }
    }
    .into()
}
