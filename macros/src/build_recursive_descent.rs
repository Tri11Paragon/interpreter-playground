use crate::build_repr::bnf;
use crate::build_repr::bnf::{Group, Repetition};
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

fn process_group(group: &Group) -> TokenStream {
    match group {
        Group::Single(single) => {

        }
        Group::AnyOf(vec) => {
            for rep in vec {
                process_repetition(rep);
            }
        }
    }
}

fn process_repetition(repetition: &Repetition) -> TokenStream {
    match repetition {
        Repetition::Once(group) => {
            process_group(group)
        }
        Repetition::ZeroOrOnce(group) => {
            process_group(group)
        }
        Repetition::AtLeastOne(group) => {
            process_group(group)
        }
        Repetition::ZeroOrMore(group) => {
            process_group(group)
        }
    }
}

pub fn build_recursive_descent(input: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(input as bnf::Grammar);

    for (ident, productions) in grammar.rules {
        for production in productions {
            for repetition in production.repetitions {
                process_repetition(&repetition);
            }
        }
    }

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
