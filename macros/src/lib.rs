use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse_macro_input;

mod bnf_decode;
mod enums;
mod utility;

#[proc_macro]
pub fn from_bnf(input: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(input as bnf_decode::Grammar);

    let mut parser_enums = Vec::new();
    let mut enum_names = Vec::new();

    for (identifier, rule) in &grammar.rules {
        let ident_str = format_ident!("Ast{}", utility::capitalise_first(&identifier));
        enum_names.push(ident_str.clone());
        let enum_data = enums::build_enum_case(&rule);
        parser_enums.push(quote! {
            enum #ident_str {
                #(#enum_data),*
            }
        })
    }

    let keywords = enums::build_keywords(&grammar);

    quote! {

        #(#parser_enums)*
        enum AstVariant {
            #(#enum_names(#enum_names)),*
        }

        #keywords

        struct Span {
            start_index: usize,
            end_index: usize,
            file: ::std::option::Option<String>
        }

        impl<Keywords: ::tokenizer::tokenizer::Keyword> From<::tokenizer::tokenizer::Token<Keywords>> for Span {
            fn from(token: ::tokenizer::tokenizer::Token<Keywords>) -> Self {
                Self {
                    start_index: token.start_index,
                    end_index: token.end_index,
                    file: token.file
                }
            }
        }

        struct AstToken {
            token_type: AstVariant,
            span: Span,
        }

        mod parser {
            use ::tokenizer::tokenizer::Keyword;

            type Lexeme<Keywords: Keyword> = ::tokenizer::tokenizer::Lexeme<Keywords>;
            type Token<Keywords: Keyword> = ::tokenizer::tokenizer::Token<Keywords>;
            type ParserError = ::tokenizer::errors::ParserError;
            type HashMap<K, V> = ::std::collections::HashMap<K, V>;

            enum ParseMapImpl<Keywords: Keyword> {
                Action(fn(&Vec<Token<Keywords>>) -> ()),
                Map(*mut ParseMap<Keywords>),
            }

            struct ParseMap<Keywords: Keyword> {
                tokens: Vec<Token<Keywords>>,
                map: HashMap<Lexeme<Keywords>, ParseMapImpl<Keywords>>,
            }

            impl<Keywords: Keyword> ParseMap<Keywords> {
                pub fn new() -> Self {
                    Self {
                        tokens: Vec::new(),
                        map: HashMap::new(),
                    }
                }

                pub fn from_map(map: HashMap<Lexeme<Keywords>, ParseMapImpl<Keywords>>) -> Self {
                    Self {
                        tokens: Vec::new(),
                        map,
                    }
                }

                pub fn associate(&mut self, lexeme: Lexeme<Keywords>, result: ParseMapImpl<Keywords>) {
                    self.map.insert(lexeme, result);
                }

                pub fn parse<'b>(&mut self, parser: &mut Parser<'b, Keywords>) {
                    if !parser.has_token() {
                        parser.errors.push(ParserError::eof(parser.file.clone()));
                        return;
                    }
                    if self.map.contains_key(&parser.peek_token().token_type) {
                        let next_action = &self.map[&parser.peek_token().token_type];
                        self.tokens.push(parser.next_token().clone());
                        match *next_action {
                            ParseMapImpl::Action(func) => {
                                (func)(&self.tokens);
                            }
                            ParseMapImpl::Map(map) => unsafe {
                                (*map).tokens = self.tokens.clone();
                                (*map).parse(parser);
                            },
                        }
                    }
                }
            }

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
        }
    }
    .into()
}
