use proc_macro2::{Span, Ident};
use crate::bnf_decode::{Grammar, Lexeme, Production};
use quote::{format_ident, quote};
use crate::utility::capitalise_first;

pub fn build_enum_case(rule: &Vec<Production>) -> Vec<proc_macro2::TokenStream> {
    let mut cases = Vec::new();

    for (i, production) in rule.iter().enumerate() {
        let ident: Ident = Ident::new(&format!("Production{}", &i.to_string()), Span::call_site());
        let mut variants = Vec::new();
        for lexeme in &production.lexemes {
            match lexeme {
                Lexeme::NonTerminal(ident) => {
                    let ident = format_ident!("Ast{}", capitalise_first(ident));
                    variants.push(quote! {
                        #ident
                    });
                }
                Lexeme::Terminal(_) => {}
                Lexeme::Intrinsic(_) => {}
            }
        }
        cases.push(quote! {
            #ident(#(Box<#variants>),*)
        })
    }

    cases
}

pub fn build_keywords(grammar: &Grammar) -> proc_macro2::TokenStream {
    let mut keywords = Vec::new();

    for (_, rules) in &grammar.rules {
        for production in rules {
            for lexeme in &production.lexemes {
                match lexeme {
                    Lexeme::Intrinsic(_) => {}
                    Lexeme::NonTerminal(_) => {}
                    Lexeme::Terminal(terminal) => {
                        if terminal.len() > 1 {
                            let ident = syn::parse_str::<Ident>(&terminal);
                            match ident {
                                Ok(ident) => {
                                    keywords.push(ident);
                                }
                                Err(e) => {
                                    panic!("Unable to create keyword '{}'. Parser returned '{}'", terminal, e.to_string());
                                }
                            }
                            
                        }
                    }
                }
            }
        } 
    }
    quote!(
        enum AstKeywords {
            #(#keywords),*
        }
    )
} 