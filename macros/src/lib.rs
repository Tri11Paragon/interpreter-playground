use crate::bnf_decode::{Lexeme, Rule};
use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::__private::Span;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{LitStr, Token, parse_macro_input};

mod bnf_decode;
fn capitalise_first(s: &Ident) -> Ident {
    let s = s.to_string();
    let mut chars = s.chars();
    Ident::new(
        &match chars.next() {
            Some(first) => first.to_uppercase().chain(chars).collect(),
            None => String::new(),
        },
        Span::call_site(),
    )
}

fn build_enum_case(rule: &Vec<Rule>) -> Vec<proc_macro2::TokenStream> {
    let mut cases = Vec::new();

    for (i, production) in rule.productions.iter().enumerate() {
        let ident: Ident = Ident::new(&format!("Production{}", &i.to_string()), Span::call_site());
        let mut variants = Vec::new();
        for lexeme in &production.lexemes {
            match lexeme {
                Lexeme::NonTerminal(ident) => {
                    let ident = format_ident!("Ast{}", ident);
                    variants.push(quote! {
                        #ident
                    });
                }
                Lexeme::Terminal(string) => {}
            }
        }
        cases.push(quote! {
            #ident(#(Box<#variants>),*)
        })
    }

    cases
}

#[proc_macro]
pub fn from_bnf(input: TokenStream) -> TokenStream {
    let bnf_decode::Grammar { rules } = parse_macro_input!(input as bnf_decode::Grammar);

    let mut parser_enums = Vec::new();

    for (identifier, rule) in rules {
        let ident_str = format_ident!("Ast{}", capitalise_first(&identifier));
        let enum_data = build_enum_case(&rule);
        parser_enums.push(quote! {
            enum #ident_str {
                #(#enum_data),*
            }
        })
    }

    quote! {
        #(#parser_enums)*
    }
    .into()
}
