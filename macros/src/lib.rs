use crate::bnf_decode::{Lexeme, Rule};
use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::__private::Span;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{LitStr, Token, parse_macro_input};

mod bnf_decode;

fn build_enum_case(rule: &Rule) -> Vec<proc_macro2::TokenStream> {
    let mut cases = Vec::new();

    for (i, production) in rule.productions.iter().enumerate() {
        let ident: Ident = Ident::new(&format!("Production{}", &i.to_string()), Span::call_site());
        let mut variants = Vec::new();
        for lexeme in &production.lexemes {
            match lexeme {
                Lexeme::NonTerminal(ident) => {
                    let ident = format_ident!("Parser{}", ident);
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

    for rule in rules {
        let ident_str = format_ident!("Parser{}", rule.identifier);
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
