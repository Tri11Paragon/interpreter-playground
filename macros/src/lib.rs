use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse_macro_input;

mod utility;

mod build_repr;

#[proc_macro]
pub fn build_repr_bnf(input: TokenStream) -> TokenStream {
    build_repr::from_bnf(input)
}

#[proc_macro]
pub fn recursive_descent(input: TokenStream) -> TokenStream {
    // let grammar = parse_macro_input!(input);
    
    todo!()
}