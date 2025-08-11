use proc_macro2::{Ident};
use syn::spanned::Spanned;

pub fn capitalise_first(s: &Ident) -> Ident {
    let s = s.to_string();
    let mut chars = s.chars();
    Ident::new(
        &match chars.next() {
            Some(first) => first.to_uppercase().chain(chars).collect(),
            None => String::new(),
        },
        s.span(),
    )
}