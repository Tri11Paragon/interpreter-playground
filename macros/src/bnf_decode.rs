use proc_macro2::Ident;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};
use syn::{LitStr, Token};

#[derive(Debug)]
pub struct Grammar {
    pub rules: HashMap<Ident, Vec<Production>>,
}

#[derive(Debug)]
pub struct Production {
    pub lexemes: Vec<Lexeme>,
}

#[derive(Debug)]
pub enum Lexeme {
    NonTerminal(Ident),
    Terminal(String),
    Intrinsic(Ident)
}

impl Parse for Grammar {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut rules: HashMap<Ident, Vec<Production>> = HashMap::new();

        while !input.is_empty() {
            let identifier: Ident = input.parse()?;
            input.parse::<Token![->]>()?;
            let productions = rules.entry(identifier).or_default();
            productions.push(input.parse()?);
            while input.peek(Token![|]) {
                input.parse::<Token![|]>()?;
                productions.push(input.parse()?);
            }
            input.parse::<Token![;]>()?;
        }

        Ok(Grammar { rules })
    }
}

impl Parse for Production {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut lexemes = Vec::new();
        while !(input.peek(Token![|]) || input.peek(Token![;])) {
            if input.peek(LitStr) {
                let lit: LitStr = input.parse()?;
                lexemes.push(Lexeme::Terminal(lit.value()));
            } else {
                if input.peek(Token![$]) {
                    input.parse::<Token![$]>()?;
                    let ident: Ident = input.parse()?;
                    lexemes.push(Lexeme::Intrinsic(ident));
                } else {
                    let ident: Ident = input.parse()?;
                    lexemes.push(Lexeme::NonTerminal(ident));
                }
            }
        }
        Ok(Production { lexemes })
    }
}
