use proc_macro2::Ident;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};
use syn::{LitStr, Token};

#[derive(Debug)]
pub struct Grammar {
    pub rules: HashMap<Ident, Vec<Rule>>,
}

#[derive(Debug)]
pub struct Rule {
    pub productions: Vec<Production>,
}

#[derive(Debug)]
pub struct Production {
    pub lexemes: Vec<Lexeme>,
}

#[derive(Debug)]
pub enum Lexeme {
    NonTerminal(Ident),
    Terminal(String),
}

impl Parse for Grammar {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut rules: HashMap<Ident, Vec<Rule>> = HashMap::new();

        while !input.is_empty() {
            let identifier: Ident = input.parse()?;
            rules.entry(identifier).or_default().push(input.parse()?);
        }

        Ok(Grammar { rules })
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![->]>()?;

        let mut productions = Vec::new();
        productions.push(input.parse()?);

        while input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
            productions.push(input.parse()?);
        }
        input.parse::<Token![;]>()?;

        Ok(Rule {
            productions,
        })
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
                let ident: Ident = input.parse()?;
                lexemes.push(Lexeme::NonTerminal(ident));
            }
        }
        Ok(Production { lexemes })
    }
}
