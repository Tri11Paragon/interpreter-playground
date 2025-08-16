use proc_macro2::Ident;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};
use syn::{LitStr, Token};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Grammar {
    pub rules: HashMap<Ident, Vec<Production>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Production {
    pub lexemes: Vec<Lexeme>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Lexeme {
    NonTerminal(Ident),
    Terminal(String),
    Intrinsic(Ident),
}

trait Produces {
    fn produces(&self, ident: &Ident) -> bool;
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

impl Grammar {
    pub fn find_roots(&self) -> Vec<&Ident> {
        let mut roots = Vec::new();
        let mut idents = Vec::new();
        for ident in self.rules.keys() {
            idents.push(ident)
        }
        for (i, productions) in &self.rules {
            let mut none_produces = true;
            for ident in &idents {
                if productions.produces(ident) {
                    none_produces = false;
                    break;
                }
            }
            if none_produces {
                roots.push(i);
            }
        }
        roots
    }
}

impl Parse for Production {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut lexemes = Vec::new();
        while !(input.peek(Token![|]) || input.peek(Token![;])) {
            if input.peek(LitStr) {
                let lit: LitStr = input.parse()?;
                lexemes.push(Lexeme::Terminal(lit.value()));
            } else if input.peek(Token![$]) {
                input.parse::<Token![$]>()?;
                let ident: Ident = input.parse()?;
                lexemes.push(Lexeme::Intrinsic(ident));
            } else {
                let ident: Ident = input.parse()?;
                lexemes.push(Lexeme::NonTerminal(ident));
            }
        }
        Ok(Production { lexemes })
    }
}

impl Produces for Production {
    fn produces(&self, ident: &Ident) -> bool {
        for lexeme in &self.lexemes {
            match (lexeme) {
                Lexeme::NonTerminal(production) => {
                    if production == ident {
                        return true;
                    }
                }
                Lexeme::Terminal(_) => {}
                Lexeme::Intrinsic(_) => {}
            }
        }
        false
    }
}

impl Produces for Vec<Production> {
    fn produces(&self, ident: &Ident) -> bool {
        for production in self {
            if production.produces(ident) {
                return true;
            }
        }
        false
    }
}