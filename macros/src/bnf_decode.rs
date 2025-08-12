use proc_macro2::Ident;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};
use syn::{LitStr, Token};

#[derive(Debug, Clone)]
pub struct Grammar {
    pub rules: HashMap<Ident, Vec<Production>>,
}

#[derive(Debug, Clone)]
pub struct Production {
    pub repetitions: Vec<Repetition>,
}

#[derive(Debug, Clone)]
pub enum Repetition {
    Once(Group),        // singular token (RULE)
    ZeroOrOnce(Group),  // (RULE)?
    AtLeastOne(Group),  // (RULE)+
    ZeroOrMore(Group)   // (RULE)*
}

#[derive(Debug, Clone)]
pub enum Group {
    Single(Lexeme), // Internal Lexeme (LEX)
    AnyOf(Vec<Repetition>) // group inside group (SILLY | (SILLY | BILLY)*)* or series of lexemes
}

#[derive(Debug, Clone)]
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
    pub fn find_roots(&self) -> Vec<String> {
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
                roots.push(i.to_string());
            }
        }
        roots
    }
}

impl Parse for Production {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut repetitions = Vec::new();
        while !input.peek(Token![;]) {

            // if input.peek(LitStr) {
            //     let lit: LitStr = input.parse()?;
            //     lexemes.push(Lexeme::Terminal(lit.value()));
            // } else if input.peek(Token![$]) {
            //     input.parse::<Token![$]>()?;
            //     let ident: Ident = input.parse()?;
            //     lexemes.push(Lexeme::Intrinsic(ident));
            // } else {
            //     let ident: Ident = input.parse()?;
            //     lexemes.push(Lexeme::NonTerminal(ident));
            // }
        }
        Ok(Production { repetitions })
    }
}

impl Parse for Repetition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let group: Group = input.parse()?;
        if input.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            Ok(Repetition::ZeroOrMore(group))
        } else if input.peek(Token![+]) {
            input.parse::<Token![+]>()?;
            Ok(Repetition::AtLeastOne(group))
        } else if input.peek(Token![?]) {
            input.parse::<Token![?]>()?;
            Ok(Repetition::ZeroOrOnce(group))
        } else {
            Ok(Repetition::Once(group))
        }
    }
}

impl Parse for Group {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        todo!()
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