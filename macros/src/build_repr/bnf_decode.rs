use proc_macro2::Ident;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};
use syn::{LitStr, Token, parenthesized};

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
    Once(Group),       // singular token (RULE)
    ZeroOrOnce(Group), // (RULE)?
    AtLeastOne(Group), // (RULE)+
    ZeroOrMore(Group), // (RULE)*
}

#[derive(Debug, Clone)]
pub enum Group {
    Single(Lexeme),         // Internal Lexeme (LEX)
    AnyOf(Vec<Repetition>), // group inside group (SILLY | (SILLY | BILLY)*)* or series of lexemes
}

pub enum IntrinsicType {
    Identifier,
    Decimal,
    Integer,
}

#[derive(Debug, Clone)]
pub enum Lexeme {
    NonTerminal(Ident),
    Terminal(String),
    Intrinsic(Ident),
}

impl Lexeme {
    pub fn get_intrinsic_type(ident: &Ident) -> IntrinsicType {
        let str = ident.to_string().to_ascii_uppercase();
        match &str[..] {
            "IDENTIFIER" => IntrinsicType::Identifier,
            "DECIMAL" => IntrinsicType::Decimal,
            "INTEGER" => IntrinsicType::Integer,
            _ => panic!("Unrecognized Intrinsic {}", str),
        }
    }
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
        while !(input.peek(Token![|]) || input.peek(Token![;])) {
            repetitions.push(input.parse()?);
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
        if input.peek(syn::token::Paren) {
            let content;
            parenthesized!(content in input);
            let mut vec = Vec::new();
            while !content.is_empty() {
                vec.push(content.parse()?);
            }
            Ok(Group::AnyOf(vec))
        } else {
            Ok(Group::Single(input.parse()?))
        }
    }
}

impl Parse for Lexeme {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            let lit: LitStr = input.parse()?;
            Ok(Lexeme::Terminal(lit.value()))
        } else if input.peek(Token![$]) {
            input.parse::<Token![$]>()?;
            let ident: Ident = input.parse()?;
            Ok(Lexeme::Intrinsic(ident))
        } else {
            let ident: Ident = input.parse()?;
            Ok(Lexeme::NonTerminal(ident))
        }
    }
}

impl Produces for Group {
    fn produces(&self, ident: &Ident) -> bool {
        match self {
            Group::Single(lexeme) => match (lexeme) {
                Lexeme::NonTerminal(production) => {
                    if production == ident {
                        return true;
                    }
                    false
                }
                Lexeme::Terminal(_) => false,
                Lexeme::Intrinsic(_) => false,
            },
            Group::AnyOf(repetitions) => {
                for repetition in repetitions {
                    if repetition.produces(ident) {
                        return true;
                    }
                }
                false
            }
        }
    }
}

impl Produces for Repetition {
    fn produces(&self, ident: &Ident) -> bool {
        match self {
            Repetition::ZeroOrMore(group) => group.produces(ident),
            Repetition::AtLeastOne(group) => group.produces(ident),
            Repetition::Once(group) => group.produces(ident),
            Repetition::ZeroOrOnce(group) => group.produces(ident),
        }
    }
}

impl Produces for Production {
    fn produces(&self, ident: &Ident) -> bool {
        for repetition in &self.repetitions {
            if repetition.produces(ident) {
                return true;
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
