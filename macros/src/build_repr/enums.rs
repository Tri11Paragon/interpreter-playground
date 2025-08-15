use crate::build_repr::bnf_decode::{
    Grammar, Group, IntrinsicType, Lexeme, Production, Repetition,
};
use crate::utility::capitalise_first;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};

pub struct NonEmptyVec<T> {
    vec: Vec<T>,
}

fn process_lexeme(lexeme: &Lexeme) -> Option<TokenStream> {
    match lexeme {
        Lexeme::NonTerminal(ident) => {
            let ident = format_ident!("Ast{}", capitalise_first(ident));
            Some(quote! {
                #ident
            })
        }
        Lexeme::Terminal(_) => None,
        Lexeme::Intrinsic(ident) => match Lexeme::get_intrinsic_type(ident) {
            IntrinsicType::Identifier => Some(quote! {
                String
            }),
            IntrinsicType::Decimal => Some(quote! {
                String
            }),
            IntrinsicType::Integer => Some(quote! {
                String
            }),
        },
    }
}

fn create_sub_repetition(rep: &Vec<Repetition>, extra_classes: &mut Vec<TokenStream>) -> Ident {
    let mut vec = Vec::new();
    for rep in rep {
        process_repetition(&mut vec, extra_classes, rep);
    }
    let vec = vec
        .iter()
        .enumerate()
        .map(|(i, t)| {
            let ident = format_ident!("Production{}", i);
            quote! {
                #ident(#t)
            }
        })
        .collect::<Vec<_>>();
    let name = format_ident!("RepetitionImpl{}", extra_classes.len());
    extra_classes.push(quote! {
        enum #name {
            #(#vec),*
        }
    });
    name
}

fn process_repetition(
    variants: &mut Vec<TokenStream>,
    extra_classes: &mut Vec<TokenStream>,
    repetition: &Repetition,
) {
    match repetition {
        Repetition::Once(group) => match group {
            Group::Single(lexeme) => {
                if let Some(tokens) = process_lexeme(lexeme) {
                    variants.push(tokens)
                }
            }
            Group::AnyOf(options) => {
                let name = create_sub_repetition(options, extra_classes);
                variants.push(quote! {#name})
            }
        },
        Repetition::ZeroOrOnce(group) => match group {
            Group::Single(lexeme) => {
                let tokens = match process_lexeme(lexeme) {
                    Some(tokens) => tokens,
                    None => {
                        panic!(
                            "Unable to create repetition 'zero or once' for the given lexeme '{:?}'
                            . It is likely that you are trying to perform a repetition on a
                            terminal value. This parser generator does not allow this behaviour.",
                            lexeme
                        )
                    }
                };
                variants.push(quote! {
                    Option<#tokens>
                })
            }
            Group::AnyOf(options) => {
                let name = create_sub_repetition(options, extra_classes);
                variants.push(quote! {
                    Option<#name>
                })
            }
        },
        Repetition::AtLeastOne(group) => match group {
            Group::Single(lexeme) => {
                let tokens = match process_lexeme(lexeme) {
                    Some(tokens) => tokens,
                    None => {
                        panic!(
                            "Unable to create repetition 'at least one' for the given lexeme '{:?}'
                            . It is likely that you are trying to perform a repetition on a
                            terminal value. This parser generator does not allow this behaviour.",
                            lexeme
                        )
                    }
                };
                variants.push(quote! {
                    NonEmptyVec<#tokens>
                })
            }
            Group::AnyOf(options) => {
                let name = create_sub_repetition(options, extra_classes);
                variants.push(quote! {
                    NonEmptyVec<#name>
                })
            }
        },
        Repetition::ZeroOrMore(group) => match group {
            Group::Single(lexeme) => {
                let tokens = match process_lexeme(lexeme) {
                    Some(tokens) => tokens,
                    None => {
                        panic!(
                            "Unable to create repetition 'zero or more' for the given lexeme
                            '{:?}'. It is likely that you are trying to perform a repetition on a
                            terminal value. This parser generator does not allow this behaviour.",
                            lexeme
                        )
                    }
                };
                variants.push(quote! {
                    Vec<#tokens>
                })
            }
            Group::AnyOf(options) => {
                let name = create_sub_repetition(options, extra_classes);
                variants.push(quote! {
                    Vec<#name>
                })
            }
        },
    }
}

pub fn build_enum_cases(
    rule: &Vec<Production>,
    extra_classes: &mut Vec<TokenStream>,
) -> Vec<TokenStream> {
    let mut cases = Vec::new();

    for (i, production) in rule.iter().enumerate() {
        let ident: Ident = Ident::new(&format!("Production{}", &i.to_string()), Span::call_site());
        let mut variants = Vec::new();
        for repetition in &production.repetitions {
            process_repetition(&mut variants, extra_classes, repetition);
        }
        cases.push(quote! {
            #ident(#(Box<#variants>),*)
        })
    }

    cases
}

fn handle_lexeme(lexeme: &Lexeme, keywords: &mut Vec<Ident>) {
    match lexeme {
        Lexeme::Intrinsic(_) => {}
        Lexeme::NonTerminal(_) => {}
        Lexeme::Terminal(terminal) => {
            if terminal.len() > 1 {
                let ident = syn::parse_str::<Ident>(terminal);
                match ident {
                    Ok(ident) => {
                        keywords.push(ident);
                    }
                    Err(e) => {
                        panic!(
                            "Unable to create keyword '{}'. Parser returned '{}'",
                            terminal, e
                        );
                    }
                }
            }
        }
    }
}

fn handle_group(group: &Group, keywords: &mut Vec<Ident>) {
    match group {
        Group::Single(lexeme) => {
            handle_lexeme(lexeme, keywords);
        }
        Group::AnyOf(options) => {
            for rep in options {
                handle_repetition(rep, keywords);
            }
        }
    }
}

fn handle_repetition(repetition: &Repetition, keywords: &mut Vec<Ident>) {
    match repetition {
        Repetition::Once(group) => {
            handle_group(group, keywords);
        }
        Repetition::ZeroOrOnce(group) => {
            handle_group(group, keywords);
        }
        Repetition::AtLeastOne(group) => {
            handle_group(group, keywords);
        }
        Repetition::ZeroOrMore(group) => {
            handle_group(group, keywords);
        }
    }
}

pub fn build_keywords(grammar: &Grammar) -> TokenStream {
    let mut keywords = Vec::new();

    for rules in grammar.rules.values() {
        for production in rules {
            for repetition in &production.repetitions {
                handle_repetition(repetition, &mut keywords);
            }
        }
    }
    quote!(
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        enum AstKeywords {
            #(#keywords),*
        }

        impl ::tokenizer::tokenizer::Keyword for AstKeywords {
            fn lookup(str: &str) -> Option<Self> {
                None
            }
        }
    )
}
