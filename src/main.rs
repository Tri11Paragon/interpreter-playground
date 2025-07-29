use std::env;
use std::fs;

#[derive(Debug)]
enum Token {
    Literal(String),
    Space,
    Dot,
    Semicolon,
    SingleQuotes(char),
    DoubleQuotes(String),
    Minus,
    Plus,
    Star,
    And,
    Or,
    Percent,
    Dollar,
    At,
    Exclamation,
    Caret,
    Tilde,
    Grave,
    Pound,
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Colon,
    Left,
    Right,
    Comma,
    Slash,
    Question,
    Newline,
    Assignment,
    Equals,
    Less,
    Greater,
    LessEquals,
    GreaterEquals,
    NotEquals,
    Integer(String),
    Decimal(String),
}

fn extract_tokens(content: &str) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut chars = content.chars().peekable();

    let mut current_line : u64 = 1;
    let mut current_char : u64 = 0;

    let mut current_string = String::new();
    while let Some(char) = chars.next() {
        current_char += 1;
        match char {
            'a'..='z' | 'A'..='Z' | '_' => {
                current_string.push(char);
                while let Some(char) = chars.peek() {
                    match char {
                        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                            current_string.push(chars.next().unwrap());
                        }
                        _ => {
                            tokens.push(Token::Literal(current_string.clone()));
                            current_string.clear();
                            break;
                        }
                    }
                }
            }
            '0'..='9' => {
                let mut number_type = false;
                current_string.push(char);
                while let Some(char) = chars.peek() {
                    match char {
                        '0'..='9' => {
                            current_string.push(chars.next().unwrap());
                        }
                        '.' => {
                            current_string.push(chars.next().unwrap());
                            number_type = true;
                        }
                        _ => {
                            if number_type {
                                tokens.push(Token::Decimal(current_string.clone()));
                            } else {
                                tokens.push(Token::Integer(current_string.clone()));
                            }
                            current_string.clear();
                            break;
                        }
                    }
                }
            }
            '.' => {
                current_string.push(char);

                if let Some(char) = chars.peek() {
                    match char {
                        '0'..='9' => {}
                        _ => {
                            current_string.clear();
                            tokens.push(Token::Dot);
                            continue;
                        }
                    }
                }

                while let Some(char) = chars.peek() {
                    match char {
                        '0'..='9' => {
                            current_string.push(chars.next().unwrap());
                        }
                        _ => {
                            tokens.push(Token::Decimal(current_string.clone()));
                            current_string.clear();
                            break;
                        }
                    }
                }
            }
            '\'' => {
                tokens.push(Token::SingleQuotes(chars.next().unwrap_or_else(|| {panic!("Expected character inside single quotes at '{current_line}:{current_char}'");})));
                let char = chars.next();
                unsafe {
                    if char.is_none() || char.unwrap_unchecked() != '\'' {
                        panic!("Expected ending quote for text '{:?}' at '{current_line}:{current_char}'", tokens.iter().nth_back(0).unwrap());
                    }
                }
            }
            '"' => {
                while let Some(char) = chars.peek() {
                    match char {
                        '"' => {
                            chars.next();
                            tokens.push(Token::DoubleQuotes(current_string.clone()));
                            current_string.clear();
                            break;
                        }
                        _ => {
                            unsafe {
                                current_string.push(chars.next().unwrap_unchecked());
                            }
                        }
                    }
                }
            }
            '=' => {
                if let Some(next) = chars.peek() {
                    if let Some(token) = match next {
                        '=' => Some(Token::Equals),
                        '<' => Some(Token::LessEquals),
                        '>' => Some(Token::GreaterEquals),
                        '!' => Some(Token::NotEquals),
                        _ => None
                    } {
                        tokens.push(token);
                    } else {
                        tokens.push(Token::Assignment);
                    }
                } else {
                    tokens.push(Token::Assignment);
                }
            }
            '<' => {

            }
            '>' => {

            }
            '!' => {

            }
            _ => {
                tokens.push(match char {
                    ' ' => Token::Space,
                    ';' => Token::Semicolon,
                    '+' => Token::Plus,
                    '-' => Token::Minus,
                    '&' => Token::And,
                    '|' => Token::Or,
                    '^' => Token::Caret,
                    '$' => Token::Dollar,
                    '~' => Token::Tilde,
                    '@' => Token::At,
                    '#' => Token::Pound,
                    '%' => Token::Percent,
                    '*' => Token::Star,
                    '`' => Token::Grave,
                    '[' => Token::OpenSquare,
                    ']' => Token::CloseSquare,
                    '{' => Token::OpenCurly,
                    '}' => Token::CloseCurly,
                    '(' => Token::OpenParen,
                    ')' => Token::CloseParen,
                    '<' => Token::Left,
                    '>' => Token::Right,
                    '?' => Token::Question,
                    '/' => Token::Slash,
                    ',' => Token::Comma,
                    ':' => Token::Colon
                    '\n' => {
                        current_char = 0;
                        current_line += 1;
                        Token::Newline
                    },
                    _ => {
                        panic!("Unidentified character type '{char}' at '{current_line}:{current_char}'");
                    }
                });
            }
        }
    }

    return tokens;
}

fn main() {
    let tokens = extract_tokens("I am a little 33 tea pot. -Zed. (\"Silly\")");

    println!("{:?}", tokens);
}

// fn main() {
//     let args: Vec<String> = env::args().collect();

//     if args.len() <= 1 {
//         println!("Usage: program file_path");
//         std::process::exit(64)
//     }

//     let file_path = &args[1];

//     let contents =
//         fs::read_to_string(&file_path).expect(&format!("Unable to read file {}!", file_path));

//     let tokens = extract_tokens(&contents);
// }
