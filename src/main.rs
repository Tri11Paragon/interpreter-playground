use parser::tokenizer::{PrettyPrint, Tokenizer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Const,
    Var,
    If,
    Else,
    Fun,
    While,
    Print,
    Return,
    For,
    Class,
    Nil,
    True,
    False
}

impl parser::tokenizer::Keyword for Keyword{
    fn lookup(str: &str) -> Option<Self> {
        match str {
            "const" => Some(Keyword::Const),
            "var" => Some(Keyword::Var),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "fun" => Some(Keyword::Fun),
            "while" => Some(Keyword::While),
            "print" => Some(Keyword::Print),
            "return" => Some(Keyword::Return),
            "for" => Some(Keyword::For),
            "class" => Some(Keyword::Class),
            "nil" => Some(Keyword::Nil),
            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),
            _ => None
        }
    }
}

#[test]
fn print_tests() {
    let code = r#"
        e ☀ silly
        class meow {
            var silly = 0;
            var iamavariable = {
                var counter = 0;
                while (counter < 10) {
                    counter = counter + 1;
                }
                return counter;
            };

            fun callme(hello, there) {
                print hello;
                print there;
                print iamavariable;
            }
        }

        fun main() {
            var wow = ~meow{silly = 50};
            wow.callme(50, 10);
        }
    "#;
    let mut tokenizer = Tokenizer::<Keyword>::new(code);

    let tokens = tokenizer.tokenize();

    match tokens {
        Ok(tokens) => {
            println!("{:?}", tokens);
        }
        Err(errors) => {
            errors.pretty_print(code);
        }
    }
}

fn main() {
    let code = "fun sillybilly () {a.b = 0;}";
    let mut tokenizer = Tokenizer::<Keyword>::new(code);

    let tokens = tokenizer.tokenize();

    match tokens {
        Ok(tokens) => {
            println!("{:?}", tokens);
        }
        Err(errors) => {
            errors.pretty_print(code);
        }
    }

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