use syntax::parse::token::{Token as RsToken};

#[derive(Debug, Clone)]
pub enum Token {
    RustToken(RsToken),
    Symbol,
    Abt,
    Enum,
    Binding,
    Use,
    Vec,
}

impl Token {
    pub fn from_rust_token(t: RsToken) -> Token {
        if let RsToken::Ident(ident) = t {
            let s = ident.name.as_str();

            // No better way to go from InternedString -> &str?
            match unsafe { s.slice_unchecked(0, s.len()) } {
                "symbol" => Token::Symbol,
                "abt" => Token::Abt,
                "enum" => Token::Enum,
                "Binding" => Token::Binding,
                "use" => Token::Use,
                "Vec" => Token::Vec,
                _ => Token::RustToken(t)
            }
        } else {
            Token::RustToken(t)
        }
    }
}
