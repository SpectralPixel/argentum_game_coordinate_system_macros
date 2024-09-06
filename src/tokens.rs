use syn::{token::Token, Ident, LitStr};

pub struct Tokens<T: Token> {
    trait_name: Ident,
    operator: T,
    error_fragment: Option<LitStr>,
}
