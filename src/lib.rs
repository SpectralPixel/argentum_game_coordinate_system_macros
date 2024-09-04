use tokens::Tokens;

mod arithmetic;
mod tokens;

#[proc_macro_derive(Coordinate, attributes(signed))]
pub fn coordinate_trait_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let tokens = Tokens::from(input);
    let arithmetic = arithmetic::generate(&tokens);
    arithmetic.into()
}
