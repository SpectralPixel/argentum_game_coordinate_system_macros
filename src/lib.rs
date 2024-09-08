use tokens::Tokens;

mod arithmetic;
mod arithmetic_token;
mod tokens;

// #[proc_macro_derive(CoordinateArithmetic)]
// pub fn coordinate_arithmetic_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//     let tokens = Tokens::from(input);
//     let arithmetic = arithmetic::generate(&tokens);
//     arithmetic.into()
// }

#[proc_macro]
pub fn generate_arithmetic_implementation(_: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::new().into()
}
