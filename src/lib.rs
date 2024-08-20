use proc_macro::TokenStream;

#[proc_macro_derive(CoordNegate)]
pub fn coord_negate_derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let name = &ast.ident;

    let gen = quote::quote! {
        impl Neg for #name {
            type Output = Self;

            fn neg(self) -> Self::Output {
                Self::new(-self.x, -self.y, -self.z)
            }
        }
    };

    gen.into()
}
