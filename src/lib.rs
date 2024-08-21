use proc_macro::TokenStream;

#[proc_macro_derive(CoordinateArithmetic, attributes(signed, unsigned))]
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

#[proc_macro_derive(Coordinate)]
pub fn coordinate_trait_derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let (name, impl_generics, type_generics, where_clause, generic) =
        split_for_impl_coordinate_type(&ast);

    let gen = quote::quote! {
        impl #impl_generics CoordinateTrait for #name #type_generics #where_clause {
            type Type = #name #type_generics;
            type FieldType = #generic;

            fn new(x: Self::FieldType, y: Self::FieldType, z: Self::FieldType) -> Self::Type {
                Self::Type { x, y, z }
            }

            fn splat(n: Self::FieldType) -> Self::Type {
                Self::new(n, n, n)
            }
        }
    };

    gen.into()
}

fn split_for_impl_coordinate_type(
    ast: &syn::DeriveInput,
) -> (
    &syn::Ident,
    syn::ImplGenerics,
    syn::TypeGenerics,
    Option<&syn::WhereClause>,
    &syn::GenericParam,
) {
    let name = &ast.ident;
    let (impl_generics, type_generics, where_clause) = ast.generics.split_for_impl();
    let generic = ast.generics.params.first().unwrap();

    (name, impl_generics, type_generics, where_clause, generic)
}
