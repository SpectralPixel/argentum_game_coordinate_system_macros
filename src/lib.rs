use tokens::Tokens;

mod arithmetic;
mod tokens;

#[proc_macro_derive(Coordinate, attributes(signed))]
pub fn coordinate_trait_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let tokens = Tokens::from(input);
    let (ast, name, impl_generics, type_generics, where_clause, generic) = tokens.split();

    let coord_trait = quote::quote! {
        impl #impl_generics CoordinateTrait for #name #type_generics #where_clause {
            type Type = #name #type_generics;
            type FieldType = #generic;

            const MAX: Self::Type = Self::Type {
                x: #generic::MAX,
                y: #generic::MAX,
                z: #generic::MAX,
            };
            const MIN: Self::Type = Self::Type {
                x: #generic::MIN,
                y: #generic::MIN,
                z: #generic::MIN,
            };

            fn new(x: Self::FieldType, y: Self::FieldType, z: Self::FieldType) -> Self::Type {
                Self::Type { x, y, z }
            }

            fn splat(n: Self::FieldType) -> Self::Type {
                Self::new(n, n, n)
            }
        }

        use core::fmt::{Display, Formatter, Result};
        impl #impl_generics Display for #name #type_generics #where_clause {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                write!(f, "({}: {}, {}, {})", stringify!(#name), self.x, self.y, self.z)
            }
        }

        impl #impl_generics quickcheck::Arbitrary for #name #type_generics #where_clause {
            fn arbitrary(g: &mut quickcheck::Gen) -> Self {
                Self::new(
                    #generic::arbitrary(g),
                    #generic::arbitrary(g),
                    #generic::arbitrary(g),
                )
            }
        }
    };

    let arithmetic = arithmetic::generate(
        &ast,
        &name,
        &impl_generics,
        &type_generics,
        &where_clause,
        &generic,
    );

    let gen = quote::quote! {
        #coord_trait
        #arithmetic
    };

    gen.into()
}
