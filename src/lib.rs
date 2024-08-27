#[proc_macro_derive(CoordinateArithmetic, attributes(signed))]
pub fn coord_negate_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let (name, impl_generics, type_generics, where_clause, generic) =
        split_for_impl_coordinate_type(&ast);

    let mut signed_extras = proc_macro2::TokenStream::new();
    if let Some(attr) = ast.attrs.get(0) {
        if attr.path().is_ident("signed") {
            signed_extras = quote::quote! {
                impl #impl_generics std::ops::Neg for #name #type_generics #where_clause {
                    type Output = #name #type_generics;

                    fn neg(self) -> Self::Output {
                        Self::new(-self.x, -self.y, -self.z)
                    }
                }
            };
        }
    }

    let gen = quote::quote! {
        #signed_extras

        impl #impl_generics std::ops::Add for #name #type_generics #where_clause {
            type Output = #name #type_generics;

            fn add(self, rhs: #name #type_generics) -> Self::Output {
                let panic_if_out_of_bounds = || panic!("{} is experiencing integer overflow after adding by {}.", self, rhs);
                let x = #generic::checked_add(&self.x, &rhs.x).unwrap_or_else(panic_if_out_of_bounds);
                let y = #generic::checked_add(&self.y, &rhs.y).unwrap_or_else(panic_if_out_of_bounds);
                let z = #generic::checked_add(&self.z, &rhs.z).unwrap_or_else(panic_if_out_of_bounds);
                Self::new(x, y, z)
            }
        }

        impl #impl_generics std::ops::AddAssign for #name #type_generics #where_clause {
            fn add_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() + rhs;
            }
        }

        impl #impl_generics std::ops::Sub for #name #type_generics #where_clause {
            type Output = #name #type_generics;

            fn sub(self, rhs: #name #type_generics) -> Self::Output {
                let panic_if_out_of_bounds = || panic!("{} is experiencing integer overflow after subtracting by {}.", self, rhs);
                let x = #generic::checked_sub(&self.x, &rhs.x).unwrap_or_else(panic_if_out_of_bounds);
                let y = #generic::checked_sub(&self.y, &rhs.y).unwrap_or_else(panic_if_out_of_bounds);
                let z = #generic::checked_sub(&self.z, &rhs.z).unwrap_or_else(panic_if_out_of_bounds);
                Self::new(x, y, z)
            }
        }

        impl #impl_generics std::ops::SubAssign for #name #type_generics #where_clause {
            fn sub_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() - rhs;
            }
        }

        impl #impl_generics std::ops::Mul for #name #type_generics #where_clause {
            type Output = #name #type_generics;

            fn mul(self, rhs: #name #type_generics) -> Self::Output {
                let panic_if_out_of_bounds = || panic!("{} is experiencing integer overflow after multiplying by {}.", self, rhs);
                let x = #generic::checked_mul(&self.x, &rhs.x).unwrap_or_else(panic_if_out_of_bounds);
                let y = #generic::checked_mul(&self.y, &rhs.y).unwrap_or_else(panic_if_out_of_bounds);
                let z = #generic::checked_mul(&self.z, &rhs.z).unwrap_or_else(panic_if_out_of_bounds);
                Self::new(x, y, z)
            }
        }

        impl #impl_generics std::ops::MulAssign for #name #type_generics #where_clause {
            fn mul_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() * rhs;
            }
        }

        impl #impl_generics std::ops::Div for #name #type_generics #where_clause {
            type Output = #name #type_generics;

            fn div(self, rhs: #name #type_generics) -> Self::Output {
                let panic_if_out_of_bounds = || panic!("{} encountered a problem while being divided by {}.", self, rhs);
                let x = #generic::checked_div(&self.x, &rhs.x).unwrap_or_else(panic_if_out_of_bounds);
                let y = #generic::checked_div(&self.y, &rhs.y).unwrap_or_else(panic_if_out_of_bounds);
                let z = #generic::checked_div(&self.z, &rhs.z).unwrap_or_else(panic_if_out_of_bounds);
                Self::new(x, y, z)
            }
        }

        impl #impl_generics std::ops::DivAssign for #name #type_generics #where_clause {
            fn div_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() / rhs;
            }
        }

        impl #impl_generics std::ops::BitAnd for #name #type_generics #where_clause {
            type Output = Self;

            fn bitand(self, rhs: Self) -> Self::Output {
                Self::new(self.x & rhs.x, self.y & rhs.y, self.z & rhs.z)
            }
        }

        impl #impl_generics std::ops::BitAndAssign for #name #type_generics #where_clause {
            fn bitand_assign(&mut self, rhs: Self) {
                *self = self.to_owned() & rhs;
            }
        }

        impl #impl_generics std::ops::BitOr for #name #type_generics #where_clause {
            type Output = Self;

            fn bitor(self, rhs: Self) -> Self::Output {
                Self::new(self.x | rhs.x, self.y | rhs.y, self.z | rhs.z)
            }
        }

        impl #impl_generics std::ops::BitOrAssign for #name #type_generics #where_clause {
            fn bitor_assign(&mut self, rhs: Self) {
                *self = self.to_owned() | rhs;
            }
        }

        impl #impl_generics std::ops::BitXor for #name #type_generics #where_clause {
            type Output = Self;

            fn bitxor(self, rhs: Self) -> Self::Output {
                Self::new(self.x ^ rhs.x, self.y ^ rhs.y, self.z ^ rhs.z)
            }
        }

        impl #impl_generics std::ops::BitXorAssign for #name #type_generics #where_clause {
            fn bitxor_assign(&mut self, rhs: Self) {
                *self = self.to_owned() ^ rhs;
            }
        }
    };

    gen.into()
}

#[proc_macro_derive(Coordinate)]
pub fn coordinate_trait_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let (name, impl_generics, type_generics, where_clause, generic) =
        split_for_impl_coordinate_type(&ast);

    let gen = quote::quote! {
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
