use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, GenericParam, Ident, ImplGenerics, TypeGenerics, WhereClause};

fn neg(
    ast: &DeriveInput,
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> Option<TokenStream> {
    match ast.attrs.get(0) {
        Some(attr) => match attr.path().is_ident("signed") {
            true => Some(quote! {
                impl #impl_generics std::ops::Neg for #name #type_generics #where_clause {
                    type Output = #name #type_generics;

                    fn neg(self) -> Self::Output {
                        Self::new(-self.x, -self.y, -self.z)
                    }
                }
            }),
            false => None,
        },
        None => None,
    }
}

fn add(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    generic: &GenericParam,
) -> TokenStream {
    quote! {
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
    }
}

fn add_assign(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::AddAssign for #name #type_generics #where_clause {
            fn add_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() + rhs;
            }
        }
    }
}

pub fn generate(
    ast: &DeriveInput,
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    generic: &GenericParam,
) -> TokenStream {
    let neg = neg(&ast, &name, &impl_generics, &type_generics, &where_clause);

    let add = add(
        &name,
        &impl_generics,
        &type_generics,
        &where_clause,
        &generic,
    );

    let add_assign = add_assign(&name, &impl_generics, &type_generics, &where_clause);

    quote! {
        #neg

        #add
        #add_assign

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

        impl #impl_generics std::ops::Rem<#generic> for #name #type_generics #where_clause {
            type Output = #name #type_generics;

            fn rem(self, rhs: #generic) -> Self::Output {
                Self::new(
                    self.x % rhs,
                    self.y % rhs,
                    self.z % rhs,
                )
            }
        }

        impl #impl_generics std::ops::RemAssign<#generic> for #name #type_generics #where_clause {
            fn rem_assign(&mut self, rhs: #generic) {
                *self = self.to_owned() % rhs;
            }
        }

        impl #impl_generics std::ops::Not for #name #type_generics #where_clause {
            type Output = #name #type_generics;

            fn not(self) -> Self::Output {
                Self::new(!self.x, !self.y, !self.z)
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
    }
}