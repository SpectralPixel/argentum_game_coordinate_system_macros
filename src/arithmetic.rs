use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, GenericParam, Ident, ImplGenerics, TypeGenerics, WhereClause};

macro_rules! arithmetic_operation {
    ($trait_name:ident, $func_name:ident, $operation:ident, $operation_failure_message:literal) => {
        fn $func_name (
            name: &Ident,
            impl_generics: &ImplGenerics,
            type_generics: &TypeGenerics,
            where_clause: &Option<&WhereClause>,
            generic: &GenericParam,
        ) -> TokenStream {
            quote! {
                impl #impl_generics std::ops::$trait_name for #name #type_generics #where_clause {
                    type Output = Self;
        
                    fn $func_name(self, rhs: #name #type_generics) -> Self::Output {
                        let panic_if_out_of_bounds = || panic!($operation_failure_message, self, rhs);
                        let x = #generic::$operation(&self.x, &rhs.x).unwrap_or_else(panic_if_out_of_bounds);
                        let y = #generic::$operation(&self.y, &rhs.y).unwrap_or_else(panic_if_out_of_bounds);
                        let z = #generic::$operation(&self.z, &rhs.z).unwrap_or_else(panic_if_out_of_bounds);
                        Self::new(x, y, z)
                    }
                }
            }
        }
    };
}

arithmetic_operation!(Add, add, checked_add, "{} is experiencing integer overflow after adding by {}.");
arithmetic_operation!(Sub, sub, checked_sub, "{} is experiencing integer overflow after subtracting by {}.");
arithmetic_operation!(Mul, mul, checked_mul, "{} is experiencing integer overflow after multiplying by {}.");
arithmetic_operation!(Div, div, checked_div, "{} encountered a problem while being divided by {}.");

fn bitand(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {impl #impl_generics std::ops::BitAnd for #name #type_generics #where_clause {
        type Output = Self;

        fn bitand(self, rhs: Self) -> Self::Output {
            Self::new(self.x & rhs.x, self.y & rhs.y, self.z & rhs.z)
        }
    }
    }
}

fn bitor(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::BitOr for #name #type_generics #where_clause {
            type Output = Self;

            fn bitor(self, rhs: Self) -> Self::Output {
                Self::new(self.x | rhs.x, self.y | rhs.y, self.z | rhs.z)
            }
        }
    }
}

fn bitxor(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::BitXor for #name #type_generics #where_clause {
            type Output = Self;

            fn bitxor(self, rhs: Self) -> Self::Output {
                Self::new(self.x ^ rhs.x, self.y ^ rhs.y, self.z ^ rhs.z)
            }
        }
    }
}

fn rem(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    generic: &GenericParam,
) -> TokenStream {
    quote! {
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

fn sub_assign(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::SubAssign for #name #type_generics #where_clause {
            fn sub_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() - rhs;
            }
        }
    }
}

fn mul_assign(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::MulAssign for #name #type_generics #where_clause {
            fn mul_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() * rhs;
            }
        }
    }
}

fn div_assign(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::DivAssign for #name #type_generics #where_clause {
            fn div_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() / rhs;
            }
        }
    }
}

fn bitand_assign(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::BitAndAssign for #name #type_generics #where_clause {
            fn bitand_assign(&mut self, rhs: Self) {
                *self = self.to_owned() & rhs;
            }
        }
    }
}

fn bitor_assign(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::BitOrAssign for #name #type_generics #where_clause {
            fn bitor_assign(&mut self, rhs: Self) {
                *self = self.to_owned() | rhs;
            }
        }
    }
}

fn bitxor_assign(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::BitXorAssign for #name #type_generics #where_clause {
            fn bitxor_assign(&mut self, rhs: Self) {
                *self = self.to_owned() ^ rhs;
            }
        }
    }
}

fn rem_assign(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    generic: &GenericParam,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::RemAssign<#generic> for #name #type_generics #where_clause {
            fn rem_assign(&mut self, rhs: #generic) {
                *self = self.to_owned() % rhs;
            }
        }
    }
}

fn not(
    name: &Ident,
    impl_generics: &ImplGenerics,
    type_generics: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
) -> TokenStream {
    quote! {
        impl #impl_generics std::ops::Not for #name #type_generics #where_clause {
            type Output = #name #type_generics;

            fn not(self) -> Self::Output {
                Self::new(!self.x, !self.y, !self.z)
            }
        }
    }
}

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

pub fn generate(
    ast: &DeriveInput,
    name: &Ident,
    impl_gnrcs: &ImplGenerics,
    type_gnrcs: &TypeGenerics,
    where_clause: &Option<&WhereClause>,
    generic: &GenericParam,
) -> TokenStream {
    let add = add(&name, &impl_gnrcs, &type_gnrcs, &where_clause, &generic);
    let sub = sub(&name, &impl_gnrcs, &type_gnrcs, &where_clause, &generic);
    let mul = mul(&name, &impl_gnrcs, &type_gnrcs, &where_clause, &generic);
    let div = div(&name, &impl_gnrcs, &type_gnrcs, &where_clause, &generic);
    let bitand = bitand(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let bitor = bitor(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let bitxor = bitxor(&name, &impl_gnrcs, &type_gnrcs, &where_clause);

    let rem = rem(&name, &impl_gnrcs, &type_gnrcs, &where_clause, &generic);

    let add_assign = add_assign(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let sub_assign = sub_assign(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let mul_assign = mul_assign(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let div_assign = div_assign(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let bitand_assign = bitand_assign(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let bitor_assign = bitor_assign(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let bitxor_assign = bitxor_assign(&name, &impl_gnrcs, &type_gnrcs, &where_clause);

    let rem_assign = rem_assign(&name, &impl_gnrcs, &type_gnrcs, &where_clause, &generic);

    let not = not(&name, &impl_gnrcs, &type_gnrcs, &where_clause);
    let neg = neg(&ast, &name, &impl_gnrcs, &type_gnrcs, &where_clause);

    quote! {
        #add
        #sub
        #mul
        #div
        #bitand
        #bitor
        #bitxor

        #rem

        #add_assign
        #sub_assign
        #mul_assign
        #div_assign
        #bitand_assign
        #bitor_assign
        #bitxor_assign

        #rem_assign

        #not
        #neg
    }
}
