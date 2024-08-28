use proc_macro2::TokenStream;
use quote::quote;

use crate::tokens::Tokens;

macro_rules! operation {
    ($tokens:ident, $trait_name:ident, $func_name:ident, $operation:ident, $operation_failure_message:literal) => {
        (|tokens: &Tokens| -> TokenStream {
            let (_, name, impl_generics, type_generics, where_clause, generic) = tokens.split();
            quote! {
                impl #impl_generics std::ops::$trait_name for #name #type_generics #where_clause {
                    type Output = Self;

                    fn $func_name(self, rhs: Self) -> Self::Output {
                        let panic_if_out_of_bounds = || panic!("{} cannot be {} by {}", self, $operation_failure_message, rhs);
                        let x = #generic::$operation(&self.x, &rhs.x).unwrap_or_else(panic_if_out_of_bounds);
                        let y = #generic::$operation(&self.y, &rhs.y).unwrap_or_else(panic_if_out_of_bounds);
                        let z = #generic::$operation(&self.z, &rhs.z).unwrap_or_else(panic_if_out_of_bounds);
                        Self::new(x, y, z)
                    }
                }
            }
        })(&$tokens)
    };
    ($tokens:ident, $trait_name:ident, $func_name:ident, $op:tt) => {
        (|tokens: &Tokens| -> TokenStream {
            let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
            quote! {
                impl #impl_generics std::ops::$trait_name for #name #type_generics #where_clause {
                    type Output = Self;

                    fn $func_name(self, rhs: Self) -> Self::Output {
                        Self::new(self.x $op rhs.x, self.y $op rhs.y, self.z $op rhs.z)
                    }
                }
            }
        })(&$tokens)
    };
}

fn rem(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, generic) = tokens.split();
    quote! {
        impl #impl_generics std::ops::Rem<#generic> for #name #type_generics #where_clause {
            type Output = Self;

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

fn add_assign(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    quote! {
        impl #impl_generics std::ops::AddAssign for #name #type_generics #where_clause {
            fn add_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() + rhs;
            }
        }
    }
}

fn sub_assign(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    quote! {
        impl #impl_generics std::ops::SubAssign for #name #type_generics #where_clause {
            fn sub_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() - rhs;
            }
        }
    }
}

fn mul_assign(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    quote! {
        impl #impl_generics std::ops::MulAssign for #name #type_generics #where_clause {
            fn mul_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() * rhs;
            }
        }
    }
}

fn div_assign(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    quote! {
        impl #impl_generics std::ops::DivAssign for #name #type_generics #where_clause {
            fn div_assign(&mut self, rhs: #name #type_generics) {
                *self = self.to_owned() / rhs;
            }
        }
    }
}

fn bitand_assign(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    quote! {
        impl #impl_generics std::ops::BitAndAssign for #name #type_generics #where_clause {
            fn bitand_assign(&mut self, rhs: Self) {
                *self = self.to_owned() & rhs;
            }
        }
    }
}

fn bitor_assign(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    quote! {
        impl #impl_generics std::ops::BitOrAssign for #name #type_generics #where_clause {
            fn bitor_assign(&mut self, rhs: Self) {
                *self = self.to_owned() | rhs;
            }
        }
    }
}

fn bitxor_assign(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    quote! {
        impl #impl_generics std::ops::BitXorAssign for #name #type_generics #where_clause {
            fn bitxor_assign(&mut self, rhs: Self) {
                *self = self.to_owned() ^ rhs;
            }
        }
    }
}

fn rem_assign(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, generic) = tokens.split();
    quote! {
        impl #impl_generics std::ops::RemAssign<#generic> for #name #type_generics #where_clause {
            fn rem_assign(&mut self, rhs: #generic) {
                *self = self.to_owned() % rhs;
            }
        }
    }
}

fn not(tokens: &Tokens) -> TokenStream {
    let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    quote! {
        impl #impl_generics std::ops::Not for #name #type_generics #where_clause {
            type Output = Self;

            fn not(self) -> Self::Output {
                Self::new(!self.x, !self.y, !self.z)
            }
        }
    }
}

fn neg(tokens: &Tokens) -> Option<TokenStream> {
    let (ast, name, impl_generics, type_generics, where_clause, _) = tokens.split();
    match ast.attrs.get(0) {
        Some(attr) => match attr.path().is_ident("signed") {
            true => Some(quote! {
                impl #impl_generics std::ops::Neg for #name #type_generics #where_clause {
                    type Output = Self;

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

pub fn generate(tokens: &Tokens) -> TokenStream {
    let add = operation!(tokens, Add, add, checked_add, "added");
    let sub = operation!(tokens, Sub, sub, checked_sub, "subtracted");
    let mul = operation!(tokens, Mul, mul, checked_mul, "multiplied");
    let div = operation!(tokens, Div, div, checked_div, "divided");
    let bitand = operation!(tokens, BitAnd, bitand, &);
    let bitor = operation!(tokens, BitOr, bitor, |);
    let bitxor = operation!(tokens, BitXor, bitxor, ^);

    let rem = rem(&tokens);

    let add_assign = add_assign(&tokens);
    let sub_assign = sub_assign(&tokens);
    let mul_assign = mul_assign(&tokens);
    let div_assign = div_assign(&tokens);
    let bitand_assign = bitand_assign(&tokens);
    let bitor_assign = bitor_assign(&tokens);
    let bitxor_assign = bitxor_assign(&tokens);

    let rem_assign = rem_assign(&tokens);

    let not = not(&tokens);
    let neg = neg(&tokens);

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
