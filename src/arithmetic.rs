use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::Ident;

use crate::tokens::Tokens;

macro_rules! operation_inner {
    ($tokens:ident, $op:ident, $operation_failure_message:literal, $failed_op:literal) => {
        (|tokens: &Tokens| -> TokenStream {
            let (_, name, impl_generics, type_generics, where_clause, generic) = tokens.split();

            let op = stringify!($op);
            let lower_op = op.to_lowercase();

            let trait_name = Ident::new(op, Span::mixed_site());
            let func_name = Ident::new(&lower_op, Span::mixed_site());
            let operation = Ident::new(&format!("checked_{}", lower_op), Span::mixed_site());

            quote! {
                impl #impl_generics std::ops::#trait_name for #name #type_generics #where_clause {
                    type Output = Self;

                    fn #func_name(self, rhs: Self) -> Self::Output {
                        let panic_if_out_of_bounds = || panic!($operation_failure_message, self, $failed_op, rhs);
                        let x = #generic::#operation(&self.x, &rhs.x).unwrap_or_else(panic_if_out_of_bounds);
                        let y = #generic::#operation(&self.y, &rhs.y).unwrap_or_else(panic_if_out_of_bounds);
                        let z = #generic::#operation(&self.z, &rhs.z).unwrap_or_else(panic_if_out_of_bounds);
                        Self::new(x, y, z)
                    }
                }
            }
        })(&$tokens)
    };
    ($tokens:ident, $op:ident, $sym:tt) => {
        (|tokens: &Tokens| -> TokenStream {
            let (_, name, impl_generics, type_generics, where_clause, _) = tokens.split();

            let op = stringify!($op);
            let lower_op = op.to_lowercase();

            let trait_name = Ident::new(op, Span::mixed_site());
            let func_name = Ident::new(&lower_op, Span::mixed_site());

            quote! {
                impl #impl_generics std::ops::#trait_name for #name #type_generics #where_clause {
                    type Output = Self;

                    fn #func_name(self, rhs: Self) -> Self::Output {
                        Self::new(self.x $sym rhs.x, self.y $sym rhs.y, self.z $sym rhs.z)
                    }
                }
            }
        })(&$tokens)
    };
}

macro_rules! operation {
    ($tokens:ident, $op:ident, $failed_op:literal) => {
        operation_inner!(
            $tokens,
            $op,
            "{} is experiencing integer overflow after {} by {}.",
            $failed_op
        )
    };
    ($tokens:ident, $op:ident) => {
        operation_inner!($tokens, $op, "{} cannot be {} by {}.", "divided")
    };
    ($tokens:ident, $op:ident, $sym:tt) => {
        operation_inner!($tokens, $op, $sym)
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
    let add = operation!(tokens, Add, "added");
    let sub = operation!(tokens, Sub, "subtracted");
    let mul = operation!(tokens, Mul, "multiplied");
    let div = operation!(tokens, Div);
    let bitand = operation!(tokens, BitAnd, &);
    let bitor = operation!(tokens, BitOr, |);
    let bitxor = operation!(tokens, BitXor, ^);

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
