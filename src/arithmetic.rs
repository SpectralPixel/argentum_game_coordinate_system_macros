use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::Ident;

use crate::tokens::Tokens;

macro_rules! gen_op_single {
    ($component:ident, $generic:ident, $op_name:ident, $operation_failure:block) => {
        (|| {
            quote! {
                #$generic::#$op_name(&self.$component, &rhs).unwrap_or_else($operation_failure)
            }
        })()
    };
}

macro_rules! gen_op_xyz {
    ($component:ident, $generic:ident, $op_name:ident, $operation_failure:block) => {
        (|| {
            quote! {
                #$generic::#$op_name(&self.$component, &rhs.$component).unwrap_or_else($operation_failure)
            }
        })()
    };
}

macro_rules! operation_inner_xyz {
    ($tokens:ident, $op:ident, $operation_failure:block) => {
        (|tokens: &Tokens| -> TokenStream {
            let (_, name, impl_generics, type_generics, where_clause, generic) = tokens.split();

            let op = stringify!($op);
            let lower_op = op.to_lowercase();

            let trait_name = Ident::new(op, Span::mixed_site());
            let func_name = Ident::new(&lower_op, Span::mixed_site());
            let op_name = Ident::new(&format!("checked_{}", lower_op), Span::mixed_site());

            let op_x = gen_op_xyz!(x, generic, op_name, $operation_failure);
            let op_y = gen_op_xyz!(y, generic, op_name, $operation_failure);
            let op_z = gen_op_xyz!(z, generic, op_name, $operation_failure);

            quote! {
                impl #impl_generics std::ops::#trait_name for #name #type_generics #where_clause {
                    type Output = Self;

                    fn #func_name(self, rhs: Self) -> Self::Output {
                        Self::new(
                            #op_x,
                            #op_y,
                            #op_z,
                        )
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

macro_rules! operation_xyz {
    ($tokens:ident, $op:ident, $failed_op:literal) => {
        operation_inner_xyz!($tokens, $op, {
            || {
                panic!(
                    "{} is experiencing integer overflow after {} by {}.",
                    self, $failed_op, rhs
                )
            }
        })
    };
    ($tokens:ident, $op:ident, "divided") => {
        operation_inner_xyz!($tokens, $op, {
            || panic!("{} cannot be divided by {}.", self, rhs)
        })
    };
    ($tokens:ident, $op:ident, $sym:tt) => {
        operation_inner_xyz!($tokens, $op, $sym)
    };
}

fn rem_single(tokens: &Tokens) -> TokenStream {
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

fn rem_assign_single(tokens: &Tokens) -> TokenStream {
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
    let add = operation_xyz!(tokens, Add, "added");
    let sub = operation_xyz!(tokens, Sub, "subtracted");
    let mul = operation_xyz!(tokens, Mul, "multiplied");
    let div = operation_xyz!(tokens, Div, "divided");
    let bitand = operation_xyz!(tokens, BitAnd, &);
    let bitor = operation_xyz!(tokens, BitOr, |);
    let bitxor = operation_xyz!(tokens, BitXor, ^);

    let rem_single = rem_single(&tokens);

    let add_assign = add_assign(&tokens);
    let sub_assign = sub_assign(&tokens);
    let mul_assign = mul_assign(&tokens);
    let div_assign = div_assign(&tokens);
    let bitand_assign = bitand_assign(&tokens);
    let bitor_assign = bitor_assign(&tokens);
    let bitxor_assign = bitxor_assign(&tokens);

    let rem_assign_single = rem_assign_single(&tokens);

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

        #rem_single

        #add_assign
        #sub_assign
        #mul_assign
        #div_assign
        #bitand_assign
        #bitor_assign
        #bitxor_assign

        #rem_assign_single

        #not
        #neg
    }
}
