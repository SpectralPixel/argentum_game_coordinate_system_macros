use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{DeriveInput, GenericParam, Ident, WhereClause};

use crate::tokens::Tokens;

macro_rules! gen_op_single {
    ($component:ident, $generic:ident, $op_name:ident, $operation_failure:block) => {
        (|| {
            quote! {
                #$generic::#$op_name(&self.$component, &rhs).unwrap_or_else($operation_failure)
            }
        })()
    };
    ($component:ident, $sym:tt) => {
        (|| {
            quote! {
                self.$component $sym rhs
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
    ($component:ident, $sym:tt) => {
        (|| {
            quote! {
                self.$component $sym rhs.$component
            }
        })()
    };
}

macro_rules! get_operation_variables {
    ($tokens:ident, $op:ident) => {
        (|tokens: &Tokens| -> (
            DeriveInput,
            Ident,
            TokenStream,
            TokenStream,
            Option<WhereClause>,
            GenericParam,
            Ident,
            Ident,
            Ident,
        ) {
            let (ast, name, impl_generics, type_generics, where_clause, generic) = tokens.split();

            let op = stringify!($op);
            let lower_op = op.to_lowercase();

            let trait_name = Ident::new(op, Span::mixed_site());
            let func_name = Ident::new(&lower_op, Span::mixed_site());
            let op_name = Ident::new(&format!("checked_{}", lower_op), Span::mixed_site());

            // silly workaround for getting rid of the reference within the Option<>
            let where_clause = match where_clause {
                Some(v) => Some(v.clone()),
                None => None,
            };

            (
                ast.clone(),
                name.clone(),
                impl_generics.to_token_stream(),
                type_generics.to_token_stream(),
                where_clause,
                generic.clone(),
                trait_name,
                func_name,
                op_name,
            )
        })(&$tokens)
    };
}

macro_rules! operation_quote {
    ($impl_generics:ident, $trait_name:ident, $name:ident, $type_generics:ident, $where_clause:ident, $func_name:ident, $op_x:ident, $op_y:ident, $op_z:ident) => {
        (|impl_generics: TokenStream,
          trait_name: Ident,
          name: Ident,
          type_generics: TokenStream,
          where_clause: Option<WhereClause>,
          func_name: Ident,
          op_x: TokenStream,
          op_y: TokenStream,
          op_z: TokenStream| {
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
        })(
            $impl_generics,
            $trait_name,
            $name,
            $type_generics,
            $where_clause,
            $func_name,
            $op_x,
            $op_y,
            $op_z,
        )
    };
    ($impl_generics:ident, $trait_name:ident, $generic:ident, $name:ident, $type_generics:ident, $where_clause:ident, $func_name:ident, $op_x:ident, $op_y:ident, $op_z:ident) => {
        (|impl_generics: TokenStream,
          trait_name: Ident,
          generic: GenericParam,
          name: Ident,
          type_generics: TokenStream,
          where_clause: Option<WhereClause>,
          func_name: Ident,
          op_x: TokenStream,
          op_y: TokenStream,
          op_z: TokenStream| {
            quote! {
                impl #impl_generics std::ops::#trait_name<#generic> for #name #type_generics #where_clause {
                    type Output = Self;

                    fn #func_name(self, rhs: #generic) -> Self::Output {
                        Self::new(
                            #op_x,
                            #op_y,
                            #op_z,
                        )
                    }
                }
            }
        })(
            $impl_generics,
            $trait_name,
            $generic,
            $name,
            $type_generics,
            $where_clause,
            $func_name,
            $op_x,
            $op_y,
            $op_z,
        )
    };
}

macro_rules! operation_inner_xyz {
    ($tokens:ident, $op:ident, $operation_failure:block) => {
        (|| -> TokenStream {
            let (
                _,
                name,
                impl_generics,
                type_generics,
                where_clause,
                generic,
                trait_name,
                func_name,
                op_name,
            ) = get_operation_variables!($tokens, $op);

            let op_x = gen_op_xyz!(x, generic, op_name, $operation_failure);
            let op_y = gen_op_xyz!(y, generic, op_name, $operation_failure);
            let op_z = gen_op_xyz!(z, generic, op_name, $operation_failure);

            operation_quote!(
                impl_generics,
                trait_name,
                name,
                type_generics,
                where_clause,
                func_name,
                op_x,
                op_y,
                op_z
            )
        })()
    };
    ($tokens:ident, $op:ident, $sym:tt) => {
        (|| -> TokenStream {
            let (_, name, impl_generics, type_generics, where_clause, _, trait_name, func_name, _) =
                get_operation_variables!($tokens, $op);

            let op_x = gen_op_xyz!(x, $sym);
            let op_y = gen_op_xyz!(y, $sym);
            let op_z = gen_op_xyz!(z, $sym);

            operation_quote!(
                impl_generics,
                trait_name,
                name,
                type_generics,
                where_clause,
                func_name,
                op_x,
                op_y,
                op_z
            )
        })()
    };
}

macro_rules! operation_inner_single {
    ($tokens:ident, $op:ident, $operation_failure:block) => {
        (|| -> TokenStream {
            let (
                _,
                name,
                impl_generics,
                type_generics,
                where_clause,
                generic,
                trait_name,
                func_name,
                op_name,
            ) = get_operation_variables!($tokens, $op);

            let op_x = gen_op_single!(x, generic, op_name, $operation_failure);
            let op_y = gen_op_single!(y, generic, op_name, $operation_failure);
            let op_z = gen_op_single!(z, generic, op_name, $operation_failure);

            operation_quote!(
                impl_generics,
                trait_name,
                generic,
                name,
                type_generics,
                where_clause,
                func_name,
                op_x,
                op_y,
                op_z
            )
        })()
    };
    ($tokens:ident, $op:ident, $sym:tt) => {
        (|| -> TokenStream {
            let (
                _,
                name,
                impl_generics,
                type_generics,
                where_clause,
                generic,
                trait_name,
                func_name,
                _,
            ) = get_operation_variables!($tokens, $op);

            let op_x = gen_op_single!(x, $sym);
            let op_y = gen_op_single!(y, $sym);
            let op_z = gen_op_single!(z, $sym);

            operation_quote!(
                impl_generics,
                trait_name,
                generic,
                name,
                type_generics,
                where_clause,
                func_name,
                op_x,
                op_y,
                op_z
            )
        })()
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

macro_rules! operation_single {
    ($tokens:ident, $op:ident, $failed_op:literal) => {
        operation_inner_single!($tokens, $op, {
            || {
                panic!(
                    "{} is experiencing integer overflow after {} by {}.",
                    self, $failed_op, rhs
                )
            }
        })
    };
    ($tokens:ident, $op:ident, "divided") => {
        operation_inner_single!($tokens, $op, {
            || panic!("{} cannot be divided by {}.", self, rhs)
        })
    };
    ($tokens:ident, $op:ident, $sym:tt) => {
        operation_inner_single!($tokens, $op, $sym)
    };
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

    let rem_single = operation_single!(tokens, Rem, %);

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
