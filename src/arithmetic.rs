use convert_case::{Case, Casing};
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
            let mut lower_op = op.from_case(Case::Pascal).to_case(Case::Snake);

            // edge case where BitAnd, BitOr and BitXor get converted to "bit_op" while the method they require is "bitop"
            if lower_op.split('_').next().unwrap() == "bit" {
                lower_op.remove(3); // removes the underscore
            }

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
    ($impl_generics:ident, $trait_name:ident, $name:ident, $type_generics:ident, $where_clause:ident, $func_name:ident, $sym:tt) => {
        (|impl_generics: TokenStream,
          trait_name: Ident,
          name: Ident,
          type_generics: TokenStream,
          where_clause: Option<WhereClause>,
          func_name: Ident| {
            quote! {
                impl #impl_generics std::ops::#trait_name for #name #type_generics #where_clause {
                    fn #func_name(&mut self, rhs: Self) {
                        *self = self.to_owned() $sym rhs;
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
        )
    };
    ($impl_generics:ident, $trait_name:ident, $generic:ident, $name:ident, $type_generics:ident, $where_clause:ident, $func_name:ident, $sym:tt) => {
        (|impl_generics: TokenStream,
          trait_name: Ident,
          generic: GenericParam,
          name: Ident,
          type_generics: TokenStream,
          where_clause: Option<WhereClause>,
          func_name: Ident| {
            quote! {
                impl #impl_generics std::ops::#trait_name<#generic> for #name #type_generics #where_clause {
                    fn #func_name(&mut self, rhs: #generic) {
                        *self = self.to_owned() $sym rhs;
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

macro_rules! operation_assign_xyz {
    ($tokens:ident, $op:ident, $sym:tt) => {
        (|| -> TokenStream {
            let (_, name, impl_generics, type_generics, where_clause, _, trait_name, func_name, _) =
                get_operation_variables!($tokens, $op);

            operation_quote!(
                impl_generics,
                trait_name,
                name,
                type_generics,
                where_clause,
                func_name,
                $sym
            )
        })()
    };
}

macro_rules! operation_assign_single {
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

            operation_quote!(
                impl_generics,
                trait_name,
                generic,
                name,
                type_generics,
                where_clause,
                func_name,
                $sym
            )
        })()
    };
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

// WARNING: BROKEN WINDOW
// Using `macro_rules!` macros to implement these operations turns the code
// from a non-DRY nightmare into slightly less WET code. I believe that
// while this is certainly better than mere trait implementation, a lot could
// these macros supported branching for certain conditions and edge-cases.
// Therefore, I propose another proc-macro crate:
// "`argentum_game_coordinate_system_arithmetic`". By turning these macros into
// function-like procedural macros, the amount of spaghetti code could be
// significantly reduced. For now, I will stick to this system of
// spaghetti-code though; I want to avoid getting burnt-out from working on the
// same system for too long.
pub fn generate(tokens: &Tokens) -> TokenStream {
    let add = operation_xyz!(tokens, Add, "added");
    let sub = operation_xyz!(tokens, Sub, "subtracted");
    let mul = operation_xyz!(tokens, Mul, "multiplied");
    let div = operation_xyz!(tokens, Div, "divided");
    let bitand = operation_xyz!(tokens, BitAnd, &);
    let bitor = operation_xyz!(tokens, BitOr, |);
    let bitxor = operation_xyz!(tokens, BitXor, ^);
    let rem = operation_xyz!(tokens, Rem, %);

    let add_single = operation_single!(tokens, Add, "added");
    let sub_single = operation_single!(tokens, Sub, "subtracted");
    let mul_single = operation_single!(tokens, Mul, "multiplied");
    let div_single = operation_single!(tokens, Div, "divided");
    let bitand_single = operation_single!(tokens, BitAnd, &);
    let bitor_single = operation_single!(tokens, BitOr, |);
    let bitxor_single = operation_single!(tokens, BitXor, ^);
    let rem_single = operation_single!(tokens, Rem, %);

    let add_assign = operation_assign_xyz!(tokens, AddAssign, +);
    let sub_assign = operation_assign_xyz!(tokens, SubAssign, -);
    let mul_assign = operation_assign_xyz!(tokens, MulAssign, *);
    let div_assign = operation_assign_xyz!(tokens, DivAssign, /);
    let bitand_assign = operation_assign_xyz!(tokens, BitAndAssign, &);
    let bitor_assign = operation_assign_xyz!(tokens, BitOrAssign, |);
    let bitxor_assign = operation_assign_xyz!(tokens, BitXorAssign, ^);
    let rem_assign = operation_assign_xyz!(tokens, RemAssign, %);

    let add_assign_single = operation_assign_single!(tokens, AddAssign, +);
    let sub_assign_single = operation_assign_single!(tokens, SubAssign, -);
    let mul_assign_single = operation_assign_single!(tokens, MulAssign, *);
    let div_assign_single = operation_assign_single!(tokens, DivAssign, /);
    let bitand_assign_single = operation_assign_single!(tokens, BitAndAssign, &);
    let bitor_assign_single = operation_assign_single!(tokens, BitOrAssign, |);
    let bitxor_assign_single = operation_assign_single!(tokens, BitXorAssign, ^);
    let rem_assign_single = operation_assign_single!(tokens, RemAssign, %);

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

        #add_single
        #sub_single
        #mul_single
        #div_single
        #bitand_single
        #bitor_single
        #bitxor_single
        #rem_single

        #add_assign
        #sub_assign
        #mul_assign
        #div_assign
        #bitand_assign
        #bitor_assign
        #bitxor_assign
        #rem_assign

        #add_assign_single
        #sub_assign_single
        #mul_assign_single
        #div_assign_single
        #bitand_assign_single
        #bitor_assign_single
        #bitxor_assign_single
        #rem_assign_single

        #not
        #neg
    }
}
