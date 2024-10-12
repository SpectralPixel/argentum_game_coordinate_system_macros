use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{GenericParam, Ident};

use crate::tokens::Tokens;

pub fn generate(tokens: &Tokens) -> TokenStream {
    let add = operation(&tokens, "Add",  Some(false));
    let sub = operation(&tokens, "Sub",  Some(false));
    let mul = operation(&tokens, "Mul",  Some(false));
    let div = operation(&tokens, "Div",  Some(false));
    let bitand = operation(&tokens, "BitAnd",  Some(false));
    let bitor = operation(&tokens, "BitOr",  Some(false));
    let bitxor = operation(&tokens, "BitXor",  Some(false));
    let rem = operation(&tokens, "Rem",  Some(false));

    let add_single = operation(&tokens, "Add",  Some(true));
    let sub_single = operation(&tokens, "Sub",  Some(true));
    let mul_single = operation(&tokens, "Mul",  Some(true));
    let div_single = operation(&tokens, "Div",  Some(true));
    let bitand_single = operation(&tokens, "BitAnd",  Some(true));
    let bitor_single = operation(&tokens, "BitOr",  Some(true));
    let bitxor_single = operation(&tokens, "BitXor",  Some(true));
    let rem_single = operation(&tokens, "Rem",  Some(true));

    let add_assign = operation(&tokens, "AddAssign", None);
    let sub_assign = operation(&tokens, "SubAssign", None);
    let mul_assign = operation(&tokens, "MulAssign", None);
    let div_assign = operation(&tokens, "DivAssign", None);
    let bitand_assign = operation(&tokens, "BitAndAssign", None);
    let bitor_assign = operation(&tokens, "BitOrAssign", None);
    let bitxor_assign = operation(&tokens, "BitXorAssign", None);
    let rem_assign = operation(&tokens, "RemAssign", None);

    let add_assign_single = operation(&tokens, "AddAssign", None);
    let sub_assign_single = operation(&tokens, "SubAssign", None);
    let mul_assign_single = operation(&tokens, "MulAssign", None);
    let div_assign_single = operation(&tokens, "DivAssign", None);
    let bitand_assign_single = operation(&tokens, "BitAndAssign", None);
    let bitor_assign_single = operation(&tokens, "BitOrAssign", None);
    let bitxor_assign_single = operation(&tokens, "BitXorAssign", None);
    let rem_assign_single = operation(&tokens, "RemAssign", None);

    let not = operation(&tokens, "Not", Some(false));

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
    }
}

fn operation(tokens: &Tokens, trait_name: &str, is_single: Option<bool>) -> TokenStream {
    let is_single = is_single.unwrap_or(true);

    let operation_punct = translator(&trait_name);

    let (name, impl_generics, type_generics, where_clause, generic) = tokens.split();

    let mut lower_op = trait_name.from_case(Case::Pascal).to_case(Case::Snake);

    // edge case where BitAnd, BitOr and BitXor get converted to "bit_op" while the method they require is "bitop"
    if lower_op.split('_').next().unwrap() == "bit" {
        lower_op.remove(3); // removes the underscore
    }

    let trait_name_ident = Ident::new(trait_name, Span::mixed_site());
    let func_name = Ident::new(&lower_op, Span::mixed_site());

    where_clause.map(|v| v.clone()); // clone Option's inner value

    let op_combined = if let Operation::Assign(_) = operation_punct {
        // if operation is an "Assign", only one operation will be generated as dimensions are irrelevant.
        operation_punct.gen_op(None, &generic)
    } else {
        let op_x = operation_punct.gen_op(Some(quote!(x)), &generic);
        let op_y = operation_punct.gen_op(Some(quote!(y)), &generic);
        let op_z = operation_punct.gen_op(Some(quote!(z)), &generic);
        quote!(Self::new(#op_x, #op_y, #op_z))
    };

    let punct_before_op = matches!(operation_punct, Operation::Before(_));
    let trait_generic = match is_single | punct_before_op {
        true => Some(quote!(<#generic>)),
        false => None,
    };

    let output_type = operation_punct.output_type();
    let return_type = operation_punct.return_type();
    let arguments = operation_punct.function_arguments(&generic, is_single);

    let name = name.clone();
    let impl_generics = impl_generics.to_token_stream();
    let type_generics = type_generics.to_token_stream();

    quote! {
        impl #impl_generics std::ops::#trait_name_ident #trait_generic for #name #type_generics #where_clause {
            #output_type

            fn #func_name(#arguments) #return_type {
                #op_combined
            }
        }
    }
}

enum Operation {
    Checked(Ident, &'static str),
    Inbetween(TokenStream),
    Before(TokenStream),
    Assign(TokenStream),
}

impl Operation {
    pub fn checked(trait_name: &str) -> Self {
        let checked_op_name = String::from("checked_") + &trait_name.to_ascii_lowercase();
        let checked_op = Ident::new(&checked_op_name, Span::mixed_site());

        let error_message_fragment = match trait_name {
            "Add" => "added",
            "Sub" => "subtracted",
            "Mul" => "multiplied",
            "Div" => "divided",
            _ => unreachable!(),
        };

        Self::Checked(checked_op, error_message_fragment)
    }

    pub fn gen_op(
        &self,
        dimension: Option<TokenStream>,
        generic: &GenericParam,
    ) -> TokenStream {let rhs = match &dimension {
            Some(v) => quote!(rhs.#v),
            None => quote!(rhs),
        };

        match self {
            Self::Checked(func, error_message_fragment) => {
                let operation_failure = quote! {
                    || { panic!("{} cannot be {} by {}! This may be caused by integer overflow.", self, #error_message_fragment, rhs) }
                };

                quote! {
                    #generic::#func(&self.#dimension, &#rhs).unwrap_or_else(#operation_failure)
                }
            }
            Self::Inbetween(punct) => quote! {
                self.#dimension #punct #rhs
            },
            Self::Before(punct) => quote! {
                #punct self.dimension
            },
            Self::Assign(punct) => quote! {
                *self = self.to_owned() #punct rhs;
            },
        }
    }
    
    pub fn output_type(&self) -> Option<TokenStream> {
        match self {
            Self::Assign(_) => None,
            _ => Some(quote!(type Output = Self;))
        }
    }
    
    pub fn return_type(&self) -> Option<TokenStream> {
        match self {
            Self::Assign(_) => None,
            _ => Some(quote!(-> Self::Output)),
        }
    }

    pub fn function_arguments(&self, generic: &GenericParam, is_single: bool) -> TokenStream {
        let first_argument = match self {
            Self::Assign(_) => quote!(&mut self),
            _ => quote!(&self),
        };
        let second_argument = match self {
            Self::Checked(_, _) | Self::Inbetween(_) => match is_single {
                true => Some(quote!(rhs: #generic)),
                false => Some(quote!(rhs: Self)),
            },
            _ => None,
        };
        match second_argument {
            None => quote!(#first_argument),
            Some(sec_arg) => quote!(#first_argument, #sec_arg),
        }
    }
}

macro_rules! translator {
    ($name:ident) => {
        syn::token::$name::default().into_token_stream()
    };
}

fn translator(name: &str) -> Operation {
    use Operation::*;
    match name {
        x @ ("Add" | "Sub" | "Mul" | "Div") => Operation::checked(x),
        "Rem" => Inbetween(translator!(Percent)),
        "Shl" => Inbetween(translator!(Shl)),
        "Shr" => Inbetween(translator!(Shr)),
        "BitAnd" => Inbetween(translator!(And)),
        "BitOr" => Inbetween(translator!(Or)),
        "BitXor" => Inbetween(translator!(CaretEq)),
        "AddAssign" => Assign(translator!(Plus)),
        "SubAssign" => Assign(translator!(Minus)),
        "MulAssign" => Assign(translator!(Star)),
        "DivAssign" => Assign(translator!(Slash)),
        "RemAssign" => Assign(translator!(Percent)),
        "ShlAssign" => Assign(translator!(Shl)),
        "ShrAssign" => Assign(translator!(Shr)),
        "BitAndAssign" => Assign(translator!(And)),
        "BitOrAssign" => Assign(translator!(Or)),
        "BitXorAssign" => Assign(translator!(Caret)),
        "Not" => Before(translator!(Not)),
        _ => panic!("Incorrect punctuation provided in matcher!"),
    }
}
