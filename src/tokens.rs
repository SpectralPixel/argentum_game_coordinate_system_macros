use syn::{DeriveInput, GenericParam, Ident, ImplGenerics, TypeGenerics, WhereClause};

pub struct Tokens<'a> {
    ast: &'a DeriveInput,
    name: &'a Ident,
    impl_generics: ImplGenerics<'a>,
    type_generics: TypeGenerics<'a>,
    where_clause: Option<&'a WhereClause>,
    generic: &'a GenericParam,
}