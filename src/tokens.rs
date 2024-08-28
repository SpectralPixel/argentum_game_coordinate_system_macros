use syn::{DeriveInput, GenericParam, Ident, ImplGenerics, TypeGenerics, WhereClause};

pub struct Tokens {
    ast: DeriveInput,
}

impl Tokens {
    pub fn split(&self) -> (&Ident, ImplGenerics, TypeGenerics, Option<&WhereClause>, &GenericParam) {
        let name = &self.ast.ident;
        let (impl_generics, type_generics, where_clause) = self.ast.generics.split_for_impl();
        let generic = self.ast.generics.params.first().unwrap();

        (
            name,
            impl_generics,
            type_generics,
            where_clause,
            generic,
        )
    }
}

impl From<proc_macro::TokenStream> for Tokens {
    fn from(value: proc_macro::TokenStream) -> Self {
        Self { ast: syn::parse(value).unwrap() }
    }
}
