use proc_macro2::TokenStream;
use quote::quote;

pub fn crate_accessor(name: &str) -> TokenStream {
    if std::env::var("CARGO_PKG_NAME").unwrap() == name {
        quote! { crate }
    } else {
        let ident = syn::Ident::new(name.replace("-", "_").as_str(), proc_macro2::Span::call_site());
        quote! { #ident }
    }
}