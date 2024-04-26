//! ```rs
//! sequence!(parser0, parser1, ...)
//! ```
//! expands to
//! ```rs
//! {
//!     let parser = pair(parser0, pair(parser1, ...));
//!     move |input: &'a str| {
//!         match parser.parse(input) {
//!             Ok(((output0, (output1, ...)), rest)) => {
//!                 Ok(((output0, output1, ...), rest))
//!             }
//!             Err(e) => Err(e)
//!         }
//!     }
//! }
//! ```

use proc_macro::TokenStream;

use quote::quote;

struct Sequence {
    parsers: Vec<syn::Expr>,
}

impl syn::parse::Parse for Sequence {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut parsers = Vec::new();
        while !input.is_empty() {
            parsers.push(input.parse()?);
            if input.is_empty() {
                break;
            }
            if input.parse::<syn::Token![,]>().is_err() {
                break;
            }
        }
        Ok(Sequence { parsers })
    }
}

pub fn sequence(input: TokenStream) -> TokenStream {
    let Sequence { parsers } = syn::parse_macro_input!(input as Sequence);

    let len = parsers.len();

    // pair(parser0, pair(parser1, ...))
    let main_parser = parsers.into_iter()
        .enumerate()
        .rev()
        .fold(quote! { _ }, |acc, (i, parser)| {
            let parser = quote! { #parser };
            if i == len - 1 {
                quote! { #parser }
            } else {
                quote! { pair(#parser, #acc) }
            }
        });

    // (output0, (output1, ...))
    let recursive_view = (0..len)
        .map(|i| format!("output{}", i))
        .map(|ident_name| syn::Ident::new(&ident_name, proc_macro2::Span::call_site()))
        .enumerate()
        .rev()
        .fold(quote! { _ }, |acc, (i, iden)| {
            if i == len - 1 {
                quote! { #iden }
            } else {
                quote! { (#iden, #acc) }
            }
        });

    // output0, output1, ...
    let output_view = (0..len)
        .map(|i| format!("output{}", i))
        .map(|ident_name| syn::Ident::new(&ident_name, proc_macro2::Span::call_site()))
        .enumerate()
        .rev()
        .fold(quote! { _ }, |acc, (i, ident)| {
            if i == len - 1 {
                quote! { #ident }
            } else {
                quote! { #ident, #acc }
            }
        });

    let output = quote! {{
        let parser = #main_parser;
        move |input: &'static str| {
            match parser.parse(input) {
                Ok((#recursive_view, rest)) => {
                    Ok(((#output_view), rest))
                }
                Err(e) => Err(e)
            }
        }
    }};

    output.into()
}