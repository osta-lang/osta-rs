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
use crate::utils::crate_utils::crate_accessor;

struct Sequence {
    input_type: syn::Type,
    parsers: Vec<syn::Expr>,
}

impl syn::parse::Parse for Sequence {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let input_type = input.parse()?;
        input.parse::<syn::Token![,]>()?;
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
        Ok(Sequence { input_type, parsers })
    }
}

pub fn sequence(input: TokenStream) -> TokenStream {
    let Sequence { input_type, parsers } = syn::parse_macro_input!(input as Sequence);

    let len = parsers.len();

    if len == 0 {
        return quote! {
            compile_error!("sequence! requires at least one parser");
        }.into();
    }

    let osta_parser_crate = crate_accessor("osta-func");
    let pair = quote! { #osta_parser_crate::combinators::fallible::foundational::pair };

    // pair(parser0, pair(parser1, ...))
    let main_parser = parsers.into_iter()
        .enumerate()
        .rev()
        .fold(quote! { _ }, |acc, (i, parser)| {
            if i == len - 1 {
                quote! { #parser }
            } else {
                quote! { #pair(#parser, #acc) }
            }
        });

    if len < 3 {
        return quote! { #main_parser }.into();
    }

    // generate output identifiers
    let outputs = (0..len)
        .map(|i| format!("output{}", i))
        .map(|ident_name| syn::Ident::new(&ident_name, proc_macro2::Span::call_site()))
        .enumerate()
        .rev()
        .collect::<Vec<_>>();

    // (output0, (output1, ...))
    let recursive_view = outputs.iter()
        .fold(quote! { _ }, |acc, (i, ident)| {
            if *i == len - 1 {
                quote! { #ident }
            } else {
                quote! { (#ident, #acc) }
            }
        });

    // output0, output1, ...
    let output_view = outputs.iter()
        .fold(quote! { _ }, |acc, (i, ident)| {
            if *i == len - 1 {
                quote! { #ident }
            } else {
                quote! { #ident, #acc }
            }
        });

    let output = quote! {{
        let parser = #main_parser;
        move |input: #input_type| {
            let (result, rest) = parser.apply(input);
            match result {
                Ok(#recursive_view) => {
                    (Ok((#output_view)), rest)
                }
                Err(e) => (Err(e), rest)
            }
        }
    }};

    output.into()
}
