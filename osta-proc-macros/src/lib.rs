extern crate proc_macro;

use proc_macro::TokenStream;

mod combinators;
mod implementations;
mod utils;

#[proc_macro]
pub fn sequence(input: TokenStream) -> TokenStream {
    combinators::sequence::sequence(input)
}

#[proc_macro]
pub fn impl_either_unwrap(input: TokenStream) -> TokenStream {
    implementations::either::impl_either_unwrap(input)
}