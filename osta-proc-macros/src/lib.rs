extern crate proc_macro;

use proc_macro::TokenStream;

mod combinators;

#[proc_macro]
pub fn sequence(input: TokenStream) -> TokenStream {
    combinators::sequence::sequence(input)
}
