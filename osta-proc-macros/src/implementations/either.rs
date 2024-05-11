//! ```rs
//! impl_either_unwrap!(2)
//! ```
//! expands to
//! ```rs
//! impl<T> Either<T, T> {
//!     pub fn unwrap(self) -> T {
//!         match self {
//!             Either::Left(v) => v,
//!             Either::Right(v) => v
//!         }
//!     }
//! }
//! impl<T> Either<T, Either<T, T>> {
//!     pub fn unwrap(self) -> T {
//!         match self {
//!             Either::Left(v) => v,
//!             Either::Right(v) => v.unwrap()
//!         }
//!     }
//! }
//! impl<T> Either<T, Either<T, Either<T, T>>> {
//!     pub fn unwrap(self) -> T {
//!         match self {
//!             Either::Left(v) => v,
//!             Either::Right(v) => v.unwrap()
//!         }
//!     }
//! }
//! ```
//! for both left and right variants of the Either enum.

use proc_macro::TokenStream;

use quote::quote;

pub fn impl_either_unwrap(input: TokenStream) -> TokenStream {
    let n = syn::parse_macro_input!(input as syn::LitInt).base10_parse::<usize>().unwrap();

    let base_either = quote! { Either<T, T> };

    let either_left = (0..n)
        .fold(Vec::new(), |mut acc, i| {
            let prev = if i == 0 { &base_either } else { &acc[i - 1] };
            acc.push(quote! { Either<#prev, T> });
            acc
        });
    let either_right = (0..n)
        .fold(Vec::new(), |mut acc, i| {
            let prev = if i == 0 { &base_either } else { &acc[i - 1] };
            acc.push(quote! { Either<T, #prev> });
            acc
        });

    let either_left = either_left.into_iter()
        .map(|either| {
            quote! {
                impl<T> #either {
                    pub fn unwrap(self) -> T {
                        match self {
                            Either::Left(v) => v.unwrap(),
                            Either::Right(v) => v
                        }
                    }
                }
            }
        })
        .collect::<Vec<_>>();
    let either_right = either_right.into_iter()
        .map(|either| {
            quote! {
                impl<T> #either {
                    pub fn unwrap(self) -> T {
                        match self {
                            Either::Left(v) => v,
                            Either::Right(v) => v.unwrap()
                        }
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    let output = quote! {
        impl<T> #base_either {
            pub fn unwrap(self) -> T {
                match self {
                    Either::Left(v) => v,
                    Either::Right(v) => v
                }
            }
        }
        #(#either_left)*
        #(#either_right)*
    };

    output.into()
}

