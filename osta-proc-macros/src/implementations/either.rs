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

use proc_macro::TokenStream;

use quote::quote;

pub fn impl_either_unwrap(input: TokenStream) -> TokenStream {
    let n = syn::parse_macro_input!(input as syn::LitInt).base10_parse::<usize>().unwrap();

    let impls = (0..=n)
        .fold(Vec::new(), |mut acc, i| {
            if i == 0 {
                acc.push(quote! { Either<T, T> });
            } else {
                let prev = &acc[i - 1];
                acc.push(quote! { Either<T, #prev> });
            }
            acc
        }).iter()
        .enumerate()
        .map(|(i, either)| {
            if i == 0 {
                quote! {
                    impl<T> #either {
                        pub fn unwrap(self) -> T {
                            match self {
                                Either::Left(v) => v,
                                Either::Right(v) => v
                            }
                        }
                    }
                }
            } else {
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
            }
        })
        .collect::<Vec<_>>();

    let output = quote! {
        #(#impls)*
    };

    output.into()
}

