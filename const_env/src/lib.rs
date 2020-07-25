#![allow(non_snake_case)]

extern crate proc_macro;
extern crate const_env_impl__value as const_env_impl;

use const_env_impl::RealEnv;
use proc_macro::TokenStream;

/// Configure a `const` or `static` item from an environment variable.
#[proc_macro_attribute]
pub fn from_env(attr: TokenStream, item: TokenStream) -> TokenStream {
    const_env_impl::from_env(attr.into(), item.into(), RealEnv {}).into()
}

/// Return the value of an environment variable as a constant literal.
#[proc_macro]
pub fn value_from_env(item: TokenStream) -> TokenStream {
    const_env_impl::value_from_env(item.into(), RealEnv {}).into()
}
