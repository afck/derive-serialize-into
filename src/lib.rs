//! # Derive `Serialize` and `Deserialize` for validating wrapper types
//!
//! This crate provides several custom derives that provide implementations of
//! [serde](https://serde.rs/)'s `Serialize` and `Deserialize` traits for wrapper types, as well as
//! `Deserialize` implementations that perform some validation.
//!
//! Sometimes you have a single-field type
//!
//! ```rust
//! struct Contact {
//!     email: String,
//! }
//! ```
//!
//! which you want to serialize and deserialize as a string instead of a struct, e.g. you want its
//! JSON representation to just be "`"user@domain.com"`" instead of
//! "`{ "email": "user@domain.com" }`". You can derive `Serialize` and `Deserialize` implementations
//! for that purpose, as well as `Into` and `From` implementations to convert between `String` and
//! `Contact`.
//!
//! Another example is a _validated_ wrapper type like
//!
//! ```rust
//! struct Email(String);
//! ```
//!
//! that should never be instantianted with a string that doesn't represent a valid email address.
//! If you implement `Email: TryFrom<String>` using the
//! [try_from](https://crates.io/crates/try_from) crate, such that conversion fails for invalid
//! addresses, the derived `Deserialize` will also fail if the string is in the wrong format.
//!
//! Custom derives for `From` and `TryFrom` are also provided for some common use cases.
//!
//!
//! ## Supported derive attributes
//!
//! ### `#[derive(SerializeInto)]`
//!
//! On structures `T`, given an `impl Into<S> for &T`, this creates an `impl Serialize for T` which
//! first converts to `S` and then serializes it.
//!
//! You can optionally specify `S` with `#[serialize_into(S)]`. If you don't, `T` must be a
//! structure with a single field, and a reference to that field's type will be used as `S`.
//! Together with `#[derive(IntoInner)]`, this makes it easy to serialize structs like
//! `struct Foo(S)` or `struct Foo { bar: S }` as if they were type `S`.
//!
//! ### `#[derive(DeserializeFrom)]`
//!
//! On structures `T`, given an `impl Into<T> for S`, this creates an
//! `impl<'de> Deserialize<'de> for T` which first deserializes to `S` and then converts to `T`.
//!
//! You can optionally specify `S` with `#[deserialize_from(S)]`. If you don't, `T` must be a
//! structure with a single field, and that field's type will be used as `S`.
//! Together with `#[derive(FromInner)]`, this makes it easy to deserialize structs like
//! `struct Foo(S)` or `struct Foo { bar: S }` as if they were just `S`.
//!
//! ### `#[derive(DeserializeTryFrom)]`
//!
//! On structures `T`, given an `impl TryInto<T> for S`, this creates an
//! `impl<'de> Deserialize<'de> for T` which first deserializes to `S` and then tries to convert to
//! `T`. A failure to convert will cause deserialization to fail. The `TryInto` implementation's
//! error type must implement `Display` and will be converted to a custom deserialization error.
//!
//! You can optionally specify `S` with `#[deserialize_from(S)]`. If you don't, `T` must be a
//! structure with a single field, and that field's type will be used as `S`.
//! Together with `#[derive(TryFromInner)]`, this allows deserializing structs like
//! `struct Foo(S)` or `struct Foo { bar: S }` as if they were just `S`, but with additional
//! validation. For deserialized values of `S` that don't validate, deserialization into `T` will
//! fail.
//!
//! ### `#[derive(IntoInner)]`
//!
//! On structures `T` with a single field of type `S`, this creates an `impl From<&T> for &S` that
//! returns a reference to the field.
//!
//! ### `#[derive(FromInner)]`
//!
//! On structures `T` with a single field of type `S`, this creates an `impl From<S> for T`.
//!
//! ### `#[derive(TryFromInner)]`
//!
//! On structures `T` with a single field of type `S`, this creates an `impl TryFrom<S> for T` that
//! applies some validation to `S`. The error type will be `&'static str`.
//!
//! You can specify the validation criterion as a function `fn check(&S) -> bool` with
//! `#[try_from_inner = "check"]` to make `try_from` fail if `check` returns `false`.
//!
//! Or, if `S` is `String`, add an attribute `#[try_from_inner_regex = "..."]` to allow
//! only values that contain a match for the given regular expression `"..."`. You need to use the
//! crates [lazy_static](https://crates.io/crates/lazy_static) and
//! [regex](https://crates.io/crates/regex) for this to work. To enforce that the regex matches
//! the _whole_ string, remember to start it with `^` and end it with `$`!
//!
//!
//! ## Example: simple wrapper types
//!
//! ```rust
//! #[macro_use]
//! extern crate derive_serialize_into;
//! extern crate serde;
//! extern crate serde_json;
//!
//! #[derive(DeserializeFrom, FromInner, IntoInner, SerializeInto, Debug, Eq, PartialEq)]
//! struct Seconds(i64);
//!
//! #[derive(DeserializeFrom, FromInner, IntoInner, SerializeInto, Debug, Eq, PartialEq)]
//! struct Days {
//!     number: i64,
//! }
//!
//! fn main() {
//!     assert_eq!(Seconds(5), serde_json::from_str("5").unwrap());
//!     assert_eq!("5", &serde_json::to_string(&Seconds(5)).unwrap());
//!     assert!(serde_json::from_str::<Seconds>("nan").is_err());
//!     assert_eq!(Days { number: 5 }, serde_json::from_str("5").unwrap());
//!     assert_eq!("5", &serde_json::to_string(&Days { number: 5 }).unwrap());
//!     assert!(serde_json::from_str::<Days>("nan").is_err());
//! }
//! ```
//!
//!
//! ## Example: validated email addresses
//!
//! ```rust
//! #[macro_use]
//! extern crate derive_serialize_into;
//! extern crate serde;
//! extern crate serde_json;
//! extern crate try_from;
//! extern crate validator;
//!
//! #[derive(DeserializeTryFrom, IntoInner, SerializeInto, TryFromInner, Debug, Eq, PartialEq)]
//! #[try_from_inner = "validator::validate_email"]
//! struct Email(String);
//!
//! fn main() {
//!     let email_json = r#""user@domain.com""#;
//!     let email = Email("user@domain.com".to_string());
//!     assert_eq!(email, serde_json::from_str(email_json).unwrap());
//!     assert_eq!(email_json, &serde_json::to_string(&email).unwrap());
//!     assert!(serde_json::from_str::<Email>(r#""missing_at_sign""#).is_err());
//! }
//! ```
//!
//!
//! ## Example: validated phone numbers
//!
//! ```rust
//! #[macro_use]
//! extern crate derive_serialize_into;
//! #[macro_use]
//! extern crate lazy_static;
//! extern crate regex;
//! extern crate try_from;
//!
//! use try_from::TryFrom;
//!
//! #[derive(TryFromInner)]
//! #[try_from_inner_regex = "^\\+?[[:digit:]]+$"]
//! struct Phone(String);
//!
//! fn main() {
//!     assert!(Phone::try_from("+12345".to_string()).is_ok());
//!     assert!(Phone::try_from("12345".to_string()).is_ok());
//!     assert!(Phone::try_from("12345XY".to_string()).is_err());
//! }
//! ```
//!
//!
//! ## Example: custom `TryFrom` and `Into`
//!
//! ```rust
//! #[macro_use]
//! extern crate derive_serialize_into;
//! extern crate serde;
//! extern crate serde_json;
//! extern crate try_from;
//!
//! #[derive(DeserializeTryFrom, SerializeInto, Debug, Eq, PartialEq)]
//! #[serialize_into(String)]
//! #[deserialize_from(String)]
//! enum Money {
//!     Eur(u64),
//!     Usd(u64),
//! }
//!
//! impl try_from::TryFrom<String> for Money {
//!     type Err = &'static str;
//!
//!     fn try_from(s: String) -> Result<Money, &'static str> {
//!         let amt_str: String = s.chars().skip(1).collect::<String>();
//!         let amt = amt_str.parse().map_err(|_| "invalid amount")?;
//!         match s.chars().next() {
//!             Some('€') => Ok(Money::Eur(amt)),
//!             Some('$') => Ok(Money::Usd(amt)),
//!             _ => Err("invalid currency"),
//!         }
//!     }
//! }
//!
//! impl<'a> From<&'a Money> for String {
//!     fn from(money: &Money) -> String {
//!         match *money {
//!             Money::Eur(amt) => format!("€{}", amt),
//!             Money::Usd(amt) => format!("${}", amt),
//!         }
//!     }
//! }
//!
//! fn main() {
//!      assert_eq!(Money::Eur(5), serde_json::from_str(r#""€5""#).unwrap());
//!      assert_eq!(r#""$5""#, &serde_json::to_string(&Money::Usd(5)).unwrap());
//!      assert!(serde_json::from_str::<Money>(r#""8""#).is_err());
//! }
//! ```
//!
//!
#![recursion_limit = "128"]
extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[doc(hidden)]
#[proc_macro_derive(DeserializeTryFrom, attributes(deserialize_from))]
pub fn derive_deserialize_try_from(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    deserialize_from_impl(&ast, true).into()
}

#[doc(hidden)]
#[proc_macro_derive(DeserializeFrom, attributes(deserialize_from))]
pub fn derive_deserialize_from(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    deserialize_from_impl(&ast, false).into()
}

fn deserialize_from_impl(ast: &syn::DeriveInput, try: bool) -> quote::Tokens {
    let name = ast.ident;
    let base = get_attr_type(&ast, "deserialize_from").unwrap_or_else(|| get_field(&ast).1);
    let (_, ty_generics, where_clause) = ast.generics.split_for_impl();
    let lifetime = syn::parse(quote! { 'de_derive_serialize_into }.into()).unwrap();
    let generics = with_lifetime(&ast.generics, &lifetime);
    let (impl_generics, _, _) = generics.split_for_impl();

    let body = if try {
        quote! {
            ::try_from::TryInto::try_into(s).map_err(<D::Error as serde::de::Error>::custom)
        }
    } else {
        quote! { Ok(s.into()) }
    };

    quote! {
        impl#impl_generics ::serde::Deserialize<#lifetime> for #name#ty_generics #where_clause {
            fn deserialize<D: ::serde::Deserializer<#lifetime>>(deserializer: D)
                -> Result<Self, D::Error> {
                let s = <#base as ::serde::Deserialize>::deserialize(deserializer)?;
                #body
            }
        }
    }
}

#[doc(hidden)]
#[proc_macro_derive(SerializeInto, attributes(serialize_into))]
pub fn derive_serialize_into(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    serialize_into_impl(&ast).into()
}

fn serialize_into_impl(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base: syn::Type = get_attr_type(&ast, "serialize_into").unwrap_or_else(|| {
        let raw_base = get_field(&ast).1;
        parse_quote! { &#raw_base }
    });
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    quote! {
        impl#impl_generics ::serde::Serialize for #name#ty_generics #where_clause {
            fn serialize<S: ::serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let b: #base = self.into();
                ::serde::Serialize::serialize(&b, serializer)
            }
        }
    }
}

#[doc(hidden)]
#[proc_macro_derive(IntoInner)]
pub fn derive_into_inner(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    into_inner_impl(&ast).into()
}

fn into_inner_impl(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let (field, base) = get_field(&ast);
    let (_, ty_generics, where_clause) = ast.generics.split_for_impl();
    let lifetime = syn::parse(quote! { 'a_derive_serialize_into }.into()).unwrap();
    let generics = with_lifetime(&ast.generics, &lifetime);
    let (impl_generics, _, _) = generics.split_for_impl();

    let body = match field {
        None => quote! { &outer.0 },
        Some(ident) => quote! { &outer.#ident },
    };

    quote! {
        impl#impl_generics From<&#lifetime #name#ty_generics> for &#lifetime #base #where_clause {
            fn from(outer: &#name#ty_generics) -> &#base {
                #body
            }
        }
    }
}

#[doc(hidden)]
#[proc_macro_derive(FromInner)]
pub fn derive_from_inner(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    from_inner_impl(&ast).into()
}

fn from_inner_impl(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let (field, base) = get_field(&ast);
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let body = match field {
        None => quote! { #name(inner) },
        Some(ident) => quote! { #name { #ident: inner } },
    };

    quote! {
        impl#impl_generics From<#base> for #name#ty_generics #where_clause {
            fn from(inner: #base) -> #name#ty_generics {
                #body
            }
        }
    }
}

#[doc(hidden)]
#[proc_macro_derive(
    TryFromInner,
    attributes(try_from_inner, try_from_inner_regex)
)]
pub fn derive_try_from_inner(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    try_from_inner_impl(&ast).into()
}

fn try_from_inner_impl(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let check = get_attr_try_from_inner(&ast);
    let (field, base) = get_field(&ast);
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let construct = match field {
        None => quote! { #name(inner) },
        Some(ident) => quote! { #name { #ident: inner } },
    };

    let fn_body = match check {
        TryFromInnerCheck::BoolFn(path) => quote! {
            if #path(&inner) {
                Ok(#construct)
            } else {
                Err("validation function returned false")
            }
        },
        TryFromInnerCheck::Regex(regex) => quote! {
            lazy_static! {
                static ref RE: ::regex::Regex = ::regex::Regex::new(#regex).expect("invalid regex");
            }
            if RE.is_match(&inner) {
                Ok(#construct)
            } else {
                Err("value does not match regular expression")
            }
        },
    };

    quote! {
        impl#impl_generics ::try_from::TryFrom<#base> for #name#ty_generics #where_clause {
            type Err = &'static str;

            fn try_from(inner: #base) -> ::std::result::Result<#name#ty_generics, &'static str> {
                #fn_body
            }
        }
    }
}

/// Returns the `generics` with the `lifetime` prepended.
fn with_lifetime(generics: &syn::Generics, lifetime: &syn::LifetimeDef) -> syn::Generics {
    let mut new_generics = generics.clone();
    new_generics.params.insert(0, lifetime.clone().into());
    new_generics
}

/// Criteria for `TryFrom` validations.
enum TryFromInnerCheck {
    /// A function returning `true` if the value is valid.
    BoolFn(syn::ExprPath),
    /// A regular expression that must have a match in the value.
    Regex(syn::LitStr),
}

/// Returns the unique `TryFromInnerCheck`. Panics if there are no or multiple `try_from_inner` and
/// `try_from_inner_regex` attributes.
fn get_attr_try_from_inner(ast: &syn::DeriveInput) -> TryFromInnerCheck {
    let bool_fn = map_unique_attr(ast, "try_from_inner", |meta| {
        let nv = match meta {
            syn::Meta::NameValue(nv) => nv,
            _ => panic!("try_from_inner attribute requires a name-value pair"),
        };
        let fn_str = match nv.lit {
            syn::Lit::Str(lit_str) => lit_str.value(),
            _ => panic!("try_from_inner attribute needs a function name as a string value"),
        };
        let fn_path = syn::parse_str(&fn_str).expect("try_from_inner argument must be a function");
        TryFromInnerCheck::BoolFn(fn_path)
    });
    let regex = map_unique_attr(ast, "try_from_inner_regex", |meta| {
        let nv = match meta {
            syn::Meta::NameValue(nv) => nv,
            _ => panic!("try_from_inner_regex attribute requires a name-value pair"),
        };
        match nv.lit {
            syn::Lit::Str(lit_str) => TryFromInnerCheck::Regex(lit_str),
            _ => panic!("try_from_inner_regex attribute needs a regex as a string value"),
        }
    });
    match (bool_fn, regex) {
        (None, None) => panic!("try_from_inner or try_from_inner_regex attribute missing"),
        (Some(_), Some(_)) => panic!("only one of try_from_inner and try_from_inner_regex allowed"),
        (Some(bool_fn), None) => bool_fn,
        (None, Some(regex)) => regex,
    }
}

/// Returns the type that should be used for serialization or deserialization, if specified
/// explicitly in the attributes.
fn get_attr_type(ast: &syn::DeriveInput, name: &str) -> Option<syn::Type> {
    map_unique_attr(ast, name, |meta| match meta {
        syn::Meta::Word(_) | syn::Meta::NameValue(_) => {
            panic!("Base type attribute must be a single identifier in brackets.");
        }
        syn::Meta::List(list) => {
            if list.nested.len() != 1 {
                panic!("Base type attribute must be a single identifier in brackets.");
            }
            match list.nested.first().unwrap().into_value().clone() {
                syn::NestedMeta::Meta(syn::Meta::Word(word)) => syn::TypeVerbatim {
                    tts: parse_quote! { #word },
                }.into(),
                _ => panic!("Base type attribute must be a single identifier in brackets."),
            }
        }
    })
}

/// Returns the image of the unique attribute with the `name` under `f`, if any. Panics if there
/// are multiple attributes with that name.
fn map_unique_attr<T, F>(ast: &syn::DeriveInput, name: &str, f: F) -> Option<T>
where
    F: Fn(syn::Meta) -> T,
{
    let mut t_iter = ast
        .attrs
        .iter()
        .filter_map(syn::Attribute::interpret_meta)
        .filter(|meta| meta.name() == name)
        .map(f);
    match (t_iter.next(), t_iter.next()) {
        (_, Some(_)) => panic!("Multiple {} attributes specified.", name),
        (result, None) => result,
    }
}

/// Returns the name and type of the field, if there is a unique one.
fn get_field(ast: &syn::DeriveInput) -> (Option<syn::Ident>, syn::Type) {
    match &ast.data {
        syn::Data::Struct(data_struct) => {
            let mut fields = data_struct.fields.iter();
            match (fields.next(), fields.next()) {
                (Some(field), None) => (field.ident, field.ty.clone()),
                _ => panic!("Can only derive base type for structs with a single field."),
            }
        }
        _ => unimplemented!(),
    }
}
