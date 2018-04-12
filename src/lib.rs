//! # Derive `Serialize` and `Deserialize` for wrapper types
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
//!
//! ## Supported derive attributes
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
//! ### `#[derive(SerializeInto)]`
//!
//! On structures `T`, given an `impl Into<S> for &T`, this creates an `impl Serialize for T` which
//! first converts to `S` and then serializes it. If `#[serialize_into(S)]` is not specified and
//! `T` is a structure with a single field, it uses a reference to that field's type as `S`.
//!
//! ### `#[derive(DeserializeFrom)]`
//!
//! On structures `T`, given an `impl Into<T> for S`, this creates an `impl Deserialize for T`
//! which first deserializes to `S` and then converts to `T`. If `#[deserialize_from(S)]` is not
//! specified and `T` is a structure with a single field, it uses that field's type as `S`.
//!
//! ### `#[derive(DeserializeTryFrom)]`
//!
//! On structures `T`, given an `impl TryInto<T> for S`, this creates an `impl Deserialize for T`
//! which first deserializes to `S` and then converts to `T`. If `#[deserialize_from(S)]` is not
//! specified and `T` is a structure with a single field, it uses that field's type as `S`.
//! Deserialization fails if conversion from `S` to `T` fails. The `TryInto` implementation's error
//! type must implement `Display` and will be converted to a custom deserialization error.
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
//! #[derive(DeserializeTryFrom, IntoInner, SerializeInto, Debug, Eq, PartialEq)]
//! struct Email(String);
//!
//! impl try_from::TryFrom<String> for Email {
//!     type Err = &'static str;
//!
//!     fn try_from(raw: String) -> Result<Email, &'static str> {
//!         if validator::validate_email(&raw) {
//!             Ok(Email(raw))
//!         } else {
//!             Err("invalid email address")
//!         }
//!     }
//! }
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
    deserialize_try_from_impl(ast).into()
}

fn deserialize_try_from_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base = get_attr_type(&ast, "deserialize_from").unwrap_or_else(|| get_field(&ast).1);
    let generics = ast.generics;
    let lifetime = syn::parse(quote! { 'de }.into()).unwrap();
    let impl_generics = with_lifetime(&generics, &lifetime);

    quote! {
        impl#impl_generics ::serde::Deserialize<#lifetime> for #name#generics {
            fn deserialize<D: ::serde::Deserializer<#lifetime>>(deserializer: D)
                -> Result<Self, D::Error> {
                let s = <#base as ::serde::Deserialize>::deserialize(deserializer)?;
                ::try_from::TryInto::try_into(s).map_err(<D::Error as serde::de::Error>::custom)
            }
        }
    }
}

#[doc(hidden)]
#[proc_macro_derive(DeserializeFrom, attributes(deserialize_from))]
pub fn derive_deserialize_from(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    deserialize_from_impl(ast).into()
}

fn deserialize_from_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base = get_attr_type(&ast, "deserialize_from").unwrap_or_else(|| get_field(&ast).1);
    let generics = ast.generics;
    let lifetime = syn::parse(quote! { 'de }.into()).unwrap();
    let impl_generics = with_lifetime(&generics, &lifetime);

    quote! {
        impl#impl_generics ::serde::Deserialize<#lifetime> for #name#generics {
            fn deserialize<D: ::serde::Deserializer<#lifetime>>(deserializer: D)
                -> Result<Self, D::Error> {
                let s = <#base as ::serde::Deserialize>::deserialize(deserializer)?;
                Ok(s.into())
            }
        }
    }
}

#[doc(hidden)]
#[proc_macro_derive(SerializeInto, attributes(serialize_into))]
pub fn derive_serialize_into(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    serialize_into_impl(ast).into()
}

fn serialize_into_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base: syn::Type = get_attr_type(&ast, "deserialize_from").unwrap_or_else(|| {
        let raw_base = get_field(&ast).1;
        parse_quote! { &#raw_base }
    });
    let generics = ast.generics;

    quote! {
        impl#generics ::serde::Serialize for #name#generics {
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
    into_inner_impl(ast).into()
}

fn into_inner_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let (field, base) = get_field(&ast);
    let generics = ast.generics;
    let lifetime = syn::parse(quote! { 'a }.into()).unwrap();
    let impl_generics = with_lifetime(&generics, &lifetime);

    let construct = match field {
        None => quote! { &outer.0 },
        Some(ident) => quote! { &outer.#ident },
    };

    quote! {
        impl#impl_generics From<&#lifetime #name#generics> for &#lifetime #base {
            fn from(outer: &#name#generics) -> &#base {
                #construct
            }
        }
    }
}

#[doc(hidden)]
#[proc_macro_derive(FromInner)]
pub fn derive_from_inner(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    from_inner_impl(ast).into()
}

fn from_inner_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let (field, base) = get_field(&ast);
    let generics = ast.generics;

    let construct = match field {
        None => quote! { #name(inner) },
        Some(ident) => quote! { #name { #ident: inner } },
    };

    quote! {
        impl#generics From<#base> for #name#generics {
            fn from(inner: #base) -> #name#generics {
                #construct
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

/// Returns the type that should be used for serialization or deserialization, if specified
/// explicitly in the attributes.
fn get_attr_type(ast: &syn::DeriveInput, name: &str) -> Option<syn::Type> {
    let attr_base_idents: Vec<_> = ast.attrs
        .iter()
        .filter_map(syn::Attribute::interpret_meta)
        .filter(|meta| meta.name() == name)
        .map(|meta| match meta {
            syn::Meta::Word(_) | syn::Meta::NameValue(_) => {
                panic!("Base type attribute must be a single identifier in brackets.");
            }
            syn::Meta::List(list) => {
                if list.nested.len() != 1 {
                    panic!("Base type attribute must be a single identifier in brackets.");
                }
                match list.nested.first().unwrap().into_value().clone() {
                    syn::NestedMeta::Meta(syn::Meta::Word(word)) => word,
                    _ => panic!("Base type attribute must be a single identifier in brackets."),
                }
            }
        })
        .collect();
    match attr_base_idents.len() {
        0 => None,
        1 => {
            let base = attr_base_idents[0];
            return Some(
                syn::TypeVerbatim {
                    tts: parse_quote! { #base },
                }.into(),
            );
        }
        _ => panic!("Multiple base types specified."),
    }
}

/// Returns the name and type of the field, if there is a unique one.
fn get_field(ast: &syn::DeriveInput) -> (Option<syn::Ident>, syn::Type) {
    // TODO: Make sure this works well with generics.
    match ast.data {
        syn::Data::Struct(ref data_struct) => {
            let mut fields = data_struct.fields.iter();
            match (fields.next(), fields.next()) {
                (Some(field), None) => (field.ident.clone(), field.ty.clone()),
                _ => panic!("Can only derive base type for structs with a single field."),
            }
        }
        _ => unimplemented!(),
    }
}
