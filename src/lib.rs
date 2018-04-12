//! # Custom derives `SerializeInto`, `DeserializeFrom` and `DeserializeTryFrom`.
//!
//! If you have a type `T: Serialize + Deserialize` which can be converted to and from
//! another type `S`, you can derive a `Serialize` implementation on it that converts to
//! `T` before serialization, and a `Deserialize` implementation that deserializes to `T` and then
//! converts to `S`. For the former, the requirement is that `&T: Into<&S>`, and for the latter it
//! is `S: Into<T>`.
//!
//! If `S` is a structure with a single field of type `T`, all you need is
//! `#[derive(SerializeInto, DeserializeFrom)]`. Otherwise you have to specify the type `T` using
//! `#[serialize_into(T)]` or `#[deserialize_from(T)]`.
//!
//! If `S: TryInto<T>` instead, possibly applying some validation, `#[derive(DeserializeTryFrom)]`
//! will create a `Deserialize` implementation that fails even if deserialization into `T` succeds
//! but conversion to `S` doesn't.
//!
//! Finally, you can `#[derive(IntoInner)]` and `#[derive(FromInner)]` to avoid typing out the
//! implementations for `&S: From<&T>` and `T: From<S>` by hand, if `T` is a single-field
//! structure.
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
//! fn main() {
//!     assert_eq!(Seconds(5), serde_json::from_str("5").unwrap());
//!     assert_eq!("5", &serde_json::to_string(&Seconds(5)).unwrap());
//!     assert!(serde_json::from_str::<Seconds>("nan").is_err());
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
//!     type Err = ();
//!
//!     fn try_from(raw: String) -> Result<Email, ()> {
//!         if validator::validate_email(&raw) {
//!             Ok(Email(raw))
//!         } else {
//!             Err(())
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
#![recursion_limit = "128"]
extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_derive(DeserializeTryFrom)]
pub fn derive_deserialize_try_from(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    deserialize_try_from_impl(ast).into()
}

fn deserialize_try_from_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base = get_base_type(&ast, "deserialize_from");
    let generics = ast.generics;
    let lifetime = syn::parse(quote! { 'de }.into()).unwrap();
    let impl_generics = with_lifetime(&generics, &lifetime);

    quote! {
        impl#impl_generics ::serde::Deserialize<#lifetime> for #name#generics {
            fn deserialize<D: ::serde::Deserializer<#lifetime>>(deserializer: D)
                -> Result<Self, D::Error> {
                let s = <#base as ::serde::Deserialize>::deserialize(deserializer)?;
                ::try_from::TryInto::try_into(s).map_err(|_| {
                    <D::Error as serde::de::Error>::invalid_value(
                        serde::de::Unexpected::Str("invalid value"), // TODO
                        &"valid value", // TODO
                    )
                })
            }
        }
    }
}

#[proc_macro_derive(DeserializeFrom)]
pub fn derive_deserialize_from(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    deserialize_from_impl(ast).into()
}

fn deserialize_from_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base = get_base_type(&ast, "deserialize_from");
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

#[proc_macro_derive(SerializeInto)]
pub fn derive_serialize_into(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    serialize_into_impl(ast).into()
}

fn serialize_into_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base = get_base_type(&ast, "serialize_into");
    let generics = ast.generics;

    quote! {
        impl#generics ::serde::Serialize for #name#generics {
            fn serialize<S: ::serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let b: &#base = self.into();
                ::serde::Serialize::serialize(b, serializer)
            }
        }
    }
}

#[proc_macro_derive(IntoInner)]
pub fn derive_into_inner(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    into_inner_impl(ast).into()
}

fn into_inner_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base = get_field_type(&ast);
    let generics = ast.generics;
    let lifetime = syn::parse(quote! { 'a }.into()).unwrap();
    let impl_generics = with_lifetime(&generics, &lifetime);

    // TODO: Support named fields.
    quote! {
        impl#impl_generics From<&#lifetime #name#generics> for &#lifetime #base {
            fn from(outer: &#name#generics) -> &#base {
                &outer.0
            }
        }
    }
}

#[proc_macro_derive(FromInner)]
pub fn derive_from_inner(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    from_inner_impl(ast).into()
}

fn from_inner_impl(ast: syn::DeriveInput) -> quote::Tokens {
    let name = ast.ident;
    let base = get_field_type(&ast);
    let generics = ast.generics;

    // TODO: Support named fields.
    quote! {
        impl#generics From<#base> for #name#generics {
            fn from(inner: #base) -> #name#generics {
                #name(inner)
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

/// Returns the type that should be used for serialization or deserialization.
fn get_base_type(ast: &syn::DeriveInput, name: &str) -> syn::Type {
    // Check if there is exactly one attribute specifying the base type.
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
        0 => (), // Try to determine the base type from the unique field.
        1 => {
            let base = attr_base_idents[0];
            return syn::TypeVerbatim {
                tts: parse_quote! { #base },
            }.into();
        }
        _ => panic!("Multiple base types specified."),
    };

    // Otherwise use the field's type, if the structure has one unique field.
    get_field_type(ast)
}

/// Returns the type of the field, if there is a unique one.
fn get_field_type(ast: &syn::DeriveInput) -> syn::Type {
    // TODO: Make sure this works well with generics.
    // TODO: Support named and unnamed fields.
    match ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Unnamed(syn::FieldsUnnamed { ref unnamed, .. }),
            ..
        }) => {
            if unnamed.len() != 1 {
                panic!("Can only derive base type for structs with a single field.")
            }
            unnamed.first().unwrap().into_value().ty.clone()
        }
        _ => unimplemented!(),
    }
}
