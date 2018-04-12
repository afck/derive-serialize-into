[![Documentation](https://docs.rs/derive-serialize-into/badge.svg)](https://docs.rs/derive-serialize-into)
[![Latest version](https://img.shields.io/crates/v/derive-serialize-into.svg)](https://crates.io/crates/derive-serialize-into)

# Derive `Serialize`, `Deserialize` for wrapper types

This crate provides several custom derives that provide implementations of
[serde](https://serde.rs/)'s `Serialize` and `Deserialize` traits for wrapper types, as well as
`Deserialize` implementations that perform some validation.

Sometimes you have a single-field type

```rust
#[derive(DeserializeFrom, FromInner, IntoInner, SerializeInto)]
struct Contact {
    email: String,
}
```

which you want to serialize and deserialize as a string instead of a struct, e.g. you want its JSON
representation to just be "`"user@domain.com"`" instead of "`{ "email": "user@domain.com" }`". The
above derive attribute creates `Serialize` and `Deserialize` implementations for that purpose, as
well as `Into` and `From` implementations to convert between `String` and `Contact`.

Another example is a _validated_ wrapper type like

```rust
#[derive(DeserializeTryFrom)]
struct Email(String);
```

that should never be instantianted with a string that doesn't represent a valid email address.
If you implement `Email: TryFrom<String>` using the [try_from](https://crates.io/crates/try_from)
crate, such that conversion fails for invalid addresses, the derived `Deserialize` will also fail if
the string is in the wrong format.
