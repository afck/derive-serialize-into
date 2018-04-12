[![Documentation](https://docs.rs/derive-serialize-into/badge.svg)](https://docs.rs/derive-serialize-into)
[![Latest version](https://img.shields.io/crates/v/derive-serialize-into.svg)](https://crates.io/crates/derive-serialize-into)

# Derive `Serialize`, `Deserialize` for wrapper types

This crate provides several custom derives that make it more convenient to create implementations of
[serde](https://serde.rs/)'s `Serialize` and `Deserialize` traits for types `T` that can be
converted to or from another type `S` which already does implement those traits.

One use case is if you have a single-field type like

```rust
#[derive(SerializeInto, DeserializeFrom)]
struct Contact {
    email: String,
}
```

which you want to serialize and deserialize as a string instead of a struct.

Another example is a _validated_ wrapper type like

```rust
#[derive(DeserializeTryFrom)]
struct Email(String);
```

that should never be instantianted with a string that doesn't represent a valid email address.
If you implement `Email: TryFrom<String>` using the [try_from](https://crates.io/crates/try_from)
crate, such that conversion fails for invalid addresses, the derived `Deserialize` will also fail if
the string is in the wrong format.
