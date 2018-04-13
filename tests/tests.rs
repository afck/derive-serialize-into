#[macro_use]
extern crate derive_serialize_into;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate try_from;

use serde::{Deserialize, Serialize};
use serde::de::DeserializeOwned;
use std::fmt::Debug;
use try_from::TryFrom;

#[derive(Serialize, Debug, Eq, PartialEq)]
struct Item<T: Serialize + DeserializeOwned>(T)
where
    T: Debug;

impl<'de, T: Serialize + DeserializeOwned + Debug> Deserialize<'de> for Item<T> {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        Ok(Item(T::deserialize(d)?))
    }
}

/// An example type with generic type parameters and where clause.
#[derive(DeserializeFrom, FromInner, IntoInner, SerializeInto, Debug, Eq, PartialEq)]
struct Anything<T: Serialize>(Item<T>)
where
    T: Debug + DeserializeOwned;

/// An example type with generic type parameters and where clause that uses `TryFrom`.
#[derive(DeserializeTryFrom, IntoInner, SerializeInto, Debug, Eq, PartialEq)]
struct NotDefault<T: Serialize + DeserializeOwned + Eq + Debug>(Item<T>)
where
    T: Default;

impl<T: Serialize + DeserializeOwned + Default + Eq + Debug> TryFrom<Item<T>> for NotDefault<T> {
    type Err = &'static str;

    fn try_from(item: Item<T>) -> Result<Self, Self::Err> {
        if item.0 == T::default() {
            Err("cannot be default")
        } else {
            Ok(NotDefault(item))
        }
    }
}

#[test]
fn anything() {
    let a5 = Anything(Item(5u8));
    assert_eq!(a5, serde_json::from_str(r#"5"#).unwrap());
    assert_eq!(r#"5"#, &serde_json::to_string(&a5).unwrap());
}

#[test]
fn not_default() {
    let nd5 = NotDefault(Item(5u8));
    assert_eq!(nd5, serde_json::from_str(r#"5"#).unwrap());
    assert_eq!(r#"5"#, &serde_json::to_string(&nd5).unwrap());
    assert!(serde_json::from_str::<NotDefault<u8>>(r#"0"#).is_err());
}
