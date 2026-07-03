//! Deterministic serialisation for `HashMap` fields.
//!
//! `HashMap` iterates in an arbitrary, run-to-run order, so serialising one
//! directly (e.g. with postcard into the prelude blob) produces different
//! bytes on every run even when the logical contents are identical.  This
//! breaks reproducible builds and confounds blob-path benchmarking (eu-c2ue).
//!
//! Apply `#[serde(with = "crate::common::serde_sorted")]` to a
//! `HashMap<K, V>` field where `K: Ord` to serialise its entries in sorted
//! key order.  The wire format is identical to a plain map (postcard encodes
//! both `HashMap` and `BTreeMap` as a length-prefixed sequence of entries),
//! so deserialisation reads straight back into a `HashMap` and nothing else
//! in the codebase needs to change.

use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// Serialise a `HashMap` with its entries in sorted key order.
pub fn serialize<S, K, V>(map: &HashMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    K: Serialize + Ord,
    V: Serialize,
{
    // Collect into a BTreeMap so the entries are emitted in a stable, sorted
    // order regardless of the source HashMap's iteration order.
    let sorted: BTreeMap<&K, &V> = map.iter().collect();
    sorted.serialize(serializer)
}

/// Deserialise a `HashMap` (order is irrelevant on the way in).
pub fn deserialize<'de, D, K, V>(deserializer: D) -> Result<HashMap<K, V>, D::Error>
where
    D: Deserializer<'de>,
    K: Deserialize<'de> + Eq + Hash,
    V: Deserialize<'de>,
{
    HashMap::deserialize(deserializer)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Serialize, Deserialize, PartialEq, Debug)]
    struct Holder {
        #[serde(with = "super")]
        map: HashMap<String, u32>,
    }

    #[test]
    fn sorted_serialisation_is_order_independent() {
        // Two logically-equal maps built with different insertion orders must
        // serialise to identical bytes.
        let mut a = HashMap::new();
        a.insert("zebra".to_string(), 1u32);
        a.insert("apple".to_string(), 2u32);
        a.insert("mango".to_string(), 3u32);

        let mut b = HashMap::new();
        b.insert("mango".to_string(), 3u32);
        b.insert("zebra".to_string(), 1u32);
        b.insert("apple".to_string(), 2u32);

        let ba = postcard::to_allocvec(&Holder { map: a }).unwrap();
        let bb = postcard::to_allocvec(&Holder { map: b }).unwrap();
        assert_eq!(ba, bb, "sorted serialisation must be order-independent");
    }

    #[test]
    fn round_trips() {
        let mut map = HashMap::new();
        map.insert("one".to_string(), 1u32);
        map.insert("two".to_string(), 2u32);
        let holder = Holder { map };
        let bytes = postcard::to_allocvec(&holder).unwrap();
        let restored: Holder = postcard::from_bytes(&bytes).unwrap();
        assert_eq!(holder, restored);
    }
}
