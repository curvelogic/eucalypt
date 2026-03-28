//! SplitMix64 PRNG core functions shared by stream intrinsics.

use serde_json::Number;

/// SplitMix64 golden ratio constant
const GOLDEN: u64 = 0x9e3779b97f4a7c15;

/// Advance the SplitMix64 state and produce an output value.
///
/// Returns `(next_state, output)` where `output` is the mixed result.
pub(super) fn splitmix64(seed: u64) -> (u64, u64) {
    let state = seed.wrapping_add(GOLDEN);
    let mut z = state;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
    z ^= z >> 31;
    (state, z)
}

/// Convert a seed Number to u64, treating signed values via
/// reinterpretation of the bit pattern.
pub(super) fn seed_to_u64(n: &Number) -> u64 {
    if let Some(v) = n.as_u64() {
        v
    } else if let Some(v) = n.as_i64() {
        v as u64
    } else if let Some(v) = n.as_f64() {
        v.to_bits()
    } else {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splitmix64_deterministic() {
        let (s1, z1) = splitmix64(42);
        let (s2, z2) = splitmix64(42);
        assert_eq!(s1, s2);
        assert_eq!(z1, z2);
    }

    #[test]
    fn splitmix64_different_seeds() {
        let (_, z1) = splitmix64(42);
        let (_, z2) = splitmix64(123);
        assert_ne!(z1, z2);
    }

    #[test]
    fn splitmix64_float_range() {
        for seed in 0u64..1000 {
            let (_, z) = splitmix64(seed);
            let f = (z >> 11) as f64 / ((1u64 << 53) as f64);
            assert!(f >= 0.0, "float {f} below 0 for seed {seed}");
            assert!(f < 1.0, "float {f} >= 1 for seed {seed}");
        }
    }

    #[test]
    fn splitmix64_chain() {
        // Verify chaining produces a sequence
        let (s1, _) = splitmix64(42);
        let (s2, _) = splitmix64(s1);
        let (s3, _) = splitmix64(s2);
        assert_ne!(s1, s2);
        assert_ne!(s2, s3);
    }
}
