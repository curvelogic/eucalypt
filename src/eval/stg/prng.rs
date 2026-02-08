//! Intrinsics for pseudo-random number generation using SplitMix64

use serde_json::Number;

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
    memory::{mutator::MutatorHeapView, syntax::Ref},
};

use super::support::{machine_return_num, num_arg};

/// SplitMix64 golden ratio constant
const GOLDEN: u64 = 0x9e3779b97f4a7c15;

/// Advance the SplitMix64 state and produce an output value.
///
/// Returns `(next_state, output)` where `output` is the mixed result.
fn splitmix64(seed: u64) -> (u64, u64) {
    let state = seed.wrapping_add(GOLDEN);
    let mut z = state;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
    z ^= z >> 31;
    (state, z)
}

/// Convert a seed Number to u64, treating signed values via
/// reinterpretation of the bit pattern.
fn seed_to_u64(n: &Number) -> u64 {
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

/// PRNG_NEXT(seed) — advance the SplitMix64 state, returning the next
/// seed as an integer.
pub struct PrngNext;

impl StgIntrinsic for PrngNext {
    fn name(&self) -> &str {
        "PRNG_NEXT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let seed_num = num_arg(machine, view, &args[0])?;
        let seed = seed_to_u64(&seed_num);
        let (next_state, _) = splitmix64(seed);
        // Return as i64 to stay within JSON number range and allow
        // round-tripping through eucalypt's number representation.
        machine_return_num(machine, view, Number::from(next_state as i64))
    }
}

impl CallGlobal1 for PrngNext {}

/// PRNG_FLOAT(seed) — produce a float in [0, 1) from the given seed
/// using SplitMix64 output mixing.
pub struct PrngFloat;

impl StgIntrinsic for PrngFloat {
    fn name(&self) -> &str {
        "PRNG_FLOAT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let seed_num = num_arg(machine, view, &args[0])?;
        let seed = seed_to_u64(&seed_num);
        let (_, z) = splitmix64(seed);
        let float_val = (z >> 11) as f64 / ((1u64 << 53) as f64);
        let result = Number::from_f64(float_val)
            .ok_or_else(|| ExecutionError::Panic("PRNG produced invalid float".to_string()))?;
        machine_return_num(machine, view, result)
    }
}

impl CallGlobal1 for PrngFloat {}

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
