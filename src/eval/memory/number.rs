//! Exact numbers for native containers.
//!
//! Set and vec elements and dict keys hold numbers as [`ExactNum`]:
//! integers stay exact (no rounding through `f64`, so distinct integers
//! above 2^53 stay distinct) and are ordered against floats by value.

use std::cmp::Ordering;

use ordered_float::OrderedFloat;
use serde_json::Number;

/// A number element, kept exact: integers are not rounded through `f64`,
/// so distinct integers above 2^53 stay distinct (as dict keys do).
#[derive(Clone, Copy, Debug)]
pub enum ExactNum {
    /// An integer in `i64` range.
    Int(i64),
    /// An integer above `i64::MAX` (never used for values in `i64` range).
    UInt(u64),
    /// A non-integer number (or one stored as a float), ordered totally.
    Float(OrderedFloat<f64>),
}

impl ExactNum {
    /// Convert a number, keeping integers exact.
    pub fn from_number(n: &Number) -> Self {
        if let Some(i) = n.as_i64() {
            ExactNum::Int(i)
        } else if let Some(u) = n.as_u64() {
            ExactNum::UInt(u)
        } else {
            ExactNum::Float(OrderedFloat(n.as_f64().unwrap_or(f64::NAN)))
        }
    }

    /// Convert back to a number (`None` for a non-finite float, which a
    /// `Number` cannot hold).
    pub fn to_number(&self) -> Option<Number> {
        match self {
            ExactNum::Int(i) => Some(Number::from(*i)),
            ExactNum::UInt(u) => Some(Number::from(*u)),
            ExactNum::Float(f) => Number::from_f64(f.into_inner()),
        }
    }

    /// The value as an `f64` (rounded for integers beyond 2^53).
    pub fn as_f64(&self) -> f64 {
        match self {
            ExactNum::Int(i) => *i as f64,
            ExactNum::UInt(u) => *u as f64,
            ExactNum::Float(f) => f.into_inner(),
        }
    }
}

impl From<f64> for ExactNum {
    fn from(f: f64) -> Self {
        ExactNum::Float(OrderedFloat(f))
    }
}

impl PartialEq for ExactNum {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for ExactNum {}

impl PartialOrd for ExactNum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ExactNum {
    /// Exact numeric order: integers and floats compared by value, so
    /// `1` and `1.0` are equal; `-0.0` = `0.0`; NaN greatest.
    fn cmp(&self, other: &Self) -> Ordering {
        use ExactNum::*;
        match (self, other) {
            (Int(x), Int(y)) => x.cmp(y),
            (UInt(x), UInt(y)) => x.cmp(y),
            (Int(x), UInt(y)) => (*x as i128).cmp(&(*y as i128)),
            (UInt(x), Int(y)) => (*x as i128).cmp(&(*y as i128)),
            (Float(x), Float(y)) => x.cmp(y),
            (Int(x), Float(y)) => cmp_int_float(*x as i128, y.into_inner()),
            (UInt(x), Float(y)) => cmp_int_float(*x as i128, y.into_inner()),
            (Float(x), Int(y)) => cmp_int_float(*y as i128, x.into_inner()).reverse(),
            (Float(x), UInt(y)) => cmp_int_float(*y as i128, x.into_inner()).reverse(),
        }
    }
}

/// Exact comparison of an integer (within ±2^64) with a float. NaN is
/// greater than every integer.
fn cmp_int_float(i: i128, f: f64) -> Ordering {
    const TWO_64: f64 = 18_446_744_073_709_551_616.0;
    if f.is_nan() || f >= TWO_64 {
        return Ordering::Less;
    }
    if f < -TWO_64 {
        return Ordering::Greater;
    }
    let t = f.trunc();
    // Exact: |t| < 2^64, and every float of that magnitude with no
    // fractional part is an integer representable in i128.
    let ti = t as i128;
    match i.cmp(&ti) {
        Ordering::Equal => {
            if f > t {
                Ordering::Less
            } else if f < t {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }
        o => o,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_num_keeps_integers_above_2_pow_53_distinct() {
        let big = Number::from(9_007_199_254_740_992_u64);
        let big1 = Number::from(9_007_199_254_740_993_u64);
        let a = ExactNum::from_number(&big);
        let b = ExactNum::from_number(&big1);
        assert_eq!(a.cmp(&b), Ordering::Less);
        assert_eq!(b.to_number(), Some(big1));
        let u = ExactNum::from_number(&Number::from(u64::MAX));
        assert!(matches!(u, ExactNum::UInt(u64::MAX)));
        assert_eq!(ExactNum::Int(i64::MAX).cmp(&u), Ordering::Less);
        assert_eq!(u.to_number(), Some(Number::from(u64::MAX)));
    }

    #[test]
    fn exact_num_compares_integers_and_floats_by_value() {
        assert_eq!(ExactNum::Int(1), ExactNum::from(1.0));
        assert_eq!(ExactNum::Int(0), ExactNum::from(-0.0));
        assert!(ExactNum::Int(2) < ExactNum::from(2.5));
        assert!(ExactNum::from(2.5) < ExactNum::Int(3));
        assert!(ExactNum::Int(-3) < ExactNum::from(-2.5));
        // 2^53 + 1 is not equal to the float 2^53 it would round to.
        assert!(ExactNum::Int(9_007_199_254_740_993) > ExactNum::from(9_007_199_254_740_992.0));
        assert!(ExactNum::UInt(u64::MAX) < ExactNum::from(f64::NAN));
        assert!(ExactNum::from(f64::INFINITY) > ExactNum::UInt(u64::MAX));
        assert!(ExactNum::from(f64::NEG_INFINITY) < ExactNum::Int(i64::MIN));
    }
}
