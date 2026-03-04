//! Create a core syntax block with environment data
//!
//! This is a poor way of inserting environment data. Whilst it does
//! has the benefit of being pure immutable data once merged, the fact
//! it is different on every run yet inserted at core syntax stage
//! means we cannot cache core output. However it'll do for now.

use crate::core::expr::{acore, RcExpr};

#[cfg(not(target_arch = "wasm32"))]
use chrono::{offset::Local, TimeZone};
#[cfg(not(target_arch = "wasm32"))]
use std::{
    env,
    time::{SystemTime, UNIX_EPOCH},
};

/// A core block of environment variables
#[cfg(not(target_arch = "wasm32"))]
fn env_vars() -> RcExpr {
    acore::block(env::vars().map(|(k, v)| (k, acore::str(&v))))
}

#[cfg(target_arch = "wasm32")]
fn env_vars() -> RcExpr {
    acore::block(std::iter::empty::<(String, RcExpr)>())
}

/// Duration since unix epoch in seconds
#[cfg(not(target_arch = "wasm32"))]
fn epoch_time() -> RcExpr {
    if let Ok(secs) = SystemTime::now().duration_since(UNIX_EPOCH) {
        acore::num(secs.as_secs())
    } else {
        acore::null()
    }
}

#[cfg(target_arch = "wasm32")]
fn epoch_time() -> RcExpr {
    acore::num(0i64)
}

/// Current timezone information
#[cfg(not(target_arch = "wasm32"))]
fn tz() -> RcExpr {
    let offset = format!(
        "{}",
        Local
            .timestamp_opt(0, 0)
            .single()
            .expect("epoch timestamp should be unambiguous")
            .offset()
    );
    acore::block(vec![("offset".to_string(), acore::str(&offset))])
}

#[cfg(target_arch = "wasm32")]
fn tz() -> RcExpr {
    acore::block(vec![("offset".to_string(), acore::str("+00:00"))])
}

/// Generate a random seed from the current time
#[cfg(not(target_arch = "wasm32"))]
fn random_seed(explicit_seed: Option<i64>) -> RcExpr {
    if let Some(seed) = explicit_seed {
        acore::num(seed)
    } else if let Ok(duration) = SystemTime::now().duration_since(UNIX_EPOCH) {
        acore::num(duration.as_nanos() as i64)
    } else {
        acore::num(0i64)
    }
}

#[cfg(target_arch = "wasm32")]
fn random_seed(explicit_seed: Option<i64>) -> RcExpr {
    acore::num(explicit_seed.unwrap_or(0))
}

/// Construct a core block containing environment data for merging
/// into core.
pub fn create_io_pseudoblock(seed: Option<i64>) -> RcExpr {
    acore::block(vec![
        ("ENV".to_string(), env_vars()),
        ("EPOCHTIME".to_string(), epoch_time()),
        ("TZ".to_string(), tz()),
        ("RANDOM_SEED".to_string(), random_seed(seed)),
    ])
}

/// Construct a core list containing command-line arguments passed
/// after the -- separator.
pub fn create_args_pseudoblock(args: &[String]) -> RcExpr {
    acore::list(args.iter().map(acore::str).collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::{Expr, Primitive};

    #[test]
    fn test_args_pseudoblock_empty() {
        let result = create_args_pseudoblock(&[]);
        if let Expr::List(_, elements) = &*result.inner {
            assert!(elements.is_empty());
        } else {
            panic!("Expected List, got {:?}", result);
        }
    }

    #[test]
    fn test_args_pseudoblock_basic() {
        let args = vec!["foo".to_string(), "bar".to_string()];
        let result = create_args_pseudoblock(&args);
        if let Expr::List(_, elements) = &*result.inner {
            assert_eq!(elements.len(), 2);
        } else {
            panic!("Expected List, got {:?}", result);
        }
    }

    #[test]
    fn test_args_pseudoblock_special_characters() {
        let args = vec![
            "hello world".to_string(),     // spaces
            "foo=bar".to_string(),         // equals
            "path/to/file".to_string(),    // slashes
            "flag-name".to_string(),       // dashes
            "--flag".to_string(),          // double dash
            "\"quoted\"".to_string(),      // quotes
            "apostrophe's".to_string(),    // apostrophe
            "unicode: 日本語".to_string(), // unicode
            "newline\nchar".to_string(),   // newline
            "".to_string(),                // empty string
        ];
        let result = create_args_pseudoblock(&args);
        if let Expr::List(_, elements) = &*result.inner {
            assert_eq!(elements.len(), 10);
            // Verify strings are preserved
            for (i, elem) in elements.iter().enumerate() {
                if let Expr::Literal(_, Primitive::Str(s)) = &*elem.inner {
                    assert_eq!(s, &args[i], "Argument {} mismatch", i);
                } else {
                    panic!("Expected Literal Str at index {}, got {:?}", i, elem);
                }
            }
        } else {
            panic!("Expected List, got {:?}", result);
        }
    }
}
