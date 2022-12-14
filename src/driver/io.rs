//! Create a core syntax block with environment data
//!
//! This is a poor way of inserting environment data. Whilst it does
//! has the benefit of being pure immutable data once merged, the fact
//! it is different on every run yet inserted at core syntax stage
//! means we cannot cache core output. However it'll do for now.

use crate::core::expr::{acore, RcExpr};
use chrono::{offset::Local, TimeZone};
use std::{
    env,
    time::{SystemTime, UNIX_EPOCH},
};

/// A core block of environment variables
fn env_vars() -> RcExpr {
    acore::block(env::vars().map(|(k, v)| (k, acore::str(&v))))
}

/// Duration since unix epoch in seconds
fn epoch_time() -> RcExpr {
    if let Ok(secs) = SystemTime::now().duration_since(UNIX_EPOCH) {
        acore::num(secs.as_secs())
    } else {
        acore::null()
    }
}

/// Current timezone information
fn tz() -> RcExpr {
    let offset = format!("{}", Local.timestamp_opt(0, 0).unwrap().offset());
    acore::block(vec![("offset".to_string(), acore::str(&offset))])
}

/// Construct a core block containing environment data for merging
/// into core.
pub fn create_io_pseudoblock() -> RcExpr {
    acore::block(vec![
        ("ENV".to_string(), env_vars()),
        ("EPOCHTIME".to_string(), epoch_time()),
        ("TZ".to_string(), tz()),
    ])
}
