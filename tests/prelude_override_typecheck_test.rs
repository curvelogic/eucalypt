//! Regression test for eu-oaz72.177: the evaluate path's type check must
//! honour a `prelude:` unit-metadata override.
//!
//! With the prelude blob active, the eval-time check seeds a loader from the
//! blob's baked prelude cores and so ignored the override: a type error
//! against a binding only the overriding prelude defines was silently missed
//! (`eu check` reported it, after eu-oaz72.173).
//!
//! Gated on `cfg(prelude_blob_ok)`: without a blob the eval path already
//! falls back to the source-prelude checker and there is nothing to assert.
#![cfg(prelude_blob_ok)]

use std::process::Command;

#[test]
fn eval_type_check_honours_prelude_override() {
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["--heap-limit-mib", "2048"])
        .arg("tests/fixtures/prelude_override_type_mismatch.eu")
        .output()
        .expect("run eu");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("type mismatch calling 'alt-show'"),
        "eval-time check ignored the prelude override; stderr:\n{stderr}"
    );
}
