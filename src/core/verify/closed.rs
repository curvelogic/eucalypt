//! Check that the prelude is closed: every name it refers to is one it
//! defines, or one of the side inputs loaded before it.
//!
//! The two ways of compiling the prelude must agree on what they accept.
//! `cargo xtask prelude-compile` compiles every prelude binding into the
//! blob, so an unresolved name anywhere in the prelude fails there. The
//! source path compiles only what survives pruning, so the same name in a
//! binding that is inlined or never used, or in its metadata, used to go
//! unnoticed. A doc string written with `\"` (which a plain string does
//! not process) is the usual cause: the string ends at the `\`, and the
//! rest of the doc becomes stray names (eu-oaz72.125). Both paths run this
//! check on the desugared prelude unit, so both reject it with the same
//! diagnostic.
use crate::common::sourcemap::Smid;
use crate::core::error::CoreError;
use crate::core::expr::{visit_free_var_occurrences, RcExpr};

/// The names the prelude may use without defining them: the `__build`,
/// `__io` and `__args` side inputs loaded before it.
pub const PRELUDE_SIDE_INPUTS: &[&str] = &["__build", "__io", "__args"];

/// Return an error for the first free variable in the desugared prelude
/// unit `expr` that is not one of [`PRELUDE_SIDE_INPUTS`].
pub fn verify_prelude_closed(expr: &RcExpr) -> Result<(), CoreError> {
    let mut first: Option<(Smid, String)> = None;
    visit_free_var_occurrences(expr, &mut |smid, name| {
        if first.is_none() && !PRELUDE_SIDE_INPUTS.contains(&name) {
            first = Some((smid, name.to_string()));
        }
    });
    match first {
        Some((smid, name)) => Err(CoreError::UnresolvedVariable(smid, name)),
        None => Ok(()),
    }
}
