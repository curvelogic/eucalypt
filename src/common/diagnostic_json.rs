//! Structured JSON diagnostic schema (shared types).
//!
//! This is the serde-serialisable contract for a structured JSON diagnostic:
//! level, code, message, primary position, labels, notes, and curated trace.
//! Task 3's `--error-format json` emitter fills this schema; Task 5's
//! invariant gate reads it back.

use serde::{Deserialize, Serialize};

/// A source position resolved for user-facing output. `in_user_file` is the
/// single most load-bearing field for the invariant gate: it distinguishes a
/// location in code the user wrote from one in the prelude/synthetic machinery.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonPos {
    /// Display path, e.g. "config.eu" or "[prelude]". `None` for a locationless (synthetic) Smid.
    pub file: Option<String>,
    pub line: Option<u32>,
    pub column: Option<u32>,
    /// True iff `file` resolves to a user (non-resource) source. False for prelude/synthetic.
    pub in_user_file: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum FrameKind {
    User, // a frame in user source
    /// A named library combinator the user invoked (nth, head, lookup, +),
    /// declared `blame: :boundary` in the prelude and classified by
    /// `SourceMap::classify_frame` (eu-1tkk.7.12).
    Boundary,
    Transparent, // library plumbing (map/fold internals); normally curated out
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonFrame {
    pub name: Option<String>, // user-facing frame name (e.g. "nth", "map", or a binding name)
    pub pos: JsonPos,
    pub kind: FrameKind,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonLabel {
    pub pos: JsonPos,
    pub message: String,
    pub primary: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum JsonLevel {
    Error,
    Warning,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct JsonDiagnostic {
    pub level: JsonLevel,
    /// Stable error code, e.g. "EU-EVAL-TYPE" (Task 4). `None` until a code is assigned.
    pub code: Option<String>,
    pub message: String,
    /// The primary label's position (duplicated out for the gate's convenience).
    pub primary: JsonPos,
    pub labels: Vec<JsonLabel>,
    pub notes: Vec<String>,
    /// Curated, outermost-first. Empty is legal (some errors have no trace).
    pub trace: Vec<JsonFrame>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrips_through_json() {
        let d = JsonDiagnostic {
            level: JsonLevel::Error,
            code: Some("EU-EVAL-TYPE".into()),
            message: "type mismatch: expected number, found string".into(),
            primary: JsonPos {
                file: Some("x.eu".into()),
                line: Some(3),
                column: Some(9),
                in_user_file: true,
            },
            labels: vec![JsonLabel {
                pos: JsonPos {
                    file: Some("x.eu".into()),
                    line: Some(3),
                    column: Some(9),
                    in_user_file: true,
                },
                message: "here".into(),
                primary: true,
            }],
            notes: vec!["use 'num' to convert".into()],
            trace: vec![JsonFrame {
                name: Some("nth".into()),
                pos: JsonPos {
                    file: Some("[prelude]".into()),
                    line: Some(1380),
                    column: Some(22),
                    in_user_file: false,
                },
                kind: FrameKind::Boundary,
            }],
        };
        let s = serde_json::to_string(&d).unwrap();
        let back: JsonDiagnostic = serde_json::from_str(&s).unwrap();
        assert_eq!(d, back);
        assert!(s.contains("\"in_user_file\""));
    }
}
