//! Pre-compiled prelude blob: `PreludeBlob` definition and helpers.
//!
//! A `PreludeBlob` encodes everything the runtime needs to use the prelude
//! without re-compiling its source.  It is generated once at development time
//! by `cargo xtask prelude-compile` and stored as `lib/prelude.blob`.
//!
//! ## Naming convention (ratified 2026-07-14, eu-rb5n Z review)
//!
//! The word **"core(s)"** is reserved for fields that are faithful snapshots
//! of a pipeline stage — every such name states both the **phase** and the
//! **granularity**:
//!
//! - **Phase** names come from the `eu dump` vocabulary (`desugared`,
//!   `cooked`, `inlined`, `pruned`, …). `SourceLoader::translate` produces
//!   the **desugared** stage — the same representation `eu dump desugared`
//!   shows.
//! - **Granularity** is one of: **unit** (per translation unit — only
//!   possible at `desugared` or earlier, since `merge_units`/`cook`/
//!   `eliminate` are cross-unit passes), **merged** (a merged unit set), or
//!   **binding** (per individual binding).
//!
//! A field that is a *derived* compiler artefact — pre-expanded, re-tagged,
//! summarised, or otherwise transformed beyond a faithful stage snapshot —
//! is **not** a "core" and must be named as the artefact it actually is
//! (e.g. `inlinable_bindings`, not `inline_cores`).
//!
//! This convention doesn't (yet) have compiler-enforced typed-stage
//! wrappers (a `CoreStage` enum / `StagedCore<Phase>` wrapper) — that's
//! deliberately deferred to eu-2sa6.18 (unit-blob generalisation), so
//! naming discipline is the only thing keeping this honest today.
//!
//! ## Contents
//!
//! | Field | Purpose | Granularity | Pipeline stage | Consumer | Snapshot / derived |
//! |---|---|---|---|---|---|
//! | `source_hash` | SHA-256 of `lib/prelude.eu`; checked by `build.rs` | — | — | `build.rs` staleness check | — |
//! | `nodes` / `forms_pool` / `binding_entries` | Shared STG arena: compiled lambda forms for every prelude global | binding (compiled) | `stg` (post `eu dump stg`) | HeapSyn engine loader | derived (compiled, not a source-structure snapshot) |
//! | `name_to_slot` | Binding name → global slot index | — | — | STG compiler (`Ref::G` resolution), loader | — |
//! | `operators` | Operator fixity/precedence, extracted pre-`cook` | merged | pre-`cook` (desugared-adjacent) | `cook`'s `Distributor` seeding | derived (extracted subset, not a full snapshot) |
//! | `type_summary` | Type schemes/aliases/branch shapes (`PreludeSummary`) | merged | post type-check | *(currently unread by the runtime; `run_type_checker_from_blob_core` uses `desugared_unit_cores` instead — retained for blob-format stability, not removed as out of scope for eu-rb5n)* | derived summary |
//! | `monad_specs` / `monad_type_hints` | Monad namespace specs (e.g. `:for`) and LSP type hints | merged | desugared | Desugarer seeding, LSP | derived |
//! | `inlinable_bindings` | Pre-expanded, `Ref::G`-linked, `Lam(_, true, _)`-tagged combinator bodies | binding | derived from desugared | `inject_prelude_inlinable_bindings` (pre-inline injection into user code) | **derived** (pre-expanded/re-tagged — not a stage snapshot, hence no "core" in the name) |
//! | `desugared_unit_cores` | Each of the 4 prelude-side units' (`__build`/`__io`/`__args`/prelude) `expr`, exactly as `SourceLoader::translate` produced it | **unit** | **desugared**, pre-merge | `run_type_checker_from_blob_core` (eval-path merged type check, eu-rb5n) | **canonical snapshot** |
//! | `bytecode` | Pre-encoded `BytecodeProgram` + global forms (BV5) | binding (compiled) | bytecode (post-`stg`) | Bytecode engine loader | derived (compiled, further downstream than `nodes`/`forms_pool`) |
//!
//! ## Global slot numbering
//!
//! Intrinsic wrappers occupy global slots `0 .. INTRINSIC_COUNT`.  Prelude
//! bindings start immediately after: binding `i` in `bindings` occupies global
//! slot `INTRINSIC_COUNT + i`.  `name_to_slot["map"]` gives the `i` (not the
//! full slot number); add `INTRINSIC_COUNT` to get the `Ref::G(slot)` value.
//!
//! ## Serialisation
//!
//! Serialised with **postcard** (compact binary format).  Numbers in `Native`
//! are stored as their decimal string representation to avoid the
//! `serde_json::Number` JSON-specific serialiser.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    core::{expr::RcExpr, typecheck::check::PreludeSummary},
    driver::unit_interface::OperatorInfo,
    eval::{
        bytecode::{BytecodeProgram, GlobalForm},
        stg::arena::{ArenaLambdaForm, ArenaStgSyn, FormIdx},
    },
};

// ── PreludeBytecodeImage ──────────────────────────────────────────────────────

/// The pre-encoded prelude bytecode embedded in the blob (BV5, eu-amp9).
///
/// Holds the flat `BytecodeProgram` for the prelude (fixed templates plus
/// every intrinsic-wrapper and prelude-binding global body — no program
/// root) together with the parallel `global_forms` the machine needs to
/// build the globals frame. The loader appends the user program root (and
/// re-encodes the per-invocation `__args` / `__io` overrides) onto this
/// image rather than re-encoding the whole prelude on every run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreludeBytecodeImage {
    /// Pre-encoded prelude program: templates + all global bodies, no root.
    pub program: BytecodeProgram,
    /// Global forms (kind/arity/smid/entry) for every global slot, in slot
    /// order (intrinsic wrappers `0..INTRINSIC_COUNT`, then prelude bindings).
    pub global_forms: Vec<GlobalForm>,
}

// ── PreludeBlob ───────────────────────────────────────────────────────────────

/// The complete pre-compiled prelude, serialised with postcard.
///
/// Generated by `cargo xtask prelude-compile`; embedded at compile time
/// via `include_bytes!("../../../lib/prelude.blob")` (see `resources.rs`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreludeBlob {
    /// SHA-256 of `lib/prelude.eu` at generation time.
    ///
    /// Compared against the hash computed by `build.rs`; a mismatch causes
    /// a build warning and triggers the source-prelude fallback path.
    pub source_hash: [u8; 32],

    /// Shared arena node pool.
    ///
    /// Every `ArenaLambdaForm` in `forms_pool` has a `body: NodeIdx` that
    /// indexes into this `Vec`.
    pub nodes: Vec<ArenaStgSyn>,

    /// Complete pool of ALL lambda forms: top-level entry thunks AND any
    /// inner lambda forms from `Let`/`LetRec` bindings within each thunk body.
    ///
    /// Nodes in `nodes` may contain `FormIdx` references into this pool.
    /// Stored in full so that `StgArena { nodes, forms: forms_pool }` can
    /// reconstruct any form without missing inner references.
    pub forms_pool: Vec<ArenaLambdaForm>,

    /// Index into `forms_pool` for the entry-level `LambdaForm` of each
    /// prelude binding, in slot order.
    ///
    /// `binding_entries[i]` is the `FormIdx` in `forms_pool` for the `i`-th
    /// prelude global (at VM global slot `INTRINSIC_COUNT + i`).
    pub binding_entries: Vec<FormIdx>,

    /// Binding name → index into `binding_entries` (= global slot − `INTRINSIC_COUNT`).
    #[serde(with = "crate::common::serde_sorted")]
    pub name_to_slot: HashMap<String, usize>,

    /// Operator metadata for seeding cook's `Distributor`.
    #[serde(with = "crate::common::serde_sorted")]
    pub operators: HashMap<String, OperatorInfo>,

    /// Type schemes, aliases, branch shapes for seeding the type checker.
    pub type_summary: PreludeSummary,

    /// Monad namespace specifications (e.g. `:for` → list monad, `:random`).
    ///
    /// Extracted from prelude source during translate/desugar.  Seeded into
    /// the desugarer so that `{ :for ... }` blocks are recognised as monadic.
    #[serde(default, with = "crate::common::serde_sorted")]
    pub monad_specs: HashMap<String, crate::core::desugar::desugarer::MonadSpec>,

    /// Monad wrapper type hints for LSP display (e.g. `"io"` → `"IO(a)"`).
    #[serde(default, with = "crate::common::serde_sorted")]
    pub monad_type_hints: HashMap<String, String>,

    /// Pre-expanded, self-contained bodies for inlinable prelude bindings.
    ///
    /// **Not a "core"** (see the module-level naming convention above): each
    /// entry is `(name, tagged_lambda)` where the lambda is tagged
    /// `Lam(_, true, _)` (inlinable combinator) and every inter-prelude
    /// reference has already been substituted in at blob-generation time —
    /// a derived, pre-expanded artefact, not a faithful stage snapshot.
    /// Injected as Let bindings before the inline pass so the existing
    /// inliner can distribute and beta-reduce prelude calls even when the
    /// full prelude source is not loaded (blob path).
    ///
    /// Bindings are self-contained: their bodies only reference intrinsics
    /// and the lambda's own parameters.  Any remaining inter-prelude
    /// `Var::Free` references resolve at compile time via `Ref::G`.
    #[serde(default)]
    pub inlinable_bindings: Vec<(String, RcExpr)>,

    /// Postcard-encoded, tagged **desugared**, per-**unit** cores of the four
    /// prelude-side units — `"build"`, `"io"`, `"args"`, `"prelude"`
    /// (matching the `__build`/`__io`/`__args` pseudoblocks and the prelude
    /// itself) — for the eval-path MERGED type check without parsing
    /// prelude source (eu-rb5n). Each entry is the unit's `expr` exactly as
    /// `SourceLoader::translate` produced it (the `desugared` stage, per
    /// `eu dump desugared`), before `merge_units` — a genuine stage
    /// snapshot, unlike `inlinable_bindings` above. Stored as raw bytes so
    /// blob load stays cheap — decoded lazily via
    /// [`PreludeBlob::decode_desugared_unit_cores`] only when the type check
    /// actually runs (never under `--suppress-type-warnings`). Empty on
    /// blobs generated before this field, which fall back to source compile.
    #[serde(default)]
    pub desugared_unit_cores: Vec<u8>,

    /// Pre-encoded prelude BytecodeProgram + global forms (BV5, eu-amp9).
    ///
    /// Present alongside the STG arena form (`nodes` / `forms_pool`): the
    /// HeapSyn engine loads the STG form, while the bytecode engine loads
    /// this pre-encoded program directly instead of re-encoding the prelude
    /// on every run. `Option` + `#[serde(default)]` so a blob generated
    /// before BV5 (or one deliberately omitting it) deserialises to `None`
    /// and the loader degrades gracefully to encode-from-STG.
    #[serde(default)]
    pub bytecode: Option<PreludeBytecodeImage>,
}

impl PreludeBlob {
    /// Serialise to postcard bytes.
    pub fn to_bytes(&self) -> Result<Vec<u8>, postcard::Error> {
        postcard::to_allocvec(self)
    }

    /// Deserialise from postcard bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, postcard::Error> {
        postcard::from_bytes(bytes)
    }

    /// Lazily decode the baked desugared, per-unit prelude-side cores, if
    /// present. Called only when the eval-path type check actually runs, so
    /// the decode cost is not paid at blob load or under
    /// `--suppress-type-warnings`.
    ///
    /// Returns `(tag, expr)` pairs — tags are `"build"`, `"io"`, `"args"`,
    /// `"prelude"` — for [`crate::driver::check::run_type_checker_from_blob_core`]
    /// to re-key against the matching `Input`s.
    pub fn decode_desugared_unit_cores(&self) -> Option<Vec<(String, RcExpr)>> {
        if self.desugared_unit_cores.is_empty() {
            None
        } else {
            postcard::from_bytes(&self.desugared_unit_cores).ok()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::desugar::desugarer::MonadSpec;

    /// Build a minimal blob for testing (empty nodes/forms/bindings).
    fn minimal_blob() -> PreludeBlob {
        PreludeBlob {
            source_hash: [0u8; 32],
            nodes: vec![],
            forms_pool: vec![],
            binding_entries: vec![],
            name_to_slot: HashMap::new(),
            operators: HashMap::new(),
            type_summary: PreludeSummary::default(),
            monad_specs: HashMap::new(),
            monad_type_hints: HashMap::new(),
            inlinable_bindings: vec![],
            desugared_unit_cores: vec![],
            bytecode: None,
        }
    }

    #[test]
    fn monad_specs_round_trip_namespace() {
        let mut blob = minimal_blob();
        blob.monad_specs
            .insert("for".to_string(), MonadSpec::Namespace("for".to_string()));

        let bytes = blob.to_bytes().unwrap();
        let restored = PreludeBlob::from_bytes(&bytes).unwrap();

        assert_eq!(restored.monad_specs.len(), 1);
        assert!(matches!(
            restored.monad_specs.get("for"),
            Some(MonadSpec::Namespace(n)) if n == "for"
        ));
    }

    #[test]
    fn monad_specs_round_trip_explicit() {
        let mut blob = minimal_blob();
        blob.monad_specs.insert(
            "custom".to_string(),
            MonadSpec::Explicit {
                bind_name: "my-bind".to_string(),
                return_name: "my-return".to_string(),
            },
        );

        let bytes = blob.to_bytes().unwrap();
        let restored = PreludeBlob::from_bytes(&bytes).unwrap();

        assert!(matches!(
            restored.monad_specs.get("custom"),
            Some(MonadSpec::Explicit { bind_name, return_name })
                if bind_name == "my-bind" && return_name == "my-return"
        ));
    }

    #[test]
    #[cfg(prelude_blob_ok)]
    fn embedded_blob_contains_monad_specs() {
        let bytes = crate::driver::resources::PRELUDE_BLOB_BYTES;
        let blob = PreludeBlob::from_bytes(bytes).expect("embedded blob should deserialise");
        assert!(
            !blob.monad_specs.is_empty(),
            "prelude blob must contain monad specs (e.g. for, random)"
        );
        assert!(
            blob.monad_specs.contains_key("for"),
            "prelude blob must contain the :for list monad spec"
        );
    }

    #[test]
    #[cfg(prelude_blob_ok)]
    fn embedded_blob_contains_expect_operators() {
        let bytes = crate::driver::resources::PRELUDE_BLOB_BYTES;
        let blob = PreludeBlob::from_bytes(bytes).expect("embedded blob should deserialise");
        let expect_ops: Vec<&String> = blob.operators.keys().filter(|k| k.contains("//")).collect();
        eprintln!("Expect operators: {:?}", expect_ops);
        eprintln!("Total operators: {}", blob.operators.len());
        assert!(
            blob.operators.contains_key("//="),
            "prelude blob must contain //= operator; found: {:?}",
            expect_ops
        );
    }

    #[test]
    fn monad_type_hints_round_trip() {
        let mut blob = minimal_blob();
        blob.monad_type_hints
            .insert("io".to_string(), "IO(a)".to_string());

        let bytes = blob.to_bytes().unwrap();
        let restored = PreludeBlob::from_bytes(&bytes).unwrap();

        assert_eq!(
            restored.monad_type_hints.get("io"),
            Some(&"IO(a)".to_string())
        );
    }

    #[test]
    fn desugared_unit_cores_empty_decodes_to_none() {
        // A blob with no baked desugared_unit_cores (stale format, or
        // generated before this field existed) must decode to `None` so the
        // caller (`bin/eu.rs`) falls back explicitly to `run_type_checker`
        // (eu-rb5n condition 3) rather than panicking or silently checking
        // nothing.
        let blob = minimal_blob();
        assert!(blob.desugared_unit_cores.is_empty());
        assert!(blob.decode_desugared_unit_cores().is_none());
    }

    #[test]
    fn desugared_unit_cores_round_trips_tagged_units() {
        use crate::common::sourcemap::Smid;
        use crate::core::expr::{Expr, Primitive, RcExpr};

        let sym = |s: &str| {
            RcExpr::from(Expr::Literal(
                Smid::default(),
                Primitive::Sym(s.to_string()),
            ))
        };
        let units: Vec<(String, RcExpr)> = vec![
            ("build".to_string(), sym("b")),
            ("io".to_string(), sym("i")),
            ("args".to_string(), sym("a")),
            ("prelude".to_string(), sym("p")),
        ];

        let mut blob = minimal_blob();
        blob.desugared_unit_cores = postcard::to_allocvec(&units).unwrap();

        let bytes = blob.to_bytes().unwrap();
        let restored = PreludeBlob::from_bytes(&bytes).unwrap();

        let decoded = restored
            .decode_desugared_unit_cores()
            .expect("non-empty desugared_unit_cores must decode");
        assert_eq!(decoded.len(), 4);
        let tags: Vec<&str> = decoded.iter().map(|(t, _)| t.as_str()).collect();
        assert_eq!(tags, vec!["build", "io", "args", "prelude"]);
    }

    #[test]
    fn empty_blob_round_trip() {
        let blob = minimal_blob();
        let bytes = blob.to_bytes().unwrap();
        let restored = PreludeBlob::from_bytes(&bytes).unwrap();

        assert_eq!(restored.source_hash, [0u8; 32]);
        assert!(restored.nodes.is_empty());
        assert!(restored.forms_pool.is_empty());
        assert!(restored.binding_entries.is_empty());
        assert!(restored.name_to_slot.is_empty());
        assert!(restored.operators.is_empty());
        assert!(restored.monad_specs.is_empty());
        assert!(restored.monad_type_hints.is_empty());
        assert!(restored.inlinable_bindings.is_empty());
        // A blob generated before BV5 has no bytecode image; the loader must
        // see `None` and degrade to encode-from-STG.
        assert!(restored.bytecode.is_none());
    }

    #[test]
    fn bytecode_image_round_trip() {
        use crate::common::sourcemap::Smid;
        use crate::eval::bytecode::GlobalForm;
        use crate::eval::stg::syntax::Native;

        let mut blob = minimal_blob();
        // A small hand-built program with one constant and one global form.
        let mut program = BytecodeProgram {
            code: vec![1, 2, 3, 4, 5],
            constants: vec![Native::Num(7.into()), Native::Sym("k".to_string())],
            global_entries: vec![0, 3],
            templates: vec![10, 11, 12],
            blackhole: 4,
            meta_template: 2,
            ..Default::default()
        };
        program.pap = vec![13, 14];
        program.apply1_template = 1;
        program.apply2_template = 2;
        program.producer_tail_template = 3;

        blob.bytecode = Some(PreludeBytecodeImage {
            program,
            global_forms: vec![
                GlobalForm {
                    kind: 1,
                    arity: 0,
                    smid: Smid::default(),
                    entry: 0,
                },
                GlobalForm {
                    kind: 3,
                    arity: 2,
                    smid: Smid::default(),
                    entry: 3,
                },
            ],
        });

        let bytes = blob.to_bytes().unwrap();
        let restored = PreludeBlob::from_bytes(&bytes).unwrap();
        let img = restored.bytecode.expect("bytecode image must round-trip");

        assert_eq!(img.program.code, vec![1, 2, 3, 4, 5]);
        assert_eq!(img.program.constants.len(), 2);
        assert_eq!(img.program.constants[0], Native::Num(7.into()));
        assert_eq!(img.program.global_entries, vec![0, 3]);
        assert_eq!(img.program.templates, vec![10, 11, 12]);
        assert_eq!(img.program.blackhole, 4);
        assert_eq!(img.program.pap, vec![13, 14]);
        assert_eq!(img.program.producer_tail_template, 3);
        assert_eq!(img.global_forms.len(), 2);
        assert_eq!(img.global_forms[1].arity, 2);
        assert_eq!(img.global_forms[1].entry, 3);
    }

    #[test]
    fn blob_with_nodes_and_bindings_round_trip() {
        use crate::common::sourcemap::Smid;
        use crate::eval::stg::arena::{ArenaLambdaForm, ArenaStgSyn};
        use crate::eval::stg::syntax::Native;

        let mut blob = minimal_blob();
        blob.source_hash = [42u8; 32];

        // Add some arena nodes: an Atom containing a symbol
        blob.nodes.push(ArenaStgSyn::Atom {
            evaluand: super::super::syntax::Ref::V(Native::Sym("hello".to_string())),
        });

        // Add a Value lambda form referencing node 0
        blob.forms_pool.push(ArenaLambdaForm::Value { body: 0 });

        // Add a Thunk lambda form referencing node 0
        blob.forms_pool.push(ArenaLambdaForm::Thunk { body: 0 });

        // Add a Lambda form
        blob.forms_pool.push(ArenaLambdaForm::Lambda {
            bound: 2,
            body: 0,
            annotation: Smid::default(),
        });

        // Binding entries point to forms_pool indices
        blob.binding_entries.push(0);
        blob.binding_entries.push(1);

        // Name-to-slot mapping
        blob.name_to_slot.insert("my-value".to_string(), 0);
        blob.name_to_slot.insert("my-thunk".to_string(), 1);

        let bytes = blob.to_bytes().unwrap();
        let restored = PreludeBlob::from_bytes(&bytes).unwrap();

        assert_eq!(restored.source_hash, [42u8; 32]);
        assert_eq!(restored.nodes.len(), 1);
        assert_eq!(restored.forms_pool.len(), 3);
        assert_eq!(restored.binding_entries.len(), 2);
        assert_eq!(restored.name_to_slot.get("my-value"), Some(&0));
        assert_eq!(restored.name_to_slot.get("my-thunk"), Some(&1));

        // Verify the lambda form variants survived
        assert!(matches!(
            &restored.forms_pool[0],
            ArenaLambdaForm::Value { body: 0 }
        ));
        assert!(matches!(
            &restored.forms_pool[1],
            ArenaLambdaForm::Thunk { body: 0 }
        ));
        assert!(matches!(
            &restored.forms_pool[2],
            ArenaLambdaForm::Lambda {
                bound: 2,
                body: 0,
                ..
            }
        ));
    }

    #[test]
    fn blob_with_operators_round_trip() {
        use crate::common::sourcemap::Smid;
        use crate::core::expr::Fixity;
        use crate::driver::unit_interface::OperatorInfo;

        let mut blob = minimal_blob();
        blob.operators.insert(
            "+".to_string(),
            OperatorInfo {
                smid: Smid::default(),
                fixity: Fixity::InfixLeft,
                precedence: 60,
                type_annotation: None,
            },
        );
        blob.operators.insert(
            "*".to_string(),
            OperatorInfo {
                smid: Smid::default(),
                fixity: Fixity::InfixLeft,
                precedence: 70,
                type_annotation: None,
            },
        );

        let bytes = blob.to_bytes().unwrap();
        let restored = PreludeBlob::from_bytes(&bytes).unwrap();

        assert_eq!(restored.operators.len(), 2);
        let plus = restored.operators.get("+").unwrap();
        assert_eq!(plus.fixity, Fixity::InfixLeft);
        assert_eq!(plus.precedence, 60);
        let times = restored.operators.get("*").unwrap();
        assert_eq!(times.precedence, 70);
    }

    #[test]
    fn blob_corrupted_bytes_fail_gracefully() {
        let result = PreludeBlob::from_bytes(&[0xFF, 0xFE, 0xFD, 0x00]);
        assert!(
            result.is_err(),
            "corrupted bytes should produce a deserialisation error"
        );
    }

    #[test]
    #[cfg(prelude_blob_ok)]
    fn embedded_bytecode_matches_fresh_encode() {
        use crate::common::sourcemap::SourceMap;
        use crate::eval::bytecode::encode;
        use crate::eval::stg::runtime::Runtime;
        use crate::eval::stg::{make_standard_runtime, syntax::dsl};

        let bytes = crate::driver::resources::PRELUDE_BLOB_BYTES;
        let blob = PreludeBlob::from_bytes(bytes).expect("embedded blob should deserialise");
        let image = blob
            .bytecode
            .as_ref()
            .expect("embedded blob must carry pre-encoded bytecode (BV5)");

        // Rebuild the runtime globals exactly as the loader does, then encode
        // a trivial root. The prelude image must be a byte-identical prefix.
        let mut sm = SourceMap::new();
        let mut rt = make_standard_runtime(&mut sm);
        let mut names = vec![String::new(); blob.binding_entries.len()];
        for (name, &slot) in &blob.name_to_slot {
            if slot < names.len() {
                names[slot] = name.clone();
            }
        }
        rt.set_prelude_bindings(
            blob.nodes.clone(),
            blob.forms_pool.clone(),
            blob.binding_entries.clone(),
            names,
        );
        let globals = rt.globals();
        let (fresh, _root, fresh_forms) = encode(&dsl::atom(dsl::num(0)), &globals);

        assert_eq!(
            image.global_forms, fresh_forms,
            "embedded global_forms must match a fresh encode"
        );
        assert_eq!(
            image.program.templates, fresh.templates,
            "templates must survive the postcard round-trip"
        );
        assert_eq!(image.program.blackhole, fresh.blackhole, "blackhole");
        assert_eq!(image.program.meta_template, fresh.meta_template, "meta");
        assert_eq!(image.program.pap, fresh.pap, "pap templates");
        assert_eq!(
            image.program.apply1_template, fresh.apply1_template,
            "apply1"
        );
        assert_eq!(
            image.program.apply2_template, fresh.apply2_template,
            "apply2"
        );
        assert_eq!(
            image.program.producer_tail_template, fresh.producer_tail_template,
            "producer tail"
        );
        assert_eq!(
            image.program.global_entries,
            fresh.global_entries[..image.program.global_entries.len()],
            "global entry table prefix"
        );
        assert!(
            image.program.constants.len() <= fresh.constants.len(),
            "prelude image constants must be a prefix (no root)"
        );
        assert_eq!(
            image.program.constants,
            fresh.constants[..image.program.constants.len()],
            "embedded prelude constants must be a byte-identical prefix"
        );
        assert!(
            image.program.code.len() <= fresh.code.len(),
            "prelude image code must be a prefix (no root)"
        );
        assert_eq!(
            image.program.code,
            fresh.code[..image.program.code.len()],
            "embedded prelude code must be a byte-identical prefix of a fresh encode"
        );
    }

    #[test]
    #[cfg(prelude_blob_ok)]
    fn embedded_blob_has_bindings_and_slots() {
        let bytes = crate::driver::resources::PRELUDE_BLOB_BYTES;
        let blob = PreludeBlob::from_bytes(bytes).expect("embedded blob should deserialise");

        assert!(!blob.nodes.is_empty(), "prelude blob must have arena nodes");
        assert!(
            !blob.forms_pool.is_empty(),
            "prelude blob must have lambda forms"
        );
        assert!(
            !blob.binding_entries.is_empty(),
            "prelude blob must have binding entries"
        );
        assert!(
            !blob.name_to_slot.is_empty(),
            "prelude blob must have name-to-slot mappings"
        );

        // Spot-check a few well-known prelude names
        assert!(
            blob.name_to_slot.contains_key("map"),
            "prelude blob must contain 'map'"
        );
        assert!(
            blob.name_to_slot.contains_key("filter"),
            "prelude blob must contain 'filter'"
        );
        assert!(
            blob.name_to_slot.contains_key("head"),
            "prelude blob must contain 'head'"
        );

        // Every binding entry must be a valid index into forms_pool
        for (i, &entry) in blob.binding_entries.iter().enumerate() {
            assert!(
                (entry as usize) < blob.forms_pool.len(),
                "binding_entries[{i}] = {entry} is out of bounds (forms_pool len = {})",
                blob.forms_pool.len()
            );
        }
    }
}
