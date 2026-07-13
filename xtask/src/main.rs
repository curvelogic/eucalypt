//! `cargo xtask` — development automation for eucalypt.
//!
//! ## Commands
//!
//! - `cargo xtask prelude-compile` — compile `lib/prelude.eu` and write
//!   the pre-compiled prelude blob to `lib/prelude.blob`.
//!
//! ## How the blob is generated
//!
//! 1. Load `lib/prelude.eu` through the full front-end pipeline
//!    (parse → desugar → cook → fuse → type-check).
//! 2. Extract `OperatorInfo` (before cook strips `Meta` wrappers) and the
//!    `PreludeSummary` from the type checker.
//! 3. Peel the merged `Let` expression back into individual `(name, body)` pairs
//!    using `open_let_scope_full`, converting `Var::Bound` intra-prelude
//!    references back to `Var::Free(name)`.
//! 4. Build `name_to_slot` from the peeled binding order.
//! 5. Compile each binding body independently with `prelude_globals` set so that
//!    `Var::Free("name")` → `Ref::G(INTRINSIC_COUNT + slot)`.
//! 6. Wrap each compiled `Rc<StgSyn>` as a `LambdaForm::Thunk`.
//! 7. Flatten all lambda form bodies into a shared `ArenaStgSyn` node pool.
//! 8. Serialise the `PreludeBlob` with postcard and write `lib/prelude.blob`.

use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    time::Instant,
};

use anyhow::{bail, Context, Result};
use eucalypt::{
    core::{
        expr::{open_let_scope_full, Expr, RcExpr},
        inline::tag::{all_free_vars_in_set, tag_combinators},
        typecheck::check::{parse_operator_overloads, type_check_for_prelude},
    },
    driver::source::SourceLoader,
    eval::stg::{
        arena::{ArenaLambdaForm, ArenaStgSyn, StgArena},
        blob::{PreludeBlob, PreludeBytecodeImage},
        make_standard_runtime,
        syntax::LambdaForm,
    },
    syntax::input::{Input, Locator},
};
use sha2::{Digest, Sha256};

mod engine_ab;

/// BV1 bytecode wire-format version, folded into the prelude-blob source hash.
/// MUST match `BYTECODE_WIRE_FORMAT_VERSION` in the crate root `build.rs`.
/// See that constant's doc comment for the version history (v2: eu-2sa6.11
/// Let/LetRec binding count widened `u16` → `u32`).
const BYTECODE_WIRE_FORMAT_VERSION: u32 = 2;

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    match args.next().as_deref() {
        Some("prelude-compile") => cmd_prelude_compile(),
        Some("engine-ab") => engine_ab::run(&mut args),
        Some(cmd) => bail!("unknown xtask command: {cmd}"),
        None => {
            eprintln!("Usage: cargo xtask <command>");
            eprintln!("Commands:");
            eprintln!("  prelude-compile   Compile lib/prelude.eu → lib/prelude.blob");
            eprintln!("  engine-ab         Run the engine A/B suite; append results.jsonl");
            eprintln!("  engine-ab --check Flag regressions vs the previous ledger run");
            std::process::exit(1);
        }
    }
}

// ── prelude-compile ───────────────────────────────────────────────────────────

fn cmd_prelude_compile() -> Result<()> {
    let workspace_root = workspace_root()?;
    let prelude_src = workspace_root.join("lib/prelude.eu");
    let blob_out = workspace_root.join("lib/prelude.blob");

    println!("Compiling prelude: {}", prelude_src.display());
    let t_start = Instant::now();

    // ── 1. Hash the prelude source ────────────────────────────────────────────
    let source_bytes = std::fs::read(&prelude_src)
        .with_context(|| format!("reading {}", prelude_src.display()))?;
    // Fold the bytecode wire-format version into the hash so a format change
    // (not just a source change) invalidates a stale blob at build time
    // (must match `build.rs`).
    let mut hasher = Sha256::new();
    hasher.update(&source_bytes);
    hasher.update(BYTECODE_WIRE_FORMAT_VERSION.to_le_bytes());
    let source_hash: [u8; 32] = hasher.finalize().into();

    // ── 2. Run the front-end pipeline ────────────────────────────────────────
    // The prelude references `__build` (from build-meta.yaml) and intrinsics
    // such as `__IO_BIND` (from the pseudo io input). These must be loaded
    // before the prelude itself so that the desugarer can resolve them.
    // Load build-meta.yaml from disk (not the compiled-in resource) so that
    // the blob picks up the freshly regenerated version during CI release
    // builds, rather than the stale committed copy.
    let build_meta_path = workspace_root.join("build-meta.yaml");
    let build_meta = Input::new(
        Locator::Fs(build_meta_path),
        Some("__build".to_string()),
        "yaml",
    );
    let io_input = Input::new(
        Locator::Pseudo("io".to_string()),
        Some("__io".to_string()),
        "core",
    );
    let args_input = Input::new(
        Locator::Pseudo("args".to_string()),
        Some("__args".to_string()),
        "core",
    );
    let locator = Locator::Fs(prelude_src.clone());
    let prelude_input = Input::new(locator, None, "eu");

    // Load order: __build, __io, __args, prelude.
    let all_inputs = vec![
        build_meta.clone(),
        io_input.clone(),
        args_input.clone(),
        prelude_input.clone(),
    ];

    // Build the `__io` pseudoblock deterministically: the blob must not embed
    // the current epoch time / random seed / environment (all overridden at
    // runtime anyway), or two prelude-compile runs would produce differing
    // blobs (see eu-c2ue).
    let mut loader = SourceLoader::new(vec![]).with_deterministic_io(true);
    for inp in &all_inputs {
        loader.load(inp).with_context(|| format!("load {inp}"))?;
    }
    for inp in &all_inputs {
        loader
            .translate(inp)
            .with_context(|| format!("translate {inp}"))?;
    }
    loader.merge_units(&all_inputs).context("merge_units")?;

    // Extract operators and visibility BEFORE cook strips Meta wrappers.
    loader.extract_operators();
    loader.extract_visibility();

    let operator_type_strings = loader.unit_interface().operator_type_strings();
    let operators = loader.unit_interface().operators.clone();

    // Extract monad specs (e.g. :for, :random) populated during translate.
    let monad_specs = loader.unit_interface().monad_specs.clone();
    let monad_type_hints = loader.unit_interface().monad_type_hints.clone();
    println!("  monad specs: {}", monad_specs.len());

    // Cook (resolve operator precedence).
    loader.cook().context("cook")?;

    // Hoist namespace members to top-level bindings.
    //
    // This lifts inlinable members of namespace blocks (e.g. `str`, `cal`,
    // `vec`) to individual `OtherLet` bindings named `__<ns>_<member>`.
    // After hoisting, the peel step produces one `binding_body` entry per
    // hoisted member, giving each its own global slot in the blob.  The
    // `inline_cores` fixed-point then picks up hoisted intrinsics (e.g.
    // `__str_to-upper`) exactly like other prelude combinators.
    loader.hoist_namespaces().context("hoist_namespaces")?;

    // Run destructure fusion only.
    // NOTE: We skip `inline()` deliberately: the inline pass aggressively
    // folds internal Let bindings into their single call site, reducing the
    // ~295 prelude top-level bindings to only a handful in the compiled STG.
    // Preserving all Let bindings here ensures the blob has one LambdaForm
    // per prelude name, which Phase 5 loads into global slots INTRINSIC_COUNT+i.
    loader.fuse_destructure().context("fuse_destructure")?;
    // No eliminate — every prelude binding must be retained in the blob.
    // The type checker needs to see all bindings for their type schemes.

    // ── 3. Type-check to extract PreludeSummary ───────────────────────────────
    let core_expr = loader.core().expr.clone();
    let (tc_warnings, mut summary) = type_check_for_prelude(&core_expr);
    if !tc_warnings.is_empty() {
        eprintln!("  {} type-check warning(s) (non-fatal)", tc_warnings.len());
    }

    // Populate operator_overloads from the pre-cook operator info.
    let operator_overloads = parse_operator_overloads(&operator_type_strings);
    summary.operator_overloads = operator_overloads;

    // ── 4. Peel the merged Let expression into individual binding bodies ──────
    // After merge_units + cook, the prelude is a nested Let where all
    // intra-prelude references are Var::Bound.  The STG compiler would emit
    // Ref::L (de Bruijn) for these, producing a blob whose forms only work
    // when loaded as a nested Let — not as independent global slots.
    //
    // To produce Ref::G for intra-prelude references, we reverse the merge:
    // repeatedly peel the outermost Let scope using open_let_scope_full, which
    // converts Var::Bound(scope=0) back to Var::Free(name) and decrements all
    // outer scope indices.  After peeling, each binding body has free-variable
    // references to other prelude names rather than de Bruijn bound references.
    //
    // We then compile each body individually with prelude_globals set so that
    // Var::Free("name") → Ref::G(INTRINSIC_COUNT + slot).
    let mut binding_bodies: Vec<(String, RcExpr)> = Vec::new();
    peel_all_let_bindings(&core_expr, &mut binding_bodies);

    println!("  prelude bindings (peeled): {}", binding_bodies.len());

    // Count hoisted namespace members: names matching `__<ns>_<member>` where
    // `<ns>` is a non-empty lowercase segment and `<member>` is non-empty.
    // These are the new globals produced by the `hoist_namespaces` step above.
    let hoisted_count = binding_bodies
        .iter()
        .filter(|(n, _)| {
            n.starts_with("__")
                && n[2..]
                    .find('_')
                    .map_or(false, |sep| sep > 0 && sep + 1 < n[2..].len())
        })
        .count();
    println!("  namespace members hoisted: {hoisted_count}");

    // ── 4b. Collect inlinable combinator bindings for inline_cores ────────────
    // After peeling, intra-prelude references are Var::Free(name) rather than
    // de Bruijn Var::Bound indices.  For combinator lambdas (bodies that are
    // App(Intrinsic, vars) or Var), the lambda parameter references are
    // Var::Bound(scope=0, binder=i) — still correct after peeling.  We tag and
    // collect these so they can be injected before the user-code inline pass.
    //
    // A fixed-point iteration is used so that lambdas whose bodies reference
    // previously collected set members (via Var::Free) are also included.
    // Round 0 seeds the set with bare intrinsic aliases (e.g. `if: __IF`).
    // Each subsequent round adds lambdas whose only free variables are names
    // already in the set, repeating until no new names are added.
    let mut inlinable_names: HashSet<String> = HashSet::new();
    let mut inline_cores: Vec<(String, RcExpr)> = Vec::new();

    // Round 0: bare intrinsics (after peel_meta)
    for (name, body) in &binding_bodies {
        let peeled = peel_meta(body);
        if matches!(&*peeled.inner, Expr::Intrinsic(_, _)) {
            inlinable_names.insert(name.clone());
            inline_cores.push((name.clone(), peeled.clone()));
        }
    }
    println!(
        "  inline cores round 0 (intrinsic aliases): {}",
        inline_cores.len()
    );

    // Iterate: add lambdas whose Free vars are all in the set
    loop {
        let mut added = 0;
        for (name, body) in &binding_bodies {
            if inlinable_names.contains(name) {
                continue;
            }
            let tagged =
                tag_combinators(body).with_context(|| format!("tag_combinators on '{name}'"))?;
            let peeled = peel_meta(&tagged);
            if let Expr::Lam(_, true, scope) = &*peeled.inner {
                if all_free_vars_in_set(&scope.body, &inlinable_names) {
                    inlinable_names.insert(name.clone());
                    inline_cores.push((name.clone(), peeled.clone()));
                    added += 1;
                }
            }
        }
        if added == 0 {
            break;
        }
        println!(
            "  inline cores round: +{added} (total {})",
            inline_cores.len()
        );
    }
    println!("  inline cores total (fixed-point): {}", inline_cores.len());

    // Pre-expand: substitute collected definitions into each other so the blob
    // carries fully-resolved expressions (no Var::Free). Since the fixed-point
    // collects in dependency order (round 0 first), a single forward pass
    // suffices.
    //
    // The inline_cores are pre-expanded: all Var::Free references to other set
    // members are substituted with their definitions at blob generation time.
    // This means the runtime inline pass sees them as immediately closed bodies
    // (no multi-pass resolution needed).
    let mut resolved_map: HashMap<String, RcExpr> = HashMap::new();
    let mut resolved_cores: Vec<(String, RcExpr)> = Vec::new();
    for (name, body) in &inline_cores {
        let subs: Vec<(String, RcExpr)> = resolved_map
            .iter()
            .map(|(n, e)| (n.clone(), e.clone()))
            .collect();
        let expanded = if subs.is_empty() {
            body.clone()
        } else {
            body.substs(&subs)
        };
        resolved_map.insert(name.clone(), expanded.clone());
        resolved_cores.push((name.clone(), expanded));
    }
    let mut inline_cores = resolved_cores;

    // ── 4c. Spike (eu-v8n8, flag-gated): fuse the low-density recursive hot
    // helper cluster into inline_cores ────────────────────────────────────────
    //
    // Option A for the blob locality penalty (eu-ttpl blob-layout-forensics):
    // the packing-density hit on string/list workloads comes from the recursive
    // list helpers `map`/`foldl` running as un-fused standalone global closures
    // (blob 018 same-block hit-rate 0.642 / ~65 frames-per-block vs source 0.870
    // / 170). Their `cons`/`head`/`tail`/`nil?`/`if` operands are already in
    // inline_cores; only these recursive wrappers are missing, because the
    // fixed-point above admits only *closed* combinators and a self-referential
    // lambda is not closed (the exact case eu-2sa6.12 §4.1 deferred).
    //
    // We force their outer lambda's closed flag so the runtime inline pass
    // distributes them into user call sites (carrying their operands inline,
    // packing densely). This is bounded and terminating: `reduce::distribute`
    // substitutes inline_cores exactly once per Let scope and does not
    // re-substitute into nested lambda bodies, and `prepare.rs` runs only two
    // inline iterations — so recursion unrolls a fixed ~2 levels and the
    // residual self-call stays `Var::Free` and resolves to the global slot
    // (`Ref::G`) at STG compile. Semantics are preserved (the inlined copies are
    // the same body); the effect is purely how densely their frames pack.
    //
    // Added AFTER the pre-expansion pass so the recursion never enters the
    // forward-substitution loop. Injected alongside the pre-expanded operands in
    // one Let scope (`inject_prelude_inline_cores`), so their `Var::Free`
    // operand references bind to those siblings.
    //
    // OFF by default: without `EU_BLOB_INLINE_CLUSTER=1` the blob is generated
    // byte-identically to master.
    if std::env::var("EU_BLOB_INLINE_CLUSTER").as_deref() == Ok("1") {
        const CLUSTER: &[&str] = &["map", "foldl"];
        let mut added = 0usize;
        for wanted in CLUSTER {
            if inlinable_names.contains(*wanted) {
                continue; // already fused by the fixed-point
            }
            let Some((name, body)) = binding_bodies.iter().find(|(n, _)| n == wanted) else {
                continue;
            };
            let tagged = tag_combinators(body)
                .with_context(|| format!("tag_combinators on cluster '{name}'"))?;
            let peeled = peel_meta(&tagged);
            // Force the outer lambda closed so the inline pass distributes it.
            let forced = match &*peeled.inner {
                Expr::Lam(smid, _, scope) => RcExpr::from(Expr::Lam(*smid, true, scope.clone())),
                _ => peeled.clone(),
            };
            inline_cores.push((name.clone(), forced));
            added += 1;
        }
        println!(
            "  [SPIKE eu-v8n8] forced hot cluster into inline_cores: +{added} (total {})",
            inline_cores.len()
        );
    }

    // ── 5. Build name→slot mapping from peeled bindings ──────────────────────
    let name_to_slot: HashMap<String, usize> = binding_bodies
        .iter()
        .enumerate()
        .map(|(i, (name, _))| (name.clone(), i))
        .collect();

    // ── 6. Compile each binding body independently with Ref::G for peers ─────
    let mut source_map = eucalypt::common::sourcemap::SourceMap::new();
    let runtime = make_standard_runtime(&mut source_map);

    let stg_settings = eucalypt::eval::stg::StgSettings {
        generate_annotations: false,
        suppress_updates: false,
        suppress_inlining: true,
        suppress_optimiser: false,
        render_type: eucalypt::eval::stg::RenderType::Headless,
        // Resolve Var::Free references to prelude names as Ref::G.
        prelude_globals: Some(name_to_slot.clone()),
        ..Default::default()
    };

    let mut forms: Vec<LambdaForm> = Vec::with_capacity(binding_bodies.len());
    for (name, body) in &binding_bodies {
        let stg = eucalypt::eval::stg::compile(&stg_settings, body.clone(), runtime.as_ref())
            .with_context(|| format!("STG compile binding '{name}'"))?;
        // Wrap the compiled STG body as a 0-arity updatable thunk.
        //
        // Simple bindings (e.g. `x = 1 + 2`) compile to a `Let { bindings:
        // [thunk], body: Ref::L(0,0) }` — one inner form; complex bindings
        // (e.g. `__build = { version: ..., ... }`) compile to a `LetRec {
        // bindings: [f0..fN], body: Ref::L(0,k) }` — many inner forms.
        //
        // Rather than extracting a single `LambdaForm` from the compiled tree
        // (which fails for complex cases), we wrap the *entire* compiled
        // `Rc<StgSyn>` as the body of a fresh `Thunk`.  When the VM forces
        // this thunk the inner Let/LetRec allocates its local bindings normally
        // and the slot is updated to the resulting WHNF — identical behaviour
        // to the single-form case, with at most one extra indirection step.
        forms.push(LambdaForm::thunk(stg));
    }

    println!("  STG lambda forms compiled: {}", forms.len());

    // ── 7. Extract binding names for the PreludeBlob (slot order) ────────────
    let names: Vec<String> = binding_bodies.into_iter().map(|(n, _)| n).collect();
    let name_to_slot: HashMap<String, usize> = names
        .iter()
        .enumerate()
        .map(|(i, n)| (n.clone(), i))
        .collect();

    // ── 8. Flatten lambda forms into a shared arena ───────────────────────────
    let (nodes, forms_pool, binding_entries) = flatten_forms_to_arena(&forms);

    println!(
        "  arena: {} nodes, {} forms ({} bindings)",
        nodes.len(),
        forms_pool.len(),
        binding_entries.len()
    );

    // ── 8b. Pre-encode the prelude BytecodeProgram (BV5, eu-amp9) ─────────────
    // Encode the fixed templates plus every global body (intrinsic wrappers
    // followed by the prelude bindings, in the same slot order the runtime
    // uses) with no program root. The bytecode engine loads this image
    // directly and appends only the user root (and the per-invocation
    // __args/__io overrides) at run time, instead of re-encoding the prelude.
    //
    // `runtime.globals()` here returns just the intrinsic wrappers (this
    // runtime has no prelude bindings set); `forms` are the prelude bindings.
    let bytecode = {
        use eucalypt::eval::stg::runtime::Runtime;
        // Encode the prelude bindings from the SAME arena-reconstructed forms
        // the runtime uses in `globals()`, so the baked bytecode is identical
        // to what a fresh `encode(&syn, &rt.globals())` would produce.
        let arena = StgArena {
            nodes: nodes.clone(),
            forms: forms_pool.clone(),
        };
        let prelude_forms: Vec<LambdaForm> = binding_entries
            .iter()
            .map(|&e| {
                arena
                    .reconstruct_form(e)
                    .expect("reconstruct prelude form for bytecode encode")
            })
            .collect();
        let mut all_globals = runtime.globals();
        all_globals.extend(prelude_forms);
        let (program, global_forms) = eucalypt::eval::bytecode::encode_prelude(&all_globals);
        println!(
            "  bytecode: {} bytes, {} constants, {} global forms",
            program.code.len(),
            program.constants.len(),
            global_forms.len(),
        );
        Some(PreludeBytecodeImage {
            program,
            global_forms,
        })
    };

    // ── 9. Serialise and write ────────────────────────────────────────────────
    let blob = PreludeBlob {
        source_hash,
        nodes,
        forms_pool,
        binding_entries,
        name_to_slot,
        operators,
        type_summary: summary,
        monad_specs,
        monad_type_hints,
        inline_cores,
        bytecode,
    };

    let bytes = blob.to_bytes().context("serialise PreludeBlob")?;
    std::fs::write(&blob_out, &bytes).with_context(|| format!("writing {}", blob_out.display()))?;

    let elapsed = t_start.elapsed();
    println!(
        "Wrote {} ({} bytes) in {:.2}s",
        blob_out.display(),
        bytes.len(),
        elapsed.as_secs_f64()
    );

    Ok(())
}

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Find the workspace root by walking up from the xtask binary's directory.
fn workspace_root() -> Result<PathBuf> {
    // When run as `cargo xtask`, the working directory is the workspace root.
    let cwd = std::env::current_dir().context("current_dir")?;
    // Confirm there is a Cargo.toml (sanity check).
    if cwd.join("Cargo.toml").exists() {
        return Ok(cwd);
    }
    bail!("could not find workspace root (no Cargo.toml in {cwd:?})")
}

/// Peel through `Meta` wrappers to reach the underlying expression node.
///
/// Prelude bindings annotated with doc strings or type signatures are wrapped
/// in `Expr::Meta` nodes.  This helper strips those wrappers so the caller can
/// inspect the actual expression variant.
fn peel_meta(expr: &RcExpr) -> &RcExpr {
    match &*expr.inner {
        Expr::Meta(_, inner, _) => peel_meta(inner),
        _ => expr,
    }
}

/// Peel all top-level `Let` scopes from the merged prelude core expression,
/// collecting each binding as a `(name, body)` pair where intra-prelude
/// references are `Var::Free(name)` rather than de Bruijn `Var::Bound` indices.
///
/// This reverses the effect of `merge_units` + `cook` on the prelude, allowing
/// each binding to be STG-compiled independently with `prelude_globals` set so
/// that `Var::Free("name")` → `Ref::G(INTRINSIC_COUNT + slot)`.
///
/// The peeling uses `open_let_scope_full` which:
/// - Converts `Var::Bound(scope=0, binder=i)` → `Var::Free(names[i])` in all positions.
/// - Decrements scope indices of references to outer Let scopes by 1 so that
///   after peeling each layer the remaining bound references stay consistent.
fn peel_all_let_bindings(expr: &RcExpr, out: &mut Vec<(String, RcExpr)>) {
    let mut current = expr.clone();
    loop {
        match &*current.inner {
            // `Expr::Let` is the only let form in the core IR — there is no
            // separate `Expr::LetRec` variant.  All core lets are semantically
            // recursive (any binding may reference any other in the same scope).
            // The wildcard on `LetType` correctly handles all variants:
            // DefaultBlockLet, OtherLet, DestructureBlockLet, DestructureListLet.
            Expr::Let(_, scope, _) => {
                let (open_bindings, open_body) = open_let_scope_full(scope);
                out.extend(open_bindings);
                current = open_body;
            }
            Expr::Meta(_, inner, _) => {
                current = inner.clone();
            }
            _ => break,
        }
    }
}

/// Flatten a list of entry-level `LambdaForm`s into a shared `StgArena`.
///
/// Returns `(nodes, forms_pool, binding_entries)` where:
/// - `nodes` is the complete arena node pool (`PreludeBlob.nodes`)
/// - `forms_pool` is ALL lambda forms — entry thunks AND any inner forms
///   allocated by `Let`/`LetRec` nodes within each thunk body
///   (`PreludeBlob.forms_pool`)
/// - `binding_entries` is one `FormIdx` per entry form pointing into
///   `forms_pool` (`PreludeBlob.binding_entries`)
///
/// Storing `forms_pool` in full ensures that `StgArena { nodes, forms:
/// forms_pool }` can reconstruct any form without missing inner references
/// from complex bindings (e.g. `__build`'s 14-field block).
fn flatten_forms_to_arena(
    forms: &[LambdaForm],
) -> (Vec<ArenaStgSyn>, Vec<ArenaLambdaForm>, Vec<u32>) {
    let mut arena = StgArena::default();
    let mut binding_entries: Vec<u32> = Vec::with_capacity(forms.len());
    for lf in forms {
        let form_idx = arena.flatten_form(lf);
        binding_entries.push(form_idx);
    }
    // arena.forms holds ALL forms (top-level wrappers + inner LetRec forms).
    (arena.nodes, arena.forms, binding_entries)
}
