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

use std::{collections::HashMap, path::PathBuf, time::Instant};

use anyhow::{bail, Context, Result};
use eucalypt::{
    core::{
        expr::{open_let_scope_full, Expr, RcExpr},
        typecheck::check::{parse_operator_overloads, type_check_for_prelude},
    },
    driver::source::SourceLoader,
    eval::stg::{
        arena::{ArenaLambdaForm, ArenaStgSyn, StgArena},
        blob::PreludeBlob,
        make_standard_runtime,
        syntax::LambdaForm,
    },
    syntax::input::{Input, Locator},
};
use sha2::{Digest, Sha256};

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    match args.next().as_deref() {
        Some("prelude-compile") => cmd_prelude_compile(),
        Some(cmd) => bail!("unknown xtask command: {cmd}"),
        None => {
            eprintln!("Usage: cargo xtask <command>");
            eprintln!("Commands:");
            eprintln!("  prelude-compile   Compile lib/prelude.eu → lib/prelude.blob");
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
    let source_hash: [u8; 32] = Sha256::digest(&source_bytes).into();

    // ── 2. Run the front-end pipeline ────────────────────────────────────────
    // The prelude references `__build` (from build-meta.yaml) and intrinsics
    // such as `__IO_BIND` (from the pseudo io input). These must be loaded
    // before the prelude itself so that the desugarer can resolve them.
    let build_meta = Input::new(
        Locator::Resource("build-meta".to_string()),
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

    let mut loader = SourceLoader::new(vec![]);
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
