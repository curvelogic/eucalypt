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
//!    (parse → desugar → cook → eliminate → inline → fuse → verify → type-check).
//! 2. Extract `OperatorInfo` (before cook strips `Meta` wrappers) and the
//!    `PreludeSummary` from the type checker.
//! 3. STG-compile the prelude expression independently.
//! 4. Walk the compiled `Rc<StgSyn>` tree to extract the top-level
//!    `LambdaForm`s in global-slot order.
//! 5. Walk the pre-cook core expression to extract binding names in the
//!    same order, building `name_to_slot`.
//! 6. Flatten all lambda form bodies into a shared `ArenaStgSyn` node pool.
//! 7. Serialise the `PreludeBlob` with postcard and write `lib/prelude.blob`.
//!
//! **Note on Ref::G vs Ref::L (Phase 7 prerequisite)**
//!
//! The blob produced by this xtask stores `Ref::L` de Bruijn indices for
//! cross-binding references within the prelude.  These indices are correct
//! for loading the prelude as a nested `Let`/`LetRec` block (current runtime
//! path), but are NOT yet suitable for loading individual bindings into
//! independent global slots.
//!
//! Phase 7 (STG compiler changes) will make the compiler emit `Ref::G` for
//! prelude-name free variables.  Once Phase 7 is complete, re-running
//! `cargo xtask prelude-compile` will produce a blob with correct `Ref::G`
//! references that Phase 5 (runtime loading) can use directly.

use std::{collections::HashMap, path::PathBuf, rc::Rc, time::Instant};

use anyhow::{bail, Context, Result};
use eucalypt::{
    core::{
        expr::{Expr, RcExpr},
        typecheck::check::{parse_operator_overloads, type_check_for_prelude},
    },
    driver::source::SourceLoader,
    eval::stg::{
        arena::{ArenaLambdaForm, ArenaStgSyn, StgArena},
        blob::PreludeBlob,
        make_standard_runtime,
        syntax::{LambdaForm, StgSyn},
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

    // Capture pre-cook core expression for name extraction (before cook
    // strips Meta wrappers from operator definitions).
    let precook_expr = loader.core().expr.clone();

    // Extract operators and visibility BEFORE cook strips Meta wrappers.
    loader.extract_operators();
    loader.extract_visibility();

    let operator_type_strings = loader.unit_interface().operator_type_strings();
    let operators = loader.unit_interface().operators.clone();

    // Cook (resolve operator precedence).
    loader.cook().context("cook")?;

    // Run all optimisation passes.
    loader.inline().context("inline")?;
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

    // ── 4. STG-compile the prelude expression ─────────────────────────────────
    let mut source_map = eucalypt::common::sourcemap::SourceMap::new();
    let runtime = make_standard_runtime(&mut source_map);

    let stg_settings = eucalypt::eval::stg::StgSettings {
        generate_annotations: false,
        suppress_updates: false,
        suppress_inlining: false,
        suppress_optimiser: false,
        render_type: eucalypt::eval::stg::RenderType::Headless,
        ..Default::default()
    };

    let stg = eucalypt::eval::stg::compile(&stg_settings, core_expr, runtime.as_ref())
        .context("STG compile")?;

    // ── 5. Extract binding names from the pre-cook expression ─────────────────
    let names = collect_binding_names(&precook_expr);

    // ── 6. Extract LambdaForms from the compiled STG ──────────────────────────
    let forms = collect_lambda_forms(&stg);

    // Report counts; a mismatch is non-fatal at this stage (Phase 7 will fix it).
    println!(
        "  core bindings: {}, STG lambda forms: {}",
        names.len(),
        forms.len()
    );

    if names.len() != forms.len() {
        eprintln!(
            "  Warning: name/form count mismatch ({} vs {}). \
             The blob's name_to_slot map will cover only the \
             min({}, {}) entries. Re-run after Phase 7 compiler \
             changes for a fully correct blob.",
            names.len(),
            forms.len(),
            names.len(),
            forms.len(),
        );
    }

    // ── 7. Build name→slot mapping ────────────────────────────────────────────
    let name_to_slot: HashMap<String, usize> = names
        .iter()
        .zip(0usize..)
        .map(|(name, idx)| (name.clone(), idx))
        .collect();

    // ── 8. Flatten lambda forms into a shared arena ───────────────────────────
    let (nodes, arena_forms) = flatten_forms_to_arena(&forms);

    // ── 9. Serialise and write ────────────────────────────────────────────────
    let blob = PreludeBlob {
        source_hash,
        nodes,
        bindings: arena_forms,
        name_to_slot,
        operators,
        type_summary: summary,
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

/// Walk the core `Let` tree in the same order the STG compiler processes it,
/// collecting binding names.
///
/// The STG compiler traverses `Let` scopes outer-to-inner; the names are
/// emitted in the same order as the STG `Let.bindings` vecs.
///
/// Strips `Meta` wrappers — the pre-cook expression still has them.
fn collect_binding_names(expr: &RcExpr) -> Vec<String> {
    let mut names = Vec::new();
    collect_names_inner(expr, &mut names);
    names
}

fn collect_names_inner(expr: &RcExpr, out: &mut Vec<String>) {
    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            for (name, _) in &scope.pattern {
                out.push(name.clone());
            }
            collect_names_inner(&scope.body, out);
        }
        Expr::Meta(_, inner, _) => collect_names_inner(inner, out),
        _ => {}
    }
}

/// Walk the compiled `Rc<StgSyn>` tree, collecting all `LambdaForm`s from
/// top-level `Let`/`LetRec` nodes in the order the compiler generated them.
///
/// This mirrors `collect_binding_names`: the i-th name from the core
/// expression corresponds to the i-th lambda form extracted here.
///
/// Requires `LambdaForm: Clone`.
fn collect_lambda_forms(stg: &Rc<StgSyn>) -> Vec<LambdaForm> {
    let mut forms: Vec<LambdaForm> = Vec::new();
    collect_forms_inner(stg, &mut forms);
    forms
}

fn collect_forms_inner(stg: &Rc<StgSyn>, out: &mut Vec<LambdaForm>) {
    match &**stg {
        StgSyn::Let { bindings, body } | StgSyn::LetRec { bindings, body } => {
            out.extend(bindings.iter().cloned());
            collect_forms_inner(body, out);
        }
        StgSyn::Ann { body, .. } => collect_forms_inner(body, out),
        _ => {}
    }
}

/// Flatten a list of `LambdaForm`s into a shared `StgArena`.
///
/// Returns `(nodes, forms)` suitable for `PreludeBlob.nodes` and
/// `PreludeBlob.bindings`.  All form bodies index into `nodes`.
fn flatten_forms_to_arena(forms: &[LambdaForm]) -> (Vec<ArenaStgSyn>, Vec<ArenaLambdaForm>) {
    let mut arena = StgArena::default();
    let mut arena_forms = Vec::with_capacity(forms.len());
    for lf in forms {
        let form_idx = arena.flatten_form(lf);
        arena_forms.push(arena.forms[form_idx as usize].clone());
    }
    (arena.nodes, arena_forms)
}
