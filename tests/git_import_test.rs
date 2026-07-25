//! Hermetic end-to-end tests for git imports (`{ import: { git: ...,
//! commit: ..., import: ... } }`) — eu-9vkqn.
//!
//! These tests exercise the full pipeline against a **local** git
//! repository (via a `file://` URL) so they never touch the network:
//!
//! (a) the fetch/cache path (`src/import/git.rs`'s `resolve_git_import`),
//! (b) the transitive-import walk resolving a relative import declared
//!     *inside* the git-fetched file from the same repository/commit
//!     rather than the local filesystem (`SourceLoader::load`/`load_tree`
//!     in `src/driver/source.rs`), and
//! (c) name binding from the git-imported unit actually resolving
//!     (desugar-phase import metadata understanding a git-import
//!     descriptor block — `Extract<Input>` in `src/core/expr.rs`).
//!
//! Each test runs the compiled `eu` binary as a subprocess with
//! `EU_CACHE_HOME` (see `git_cache_base` in `src/import/git.rs`) overridden
//! to an isolated temporary directory, so the git cache
//! (`<home>/.eu/cache/git/...`) never touches the real cache location and
//! each test gets a fresh cache. `HOME` is set too, but `EU_CACHE_HOME` is
//! what actually isolates the cache portably: `dirs::home_dir()` (the
//! fallback without the override) resolves via the OS profile API on
//! Windows, not the `HOME` environment variable, so `HOME` alone would
//! silently leave the cache pointed at the real user profile there. The
//! repro `.eu` file lives in a directory entirely separate from the git
//! repository checkout, so a passing result can only come from the
//! git-cache path — not from an ordinary relative filesystem import
//! coincidentally finding the checked-out working tree file of the same
//! name (see eu-9vkqn's investigation notes: this exact coincidence masked
//! the underlying bug during manual reproduction).

use std::path::{Path, PathBuf};
use std::process::Command;

/// Path to the `eu` binary built by cargo for this test run.
fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run `git <args>` in `cwd`, with a fixed author/committer identity so the
/// test doesn't depend on the host's git config, panicking on failure.
fn run_git(args: &[&str], cwd: &Path) {
    let output = Command::new("git")
        .args(args)
        .current_dir(cwd)
        .env("GIT_AUTHOR_NAME", "eu-test")
        .env("GIT_AUTHOR_EMAIL", "eu-test@example.invalid")
        .env("GIT_COMMITTER_NAME", "eu-test")
        .env("GIT_COMMITTER_EMAIL", "eu-test@example.invalid")
        .output()
        .expect("failed to run git");
    assert!(
        output.status.success(),
        "git {:?} failed:\nstdout: {}\nstderr: {}",
        args,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

/// Build a small two-file eucalypt "library" git repository fixture in a
/// fresh temp dir, with `lib/xml.eu` importing `lib/helpers.eu` (a
/// transitive, relative import — see (b) above) and both files' bindings
/// exercised by the harness. Returns `(repo_dir, commit_sha)`.
fn make_git_fixture() -> (tempfile::TempDir, String) {
    let repo_dir = tempfile::tempdir().expect("create repo tempdir");

    std::fs::create_dir_all(repo_dir.path().join("lib")).expect("create lib dir");
    std::fs::write(
        repo_dir.path().join("lib/helpers.eu"),
        "escape(s): \"{}!\"(s)\n",
    )
    .expect("write helpers.eu");
    std::fs::write(
        repo_dir.path().join("lib/xml.eu"),
        concat!(
            "{ import: \"helpers.eu\" }\n",
            "leaf(t, a, c): { tag: t  attrs: a  content: c }\n",
            "to-xml(l): \"<{}>{}</{}>\"(l.tag, escape(l.content), l.tag)\n",
        ),
    )
    .expect("write xml.eu");

    run_git(&["init", "-q"], repo_dir.path());
    run_git(&["add", "lib/helpers.eu", "lib/xml.eu"], repo_dir.path());
    run_git(&["commit", "-q", "-m", "add xml lib"], repo_dir.path());

    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(repo_dir.path())
        .output()
        .expect("failed to run git rev-parse");
    assert!(output.status.success(), "git rev-parse HEAD failed");
    let commit = String::from_utf8_lossy(&output.stdout).trim().to_string();
    assert_eq!(commit.len(), 40, "expected a full git SHA, got {commit:?}");

    (repo_dir, commit)
}

/// Run `eu <path>` with the git cache location overridden to `home_dir`
/// (via `EU_CACHE_HOME` — see `git_cache_base` in `src/import/git.rs`) and
/// no other lib-path additions, returning the completed `Output`.
///
/// `HOME` is set too, for good measure on platforms/tools that consult it
/// directly, but `EU_CACHE_HOME` is what actually isolates the cache:
/// `dirs::home_dir()` (what `git_cache_base` falls back to without the
/// override) resolves via the OS profile API on Windows, not the `HOME`
/// environment variable, so `HOME` alone would not isolate the cache
/// there.
///
/// The subject file's own directory is still searched (that's ordinary,
/// unrelated-to-git relative import resolution), but nothing else is on
/// the lib path, so a relative import can only succeed via the git cache
/// or a file that is genuinely alongside the subject file.
fn run_eu_with_isolated_home(subject: &Path, home_dir: &Path) -> std::process::Output {
    Command::new(eu_binary())
        .arg("--heap-limit-mib")
        .arg("512")
        .arg(subject)
        .env("HOME", home_dir)
        .env("EU_CACHE_HOME", home_dir)
        .output()
        .expect("failed to run eu binary")
}

fn assert_success(output: &std::process::Output, context: &str) {
    assert!(
        output.status.success(),
        "{context}: eu exited with status {:?}\nstdout: {}\nstderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

/// Unnamed git import: fetch, transitive import, and name binding all in
/// one hermetic pass — the core regression test for eu-9vkqn layers 1 and 2.
#[test]
fn git_import_fetches_transitively_and_binds_names() {
    let (repo, commit) = make_git_fixture();
    let home = tempfile::tempdir().expect("create home tempdir");

    // The subject file lives in its own directory, entirely separate from
    // the git checkout, so there is no `lib/` directory alongside it that
    // an ordinary (non-git) relative import could coincidentally resolve
    // against.
    let subject_dir = tempfile::tempdir().expect("create subject tempdir");
    let subject = subject_dir.path().join("repro.eu");
    std::fs::write(
        &subject,
        format!(
            concat!(
                "{{ import: {{ git: \"file://{}\"\n",
                "            commit: \"{}\"\n",
                "            import: \"lib/xml.eu\" }} }}\n",
                "result: leaf(\"a\", {{}}, \"b\") to-xml\n",
            ),
            repo.path().display(),
            commit,
        ),
    )
    .expect("write repro.eu");

    let output = run_eu_with_isolated_home(&subject, home.path());
    assert_success(&output, "first (cold-cache) run");
    let stdout = String::from_utf8_lossy(&output.stdout);
    // `to-xml` calls the transitively-imported `escape`, which appends
    // "!" — so a correct result depends on (a) the fetch, (b) the
    // transitive import resolving from the git cache, and (c) both
    // `leaf` and `to-xml` binding from the git-imported unit.
    assert!(
        stdout.contains("<a>b!</a>"),
        "expected rendered XML with escaped content, got: {stdout}"
    );

    // (a) the fetch/cache path: both files should now be cached under the
    // isolated HOME, keyed by commit.
    let cached_xml = find_cached_file(home.path(), "xml.eu");
    let cached_helpers = find_cached_file(home.path(), "helpers.eu");
    assert!(
        cached_xml.is_some(),
        "expected lib/xml.eu to be cached under the isolated HOME"
    );
    assert!(
        cached_helpers.is_some(),
        "expected lib/helpers.eu to be cached under the isolated HOME (transitive import)"
    );

    // Re-run against the now-warm cache: should still succeed, purely from
    // cache (no further git operations required — see `git.rs`'s "Fast
    // path: already cached" behaviour).
    let output2 = run_eu_with_isolated_home(&subject, home.path());
    assert_success(&output2, "second (warm-cache) run");
    let stdout2 = String::from_utf8_lossy(&output2.stdout);
    assert!(
        stdout2.contains("<a>b!</a>"),
        "expected the same result on a warm-cache run, got: {stdout2}"
    );
}

/// Named git import (`import: "x=lib/xml.eu"`): the imported unit's
/// bindings should be namespaced under `x.`, exactly like a named plain
/// filesystem import — eu-9vkqn layer 3.
#[test]
fn named_git_import_namespaces_bindings() {
    let (repo, commit) = make_git_fixture();
    let home = tempfile::tempdir().expect("create home tempdir");
    let subject_dir = tempfile::tempdir().expect("create subject tempdir");
    let subject = subject_dir.path().join("repro.eu");
    std::fs::write(
        &subject,
        format!(
            concat!(
                "{{ import: {{ git: \"file://{}\"\n",
                "            commit: \"{}\"\n",
                "            import: \"x=lib/xml.eu\" }} }}\n",
                "result: x.leaf(\"a\", {{}}, \"b\") x.to-xml\n",
            ),
            repo.path().display(),
            commit,
        ),
    )
    .expect("write repro.eu");

    let output = run_eu_with_isolated_home(&subject, home.path());
    assert_success(&output, "named git import run");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("<a>b!</a>"),
        "expected rendered XML via namespaced x.leaf/x.to-xml, got: {stdout}"
    );
}

/// Search `home_dir/.eu/cache/git` recursively for a file named `name`.
fn find_cached_file(home_dir: &Path, name: &str) -> Option<PathBuf> {
    fn walk(dir: &Path, name: &str) -> Option<PathBuf> {
        let entries = std::fs::read_dir(dir).ok()?;
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                if let Some(found) = walk(&path, name) {
                    return Some(found);
                }
            } else if path.file_name().and_then(|n| n.to_str()) == Some(name) {
                return Some(path);
            }
        }
        None
    }
    walk(&home_dir.join(".eu").join("cache").join("git"), name)
}
