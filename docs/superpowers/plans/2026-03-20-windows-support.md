# Eucalypt on Windows

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **MANDATORY**: Before writing ANY eucalypt (.eu) code, read: `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`, `docs/appendices/cheat-sheet.md`

**Goal:** Build, test and release eucalypt on Windows. Users should be able to download an `.exe` from GitHub releases and use all core functionality including `io.shell` (via PowerShell).

**Bead:** eu-vzi

**Architecture:** The codebase is largely cross-platform already. Changes are concentrated in three areas: (1) crash signal handler needs `cfg(unix)` gating, (2) shell execution needs platform dispatch to PowerShell on Windows, (3) CI needs a Windows runner and release artefact.

**Tech Stack:** Rust (cross-platform conditionals), GitHub Actions (Windows runner), PowerShell

---

## Chunk 1: Platform-gate Unix-only Code

Gate the crash signal handler behind `cfg(unix)` so the codebase compiles on Windows. Provide no-op stubs on non-Unix, non-WASM platforms.

### Task 1: Gate crash handler with `cfg(unix)`

**Files:**
- Modify: `src/eval/machine/crash.rs`
- Modify: `src/bin/eu.rs`

- [ ] **Step 1: Change all `cfg(not(target_arch = "wasm32"))` gates in `crash.rs` to `cfg(unix)`**
  - This covers: `GcEventKind::label()`, `write_usize()`, `write_bytes()`, `install_crash_handler()`, `uninstall_crash_handler()`, `crash_signal_handler()`
  - The non-signal-safe data structures (`GcEventRing`, `CrashDiagnostics`, etc.) remain unconditionally compiled — they're used by the VM regardless of platform
  - `install_crash_handler()` and `uninstall_crash_handler()` already have `cfg` gates; just change the condition

- [ ] **Step 2: Add no-op stubs for non-Unix, non-WASM platforms**
  - Add `#[cfg(not(any(unix, target_arch = "wasm32")))]` versions of `install_crash_handler()` and `uninstall_crash_handler()` that are empty
  - This covers Windows (and any future non-Unix targets)

- [ ] **Step 3: Update `src/bin/eu.rs`**
  - The `install_crash_handler()` call in `main()` should compile on all platforms now (no-op on Windows)
  - Verify no other `libc`-dependent code is called unconditionally

- [ ] **Step 4: Gate `libc` dependency**
  - In `Cargo.toml`, make `libc` a `[target.'cfg(unix)'.dependencies]` dependency instead of unconditional
  - Verify nothing else uses `libc` outside crash.rs

**Verification:** `cargo check --target x86_64-pc-windows-msvc` (cross-check, does not need Windows — just verifies compilation). If cross-target not available locally, defer to CI.

---

## Chunk 2: Cross-platform Shell Execution

Make `io.shell` use PowerShell on Windows and `sh` on Unix.

### Task 2: Platform-aware shell dispatch

**Files:**
- Modify: `src/driver/io_run.rs`

- [ ] **Step 1: Add platform dispatch in `execute_shell()`**
  ```rust
  fn execute_shell(
      cmd: &str,
      stdin_data: Option<&str>,
      timeout_secs: u64,
  ) -> Result<CommandResult, IoRunError> {
      let mut command = if cfg!(windows) {
          let mut c = Command::new("pwsh");
          c.args(["-NoProfile", "-Command", cmd]);
          c
      } else {
          let mut c = Command::new("sh");
          c.args(["-c", cmd]);
          c
      };
      run_command(command, stdin_data, timeout_secs)
  }
  ```
  - Use `pwsh` (PowerShell Core) rather than `powershell.exe` (legacy Windows PowerShell) — it's cross-platform and more modern
  - `-NoProfile` prevents user profile scripts from interfering

- [ ] **Step 2: Document platform-specific shell behaviour**
  - Update `docs/reference/agent-reference.md` or relevant docs to note that `io.shell` uses `sh` on Unix and `pwsh` on Windows
  - Shell command strings are inherently platform-specific

**Verification:** Manual test on Windows — `eu -e 'io.shell("echo hello")'` should work via PowerShell.

---

## Chunk 3: Build Metadata on Windows

Replace shell-dependent OS/arch detection in CI with cross-platform approach.

### Task 3: Cross-platform build metadata in CI

**Files:**
- Modify: `.github/workflows/build-rust.yaml`

- [ ] **Step 1: Add PowerShell equivalents for build metadata generation**
  - The existing release jobs use:
    ```bash
    export OSTYPE=$(uname)
    export HOSTTYPE=$(uname -m)
    eu build.eu -t build-meta > build-meta.yaml.new
    ```
  - For the Windows release job, use PowerShell:
    ```powershell
    $env:OSTYPE = "Windows"
    $env:HOSTTYPE = "x86_64"
    eu build.eu -t build-meta > build-meta.yaml.new
    ```
  - `build.eu` uses `env lookup-alts([:HOSTTYPE, :_system_arch], "unknown")` so setting the env vars is sufficient

- [ ] **Step 2: Bootstrap problem — building `eu` to run `build.eu`**
  - The release workflow already does `cargo install --path .` to get a temporary `eu` for generating build metadata
  - On Windows, the installed binary will be `eu.exe` — verify the subsequent `eu build.eu` invocation works (it should, Cargo handles this)
  - If `eu.exe` can't run `build.eu` on Windows (e.g. due to io.shell calls in build.eu), fall back to generating `build-meta.yaml` directly in the workflow without running `eu`

**Verification:** Windows CI job produces correct `build-meta.yaml` with `os: Windows` and `arch: x86_64`.

---

## Chunk 4: Windows CI

Add Windows to the test matrix and create a Windows release artefact.

### Task 4: Add Windows to test matrix

**Files:**
- Modify: `.github/workflows/build-rust.yaml`

- [ ] **Step 1: Add `windows-latest` to the test matrix**
  - Update the `os` matrix to include `windows-latest`:
    ```yaml
    os: ${{ github.event_name == 'push' && github.ref == 'refs/heads/master' && fromJSON('["ubuntu-latest", "macos-latest", "windows-latest"]') || fromJSON('["ubuntu-latest"]') }}
    ```
  - The `Test doc examples` step already has `if: runner.os == 'Linux'` guard, so it won't run on Windows

- [ ] **Step 2: Handle Windows-specific test considerations**
  - Some harness tests use `io.shell` with Unix commands — these will fail on Windows
  - Options:
    (a) Skip IO-dependent tests on Windows with a test filter
    (b) Make the test harness detect Windows and skip `--allow-io` tests
    (c) Rewrite IO tests to be cross-platform (too much work for initial port)
  - Recommend (a) or (b) — add `if: runner.os != 'Windows'` for the `--allow-io` harness run, or use a separate test invocation for Windows that excludes IO tests

- [ ] **Step 3: Verify non-IO tests pass on Windows**
  - `cargo test` should pass for all unit tests and non-IO harness tests
  - Fix any Windows-specific failures (path separators in test expectations, line endings, etc.)

### Task 5: Add Windows release artefact

**Files:**
- Modify: `.github/workflows/build-rust.yaml`

- [ ] **Step 1: Add Windows release job**
  - New job `release-candidate-windows` following the pattern of existing release jobs
  - Runs on `windows-latest`
  - Produces `eu_windows.exe` (or `eu.exe`) as a release artefact
  - Uses PowerShell for build metadata generation (from Chunk 3)

- [ ] **Step 2: Upload Windows binary to GitHub release**
  - Add Windows binary to the release step alongside existing `eu_amd64`, `eu_darwin`, `eu_aarch64`
  - Name: `eu_windows_amd64.exe`

- [ ] **Step 3: Update `install.sh` or add `install.ps1`**
  - The existing `install.sh` won't work on Windows
  - Add a simple `install.ps1` PowerShell script that downloads and installs the Windows binary
  - Or document manual download from releases page (simpler for initial release)

**Verification:** GitHub Actions produces a working `eu_windows_amd64.exe` in releases. Download and run `eu --version` on Windows.

---

## Chunk 5: Windows-specific Fixes (as discovered)

This chunk captures fixes that emerge during testing. Expected areas:

### Task 6: Path and filesystem fixes

- [ ] **Step 1: Audit path handling in import resolution**
  - `src/import/` and `src/syntax/import.rs` — verify `PathBuf` is used consistently
  - Check for any hardcoded `/` as path separator (vs `std::path::MAIN_SEPARATOR`)
  - Verify `.eucalypt` config file discovery works on Windows

- [ ] **Step 2: Line ending handling**
  - Verify the lexer handles `\r\n` correctly
  - LALRPOP-generated parser may need `\r` stripping
  - Test files checked out with `git` on Windows may have `\r\n` — ensure `.gitattributes` marks `.eu` files as `text eol=lf`

- [ ] **Step 3: Fix any test failures**
  - Address Windows-specific test failures as they arise
  - Document any permanent platform differences (e.g. `io.shell` syntax)

**Verification:** Full `cargo test` passes on Windows (excluding IO-shell tests that use Unix commands).

---

## Implementation Order

1. **Chunk 1** (gate crash handler) — prerequisite for compilation
2. **Chunk 2** (shell dispatch) — prerequisite for IO tests
3. **Chunk 3** (build metadata) — prerequisite for release job
4. **Chunk 4** (CI) — validates everything
5. **Chunk 5** (fixes) — iterative, driven by CI results

Chunks 1-3 can be developed and tested locally (or with cross-compilation checks). Chunk 4 enables CI-driven discovery of remaining issues. Chunk 5 is iterative cleanup.

## Risk Assessment

- **Low risk:** Chunks 1-3 are small, well-scoped changes that don't affect Unix behaviour
- **Medium risk:** Chunk 4 may reveal unexpected test failures — line endings, path handling, LALRPOP-generated code
- **Unknown:** Whether `build.eu` itself runs correctly on Windows (it uses `env` lookups which should work, but untested). Fallback: generate `build-meta.yaml` directly in CI without running `eu`
- **Out of scope:** Windows SEH crash handler (future enhancement), package managers (Scoop/WinGet/Chocolatey — future)
