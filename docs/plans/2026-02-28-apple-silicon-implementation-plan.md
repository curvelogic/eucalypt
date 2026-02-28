# Native Apple Silicon Build Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure CI produces an explicitly native aarch64-apple-darwin binary and verify local Apple Silicon builds work correctly.

**Architecture:** The CI release workflow already runs on `macos-latest` (which is ARM64) but doesn't specify an explicit target triple. Make the target explicit, verify tests pass natively, and tidy up loose ends.

**Tech Stack:** GitHub Actions, Rust toolchain, shell scripting

---

### Task 1: Make macOS release job target explicit

**Files:**
- Modify: `.github/workflows/release.yaml` (macOS job, lines ~141-184)

**Step 1: Read the current macOS release job**

Read `.github/workflows/release.yaml` and locate the `release-candidate-macos` job.

**Step 2: Add explicit target to the macOS build step**

In the `release-candidate-macos` job, change the cargo build step from:

```yaml
cargo build --all --release
```

to:

```yaml
cargo build --all --release --target aarch64-apple-darwin
```

Also update the binary path in subsequent steps — the release binary will be at `target/aarch64-apple-darwin/release/eu` instead of `target/release/eu`.

Update the strip and rename step similarly:

```yaml
strip target/aarch64-apple-darwin/release/eu
cp target/aarch64-apple-darwin/release/eu eu_darwin
```

And update the test step if it uses `--release`:

```yaml
cargo test --release --target aarch64-apple-darwin
```

**Step 3: Add target installation step**

After the `dtolnay/rust-toolchain@stable` step, add:

```yaml
- name: add aarch64-apple-darwin target
  run: rustup target add aarch64-apple-darwin
```

(This is a no-op if the runner is already aarch64, but makes intent explicit.)

**Step 4: Commit**

```bash
git add .github/workflows/release.yaml
git commit -m "ci: make aarch64-apple-darwin target explicit in macOS release job"
```

---

### Task 2: Verify local native build

**Step 1: Check current toolchain**

```bash
rustup show
```

Verify whether the default toolchain is `stable-x86_64-apple-darwin` (Rosetta) or `stable-aarch64-apple-darwin` (native).

**Step 2: Install native toolchain if needed**

```bash
rustup toolchain install stable-aarch64-apple-darwin
rustup default stable-aarch64-apple-darwin
```

**Step 3: Run full test suite**

```bash
cargo test
```

Verify all tests pass on the native aarch64 toolchain.

**Step 4: Build and install**

```bash
cargo install --path .
eu --version
```

Verify the installed binary runs natively (check with `file $(which eu)` — should show `arm64`).

**Step 5: Document**

This is a local-only step; no commit needed. If any tests fail on aarch64 that pass on x86_64, file a bug.

---

### Task 3: Close bead

**Step 1: Close the bead**

```bash
bd close eu-0x7y --reason="CI macOS job now targets aarch64-apple-darwin explicitly; local build verified"
bd sync
```

**Step 2: Push**

```bash
git push
```
