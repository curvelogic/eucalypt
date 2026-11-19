#!/usr/bin/env node
// Run `tree-sitter test` and make the corpus cases it lets through gate
// (eu-2skal).
//
// tree-sitter CLI 0.22.6 (cli/src/test.rs) exits 0 in three cases where a
// corpus case does not hold:
//
//   1. A case marked `:error` whose input parses with no ERROR or MISSING
//      node is printed in red but never counted as a failure.
//   2. A case marked `:error` and `:fail-fast` ends the whole run, with
//      exit 0, whether it passes or not; later cases never run.
//   3. A case with no `---` divider line is dropped without being run.
//
// This wrapper runs `tree-sitter test` (passing its arguments through and
// failing if it fails), then reads test/corpus the way the CLI does and
// fails if:
//
//   - a non-skipped `:error` case's input parses without an error
//     (checked with `tree-sitter parse --quiet`, one case at a time);
//   - a case combines `:error` with `:fail-fast`, or names a `:language`
//     (this package has one language; the wrapper does not resolve others);
//   - a case header has no divider, so the CLI would drop it.
//
// Usage: npm test [-- <tree-sitter test args>]
//        node scripts/test-corpus.mjs --corpus-only   (skip `tree-sitter test`)

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const PKG_DIR = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const CORPUS_DIR = path.join(PKG_DIR, "test", "corpus");

function treeSitterBin() {
  const local = path.join(PKG_DIR, "node_modules", ".bin", "tree-sitter");
  return fs.existsSync(local) ? local : "tree-sitter";
}

// The CLI's header and divider syntax (test.rs HEADER_REGEX, DIVIDER_REGEX).
// `[^=]` matches a newline, as in the Rust regex, so blank lines may sit in
// the name block.
const HEADER_RE =
  /^(={3,})([^=\r\n][^\r\n]*)?\r?\n((?:[^=][^\r\n]*\r?\n)+)===+([^=\r\n][^\r\n]*)?\r?\n/gm;
const DIVIDER_RE = /^(-{3,})([^-\r\n][^\r\n]*)?\r?\n/gm;

// std::env::consts::OS for the current platform.
const RUST_OS = { darwin: "macos", win32: "windows" }[process.platform] ?? process.platform;

function parseAttributes(block) {
  const attrs = { skip: false, error: false, failFast: false, platform: null, languages: [] };
  let name = "";
  let seenMarker = false;
  for (const line of block.split(/\r?\n/).filter((l) => l !== "")) {
    const key = line.split("(")[0];
    const arg = (prefix) =>
      line.startsWith(prefix) && line.endsWith(")") ? line.slice(prefix.length, -1) : null;
    if (key === ":skip") {
      seenMarker = attrs.skip = true;
    } else if (key === ":error") {
      seenMarker = attrs.error = true;
    } else if (key === ":fail-fast") {
      seenMarker = attrs.failFast = true;
    } else if (key === ":platform" && arg(":platform(") !== null) {
      seenMarker = true;
      attrs.platform = (attrs.platform ?? false) || arg(":platform(").trim() === RUST_OS;
    } else if (key === ":language" && arg(":language(") !== null) {
      seenMarker = true;
      attrs.languages.push(arg(":language("));
    } else if (!seenMarker) {
      name += line + "\n";
    }
  }
  if (attrs.skip) attrs.error = false;
  return { name: name.trimEnd(), attrs };
}

// Every case in one corpus file: { name, attrs, input | null }. `input` is
// null when the CLI would drop the case for want of a divider.
export function parseCorpusFile(content) {
  const headers = [];
  let firstSuffix;
  for (const m of content.matchAll(HEADER_RE)) {
    if (firstSuffix === undefined) firstSuffix = m[2];
    if (m[2] !== firstSuffix || m[4] !== firstSuffix) continue;
    headers.push({ start: m.index, end: m.index + m[0].length, ...parseAttributes(m[3]) });
  }
  return headers.map((h, i) => {
    const bodyEnd = i + 1 < headers.length ? headers[i + 1].start : content.length;
    const body = content.slice(h.end, bodyEnd);
    // The longest divider wins; on a tie, the last (Rust's max_by_key).
    let best = null;
    for (const m of body.matchAll(DIVIDER_RE)) {
      if (m[2] !== firstSuffix) continue;
      if (best === null || m[0].length >= best[0].length) best = m;
    }
    const input = best === null ? null : body.slice(0, best.index).replace(/\r?\n$/, "");
    return { name: h.name, attrs: h.attrs, input };
  });
}

function corpusFiles(dir) {
  if (!fs.existsSync(dir)) return [];
  const out = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    if (entry.name.startsWith(".")) continue;
    const p = path.join(dir, entry.name);
    if (entry.isDirectory()) out.push(...corpusFiles(p));
    else out.push(p);
  }
  return out.sort();
}

function checkCorpus() {
  const problems = [];
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "ts-eucalypt-error-cases-"));
  let errorCases = 0;
  try {
    for (const file of corpusFiles(CORPUS_DIR)) {
      const rel = path.relative(PKG_DIR, file);
      for (const c of parseCorpusFile(fs.readFileSync(file, "utf8"))) {
        const where = `${rel}: "${c.name}"`;
        if (c.input === null) {
          problems.push(`${where}: no '---' divider, so tree-sitter test never runs it`);
          continue;
        }
        if (!c.attrs.error) continue;
        if (c.attrs.failFast) {
          problems.push(`${where}: :error with :fail-fast ends tree-sitter test with exit 0`);
          continue;
        }
        if (c.attrs.languages.length > 0) {
          problems.push(`${where}: :language on an :error case is not supported here`);
          continue;
        }
        if (c.attrs.platform === false) continue;
        errorCases += 1;
        const input = path.join(tmp, `case-${errorCases}.eu`);
        fs.writeFileSync(input, c.input);
        const r = spawnSync(treeSitterBin(), ["parse", "--quiet", input], {
          cwd: PKG_DIR,
          encoding: "utf8",
        });
        if (r.error) throw r.error;
        const reported = /\((ERROR|MISSING)\b/.test(r.stdout);
        if (r.status === 0 && !reported) {
          problems.push(`${where}: marked :error but parses without an ERROR or MISSING node`);
        } else if (r.status !== 1 || !reported) {
          problems.push(
            `${where}: tree-sitter parse gave exit ${r.status}:\n${r.stdout}${r.stderr}`,
          );
        }
      }
    }
  } finally {
    fs.rmSync(tmp, { recursive: true, force: true });
  }
  return { problems, errorCases };
}

const args = process.argv.slice(2);
const corpusOnly = args[0] === "--corpus-only";
if (!corpusOnly) {
  const r = spawnSync(treeSitterBin(), ["test", ...args], { cwd: PKG_DIR, stdio: "inherit" });
  if (r.error) throw r.error;
  if (r.status !== 0) process.exit(r.status ?? 1);
}

const { problems, errorCases } = checkCorpus();
if (problems.length > 0) {
  console.error(`\ncorpus check: ${problems.length} problem(s) tree-sitter test does not fail on:`);
  for (const p of problems) console.error(`  - ${p}`);
  process.exit(1);
}
console.log(`\ncorpus check: ${errorCases} :error case(s) hold; every case has a divider`);
