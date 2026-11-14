#!/usr/bin/env node
// Headless acceptance check for the published browser build (eu-ioz21.5).
//
// Serves the repository over HTTP, opens web/index.html in Chromium via
// Playwright for each sample file, and checks that web-tree-sitter loaded
// tree-sitter-eucalypt.wasm and queries/highlights.scm, parsed without
// errors, and produced exactly the captures that the native tree-sitter CLI
// (`tree-sitter query --captures`, compiled from the same C sources)
// produces. Run with `npm run test-wasm` after `npm ci`; needs Playwright
// with a Chromium (set CHROMIUM_PATH to use a particular binary).

const { execFileSync } = require("node:child_process");
const { createReadStream, existsSync, statSync } = require("node:fs");
const http = require("node:http");
const path = require("node:path");

const grammarDir = path.resolve(__dirname, "..");
const repoRoot = path.resolve(grammarDir, "../..");
const pagePath = "/editors/tree-sitter-eucalypt/web/index.html";

// Samples, relative to the repository root: the page's default plus real
// sources covering doc metadata, unicode operators, anaphora, sections,
// juxtaposed calls and string interpolation.
//
// tests/harness/054_qsort.eu declares several Greek names in one block: the
// native scanner once classified identifier characters with the C locale and
// mis-parsed it, while the WASM build did not (eu-ioz21.9).
const SAMPLES = [
  null,
  "tests/harness/093_cons_operator.eu",
  "tests/harness/094_juxtaposed_call.eu",
  "tests/harness/128_parse_args.eu",
  "tests/harness/054_qsort.eu",
  "examples/aoc25/day01.eu",
];

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript",
  ".wasm": "application/wasm",
  ".scm": "text/plain; charset=utf-8",
  ".eu": "text/plain; charset=utf-8",
};

function serve() {
  const server = http.createServer((req, res) => {
    const rel = decodeURIComponent(new URL(req.url, "http://x").pathname);
    const file = path.join(repoRoot, path.normalize(rel));
    if (!file.startsWith(repoRoot) || !existsSync(file) || !statSync(file).isFile()) {
      res.writeHead(404).end();
      return;
    }
    res.writeHead(200, { "content-type": MIME[path.extname(file)] || "application/octet-stream" });
    createReadStream(file).pipe(res);
  });
  return new Promise((resolve) => server.listen(0, "127.0.0.1", () => resolve(server)));
}

// Captures from the native CLI, as {name, row, text}; text is compared
// for single-line captures only.
function nativeCaptures(file) {
  const out = execFileSync(
    "npx",
    ["--no-install", "tree-sitter", "query", "--captures", "queries/highlights.scm", file],
    { cwd: grammarDir, stdio: ["ignore", "pipe", "ignore"] },
  ).toString();
  // A multi-line capture's text runs over several output lines, so match
  // over the whole output: each capture ends where the next one starts.
  const re =
    /capture: \d+ - ([\w.]+), start: \((\d+), \d+\), end: \((\d+), \d+\)(?:, text: `([\s\S]*?)`)?(?=\n\s+pattern:|\n\S|\n*$)/g;
  return [...out.matchAll(re)].map((m) => ({
    name: m[1],
    row: Number(m[2]),
    text: m[2] === m[3] ? m[4] : null,
  }));
}

async function main() {
  const { chromium } = require("playwright");
  const server = await serve();
  const base = `http://127.0.0.1:${server.address().port}`;
  const browser = await chromium.launch(
    process.env.CHROMIUM_PATH ? { executablePath: process.env.CHROMIUM_PATH } : {},
  );
  let failures = 0;
  try {
    for (const sample of SAMPLES) {
      const label = sample || "web/sample.eu (default)";
      const page = await browser.newPage();
      const errors = [];
      page.on("pageerror", (e) => errors.push(String(e)));
      const query = sample ? `?file=${encodeURIComponent("/" + sample)}` : "";
      await page.goto(base + pagePath + query);
      await page.waitForSelector("#status:not([data-state=loading])", { timeout: 30000 });
      const state = await page.getAttribute("#status", "data-state");
      const result = await page.evaluate(() => window.highlightResult || null);
      const spans = await page.locator("#out span").count();
      const problems = [...errors];
      if (state !== "done") problems.push(await page.textContent("#status"));
      if (result) {
        if (result.hasError) problems.push("parse tree contains ERROR nodes");
        if (spans === 0) problems.push("no highlighted spans rendered");
        const web = result.captures.map(({ name, row, text }) => ({
          name, row, text: text.includes("\n") ? null : text,
        }));
        const native = nativeCaptures(
          sample ? path.join(repoRoot, sample) : path.join(grammarDir, "web/sample.eu"),
        );
        const a = JSON.stringify(web);
        const b = JSON.stringify(native);
        if (a !== b) {
          const i = web.findIndex((c, k) => JSON.stringify(c) !== JSON.stringify(native[k]));
          problems.push(
            `captures differ from native CLI (web ${web.length}, native ${native.length}); ` +
              `first difference at ${i}: web ${JSON.stringify(web[i])} native ${JSON.stringify(native[i])}`,
          );
        }
      }
      if (problems.length) {
        failures++;
        console.log(`FAIL ${label}\n  ${problems.join("\n  ")}`);
      } else {
        console.log(`PASS ${label}: ${result.captures.length} captures, ${spans} spans`);
      }
      await page.close();
    }
  } finally {
    await browser.close();
    server.close();
  }
  if (failures) process.exit(1);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
