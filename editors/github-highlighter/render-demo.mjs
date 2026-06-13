// Render a self-contained preview (demo-rendered.html) showing the highlighter
// output for examples/sample.eu, in GitHub-dark and GitHub-light, as both a
// blob and a small unified diff. Useful for eyeballing the theme without
// installing the userscript. Run with: npm run demo
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { highlightFileLines, highlightDiffRows } from "./src/eucalypt-highlight.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const sample = readFileSync(join(here, "examples/sample.eu"), "utf8");
const classRules = readFileSync(join(here, "src/theme.css"), "utf8")
  .slice(readFileSync(join(here, "src/theme.css"), "utf8").indexOf(".eu-comment"));

const LIGHT = {
  "--eu-comment": "#a0a1a7", "--eu-string": "#50a14f", "--eu-escape": "#0184bc",
  "--eu-datetime": "#0184bc", "--eu-symbol": "#986801", "--eu-number": "#b76b01",
  "--eu-metadata": "#a626a4", "--eu-builtin": "#0184bc", "--eu-keyword": "#a626a4",
  "--eu-prelude": "#4078f2", "--eu-anaphor": "#b76b01", "--eu-function": "#4078f2",
  "--eu-property": "#e45649", "--eu-call": "#4078f2", "--eu-operator": "#0184bc",
  "--eu-punct": "#383a42", "--eu-interp-punct": "#e45649", "--eu-interp": "#b76b01",
  "--eu-interp-fmt": "#0184bc",
};
const DARK = {
  "--eu-comment": "#7f848e", "--eu-string": "#98c379", "--eu-escape": "#56b6c2",
  "--eu-datetime": "#56b6c2", "--eu-symbol": "#e5c07b", "--eu-number": "#d19a66",
  "--eu-metadata": "#c678dd", "--eu-builtin": "#56b6c2", "--eu-keyword": "#c678dd",
  "--eu-prelude": "#61afef", "--eu-anaphor": "#d19a66", "--eu-function": "#61afef",
  "--eu-property": "#e06c75", "--eu-call": "#61afef", "--eu-operator": "#56b6c2",
  "--eu-punct": "#abb2bf", "--eu-interp-punct": "#e06c75", "--eu-interp": "#d19a66",
  "--eu-interp-fmt": "#56b6c2",
};
const vars = (o) => Object.entries(o).map(([k, v]) => `${k}:${v}`).join(";");

const blobHtml = highlightFileLines(sample.split("\n"))
  .map((h, i) => `<tr><td class="ln">${i + 1}</td><td class="code">${h || "&nbsp;"}</td></tr>`)
  .join("\n");

// A small synthetic diff demonstrating add/del/context + a multi-line string.
const diffModel = [
  { sign: " ", type: "context", text: 'region: "eu-west-1"' },
  { sign: "-", type: "del", text: "ports: [22, 80]" },
  { sign: "+", type: "add", text: "ports: [22, 80, 443, 5432]" },
  { sign: " ", type: "context", text: "ratio: 3.14" },
  { sign: "+", type: "add", text: '` { doc: "`instance(local, group)` — an `aws_db_instance`' },
  { sign: "+", type: "add", text: '    placed in `group` (a DB subnet-group handle)." }' },
];
const diffRowsHtml = highlightDiffRows(diffModel)
  .map((h, i) => {
    const { sign, type } = diffModel[i];
    return `<tr class="d-${type}"><td class="sign">${sign}</td><td class="code">${h || "&nbsp;"}</td></tr>`;
  })
  .join("\n");

const panel = (title, palette, bg, fg, addBg, delBg) => `
<section class="panel" style="${vars(palette)}; background:${bg}; color:${fg};">
  <h2>${title}</h2>
  <h3>Blob view — examples/sample.eu</h3>
  <table class="src">${blobHtml}</table>
  <h3>Unified diff</h3>
  <table class="src diff" style="--add:${addBg}; --del:${delBg};">${diffRowsHtml}</table>
</section>`;

const html = `<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Eucalypt GitHub highlighter — preview</title>
<style>
  body { margin:0; font-family:-apple-system,Segoe UI,Helvetica,Arial,sans-serif; background:#f6f8fa; }
  header { padding:16px 24px; border-bottom:1px solid #d0d7de; }
  header h1 { margin:0 0 4px; font-size:18px; }
  header p { margin:0; color:#57606a; font-size:13px; }
  .panels { display:grid; grid-template-columns:1fr 1fr; gap:1px; }
  .panel { padding:16px 24px; }
  .panel h2 { font-size:14px; text-transform:uppercase; letter-spacing:.04em; opacity:.7; }
  .panel h3 { font-size:12px; opacity:.6; margin:18px 0 6px; font-weight:600; }
  table.src { border-collapse:collapse; font-family:SFMono-Regular,Consolas,Liberation Mono,monospace; font-size:12.5px; line-height:1.5; width:100%; }
  table.src td.code { white-space:pre; padding:0 8px; }
  table.src td.ln { text-align:right; padding:0 10px; opacity:.35; user-select:none; width:1%; }
  table.diff td.sign { text-align:center; opacity:.5; user-select:none; width:1%; padding:0 6px; }
  .d-add { background:var(--add); }
  .d-del { background:var(--del); }
  @media (max-width:900px){ .panels{ grid-template-columns:1fr; } }
</style></head>
<body>
<header>
  <h1>Eucalypt syntax highlighting for the GitHub web UI — preview</h1>
  <p>Generated from <code>examples/sample.eu</code>. Left = GitHub dark, right = GitHub light. The userscript injects exactly this colouring into <code>.eu</code> diffs and blobs on github.com.</p>
</header>
<div class="panels">
  ${panel("GitHub dark", DARK, "#0d1117", "#e6edf3", "rgba(46,160,67,.15)", "rgba(248,81,73,.15)")}
  ${panel("GitHub light", LIGHT, "#ffffff", "#1f2328", "rgba(46,160,67,.15)", "rgba(255,129,130,.2)")}
</div>
<style>${classRules}</style>
</body></html>`;

writeFileSync(join(here, "demo-rendered.html"), html);
console.log("wrote demo-rendered.html");
