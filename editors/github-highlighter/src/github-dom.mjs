// GitHub web UI glue for the Eucalypt highlighter.
//
// Finds `.eu` / `.eucalypt` files in GitHub's classic diff tables (PR "Files
// changed", commit and compare views) and blob views, and rewrites the code
// cells with highlighted markup from the pure core. No network, no eval —
// everything it needs is already in the page.
//
// GitHub's DOM is not a public API, so all the selectors live together in
// SELECTORS below; if GitHub changes its markup this is the one place to look.
// The code is defensive: anything it does not recognise is left untouched.

import {
  highlightDiffRows,
  highlightFileLines,
  highlightOneLine,
} from "./eucalypt-highlight.mjs";

const EU_PATH = /\.(eu|eucalypt)$/i;
const EU_FILENAME = /(^|\/)Eufile$/;

const SELECTORS = {
  // A changed-file container in a diff view.
  diffFile: ".file, .js-file, copilot-diff-entry",
  // Where the file's path is recorded on a diff-file container.
  diffPath: "[data-path], [data-tagsearch-path], .file-info a[title]",
  // The diff table and its code cells.
  diffTable: "table.diff-table, table.js-diff-table",
  codeCell: "td.blob-code-inner, td.blob-code.blob-code-inner",
  // Classic blob (single file) view.
  blobTable: "table.highlight, table.js-file-line-container",
  blobCodeCell: "td.blob-code.blob-code-inner, td.blob-code-inner",
};

const PROCESSED = "euHl"; // dataset flag (data-eu-hl)

export function isEuPath(path) {
  if (!path) return false;
  const clean = path.split("?")[0].split("#")[0];
  return EU_PATH.test(clean) || EU_FILENAME.test(clean);
}

function diffFilePath(fileEl) {
  const el = fileEl.querySelector(SELECTORS.diffPath) || fileEl;
  return (
    el.getAttribute("data-path") ||
    el.getAttribute("data-tagsearch-path") ||
    el.getAttribute("title") ||
    el.textContent ||
    ""
  ).trim();
}

// Classify a unified-diff code cell by its CSS classes.
function rowType(cell) {
  const cl = cell.classList;
  if (cl.contains("blob-code-addition")) return "add";
  if (cl.contains("blob-code-deletion")) return "del";
  if (cl.contains("blob-code-context")) return "context";
  return "other"; // hunk header, injected comment rows, empty cells, ...
}

function applyCell(cell, html) {
  if (html == null) return;
  cell.innerHTML = html;
  cell.dataset[PROCESSED] = "1";
}

function processDiffTable(table) {
  const rows = Array.from(table.querySelectorAll("tr"));
  // Gather code cells per row; >1 per row means a split (side-by-side) diff.
  const perRow = rows.map((tr) =>
    Array.from(tr.querySelectorAll(SELECTORS.codeCell))
  );
  const isSplit = perRow.some((cells) => cells.length > 1);

  if (isSplit) {
    // No cross-line reconstruction for split view; highlight each cell alone.
    for (const cells of perRow) {
      for (const cell of cells) {
        if (cell.dataset[PROCESSED]) continue;
        if (rowType(cell) === "other") continue;
        applyCell(cell, highlightOneLine(cell.textContent));
      }
    }
    return;
  }

  // Unified diff: reconstruct old/new sides so multi-line strings highlight.
  const cells = perRow.map((c) => c[0]).filter(Boolean);
  if (!cells.length || cells.every((c) => c.dataset[PROCESSED])) return;
  const model = cells.map((cell) => ({ type: rowType(cell), text: cell.textContent }));
  const html = highlightDiffRows(model);
  cells.forEach((cell, idx) => {
    if (cell.dataset[PROCESSED]) return;
    applyCell(cell, html[idx]);
  });
}

function processDiffFile(fileEl) {
  if (!isEuPath(diffFilePath(fileEl))) return;
  for (const table of fileEl.querySelectorAll(SELECTORS.diffTable)) {
    processDiffTable(table);
  }
}

function processBlob() {
  // Blob view: detect the language from the URL path.
  const m = location.pathname.match(/\/blob\/[^/]+\/(.+)$/);
  if (!m || !isEuPath(decodeURIComponent(m[1]))) return;
  for (const table of document.querySelectorAll(SELECTORS.blobTable)) {
    const cells = Array.from(table.querySelectorAll(SELECTORS.blobCodeCell));
    if (!cells.length || cells.every((c) => c.dataset[PROCESSED])) continue;
    const html = highlightFileLines(cells.map((c) => c.textContent));
    cells.forEach((cell, idx) => {
      if (cell.dataset[PROCESSED]) return;
      applyCell(cell, html[idx]);
    });
  }
}

export function scan() {
  try {
    for (const fileEl of document.querySelectorAll(SELECTORS.diffFile)) {
      processDiffFile(fileEl);
    }
    processBlob();
  } catch (err) {
    // Never let a DOM surprise break the page.
    console.warn("[eucalypt-highlight] scan failed:", err);
  }
}

let scheduled = false;
function scheduleScan() {
  if (scheduled) return;
  scheduled = true;
  requestAnimationFrame(() => {
    scheduled = false;
    scan();
  });
}

function injectStyle(css) {
  if (document.getElementById("eu-hl-style")) return;
  const style = document.createElement("style");
  style.id = "eu-hl-style";
  style.textContent = css;
  (document.head || document.documentElement).appendChild(style);
}

// Public entry point. `css` is the theme stylesheet text (inlined by the
// build). Wires up an initial scan plus observers for GitHub's lazy diff
// loading and Turbo navigation.
export function initEucalyptHighlighter({ css } = {}) {
  if (css) injectStyle(css);
  scheduleScan();

  // Diffs and files load lazily and on client-side navigation; rescan on any
  // relevant DOM growth.
  const observer = new MutationObserver((mutations) => {
    for (const m of mutations) {
      if (m.addedNodes && m.addedNodes.length) {
        scheduleScan();
        return;
      }
    }
  });
  observer.observe(document.body, { childList: true, subtree: true });

  // GitHub (Turbo / pjax) navigations.
  for (const evt of ["turbo:render", "turbo:load", "pjax:end", "pageshow"]) {
    document.addEventListener(evt, scheduleScan);
  }

  return { rescan: scheduleScan, scan };
}
