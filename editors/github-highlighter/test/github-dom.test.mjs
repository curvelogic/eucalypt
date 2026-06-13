// DOM-glue tests using jsdom with a synthetic copy of GitHub's classic diff
// and blob markup. This validates selector wiring, row classification, split
// detection and cell rewriting without a real browser. The live selectors
// still need a quick manual check against github.com (see README), since
// GitHub's markup is not a contract.
import { test } from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";

function setupDom(html, url = "https://github.com/curvelogic/eucalypt-grove/pull/14/files") {
  const dom = new JSDOM(`<!DOCTYPE html><html data-color-mode="dark"><body>${html}</body></html>`, { url });
  global.window = dom.window;
  global.document = dom.window.document;
  global.MutationObserver = dom.window.MutationObserver;
  global.requestAnimationFrame = (cb) => setTimeout(cb, 0);
  global.location = dom.window.location;
  return dom;
}

// One unified-diff row of GitHub's classic markup.
function diffRow(kind, code) {
  const codeClass =
    kind === "add" ? "blob-code-addition" : kind === "del" ? "blob-code-deletion" : "blob-code-context";
  return `<tr>
    <td class="blob-num" data-line-number="1"></td>
    <td class="blob-num" data-line-number="1"></td>
    <td class="blob-code blob-code-inner ${codeClass}">${code}</td>
  </tr>`;
}

function diffFile(path, rows) {
  return `<div class="file js-file">
    <div class="file-header" data-path="${path}"></div>
    <div class="js-file-content">
      <table class="diff-table js-diff-table"><tbody>${rows}</tbody></table>
    </div>
  </div>`;
}

test("isEuPath recognises .eu / .eucalypt / Eufile", async () => {
  const { isEuPath } = await import("../src/github-dom.mjs");
  assert.ok(isEuPath("terraform/aws-rds.eu"));
  assert.ok(isEuPath("a/b.eucalypt"));
  assert.ok(isEuPath("project/Eufile"));
  assert.ok(!isEuPath("README.md"));
  assert.ok(!isEuPath("src/main.rs"));
});

test("a .eu unified diff gets highlighted, other files left alone", async () => {
  const html =
    diffFile("terraform/aws-rds.eu", diffRow("context", "region: &quot;eu-west-1&quot;") + diffRow("add", "ports: [22, 80]")) +
    diffFile("docs/README.md", diffRow("add", "# Heading"));
  setupDom(html);
  const { scan } = await import("../src/github-dom.mjs?case=diff");
  scan();

  const cells = [...document.querySelectorAll("td.blob-code-inner")];
  const euCells = cells.filter((c) => c.closest(".file").querySelector(".file-header").dataset.path.endsWith(".eu"));
  const mdCell = cells.find((c) => !c.closest(".file").querySelector(".file-header").dataset.path.endsWith(".eu"));

  assert.ok(euCells.every((c) => c.dataset.euHl === "1"), "all eu cells marked processed");
  assert.ok(euCells.some((c) => c.querySelector(".eu-property")), "property head coloured");
  assert.ok(euCells.some((c) => c.querySelector(".eu-number")), "numbers coloured");
  assert.equal(mdCell.dataset.euHl, undefined, "markdown cell untouched");
  assert.equal(mdCell.innerHTML, "# Heading");
});

test("multi-line string across added rows is not mis-highlighted", async () => {
  const rows =
    diffRow("add", 'doc: &quot;first line') +
    diffRow("add", "second: line (with) parens and colons:") +
    diffRow("add", 'third&quot;');
  setupDom(diffFile("x.eu", rows));
  const { scan } = await import("../src/github-dom.mjs?case=multiline");
  scan();
  const cells = [...document.querySelectorAll("td.blob-code-inner")];
  const middle = cells[1];
  assert.ok(middle.querySelector(".eu-string"), "middle line is string-classed");
  assert.ok(!middle.querySelector(".eu-property"), "no property head inside the string");
  assert.ok(!middle.querySelector(".eu-punct"), "no stray punctuation inside the string");
});

test("split (side-by-side) diff highlights each side", async () => {
  const split = `<table class="diff-table"><tbody>
    <tr>
      <td class="blob-num" data-line-number="1"></td>
      <td class="blob-code blob-code-inner blob-code-context">region: 1</td>
      <td class="blob-num" data-line-number="1"></td>
      <td class="blob-code blob-code-inner blob-code-context">region: 2</td>
    </tr>
  </tbody></table>`;
  setupDom(`<div class="file js-file"><div class="file-header" data-path="x.eu"></div>${split}</div>`);
  const { scan } = await import("../src/github-dom.mjs?case=split");
  scan();
  const cells = [...document.querySelectorAll("td.blob-code-inner")];
  assert.equal(cells.length, 2);
  assert.ok(cells.every((c) => c.querySelector(".eu-property")), "both sides highlighted");
});

test("blob view highlights a whole .eu file", async () => {
  const blob = `<table class="highlight"><tbody>
    <tr><td id="L1" class="blob-num"></td><td id="LC1" class="blob-code blob-code-inner">region: &quot;x&quot;</td></tr>
    <tr><td id="L2" class="blob-num"></td><td id="LC2" class="blob-code blob-code-inner">ports: [22]</td></tr>
  </tbody></table>`;
  setupDom(blob, "https://github.com/curvelogic/eucalypt-grove/blob/main/terraform/aws-rds.eu");
  const { scan } = await import("../src/github-dom.mjs?case=blob");
  scan();
  const cells = [...document.querySelectorAll("td.blob-code-inner")];
  assert.ok(cells.every((c) => c.dataset.euHl === "1"));
  assert.ok(cells[0].querySelector(".eu-string"));
  assert.ok(cells[1].querySelector(".eu-number"));
});
