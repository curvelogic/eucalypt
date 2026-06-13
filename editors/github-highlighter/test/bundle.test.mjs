// End-to-end test of the BUILT userscript: evaluate the bundled IIFE in a
// jsdom page containing a synthetic GitHub .eu diff and confirm it injects the
// stylesheet and highlights the code. Guards the build output (strip/concat/
// inline), not just the source modules. Run `npm run build` first.
import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync, existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { JSDOM } from "jsdom";

const here = dirname(fileURLToPath(import.meta.url));
const bundlePath = join(here, "..", "dist", "eucalypt-github.user.js");

test("built userscript highlights a .eu diff end-to-end", async (t) => {
  if (!existsSync(bundlePath)) {
    t.skip("dist/eucalypt-github.user.js missing — run `npm run build`");
    return;
  }
  const code = readFileSync(bundlePath, "utf8");

  const diff = `<div class="file js-file">
    <div class="file-header" data-path="terraform/aws-rds.eu"></div>
    <table class="diff-table"><tbody>
      <tr><td class="blob-num"></td><td class="blob-num"></td>
          <td class="blob-code blob-code-inner blob-code-addition">ports: [22, 80]</td></tr>
    </tbody></table>
  </div>`;
  const dom = new JSDOM(`<!DOCTYPE html><html data-color-mode="dark"><body>${diff}</body></html>`, {
    url: "https://github.com/curvelogic/eucalypt-grove/pull/14/files",
  });

  global.window = dom.window;
  global.document = dom.window.document;
  global.MutationObserver = dom.window.MutationObserver;
  global.location = dom.window.location;
  global.requestAnimationFrame = (cb) => setTimeout(cb, 0);

  // Run the userscript body (comment header is harmless).
  new Function(code)();

  // The initial scan is scheduled via requestAnimationFrame; let it run.
  await new Promise((r) => setTimeout(r, 10));

  assert.ok(document.getElementById("eu-hl-style"), "theme stylesheet injected");
  const cell = document.querySelector("td.blob-code-inner");
  assert.equal(cell.dataset.euHl, "1", "code cell processed");
  assert.ok(cell.querySelector(".eu-property"), "property head coloured");
  assert.ok(cell.querySelector(".eu-number"), "numbers coloured");
});
