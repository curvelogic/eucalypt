#!/usr/bin/env node
// Run the extension's tests (test/*.test.js) with node's test runner.
//
// The file list is built here rather than by a shell glob in package.json:
// Windows shells do not expand `test/*.test.js`, and node only expands
// globs itself from version 21.

"use strict";

const { spawnSync } = require("child_process");
const fs = require("fs");
const path = require("path");

const testDir = path.resolve(__dirname, "..", "test");
const files = fs
  .readdirSync(testDir)
  .filter((f) => f.endsWith(".test.js"))
  .sort()
  .map((f) => path.join(testDir, f));

const result = spawnSync(process.execPath, ["--test", ...files], {
  stdio: "inherit",
});
if (result.error) {
  console.error(`could not run node --test: ${result.error.message}`);
  process.exit(1);
}
process.exit(result.status === null ? 1 : result.status);
