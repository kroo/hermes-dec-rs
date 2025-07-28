"use strict";
"show source";

/**
 * Very small assertion helper.
 */
function assert(cond, msg) {
  if (!cond) {
    // `print` is available inside the Hermes REPL/CLI; if we’re being
    // executed under Node (e.g. during debugging) fall back to console.log.
    const out = typeof print === "function" ? print : globalThis.console.log;
    out("Assertion failed ‑ " + msg);
    // `quit` exists on Hermes; `process.exit` on Node.
    if (typeof globalThis.quit === "function") {
      globalThis.quit(1);
    } else if (typeof globalThis.process !== "undefined") {
      globalThis.process.exit(1);
    }
    throw new Error(msg); // for completeness
  }
}

if (typeof print !== "function") {
  globalThis.print = globalThis.console.log; // make “print” work under Node as well
}

print("Starting Hermes CJS smoke test …");

// ---------------------------------------------------------------------------
// 1.  Basic CommonJS plumbing
// ---------------------------------------------------------------------------
const math = require("math");

assert(math.add(2, 3) === 5, "add() should add");
assert(math.mul(6, 7) === 42, "mul() should multiply");

// Module caching: second require() must return the exact same object.
assert(require("math") === math, "require cache");

// ---------------------------------------------------------------------------
// 2.  __dirname / __filename
// ---------------------------------------------------------------------------
const path = require("path");
assert(
  globalThis.__dirname === path.dirname(globalThis.__filename),
  "__dirname works",
);

// ---------------------------------------------------------------------------
// 3.  A bit of ES-2020+ syntax to verify Hermes can evaluate it
// ---------------------------------------------------------------------------
const spreadSum = (...nums) => nums.reduce((a, b) => a + b, 0);
assert(spreadSum(1, 2, 3, 4) === 10, "arrow + rest/spread");
