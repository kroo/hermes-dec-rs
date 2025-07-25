"use strict";
"show source";

// Export two functions in two different CJS idioms.
exports.add = function add(a, b) {
  return a + b;
};

function mul(a, b) {
  return a * b;
}

module.exports.mul = mul;
