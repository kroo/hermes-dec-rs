// Probe Hermes-specific behavior, not just JavaScript source equivalence in Node.
(function() {
  const results = [];
  function describe(strict) {
    const fn = strict ? function() { 'use strict'; return arguments; } : function() { return arguments; };
    const args = fn(1,2,3);
    const callee = Object.getOwnPropertyDescriptor(args, 'callee');
    const caller = Object.getOwnPropertyDescriptor(args, 'caller');
    results.push([Object.prototype.toString.call(args), args.length, Array.from(args),
      callee.configurable, callee.enumerable, typeof callee.get, !!caller, caller && caller.configurable]);
    try { results.push(args.callee === fn); } catch(error) { results.push(error.name); }
  }
  describe(false); describe(true);
  function directCallee() { return arguments.callee === directCallee; }
  results.push(directCallee(4));
  const arrayIterator = Array.prototype[Symbol.iterator];
  const iteratorPrototype = Object.getPrototypeOf([][Symbol.iterator]());
  let returns = 0;
  iteratorPrototype.return = function() { returns++; return {done:true}; };
  for (const value of [1,2,3]) break;
  results.push(returns);
  delete iteratorPrototype.return;
  let reads = 0;
  const array = [2,3];
  Object.defineProperty(array, Symbol.iterator, {get() { reads++; return arrayIterator; }});
  const values = [];
  for (const value of array) values.push(value);
  results.push(reads, values);
  const arrow = () => 1;
  const method = {method() { return 2; }}.method;
  for (const fn of [arrow, method]) {
    const descriptor = Object.getOwnPropertyDescriptor(fn, 'prototype');
    results.push(descriptor ? [descriptor.writable, descriptor.enumerable,
      descriptor.configurable, descriptor.value.constructor === fn] : null);
    try { new fn(); results.push('constructable'); }
    catch(error) { results.push(error.name); }
  }
  globalThis.hermesResults = results;
  print(JSON.stringify(results));
})();
