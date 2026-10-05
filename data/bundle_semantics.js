// Deterministic whole-bundle edge cases, compiled with Hermes 90 and 96.
(function() {
  const results = [];
  function capture(fn) {
    // Engine-generated TypeError messages differ between Hermes and Node.
    try { return ['ok', fn()]; } catch (error) { return ['error', error instanceof TypeError ? error.name : error.message]; }
  }
  function iteratorScenario(mode) {
    const events = [];
    let count = 0;
    const iterator = {
      get next() {
        events.push('get next');
        return function() {
          events.push(this === iterator ? 'next this' : 'bad this');
          if (mode === 'next throws') throw new Error('next error');
          return {value: ++count, done: count > 3};
        };
      },
      get return() {
        events.push('get return');
        if (mode === 'getter throws') throw new Error('getter error');
        return function() {
          events.push(this === iterator ? 'return this' : 'bad this');
          if (mode === 'return throws' || mode === 'body throws') throw new Error('return error');
          if (mode === 'return primitive') return 4;
          return {done: true};
        };
      },
      [Symbol.iterator]() { events.push('iterator'); return this; }
    };
    const value = capture(function() {
      const values = [];
      for (const item of iterator) {
        values.push(item);
        if (mode === 'body throws') throw new Error('body error');
        if (mode !== 'exhaustion') break;
      }
      return values;
    });
    return {events, value};
  }
  for (const mode of ['exhaustion','break','next throws','return throws','body throws','getter throws','return primitive'])
    results.push(iteratorScenario(mode));
  function* gen() {
    try { const value = yield 1; yield value + 2; }
    catch(error) { yield error.message; }
    finally { results.push('generator cleanup'); }
    return 99;
  }
  const a = gen(); results.push(a.next(), a.next(8), a.return(20), a.next());
  const b = gen(); results.push(b.next(), b.throw(new Error('injected')), b.next());
  const c = gen(); results.push(c.return(3), c.next());
  function* delegated() { yield* [1,2]; return 3; }
  const d = delegated(); results.push(d.next(), d.next(), d.next());
  function outer(start) {
    let count = start;
    return function(step) { count += step; return () => count; };
  }
  const x = outer(1), y = outer(50), get = x(2);
  results.push(get(), x(4)(), get(), y(1)());
  function Constructor(value) { this.value = value; this.target = new.target === Constructor; }
  const instance = new Constructor(8);
  results.push(instance.value, instance.target, instance instanceof Constructor);
  const arrow = () => 5;
  results.push(capture(() => new arrow()));
  function strictThis() { 'use strict'; return this; }
  function looseThis() { return this === globalThis; }
  results.push(strictThis.call(null), looseThis(), strictThis.call(7));
  function rest(first, ...tail) { return [first, tail, arguments.length, arguments[2]]; }
  results.push(rest(1,2,3,4));
  const symbol = Symbol('test');
  const original = {a: 1, [symbol]: 2};
  const copy = {...original};
  results.push(copy.a, copy[symbol]);
  function tag(strings, value) { return [strings[0], strings.raw[0], value, Object.isFrozen(strings)]; }
  results.push(tag`line\n${4}`);
  function bigint(value) { value++; return value; }
  results.push(String(bigint(99n)));
  const utf16 = ['\ud800', '\udfff', '\ud800A\udfff', '\ud83d\ude00', '\u2028\u2029'];
  results.push(utf16.map(text => {
    const units = [];
    for (let i = 0; i < text.length; i++) units.push(text.charCodeAt(i));
    return units;
  }));
  const surrogateKey = {'\ud800': 17};
  results.push(surrogateKey['\ud800']);
  function* reflectedGenerator(value) { yield value; return value + 1; }
  const reflected = reflectedGenerator(10), other = reflectedGenerator(20);
  results.push(Object.getPrototypeOf(reflected) === reflectedGenerator.prototype,
    Object.getPrototypeOf(reflectedGenerator.prototype) === Object.getPrototypeOf(reflectedGenerator).prototype,
    Object.prototype.toString.call(reflected), Object.prototype.toString.call(reflectedGenerator),
    Object.getOwnPropertyNames(reflected), reflected.next === other.next,
    Object.getOwnPropertyDescriptor(reflectedGenerator, 'prototype'));
  results.push(reflected.next.call(other), capture(() => reflected.next.call({})),
    capture(() => new reflectedGenerator()), reflected.next.length);
  const alternate = Object.create(reflectedGenerator.prototype);
  reflectedGenerator.prototype = alternate;
  const changed = reflectedGenerator(30);
  results.push(Object.getPrototypeOf(changed) === alternate, changed.next());
  reflectedGenerator.prototype = 7;
  const fallback = reflectedGenerator(40);
  results.push(Object.getPrototypeOf(fallback) === Object.getPrototypeOf(Object.getPrototypeOf(alternate)), fallback.next());
  async function reflectedAsync() { return 1; }
  results.push(Object.prototype.toString.call(reflectedAsync),
    Object.prototype.hasOwnProperty.call(reflectedAsync, 'prototype'),
    capture(() => new reflectedAsync()));
  globalThis.bundleResults = results;
})();
