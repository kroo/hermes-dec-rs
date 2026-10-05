// Private runtime; capture intrinsics before the bundle can replace globals.
const F = [], M = [], EMPTY = Symbol('hbc.empty');
const ObjectCtor = Object, ArrayCtor = Array, RegExpCtor = RegExp;
const ErrorCtor = Error, TypeErrorCtor = TypeError, ReferenceErrorCtor = ReferenceError;
const SymbolCtor = Symbol, BigIntCtor = BigInt, SyntaxErrorCtor = SyntaxError;
const FunctionCtor = Function, nativeEval = eval, stringify = JSON.stringify;
const apply = Reflect.apply, nativeConstruct = Reflect.construct;
const define = Object.defineProperty, getDescriptor = Object.getOwnPropertyDescriptor;
const objectCreate = Object.create, getPrototype = Object.getPrototypeOf;
const reflectSet = Reflect.set, reflectDelete = Reflect.deleteProperty;
const arrayIsArray = Array.isArray, arrayValues = Array.prototype[Symbol.iterator];
const weakGet = WeakMap.prototype.get, weakSet = WeakMap.prototype.set;
const mapHas = Map.prototype.has, mapGet = Map.prototype.get, mapSet = Map.prototype.set;
const objectKeys = Object.keys, PromiseCtor = Promise, promiseResolve = Promise.resolve;
const metadata = new WeakMap();
const generatorStates = new WeakMap();
const nativeGenerator = function*() {};
const nativeGeneratorFunctionPrototype = getPrototype(nativeGenerator);
const nativeGeneratorPrototype = getPrototype(nativeGenerator.prototype);
const generatorFunctionPrototype = objectCreate(getPrototype(nativeGeneratorFunctionPrototype));
const generatorPrototype = objectCreate(getPrototype(nativeGeneratorPrototype));
const asyncFunctionPrototype = getPrototype(async function() {});
for (const key of Reflect.ownKeys(nativeGeneratorFunctionPrototype))
  define(generatorFunctionPrototype, key, getDescriptor(nativeGeneratorFunctionPrototype, key));
for (const key of Reflect.ownKeys(nativeGeneratorPrototype))
  define(generatorPrototype, key, getDescriptor(nativeGeneratorPrototype, key));
define(generatorFunctionPrototype, 'prototype', {value: generatorPrototype});
define(generatorPrototype, 'constructor', {value: generatorFunctionPrototype});
function resumeGenerator(receiver, action, value) {
  const resume = apply(weakGet, generatorStates, [receiver]);
  if (!resume) throw new TypeErrorCtor('Invalid generator receiver');
  return resume(action, value);
}
const generatorMethods = {
  next(value) { return resumeGenerator(this, 'next', value); },
  throw(value) { return resumeGenerator(this, 'throw', value); },
  return(value) { return resumeGenerator(this, 'return', value); }
};
for (const key of ['next', 'throw', 'return'])
  define(generatorPrototype, key, {value: generatorMethods[key], writable: true, configurable: true});
function isObject(value) {
  return value !== null && (typeof value === 'object' || typeof value === 'function');
}
function closure(id, env, kind = 0) {
  const info = M[id];
  let fn;
  const invoke = (self, args, target) => {
    if (target ? info[2] === 1 : info[2] === 0)
      throw new TypeErrorCtor('Invalid function invocation');
    return F[id](env, self, args, target, fn);
  };
  // Method functions have no [[Construct]] or own prototype, like Hermes NC functions.
  fn = info[2] === 1
    ? {invoke(...args) { return invoke(this, args, void 0); }}.invoke
    : function(...args) { return invoke(this, args, new.target); };
  define(fn, 'name', {value: info[0], configurable: true});
  define(fn, 'length', {value: info[1], configurable: true});
  if (kind === 0 && info[2] === 1 && bytecodeVersion === 90) {
    // Older Hermes gives ordinary NC closures (including arrows) a prototype.
    const prototype = {};
    own(prototype, 'constructor', fn, false);
    define(fn, 'prototype', {value: prototype, writable: true});
  }
  if (kind === 1) {
    setPrototype(fn, generatorFunctionPrototype);
    define(fn, 'prototype', {value: objectCreate(generatorPrototype), writable: true});
  } else if (kind === 2) setPrototype(fn, asyncFunctionPrototype);
  apply(weakSet, metadata, [fn, {id, env}]);
  return fn;
}
function environment(parent, size) { return {parent, slots: new ArrayCtor(size)}; }
function parentEnvironment(env, levels) {
  while (levels--) env = env.parent;
  return env;
}
function coerceThis(value) { return value == null ? G : ObjectCtor(value); }
function directEval(value, strict) {
  if (G.eval !== nativeEval) return apply(G.eval, void 0, [value]);
  if (typeof value !== 'string') return value;
  // Hermes DirectEval uses a dummy function scope without captured locals.
  // Embed the input as a literal so no helper parameters/locals leak into eval.
  return apply(FunctionCtor('return eval(' + stringify((strict ? '"use strict";\n' : '') + value) + ');'), G, []);
}
function numeric(value) {
  if (isObject(value)) {
    const method = value[SymbolCtor.toPrimitive];
    if (method != null) {
      value = apply(method, value, ['number']);
      if (isObject(value)) throw new TypeErrorCtor('Cannot convert object to primitive');
    } else {
      // Unary arithmetic performs ToNumeric, including BigInt, without string addition.
      let primitive;
      for (const name of ['valueOf', 'toString']) {
        const fn = value[name];
        if (typeof fn === 'function') {
          primitive = apply(fn, value, []);
          if (!isObject(primitive)) { value = primitive; break; }
        }
      }
      if (isObject(value)) throw new TypeErrorCtor('Cannot convert object to primitive');
    }
  }
  return typeof value === 'bigint' ? value : +value;
}
function increment(value, delta) {
  value = numeric(value);
  return value + (typeof value === 'bigint' ? BigIntCtor(delta) : delta);
}
function createObject(parent) {
  return objectCreate(parent === null || isObject(parent) ? parent : ObjectCtor.prototype);
}
function own(object, key, value, enumerable) {
  define(object, key, {value, enumerable, writable: true, configurable: true});
}
function objectLiteral(keys, values) {
  const object = {};
  for (let i = 0; i < keys.length; i++) own(object, keys[i], values[i], true);
  return object;
}
function accessor(object, key, getter, setter, enumerable) {
  define(object, key, {get: getter, set: setter, enumerable, configurable: true});
}
function globalGet(object, key) {
  if (!(key in object)) throw new ReferenceErrorCtor(key + ' is not defined');
  return object[key];
}
function put(object, key, value, mustExist, strict) {
  if (mustExist && !(key in ObjectCtor(object)))
    throw new ReferenceErrorCtor(key + ' is not defined');
  if (object == null) throw new TypeErrorCtor('Cannot set property of null or undefined');
  const success = reflectSet(ObjectCtor(object), key, value, object);
  if (!success && strict) throw new TypeErrorCtor('Cannot assign property ' + String(key));
}
function remove(object, key, strict) {
  if (object == null) throw new TypeErrorCtor('Cannot delete property of null or undefined');
  const success = reflectDelete(ObjectCtor(object), key);
  if (!success && strict) throw new TypeErrorCtor('Cannot delete property ' + String(key));
  return success;
}
function declareGlobal(key) {
  if (!getDescriptor(G, key)) define(G, key, {value: void 0, writable: true, enumerable: true});
}
function checkGlobal(key) {
  const descriptor = getDescriptor(G, key);
  if (descriptor && !descriptor.configurable) throw new SyntaxErrorCtor('Restricted global property ' + key);
}
function createThis(proto, constructor) {
  if (typeof constructor !== 'function') throw new TypeErrorCtor('Not a constructor');
  return createObject(proto);
}
function construct(fn, self, args) {
  const meta = apply(weakGet, metadata, [fn]);
  if (meta) {
    if (M[meta.id][2] === 1) throw new TypeErrorCtor('Not a constructor');
    return F[meta.id](meta.env, self, args, fn, fn);
  }
  return nativeConstruct(fn, args);
}
let looseArgumentsFactory;
function argumentsObject(args, callee, strict) {
  // Native arguments preserve their internal class without an observable tag.
  // Use an isolated non-strict function for an unmapped, configurable callee.
  const factory = strict ? function() { return arguments; }
    : (looseArgumentsFactory || (looseArgumentsFactory = FunctionCtor('return arguments;')));
  const object = apply(factory, void 0, args);
  if (strict) {
    const poison = getDescriptor(object, 'callee').get;
    define(object, 'caller', {get: poison, set: poison});
  } else own(object, 'callee', callee, false);
  own(object, SymbolCtor.iterator, arrayValues, false);
  return object;
}
function propertyNames(object) {
  if (object == null) return void 0;
  const names = [];
  for (const name in ObjectCtor(object)) names.push(name);
  return names;
}
function genericIteratorBegin(source) {
  const iterator = apply(source[SymbolCtor.iterator], source, []);
  if (!isObject(iterator)) throw new TypeErrorCtor('Iterator is not an object');
  return [iterator, iterator.next];
}
function iteratorBegin(source) {
  // Hermes represents built-in array iterators as an index, not an object.
  if (arrayIsArray(source) && source[SymbolCtor.iterator] === arrayValues) return [0, source];
  return genericIteratorBegin(source);
}
function iteratorNext(iterator, next) {
  if (iterator === void 0) return [void 0, void 0];
  if (typeof iterator === 'number') {
    return iterator >= next.length ? [void 0, void 0] : [next[iterator], iterator + 1];
  }
  const result = apply(next, iterator, []);
  if (!isObject(result)) throw new TypeErrorCtor('Iterator result is not an object');
  return result.done ? [void 0, void 0] : [result.value, iterator];
}
function iteratorClose(iterator, ignoreErrors) {
  if (!isObject(iterator)) return;
  try {
    const method = iterator.return;
    if (method == null) return;
    const result = apply(method, iterator, []);
    if (!isObject(result)) throw new TypeErrorCtor('Iterator return result is not an object');
  } catch (error) { if (!ignoreErrors) throw error; }
}
function generator(id, env, self, args, callee) {
  const state = {r: [], pc: 0, resume: 0, caught: void 0, done: false, running: false, started: false};
  function resume(action, value) {
    if (state.running) throw new TypeErrorCtor('Generator already executing');
    if (!state.started && action !== 'next') state.done = true;
    if (state.done) {
      if (action === 'throw') throw value;
      return {value: action === 'return' ? value : void 0, done: true};
    }
    state.value = state.started ? value : void 0;
    state.action = action;
    state.started = true;
    state.running = true;
    try { return F[id](env, self, args, void 0, callee, state); }
    catch (error) { state.done = true; throw error; }
    finally { state.running = false; }
  }
  const prototype = callee.prototype;
  const object = objectCreate(isObject(prototype) ? prototype : generatorPrototype);
  apply(weakSet, generatorStates, [object, resume]);
  return object;
}
const publicBuiltins = [Array.isArray, Date.UTC, Date.parse, JSON.parse, JSON.stringify,
  Math.abs, Math.acos, Math.asin, Math.atan, Math.atan2, Math.ceil, Math.cos,
  Math.exp, Math.floor, Math.hypot, Math.imul, Math.log, Math.max, Math.min,
  Math.pow, Math.round, Math.sin, Math.sqrt, Math.tan, Math.trunc,
  Object.create, Object.defineProperties, Object.defineProperty, Object.freeze,
  Object.getOwnPropertyDescriptor, Object.getOwnPropertyNames, Object.getPrototypeOf,
  Object.isExtensible, Object.isFrozen, Object.keys, Object.seal, String.fromCharCode];
const freeze = Object.freeze, ownKeys = Reflect.ownKeys, setPrototype = Object.setPrototypeOf;
const slice = Array.prototype.slice, templates = new Map();
const cjsFunctions = objectCreate(null), cjsCache = objectCreate(null);
function requireModule(id, parent) {
  if (typeof id === 'string' && id.startsWith('.')) {
    const segments = (parent ? parent.split('/').slice(0, -1) : []).concat(id.split('/'));
    const normalized = [];
    for (const part of segments) {
      if (part === '..') normalized.pop();
      else if (part !== '.' && part !== '') normalized.push(part);
    }
    id = normalized.join('/');
  }
  if (cjsFunctions[id] === void 0 && cjsFunctions[id + '.js'] !== void 0) id += '.js';
  if (cjsCache[id]) return cjsCache[id].exports;
  if (cjsFunctions[id] === void 0) throw new ErrorCtor('Unknown CommonJS module ' + id);
  const module = {exports: {}};
  cjsCache[id] = module;
  try { apply(closure(cjsFunctions[id], null), module.exports, [module.exports, dependency => requireModule(dependency, String(id)), module]); }
  catch (error) { delete cjsCache[id]; throw error; }
  return module.exports;
}
function builtinClosure(id) {
  if (id === (bytecodeVersion >= 92 ? 52 : 51)) return spawnAsync;
  if (id < publicBuiltins.length) return publicBuiltins[id];
  return function(...values) { return builtin(id, values, []); };
}
function spawnAsync(factory, self, args) {
  return new PromiseCtor((resolve, reject) => {
    let iterator;
    try { iterator = apply(factory, self, args); } catch (error) { reject(error); return; }
    function step(action, value) {
      let result;
      try { result = apply(iterator[action], iterator, [value]); }
      catch (error) { reject(error); return; }
      if (result.done) resolve(result.value);
      else apply(promiseResolve, PromiseCtor, [result.value]).then(value => step('next', value), error => step('throw', error));
    }
    step('next');
  });
}
function builtin(id, values, callerArgs, state) {
  if (id < publicBuiltins.length) return apply(publicBuiltins[id], void 0, values);
  const [a, b, c] = values;
  switch (id) {
    case 37: if (isObject(a) && (b === null || isObject(b))) setPrototype(a, b); return void 0;
    case 38: return requireModule(b);
    case 39: {
      if (apply(mapHas, templates, [a])) return apply(mapGet, templates, [a]);
      const count = b ? values.length - 2 : values.length / 2 - 1;
      const raw = apply(slice, values, [2, 2 + count]);
      const cooked = apply(slice, values, [b ? 2 : 2 + count]);
      define(cooked, 'raw', {value: freeze(raw)});
      freeze(cooked); apply(mapSet, templates, [a, cooked]); return cooked;
    }
    case 40: if (!isObject(a)) throw new TypeErrorCtor('Expected object'); return a;
    case 41: {
      const method = a[b];
      if (method == null) return void 0;
      if (typeof method !== 'function') throw new TypeErrorCtor('Method is not callable');
      return method;
    }
    case 42: throw new TypeErrorCtor(a);
    case 43:
      if (!state) throw new TypeErrorCtor('Delegation outside generator');
      state.delegated = true; return void 0;
    case 44: {
      if (b == null) return a;
      for (const key of ownKeys(ObjectCtor(b))) {
        if (c != null && key in c) continue;
        const descriptor = getDescriptor(ObjectCtor(b), key);
        if (descriptor && descriptor.enumerable) own(a, key, b[key], true);
      }
      return a;
    }
    case 45: return apply(slice, callerArgs, [a >>> 0]);
    case 46: {
      let index = c;
      const [iterator, next] = genericIteratorBegin(b);
      for (;;) {
        const result = apply(next, iterator, []);
        if (!isObject(result)) throw new TypeErrorCtor('Iterator result is not an object');
        if (result.done) return index;
        own(a, index++, result.value, true);
      }
    }
    case 47: return values.length === 2 ? nativeConstruct(a, b) : apply(a, c, b);
    case 48:
      for (const key of objectKeys(b)) if (key !== 'default')
        define(a, key, {value: b[key], writable: true, enumerable: true});
      return void 0;
    case 49: return a ** b;
    // Native RegExp already includes the named groups parsed from its pattern.
    case 50: return void 0;
    case 51: if (bytecodeVersion >= 92) return ErrorCtor; return apply(spawnAsync, void 0, values);
    case 52: return apply(spawnAsync, void 0, values);
    default: throw new ErrorCtor('Unsupported builtin ' + id);
  }
}
