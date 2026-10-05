// Authored regression fixture; compiled with hermesc -O0 -emit-binary (HBC 96).
var gets = [0];
var sets = [0];
var results;
var failure = '';
function arithmetic(x) {
  function add(y) { return x * 3 + y; }
  return add(2);
}
function params(a, b, c) {
  return [a, b, typeof c, arguments.length, typeof arguments[2]];
}
function missing(a) {
  return [typeof a, arguments.length, typeof arguments[0]];
}
function* sequence(x) {
  var next = yield x + 1;
  yield next + x;
  return x * 3;
}
function names() {
  var text = '';
  for (var key in {alpha: 1, beta: 2}) text += key + ';';
  return text;
}
var sparse = new Array(1);
var iterator;
var first;
var second;
var third;
var capturedCreate = Object.create;
Object.defineProperty(Array.prototype, '0', {
  configurable: true,
  get: Array.prototype.push.bind(gets, 1),
  set: Array.prototype.push.bind(sets)
});
try {
  // Replacing the public intrinsic must not affect private allocations.
  Object.create = null;
  var value = arithmetic(7);
  var supplied = params(4, 5, 6);
  var partial = params(4, 5);
  var absent = missing();
  iterator = sequence(7);
  first = iterator.next();
  second = iterator.next(10);
  third = iterator.next();
  var listed = names();
  var privateGets = gets.length - 1;
  var privateSets = sets.length - 1;
  sparse[0] = 99;
  var appValue = sparse[0];
  results = [value, supplied, partial, absent,
    first.value, first.done, second.value, second.done, third.value, third.done,
    listed, privateGets, privateSets, appValue, gets.length - 1, sets.length - 1,
    Object.prototype.hasOwnProperty.call(sparse, '0')];
} catch (error) {
  failure = error.name;
} finally {
  delete Array.prototype[0];
  Object.create = capturedCreate;
}
print(JSON.stringify({results: results, failure: failure}));
