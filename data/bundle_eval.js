// Hermes compiled DirectEval deliberately cannot see the caller's lexical scope.
function evalProbe(text) {
  let secretLocal = 77;
  try { return eval(text); } catch(error) { return error.name; }
}
globalThis.evalResults = [
  evalProbe('typeof secretLocal'), evalProbe('typeof arguments'),
  evalProbe('arguments.length'), evalProbe('arguments[0]'),
  evalProbe('this === globalThis'), evalProbe('var dummy = 4; dummy'),
  typeof dummy, evalProbe('let item = 7; item'), evalProbe('typeof F')
];
