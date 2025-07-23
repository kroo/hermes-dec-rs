const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function ifElseTest(x) {
  if (x > 10) {
    return "large";
  }
  return "small";
}

function forLoopTest(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
    if (sum > 50) break;
  }
  return sum;
}

function switchTest(val) {
  switch (val) {
    case "a":
      return 1;
    case "b":
      return 2;
    default:
      return 0;
  }
}

function whileTest(n) {
  let result = 1;
  while (n > 0) {
    result *= n;
    n--;
  }
  return result;
}

function tryCatchTest(fn) {
  try {
    return fn();
  } catch {
    return "error";
  }
}

async function asyncAwaitTest() {
  await sleep(100);
  return "done";
}

function promiseChainTest() {
  return Promise.resolve(5)
    .then((v) => v * 2)
    .catch(() => 0);
}

function generatorTest(max) {
  function* gen() {
    for (let i = 0; i < max; i++) {
      yield i;
    }
  }
  return [...gen()];
}

function callbackTest(cb) {
  setTimeout(() => cb("callback"), 50);
}
