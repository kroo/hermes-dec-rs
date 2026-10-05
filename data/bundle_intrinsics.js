(function() {
  const weakGet = WeakMap.prototype.get, weakSet = WeakMap.prototype.set;
  const mapHas = Map.prototype.has, mapGet = Map.prototype.get, mapSet = Map.prototype.set;
  const slice = Array.prototype.slice;
  function poison() { throw new Error('private runtime consulted replaced intrinsic'); }
  try {
    WeakMap.prototype.get = WeakMap.prototype.set = poison;
    Map.prototype.has = Map.prototype.get = Map.prototype.set = poison;
    Array.prototype.slice = poison;
    function outer(value) { return function(step) { return value + step; }; }
    function Constructor(value) { this.value = value; }
    function rest(...values) { return values; }
    function tag(strings, value) { return [strings[0], value]; }
    globalThis.intrinsicResults = [outer(4)(3), new Constructor(9).value, rest(1,2,3), tag`value:${5}`];
  } finally {
    WeakMap.prototype.get = weakGet; WeakMap.prototype.set = weakSet;
    Map.prototype.has = mapHas; Map.prototype.get = mapGet; Map.prototype.set = mapSet;
    Array.prototype.slice = slice;
  }
})();
