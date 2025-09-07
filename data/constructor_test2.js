// Test custom constructors and built-ins
function testCustomConstructor() {
    function Person(name, age) {
        this.name = name;
        this.age = age;
    }
    
    function Animal(type) {
        this.type = type;
    }
    
    const p1 = new Person("Alice", 30);
    const p2 = new Person("Bob", 25);
    const a1 = new Animal("Dog");
    
    // Test with conditional
    const p3 = Math.random() > 0.5 ? new Person("Charlie", 35) : new Animal("Cat");
    
    return {p1, p2, a1, p3};
}

// Test Map, Set, and other built-ins
function testBuiltins() {
    const map = new Map();
    const set = new Set();
    const weakMap = new WeakMap();
    const weakSet = new WeakSet();
    const promise = new Promise(function(resolve) { resolve(42); });
    
    return {map, set, weakMap, weakSet, promise};
}

testCustomConstructor();
testBuiltins();
