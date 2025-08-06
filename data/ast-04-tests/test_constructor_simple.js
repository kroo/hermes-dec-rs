// Test constructor patterns
function Person(name, age) {
    this.name = name;
    this.age = age;
    this.greet = function() {
        console.log("Hello, I'm " + this.name);
    };
}

// Regular function (not a constructor)
function add(a, b) {
    return a + b;
}

// Factory function (not a constructor but creates objects)
function createPoint(x, y) {
    return {
        x: x,
        y: y
    };
}

// Test usage
var p = new Person("Alice", 30);
p.greet();