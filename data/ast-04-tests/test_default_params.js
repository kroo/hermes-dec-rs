// Test for default parameters
function greet(name = "World") {
    console.log("Hello, " + name);
}

function add(a = 0, b = 0) {
    return a + b;
}

// Call with default
greet();
greet("Alice");

// Call with partial defaults
console.log(add());
console.log(add(5));
console.log(add(5, 3));