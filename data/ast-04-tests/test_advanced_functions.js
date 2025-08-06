// Test default parameters
function greet(name) {
    // Default parameter simulation
    if (name === undefined) {
        name = "World";
    }
    return "Hello, " + name;
}

// Test async function (if supported)
async function fetchData() {
    console.log("Fetching...");
    return Promise.resolve(42);
}

// Test generator function
function* counter() {
    let i = 0;
    while (true) {
        yield i++;
    }
}

// Test constructor function (old-style class)
function Person(name, age) {
    this.name = name;
    this.age = age;
}

// Instance method
Person.prototype.greet = function() {
    return "Hi, I'm " + this.name;
};

// Static method
Person.create = function(name, age) {
    return new Person(name, age);
};

// Test to distinguish method vs function
const obj = {
    method: function() {
        return "I'm a method";
    },
    arrowMethod: () => {
        return "I'm an arrow function";
    }
};

// Call them
console.log(greet());
console.log(greet("Alice"));

// Test if async works
try {
    fetchData().then(console.log);
} catch (e) {
    console.log("Async not supported:", e.message);
}

// Test if generators work
try {
    const gen = counter();
    console.log(gen.next().value);
    console.log(gen.next().value);
} catch (e) {
    console.log("Generators not supported:", e.message);
}

const person = new Person("Bob", 30);
console.log(person.greet());
console.log(Person.create("Charlie", 25));

console.log(obj.method());
console.log(obj.arrowMethod());