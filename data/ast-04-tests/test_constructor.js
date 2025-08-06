// Test constructor patterns
function Person(name, age) {
    this.name = name;
    this.age = age;
    this.greet = function() {
        console.log("Hello, I'm " + this.name);
    };
}

// ES6 class (which compiles to constructor function)
class Animal {
    constructor(species) {
        this.species = species;
        this.alive = true;
    }
    
    makeSound() {
        console.log("Some sound");
    }
}

// Regular function (not a constructor)
function add(a, b) {
    return a + b;
}

// Method-like function
const obj = {
    method: function(x) {
        return x * 2;
    }
};

// Test usage
const p = new Person("Alice", 30);
const a = new Animal("Dog");