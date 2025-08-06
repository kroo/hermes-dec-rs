// Test rest parameters
function sum(...numbers) {
    let total = 0;
    for (let num of numbers) {
        total += num;
    }
    return total;
}

// Test with regular params and rest
function greet(greeting, ...names) {
    return greeting + " " + names.join(", ");
}

console.log(sum(1, 2, 3, 4, 5));
console.log(greet("Hello", "Alice", "Bob", "Charlie"));