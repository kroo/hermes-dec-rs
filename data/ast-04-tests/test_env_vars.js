// Test environment variable naming with closures
function outer() {
    let counter = 0;
    let message = "Hello";
    let temp = 42;
    
    function increment() {
        counter = counter + 1;
        console.log(message + " count: " + counter);
        return counter;
    }
    
    function decrement() {
        counter = counter - 1;
        return counter;
    }
    
    function getMessage() {
        let localVar = "World";
        return message + " " + localVar + " " + counter;
    }
    
    // Use temp only once
    console.log(temp);
    
    increment();
    increment();
    decrement();
    
    return getMessage();
}

console.log(outer());