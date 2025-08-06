// Test variable hoisting
function testHoisting() {
    console.log(x); // undefined (var is hoisted)
    console.log(y); // ReferenceError (let has TDZ)
    
    var x = 5;
    let y = 10;
    const z = 15;
    
    // Function hoisting
    console.log(inner()); // Works - function is hoisted
    
    function inner() {
        return "hoisted function";
    }
    
    // Var in block scope
    if (true) {
        var blockVar = 20; // Hoisted to function scope
        let blockLet = 25; // Block scoped
    }
    
    console.log(blockVar); // 20 - accessible
    console.log(blockLet); // ReferenceError - not accessible
}

// Test var vs let in loops
function testLoops() {
    // Var in for loop
    for (var i = 0; i < 3; i++) {
        setTimeout(() => console.log("var:", i), 100); // All print 3
    }
    
    // Let in for loop  
    for (let j = 0; j < 3; j++) {
        setTimeout(() => console.log("let:", j), 100); // Prints 0, 1, 2
    }
    
    console.log(i); // 3 - accessible outside loop
    console.log(j); // ReferenceError - not accessible
}